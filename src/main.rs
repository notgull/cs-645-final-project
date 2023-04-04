use piet::{FontFamily, FontWeight, RenderContext as _, Text, TextAttribute, TextLayoutBuilder};

use piet::kurbo::{Affine, Line, Point, Rect, RoundedRect, Vec2};
use slab::Slab;

use std::cell::RefCell;
use std::fmt::Write as _;
use std::ops::Shr;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use winit::dpi::{LogicalSize, PhysicalPosition};
use winit::event::{ElementState, Event, MouseButton, MouseScrollDelta, WindowEvent};
use winit::event_loop::{EventLoop, EventLoopBuilder};
use winit::window::WindowBuilder;

use theo::{Brush, Display, RenderContext, TextLayout};

const COOL_NAMES: &[&str] = &[
    "snake",
    "solid",
    "liquid",
    "raiden",
    "mario",
    "kirby",
    "pikachu",
    "hulk",
    "spiderman",
    "metaverse",
];

fn main() {
    tracing_subscriber::fmt::init();
    main2(EventLoopBuilder::new().build())
}

fn main2(evl: EventLoop<()>) {
    let window_builder = || {
        WindowBuilder::new()
            .with_title("CS 645 Final Project")
            .with_inner_size(LogicalSize::new(800, 600))
            .with_transparent(true)
    };
    let mut window = None;

    // Create the display.
    let mut display = {
        let mut builder = Display::builder();

        #[cfg(windows)]
        {
            let start = window_builder()
                .build(&evl)
                .expect("Failed to create window");
            builder = builder.window(&start);
            window = Some(start);
        }

        #[cfg(all(
            unix,
            not(any(target_os = "ios", target_os = "macos", target_os = "android"))
        ))]
        {
            builder = builder.glx_error_hook(winit::platform::x11::register_xlib_error_hook);
        }

        unsafe { builder.build(&evl).expect("Failed to create display") }
    };

    // Framerate setup.
    let framerate = Duration::from_millis({
        let fraction = 1.0 / 60.0;
        (fraction * 1000.0) as u64
    });
    let mut next_frame = Instant::now() + framerate;

    let mut state = None;
    let mut dragging = DragState::NotDragging;
    let mut network = Network::new(0xDEADBEEF);
    network.spawn_packet_driver();

    // Run the event loop.
    evl.run(move |event, elwt, control_flow| {
        control_flow.set_wait_until(next_frame);

        match event {
            Event::Resumed => {
                // Create a window and a theo surface.
                let window = window.take().unwrap_or_else(|| {
                    let mut window_builder = window_builder();

                    if !display.supports_transparency() {
                        window_builder = window_builder.with_transparent(false);
                    }

                    #[cfg(all(
                        unix,
                        not(any(target_os = "ios", target_os = "macos", target_os = "android"))
                    ))]
                    {
                        use winit::platform::x11::WindowBuilderExtX11;

                        if let Some(visual) = display.x11_visual() {
                            window_builder = window_builder.with_x11_visual(visual.as_ptr());
                        }
                    }

                    window_builder.build(elwt).expect("Failed to create window")
                });

                // Create a new theo surface.
                let size = window.inner_size();
                let surface = unsafe {
                    display
                        .make_surface(&window, size.width, size.height)
                        .expect("Failed to create surface")
                };

                // Save the state.
                state = Some((window, surface));
            }

            Event::Suspended => {
                // Bail out of the window and GL context.
                state = None;
                dragging = DragState::NotDragging;
            }

            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => control_flow.set_exit(),

            Event::WindowEvent {
                event:
                    WindowEvent::MouseInput {
                        state,
                        button: MouseButton::Left,
                        ..
                    },
                ..
            } => match state {
                ElementState::Pressed => dragging = DragState::Dragging { last_point: None },
                ElementState::Released => dragging = DragState::NotDragging,
            },

            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                ..
            } => {
                if let DragState::Dragging { last_point } = &mut dragging {
                    if let Some(last_point) = last_point.replace(position) {
                        let delta = Vec2::new(position.x - last_point.x, position.y - last_point.y);
                        network.origin += delta;
                    }
                }
            }

            Event::WindowEvent {
                event:
                    WindowEvent::MouseWheel {
                        delta: MouseScrollDelta::LineDelta(_, y),
                        ..
                    },
                ..
            } => {
                network.scale *= 1.0 + y as f64 * 0.1;
            }

            Event::RedrawEventsCleared => {
                if Instant::now() < next_frame {
                    return;
                }

                // Use the surface to draw.
                if let Some((window, surface)) = state.as_mut() {
                    let size = window.inner_size();
                    let mut render_context =
                        RenderContext::new(&mut display, surface, size.width, size.height)
                            .expect("Failed to create render context");

                    // Draw the frame.
                    render_context.clear(None, piet::Color::PURPLE);
                    network.draw(&mut render_context);

                    // Clear and flush.
                    render_context
                        .finish()
                        .expect("Failed to finish render context");
                    render_context
                        .status()
                        .expect("Failed to flush render context");
                }

                next_frame += framerate;
                control_flow.set_wait_until(next_frame);
            }

            _ => {}
        }
    })
}

enum DragState {
    NotDragging,
    Dragging {
        last_point: Option<PhysicalPosition<f64>>,
    },
}

#[derive(Default)]
struct Network {
    // Nodes and edges.
    nodes: RefCell<Slab<Node>>,
    edges: Vec<(usize, usize)>,
    update_frequency: Arc<Mutex<Duration>>,

    // The packet.
    packet_state: Arc<Mutex<Option<Packet>>>,

    // Drawing utilities.
    origin: Point,
    scale: f64,
    black_brush: Option<Brush>,
    orange_brush: Option<Brush>,
}

const NODE_X_SPACING: f64 = NODE_WIDTH + 100.0;
const NODE_Y_SPACING: f64 = NODE_HEIGHT + 100.0;

impl Network {
    fn new(seed: u64) -> Self {
        let rand = fastrand::Rng::with_seed(seed);

        let mut network = Network {
            scale: 1.0,
            update_frequency: Arc::new(Mutex::new(Duration::from_millis(500))),
            ..Default::default()
        };

        // Generate broader internet.
        let width = 10;
        let node_count = rand.usize(50..100);
        for i in 0..node_count {
            let origin = {
                let x = i % width;
                let y = i / width;

                Point::new(x as f64 * NODE_X_SPACING, y as f64 * NODE_Y_SPACING)
            };

            let ip = loop {
                let ip = rand.u32(0x1000_0000..=0xEFFF_FFFF);

                if ip & 0xFFFF_FF00 != 0xC0A8_0100 {
                    break ip;
                }
            };

            let malicious = if rand.u8(..) & 0xF == 0xF {
                NodeType::Malicious
            } else {
                NodeType::Normal
            };

            let vacant = network.nodes.borrow_mut().vacant_key();
            network.nodes.get_mut().insert(Node {
                shared: Arc::new(SharedNode {
                    name: None,
                    ip,
                    ty: malicious,
                    color: if malicious == NodeType::Malicious {
                        piet::Color::RED
                    } else {
                        piet::Color::TEAL
                    },
                    posn: origin,
                    index: vacant,
                }),
                brush: None,
                text: None,
            });
        }

        // Link two nodes together.
        let mut link_two_nodes = |start: usize, end: usize| {
            let end_posn = network.nodes.borrow()[end].rectangle().origin();

            let mut current = start;
            let mut i = 0;

            while current != end {
                // Get the index of every neighbor of the current node.
                let neighbors = {
                    let mut neighbors = vec![
                        current.saturating_sub(width).saturating_sub(1),
                        current.saturating_sub(width),
                        current.saturating_sub(width).saturating_add(1),
                        current.saturating_sub(1),
                        current.saturating_add(1),
                        current.saturating_add(width).saturating_sub(1),
                        current.saturating_add(width),
                        current.saturating_add(width).saturating_add(1),
                    ];

                    // Remove any neighbors that are out of bounds.
                    neighbors.sort_unstable();
                    neighbors.dedup();
                    neighbors.retain(|&index| index < network.nodes.borrow().len());

                    let current_origin = network.nodes.borrow()[current].rectangle().origin();
                    neighbors.retain(|&index| {
                        let index_origin = network.nodes.borrow()[index].rectangle().origin();
                        (index_origin - current_origin).hypot() <= (NODE_WIDTH * 3.0)
                    });

                    neighbors
                };

                if neighbors.is_empty() {
                    panic!("No neighbors found! ({} -> {})", start, end);
                }

                // Find the neighbor closest to the end node.
                let closest_neighbor = {
                    // If any of the neighbors are the end node, we're done.
                    if neighbors.contains(&end) {
                        end
                    } else {
                        neighbors
                            .iter()
                            .min_by_key(|&&index| {
                                let neighbor_posn =
                                    network.nodes.borrow()[index].rectangle().origin();
                                (neighbor_posn - end_posn).hypot() as i32
                            })
                            .copied()
                            .unwrap()
                    }
                };

                // Add the edge if it doesn't already exist.
                if !network.edges.contains(&(current, closest_neighbor)) {
                    network.edges.push((current, closest_neighbor));
                    network.edges.push((closest_neighbor, current));
                }

                tracing::info!("[{}->{}]: {} -> {}", start, end, current, closest_neighbor);

                current = closest_neighbor;

                i += 1;
                if i > 10_000 {
                    panic!("Infinite loop detected! ({} -> {})", start, end);
                }
            }
        };

        // Start the tree off with two nodes.
        let mut linked_nodes = {
            let (n1, n2) = loop {
                let n1 = rand.usize(..network.nodes.borrow().len());
                let n2 = rand.usize(..network.nodes.borrow().len());

                if n1 != n2 {
                    break (n1, n2);
                }
            };

            link_two_nodes(n1, n2);
            vec![n1, n2]
        };

        // Add more nodes until all nodes are connected.
        loop {
            let mut progress_made = false;

            for i in 0..node_count {
                if !linked_nodes.contains(&i) {
                    let start = {
                        let index = rand.usize(..linked_nodes.len());
                        linked_nodes[index]
                    };
                    link_two_nodes(start, i);
                    linked_nodes.push(i);
                    progress_made = true;
                    break;
                }
            }

            if !progress_made {
                break;
            }
        }

        // Add the router node to the network.
        let max_height = network
            .nodes
            .borrow()
            .iter()
            .map(|(_, node)| node.rectangle().max_y())
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap();

        let vacant = network.nodes.borrow_mut().vacant_key();
        let router = network.nodes.borrow_mut().insert(Node {
            shared: Arc::new(SharedNode {
                name: Some("Router".to_string()),
                ip: rand.u32(0x1000_0000..=0xEFFF_FFFF),
                ty: NodeType::Router,
                color: piet::Color::GREEN,
                posn: Point::new((width as f64 * NODE_X_SPACING) / 2.0, max_height + 300.0),
                index: vacant,
            }),
            brush: None,
            text: None,
        });

        // Link to all of the bottom nodes.
        for i in (node_count - width)..node_count {
            network.edges.push((router, i));
        }

        // Add the target nodes to the network.
        for (i, name) in COOL_NAMES.iter().enumerate() {
            let vacant = network.nodes.borrow_mut().vacant_key();
            let site = network.nodes.borrow_mut().insert(Node {
                shared: Arc::new(SharedNode {
                    name: Some(format!("{name}.cs645.net")),
                    ip: rand.u32(0xC0A8_0101..=0xC0A8_01FF),
                    ty: NodeType::Site,
                    color: piet::Color::OLIVE,
                    posn: (i as f64 * NODE_X_SPACING, max_height + 600.0).into(),
                    index: vacant,
                }),
                brush: None,
                text: None,
            });

            network.edges.push((router, site));
        }

        network
    }

    fn spawn_packet_driver(&self) {
        let nodes = self
            .nodes
            .borrow()
            .iter()
            .map(|(_, node)| node.shared.clone())
            .collect::<Vec<_>>();
        let edges = self.edges.clone();
        let frequency = self.update_frequency.clone();
        let packet_lock = self.packet_state.clone();

        thread::Builder::new()
            .name("Packet Driver".to_string())
            .spawn(move || {
                let rng = fastrand::Rng::new();

                let wait = move || {
                    let frequency = *frequency.lock().unwrap();
                    thread::sleep(frequency);
                };

                loop {
                    // Choose either a malicious or benign packet.
                    let desired_type = if rng.u8(..) & 1 == 0 {
                        NodeType::Malicious
                    } else {
                        NodeType::Normal
                    };
                    let chosen_sources = nodes
                        .iter()
                        .filter(|node| node.ty == desired_type)
                        .collect::<Vec<_>>();

                    // Destination is always a site.
                    let chosen_destinations = nodes
                        .iter()
                        .filter(|node| node.ty == NodeType::Site)
                        .collect::<Vec<_>>();

                    // Choose a random source and destination.
                    let source = rng.usize(..chosen_sources.len());
                    let destination = rng.usize(..chosen_destinations.len());
                    let source = chosen_sources[source].index;
                    let destination = chosen_destinations[destination].index;

                    // Generate a path between them.
                    let mut path = pathfinder(&nodes, &edges, source, destination);
                    path.insert(0, source);

                    let traverse_path = |path: &[usize]| {
                        let is_malicious = matches!(desired_type, NodeType::Malicious);

                        for (i, item) in path.iter().copied().enumerate() {
                            // Put the packet on the current node.
                            let mut packet = packet_lock.lock().unwrap();
                            *packet = Some(Packet {
                                posn: nodes[item].rectangle().center(),
                                malicious: is_malicious,
                            });
                            drop(packet);

                            wait();

                            // If this isn't the end, also put the packet on the edge.
                            if i != path.len() - 1 {
                                let mut packet = packet_lock.lock().unwrap();
                                let centerpoint = {
                                    let c1 = nodes[item].rectangle().center();
                                    let c2 = nodes[path[i + 1]].rectangle().center();

                                    c1.midpoint(c2)
                                };

                                *packet = Some(Packet {
                                    posn: centerpoint,
                                    malicious: is_malicious,
                                });
                                drop(packet);

                                wait();
                            }
                        }
                    };

                    traverse_path(&path);

                    wait();
                }
            })
            .expect("Failed to spawn packet driver thread!");
    }

    fn draw(&mut self, context: &mut RenderContext<'_, '_>) {
        let black_brush = self
            .black_brush
            .get_or_insert_with(|| context.solid_brush(piet::Color::rgb(0.1, 0.2, 0.1)));
        let orange_brush = self
            .orange_brush
            .get_or_insert_with(|| context.solid_brush(piet::Color::rgb(0.9, 0.6, 0.1)));

        context
            .with_save(|context| {
                // Transform.
                context.transform(
                    Affine::translate(self.origin.to_vec2()) * Affine::scale(self.scale),
                );

                // Draw edges.
                for (start, end) in self.edges.iter() {
                    let get_origin = |index| self.nodes.borrow()[index].rectangle().center();

                    let (start_pt, end_pt) = (get_origin(*start), get_origin(*end));
                    context.stroke(Line::new(start_pt, end_pt), orange_brush, 20.0)
                }

                // Draw nodes.
                for (_, node) in self.nodes.borrow_mut().iter_mut() {
                    node.draw(context, black_brush);
                }

                // Draw the packet.
                if let Some(packet) = &mut *self.packet_state.lock().unwrap() {
                    packet.draw(context);
                }

                Ok(())
            })
            .unwrap();
    }
}

struct Node {
    // Node data.
    shared: Arc<SharedNode>,

    // Drawing utilities.
    brush: Option<Brush>,
    text: Option<TextLayout>,
}

struct SharedNode {
    name: Option<String>,
    ip: u32,
    ty: NodeType,
    color: piet::Color,
    posn: Point,
    index: usize,
}

const NODE_WIDTH: f64 = 175.0;
const NODE_HEIGHT: f64 = 125.0;

impl SharedNode {
    fn rectangle(&self) -> Rect {
        Rect::from_origin_size(self.posn, (NODE_WIDTH, NODE_HEIGHT))
    }
}

impl Node {
    fn rectangle(&self) -> Rect {
        self.shared.rectangle()
    }

    fn draw(&mut self, context: &mut RenderContext<'_, '_>, outline_brush: &Brush) {
        const NODE_RADIUS: f64 = 10.0;

        let rect = RoundedRect::from_rect(self.rectangle(), NODE_RADIUS);
        let brush = self
            .brush
            .get_or_insert_with(|| context.solid_brush(self.shared.color));
        let layout = self.text.get_or_insert_with(|| {
            let text = {
                let ip_octets = self.shared.ip.to_be_bytes();
                let mut text = "IP: ".to_string();

                // Write the IP address.
                for (i, octet) in ip_octets.iter().enumerate() {
                    write!(text, "{}{}", if i > 0 { "." } else { "" }, octet).unwrap();
                }

                // Write the node name, if any.
                if let Some(name) = &self.shared.name {
                    write!(text, "\nName: {name}").unwrap();
                }

                text
            };

            context
                .text()
                .new_text_layout(text)
                .font(FontFamily::SANS_SERIF, 12.0)
                .default_attribute(TextAttribute::Weight(FontWeight::BOLD))
                .max_width(NODE_WIDTH - 10.0)
                .text_color(piet::Color::rgb(0.1, 0.1, 0.1))
                .build()
                .expect("Failed to build text layout")
        });

        context.fill(rect, brush);
        context.stroke(rect, outline_brush, 5.0);
        let text_origin = {
            let mut text_origin = rect.origin();
            text_origin.x += 5.0;
            text_origin
        };
        context.draw_text(layout, text_origin);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum NodeType {
    Normal,
    Malicious,
    Router,
    Site,
}

struct Packet {
    /// The position of the packet.
    posn: Point,

    /// Is this a malicious packet?
    malicious: bool,
}

impl Packet {
    fn rectangle(&self) -> Rect {
        const PACKET_WIDTH: f64 = 100.0;
        const PACKET_HEIGHT: f64 = 50.0;

        Rect::from_center_size(self.posn, (PACKET_WIDTH, PACKET_HEIGHT))
    }

    fn draw(&self, context: &mut RenderContext<'_, '_>) {
        const PACKET_RADIUS: f64 = 10.0;

        let rect = RoundedRect::from_rect(self.rectangle(), PACKET_RADIUS);
        let brush = context.solid_brush(if self.malicious {
            piet::Color::rgb(0.9, 0.1, 0.1)
        } else {
            piet::Color::rgb(0.1, 0.1, 0.9)
        });
        let outline_brush = context.solid_brush(if self.malicious {
            piet::Color::rgb(0.7, 0.1, 0.1)
        } else {
            piet::Color::rgb(0.1, 0.1, 0.7)
        });

        context.fill(rect, &brush);
        context.stroke(rect, &outline_brush, 5.0);
    }
}

/// Find the path between two nodes.
fn pathfinder(
    nodes: &[Arc<SharedNode>],
    edges: &[(usize, usize)],
    start: usize,
    end: usize,
) -> Vec<usize> {
    use std::cmp::{self, Reverse};
    use std::collections::BinaryHeap;

    struct HeapEntry {
        distance: f64,
        node: usize,
    }

    impl PartialEq for HeapEntry {
        fn eq(&self, other: &Self) -> bool {
            self.distance == other.distance
        }
    }

    impl Eq for HeapEntry {}

    impl PartialOrd for HeapEntry {
        fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
            other.distance.partial_cmp(&self.distance)
        }
    }

    impl Ord for HeapEntry {
        fn cmp(&self, other: &Self) -> cmp::Ordering {
            other.distance.partial_cmp(&self.distance).unwrap()
        }
    }

    // Use Dijkstra's algorithm to find the shortest path between the two nodes.
    let mut distances = vec![std::f64::INFINITY; nodes.len()];
    let mut previous = vec![None; nodes.len()];

    distances[start] = 0.0;

    let mut queue = BinaryHeap::new();
    queue.push(Reverse(HeapEntry {
        distance: 0.0,
        node: start,
    }));

    while let Some(Reverse(HeapEntry { distance, node })) = queue.pop() {
        if distance > distances[node] {
            continue;
        }

        for (start, end) in edges.iter() {
            let (start, end) = (*start, *end);
            let (start, end) = if start == node {
                (start, end)
            } else if end == node {
                (end, start)
            } else {
                continue;
            };

            let new_distance = distances[start] + nodes[start].posn.distance(nodes[end].posn);
            if new_distance < distances[end] {
                distances[end] = new_distance;
                previous[end] = Some(start);
                queue.push(Reverse(HeapEntry {
                    distance: new_distance,
                    node: end,
                }));
            }
        }
    }

    // Reconstruct the path.
    let mut path = Vec::new();
    let mut node = end;
    while let Some(prev) = previous[node] {
        path.push(node);
        node = prev;
    }

    path.reverse();
    path
}
