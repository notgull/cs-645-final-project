use piet::RenderContext as _;

use slab::Slab;

use std::time::{Duration, Instant};

use winit::dpi::LogicalSize;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{EventLoop, EventLoopBuilder};
use winit::window::WindowBuilder;

use theo::{Brush, Display, RenderContext};

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
    let mut network = Network::default();

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

                    #[cfg(x11_platform)]
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
            }

            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => control_flow.set_exit(),

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

#[derive(Default)]
struct Network {
    // Nodes and edges.
    nodes: Slab<Node>,
    edges: Vec<(usize, usize)>,

    // Drawing utilities.
    black_brush: Option<Brush>,
    orange_brush: Option<Brush>,
}

impl Network {
    fn draw<R: piet::RenderContext<Brush = Brush>>(&mut self, context: &mut R)
    where
        Brush: piet::IntoBrush<R>,
    {
        let rect = piet::kurbo::RoundedRect::new(25.0, 25.0, 200.0, 125.0, 10.0);

        let black_brush = self
            .black_brush
            .get_or_insert_with(|| context.solid_brush(piet::Color::rgb(0.1, 0.2, 0.1)));
        let orange_brush = self
            .orange_brush
            .get_or_insert_with(|| context.solid_brush(piet::Color::TEAL));

        context.fill(rect, orange_brush);
        context.stroke(rect, black_brush, 5.0);

        // TODO: Draw nodes.
    }
}

struct Node {
    name: Option<String>,
    ip: u32,
    color: piet::Color,
    posn: (f64, f64),
}
