use super::Packet;
use std::collections::HashMap;

/// The filter running on the router.
#[derive(Default)]
pub struct Filter {
    /// Current filtered IP.
    pub filters: Vec<FilteredIp>,

    /// Remembered IP.
    pub remembered: HashMap<u32, RememberedIp>,
}

pub struct FilteredIp {
    /// The IP address of the node.
    pub ip: u32,

    /// The tick to filter until.
    pub tick: u32,
}

pub struct RememberedIp {
    /// The IP address of the node.
    pub ip: u32,

    /// The duration to filter for.
    pub duration: RememberedIpState,
}

pub enum RememberedIpState {
    /// Grace period, if we get this number more flags, we filter.
    GracePeriod(usize),

    /// Filter for this number of ticks.
    Filtered(u32),
}

impl Filter {
    pub fn filters(&self, ip: u32) -> bool {
        self.filters.iter().any(|f| f.ip == ip)
    }

    pub fn tick(&mut self) {
        self.filters
            .iter_mut()
            .for_each(|f| f.tick = f.tick.saturating_sub(1));
        self.filters.retain(|f| f.tick != 0);
    }

    pub fn hit_ip(&mut self, ip: u32) {
        // Adjust to taste.
        const DEFAULT_HIT_COUNT: usize = 4;
        const STARTING_TICK: u32 = 5;

        let rem = self.remembered.entry(ip).or_insert(RememberedIp {
            ip,
            duration: RememberedIpState::GracePeriod(DEFAULT_HIT_COUNT),
        });
        match rem {
            RememberedIp {
                duration: RememberedIpState::GracePeriod(hits),
                ..
            } => match hits.checked_sub(1) {
                Some(new_hits) => {
                    *hits = new_hits;
                }

                None => {
                    rem.duration = RememberedIpState::Filtered(STARTING_TICK);
                }
            },

            RememberedIp {
                ip,
                duration: RememberedIpState::Filtered(tick),
            } => {
                *tick = tick.saturating_mul(2);

                if let Some(filt) = self.filters.iter_mut().find(|f| f.ip == *ip) {
                    filt.tick = *tick;
                }
            }
        }
    }
}

/// Represents the IDS.
pub struct Ids;

impl Ids {
    pub fn report(&self, packet: &Packet) -> Option<u32> {
        const FALSE_POSITIVE_RATE: f32 = 0.1;

        let mut is_malicious = packet.malicious;
        if fastrand::f32() < FALSE_POSITIVE_RATE {
            is_malicious = !is_malicious;
        }

        if is_malicious {
            Some(packet.ip)
        } else {
            None
        }
    }
}
