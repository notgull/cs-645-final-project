use super::Packet;
use std::collections::HashMap;

/// The filter running on the router.
#[derive(Default, Clone)]
pub struct Filter {
    /// Current filtered IP.
    pub filters: Vec<FilteredIp>,

    /// Remembered IP.
    pub remembered: HashMap<u32, RememberedIp>,
}

#[derive(Clone)]
pub struct FilteredIp {
    /// The IP address of the node.
    pub ip: u32,

    /// The tick to filter until.
    pub tick: u32,
}

#[derive(Clone)]
pub struct RememberedIp {
    /// The IP address of the node.
    pub ip: u32,

    /// The duration to filter for.
    pub duration: RememberedIpState,
}

#[derive(Clone)]
pub enum RememberedIpState {
    /// Grace period, if we get this number more flags, we filter.
    GracePeriod {
        grace_period: usize,
        last_ticks: Option<u32>,
    },

    /// Filter for this number of ticks.
    Filtered(u32),
}

// Adjust to taste.
const DEFAULT_HIT_COUNT: usize = 2;
const STARTING_TICK: u32 = 2;

impl Filter {
    pub fn filters(&self, ip: u32) -> bool {
        self.filters.iter().any(|f| f.ip == ip)
    }

    pub fn tick(&mut self) {
        self.filters.iter_mut().for_each(|f| {
            f.tick = f.tick.saturating_sub(1);
            if f.tick == 0 {
                if let Some(rem) = self.remembered.get_mut(&f.ip) {
                    if let RememberedIpState::Filtered(tick) = &mut rem.duration {
                        rem.duration = RememberedIpState::GracePeriod {
                            grace_period: DEFAULT_HIT_COUNT,
                            last_ticks: Some(match tick.saturating_div(1) {
                                0 => 1,
                                x => x,
                            }),
                        };

                        // Randomly decide to remove it.
                        if fastrand::u8(..) & 8 == 8 {
                            self.remembered.remove(&f.ip);
                        }
                    }
                }
            }
        });
        self.filters.retain(|f| f.tick != 0);
    }

    pub fn hit_ip(&mut self, ip: u32) {
        let rem = self.remembered.entry(ip).or_insert(RememberedIp {
            ip,
            duration: RememberedIpState::GracePeriod {
                grace_period: DEFAULT_HIT_COUNT,
                last_ticks: None,
            },
        });
        match rem {
            RememberedIp {
                duration:
                    RememberedIpState::GracePeriod {
                        grace_period,
                        last_ticks,
                    },
                ..
            } => match grace_period.checked_sub(1) {
                Some(new_hits) => {
                    *grace_period = new_hits;
                }

                None => {
                    rem.duration = RememberedIpState::Filtered(last_ticks.unwrap_or(STARTING_TICK));
                }
            },

            RememberedIp {
                ip,
                duration: RememberedIpState::Filtered(tick),
            } => {
                *tick = tick.saturating_mul(2);

                if let Some(filt) = self.filters.iter_mut().find(|f| f.ip == *ip) {
                    filt.tick = *tick;
                } else {
                    self.filters.push(FilteredIp {
                        ip: *ip,
                        tick: *tick,
                    });
                }
            }
        }
    }
}

/// Represents the IDS.
pub struct Ids;

impl Ids {
    pub(crate) fn report(&self, packet: &Packet) -> ReportResult {
        const FALSE_POSITIVE_RATE: f32 = 0.10;

        let is_malicious = packet.malicious;
        let fp = fastrand::f32() < FALSE_POSITIVE_RATE;

        match (is_malicious, fp) {
            (true, true) => ReportResult::FalseNegative,
            (true, false) => ReportResult::TruePositive(packet.ip),
            (false, true) => ReportResult::FalsePositive(packet.ip),
            (false, false) => ReportResult::TrueNegative,
        }
    }
}

#[derive(Clone)]
pub enum ReportResult {
    FalsePositive(u32),
    FalseNegative,
    TruePositive(u32),
    TrueNegative,
}

impl ReportResult {
    pub fn positive(&self) -> Option<u32> {
        match self {
            ReportResult::FalsePositive(ip) => Some(*ip),
            ReportResult::TruePositive(ip) => Some(*ip),
            _ => None,
        }
    }
}
