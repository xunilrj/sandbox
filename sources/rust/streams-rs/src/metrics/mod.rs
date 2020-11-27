use std::time::*;

struct PerSecond
{
    zero_timestamp: u64,
    last_index: u64,
    seconds: [u64; 60]
}

impl PerSecond
{
    pub fn new() -> PerSecond
    {
        let start = SystemTime::now();
        Self {
            zero_timestamp: 0,
            last_index: 0,
            seconds: [0u64; 60]
        }
    }

    pub fn incr(&mut self, timestamp: u64, count: u64) -> Option<(u64,u64)>
    {
        use chrono::{TimeZone, Utc, Timelike};

        let return_zero_timestamp = self.zero_timestamp;
        let return_idx = self.last_index;
        let return_count = self.seconds[return_idx as usize];

        let seconds = timestamp.rem_euclid(60);
        let minute = timestamp - seconds;

        if minute >= (self.zero_timestamp + 60) {
            self.zero_timestamp = minute;
            self.seconds = [0;60];
        }

        self.seconds[seconds as usize] += count;
        self.last_index = seconds;

        if return_idx != seconds {            
            Some((return_zero_timestamp + return_idx, return_count))
        } else {
            None
        }
    }
}

#[test]
fn name() {
    use chrono::{TimeZone, Utc, Timelike};

    let mut s = PerSecond::new();
    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 0, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 0, 0).timestamp() as u64, 1); 
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 1, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 1, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 59, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 19, 59, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 20, 0, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);

    let rate = s.incr(Utc.ymd(2020, 11, 23).and_hms_milli(19, 20, 1, 0).timestamp() as u64, 1);   
    println!("{:?}", rate);
}