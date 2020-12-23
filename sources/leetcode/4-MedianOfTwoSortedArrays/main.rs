struct Iterator<'a>(&'a Vec<i32>, usize, &'a Vec<i32>, usize);
impl<'a> Iterator<'a>
{
    fn advance(&mut self) -> bool {
        let a = self.0.get(self.1);
        let c = self.2.get(self.3);
        
        match (a,c) {
            (Some(a),Some(c))  => {
                if a <= c {
                    self.1 += 1;
                } else {
                    self.3 += 1;
                }
                true
            }
            (Some(a),None) => {self.1 += 1; true}
            (None,Some(c)) => {self.3 += 1; true}
            (None,None) => false,
        }
    }
    
    fn value(&self) -> Option<i32> {
        match (self.0.get(self.1), self.2.get(self.3)) {
            (Some(&a),Some(&b)) => Some(a.min(b)),
            (Some(&a),_) => Some(a),
            (_,Some(&b)) => Some(b),
            (None,None) => None
        }
    }
}

impl Solution {
    pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
        let size = nums1.len() + nums2.len();
        let mediani = if size % 2 == 0 {
            size / 2 - 1
        } else {
            size / 2
        };

        
        let mut i = Iterator(&nums1, 0, &nums2, 0);
        let mut counti = 0;
        loop {           
            //println!("{:?}", i.value());
                     
            if counti >= mediani {
                break;
            } else {
                i.advance();
                counti += 1;
            }
        }
        
        //println!("Median {:?}", i.value());
        if size % 2 == 0 {
            let a = i.value().unwrap();
            i.advance();
            let b = i.value().unwrap();
            (a + b) as f64 / 2.0f64
        } else {
            i.value().unwrap() as f64
        }
    }
}