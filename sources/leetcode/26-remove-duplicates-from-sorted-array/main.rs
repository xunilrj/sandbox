impl Solution
{
    pub fn remove_duplicates(nums: &mut Vec<i32>) -> i32
    {
        if(nums.len() == 0) { return 0; }
        
        let mut copyto: usize = 1;
        let mut i: usize = 1;
        
        while(i < nums.len())
        {
            if(nums[i] != nums[i-1])
            {
                nums[copyto] = nums[i];    
                copyto+=1;
            }
            i+=1;
        }
        
        return copyto as i32;
    }
}