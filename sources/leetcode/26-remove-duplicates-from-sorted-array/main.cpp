class Solution {
public:
    int removeDuplicates(vector<int>& nums)
    {
        size_t size = nums.size();
        if(size == 0) return 0;
        
        size_t copyTo = 1;
        for(size_t i = 1;i < size;++i)
        {
            if(nums[i-1] != nums[i])
            {
                nums[copyTo] = nums[i];    
                ++copyTo;
            }
        }
        
        return copyTo;
    }
};