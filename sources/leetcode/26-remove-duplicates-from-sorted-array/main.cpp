class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        auto size = nums.size();
        auto copyTo = 0;
        
        for(int i = 0;i < size;)
        {
            //std::cout << "for" << i << std::endl;
            
            nums[copyTo] = nums[i];
            auto& current = nums[i];
     
            do
            {
                ++i;
                //std::cout << i << std::endl;
            }
            while((i < size) && (current == nums[i]));
            
            ++copyTo;
        }
        
        return copyTo;
    }
};