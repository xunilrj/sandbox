# Submissions

https://leetcode.com/xunilrj/

# Simple Algorithms

## Remove Duplicates from Sorted Array

Runtime: 20 ms  
Memory Usage: 7.6 MB  
https://leetcode.com/submissions/detail/321115997/  

![alt text](./RemoveDuplicatesFromSortedArray.cpu.png "Remove Duplicates from Sorted Array - CPU")
![alt text](./RemoveDuplicatesFromSortedArray.mem.png "Remove Duplicates from Sorted Array - Memory")

```c++
class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        auto size = nums.size();
        auto copyTo = 0;
        
        for(int i = 0;i < size;)
        {
            nums[copyTo] = nums[i];
            auto& current = nums[i];
     
            do
            {
                ++i;
            }
            while((i < size) && (current == nums[i]));
            
            ++copyTo;
        }
        
        return copyTo;
    }
};
```

# Data Structures

## LRU Cache

Runtime: 104 ms  
Memory Usage: 40.1 MB  
https://leetcode.com/submissions/detail/292957724/  

![alt text](./LRUCache.cpu.png "LRU Cache - CPU")
![alt text](./LRUCache.mem.png "LRU Cache - Memory")

[Code and Tutorial](./146-lru-cache/Readme.md)