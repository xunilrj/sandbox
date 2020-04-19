# Submissions

https://leetcode.com/xunilrj/

# Algorithms

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

## Lowest Common Ancestor of a Binary Tree

```
Runtime: 16 ms (Your runtime beats 95.94 % of cpp submissions.)
Memory Usage: 14.7 MB (Your runtime beats 100 % of cpp submissions.)
```
https://leetcode.com/submissions/detail/327338526/

![alt text](./lowest-common-ancestor-of-a-binary-tree.cpu.png "Lowest Common Ancestor of a Binary Tree - CPU")
![alt text](./lowest-common-ancestor-of-a-binary-tree.mem.png "Lowest Common Ancestor of a Binary Tree - Memory")

[See More](./146-lru-cache/Readme.md) 

# Data Structures

## LRU Cache

Runtime: 104 ms  
Memory Usage: 40.1 MB  
https://leetcode.com/submissions/detail/292957724/  

![alt text](./LRUCache.cpu.png "LRU Cache - CPU")  
![alt text](./LRUCache.mem.png "LRU Cache - Memory")  

[More LRU Cache Details](./146-lru-cache/Readme.md)  
[LRU Cache Tutorial](https://github.com/xunilrj/sandbox/blob/master/sources/cpp/lru/readme.md)  

# Stream processing

## Sliding Window Median

Runtime: 88 ms  
Memory Usage: 15.2 MB  
https://leetcode.com/submissions/detail/322571550/  

![alt text](./SlidingWindowMedian.cpu.png "Sliding Window Median - CPU")  
![alt text](./SlidingWindowMedian.mem.png "Sliding Window Median - Memory")  

[More LRU Cache Details](./146-lru-cache/Readme.md)  