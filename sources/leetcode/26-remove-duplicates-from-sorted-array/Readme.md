#Problem

https://leetcode.com/problems/remove-duplicates-from-sorted-array/  

```
Runtime: 20 ms (Your runtime beats 89.29% of cpp submissions.)
Memory Usage: 7.6 MB (Your runtime beats 100% of cpp submissions.)
```
https://leetcode.com/submissions/detail/321115997

# Solution

Iterate the array with two indices:
1 - The current item;
2 - The next free slot;

For every item, copy it to the "next free slot".
Fast skip all duplicate items, advance the "current item".
Advance "next free slot".
