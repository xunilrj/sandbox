# Binary Search Tree : Lowest Common Ancestor

https://www.hackerrank.com/challenges/binary-search-tree-lowest-common-ancestor/problem   
Solution: https://www.hackerrank.com/challenges/binary-search-tree-lowest-common-ancestor/submissions/code/154124208

# Explanation

Although this problem does not need I chose to solve it using Euler Tour.  
A more advanced implementation would build the segment tree whilst DFsing the tree (like the cp-algotihm one does).

For this particular problem is faster to calculate the LCA inside the Euler Tour like [here](../../leetcode/236-lowest-common-ancestor-of-a-binary-tree).

https://en.wikipedia.org/wiki/Euler_tour_technique
https://www.geeksforgeeks.org/euler-tour-tree  
https://www.geeksforgeeks.org/lca-n-ary-tree-constant-query-o1  
https://cp-algorithms.com/graph/lca.html  