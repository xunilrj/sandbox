# LRU Cache

to read my tutorial click [here](../../cpp/lru/readme.md)

# What is LRU

The Magic of LRU Cache (100 Days of Google Dev)  
https://www.youtube.com/watch?v=R5ON3iwx78M  

# Result

```
Runtime: 104 ms, (Your runtime beats 90.34%% of cpp submissions)
Memory Usage: 40.1 MB, (Your runtime beats 32.93% of cpp submissions)
```
https://leetcode.com/submissions/detail/292957724/  

# Possible Implementations

LeetCode 146. LRU Cache (Algorithm Explained)  
https://www.youtube.com/watch?v=8-FZRAjR7qU  

LRU cache design  
```
A linked list + hashtable of pointers to the linked list nodes is 
the usual way to implement LRU caches. 
```
https://stackoverflow.com/questions/2504178/lru-cache-design  
  
How to implement LRU cache using HashMap and Doubly Linked List  
```
So our Implementation of LRU cache will have HashMap and Doubly LinkedList. 
In Which HashMap will hold the keys and address of the Nodes of Doubly LinkedList. 
And Doubly LinkedList will hold the values of keys.
```
https://medium.com/@krishankantsinghal/my-first-blog-on-medium-583159139237  

## Hash Map + Linked List

```c++
class LRUCache {
...    
    struct kv
    {
        int key;
        int value;
    };
    int max;
    std::list<kv> lru;
    using iterator = decltype(lru)::iterator;
    std::unordered_map<int, iterator> data;
    using map_iterator = decltype(data)::iterator;
};
```
see [linkedlist.hash.cpp](./linkedlist.hash.cpp)

## About std::unordered_map vs std::map

std::map  
https://en.cppreference.com/w/cpp/container/map

std::unordered_map  
https://en.cppreference.com/w/cpp/container/unordered_map

C++ STL (Standard Template Library) Part-3 : Map, MultiMap, Unordered Map and Unordered MultiMap  
https://www.youtube.com/watch?v=TVgNpUWuhWs  

Alan Talbot “How to Choose the Right Standard Library Container, and Why You Should Want Some More”  
https://www.youtube.com/watch?v=yjPKVOYcw28

CppCon 2015: Sean Parent "Better Code: Data Structures"  
https://www.youtube.com/watch?v=sWgDk-o-6ZE  

CppCon 2014: Chandler Carruth "Efficiency with Algorithms, Performance with Data Structures"  
https://www.youtube.com/watch?v=fHNmRkzxHWs  

C++Now 2018: You Can Do Better than std::unordered_map: New Improvements to Hash Table Performance  
https://www.youtube.com/watch?v=M2fKMP47slQ  

CppCon 2017: Matt Kulukundis “Designing a Fast, Efficient, Cache-friendly Hash Table, Step by Step”  
https://www.youtube.com/watch?v=ncHmEUmJZf4  

CppCon 2019: Matt Kulukundis “Abseil's Open Source Hashtables: 2 Years In”
https://www.youtube.com/watch?v=JZE3_0qvrMg

# Papers

2WQ: A Low Overhead High Performance Buffer Management Replacement Algorithm *  
http://www.vldb.org/conf/1994/P439.PDF  

The LRU-K Page Replacement Algorithm For Database Disk Buffering  
http://www.cs.cmu.edu/~christos/courses/721-resources/p297-o_neil.pdf  