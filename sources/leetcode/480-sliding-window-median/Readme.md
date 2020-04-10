# Sliding Window Median


# Result

```
Runtime: 88 ms (Your runtime beats 40.34 % of cpp submissions)  
Memory Usage: 15.2 MB  (Your memory beats 100% of cpp submissions)
```
https://leetcode.com/submissions/detail/322571550

# Possible Implementations

```
These routines implement a running median smoother according to the
 * algorithm described in Haerdle und Steiger (1995),
 *			  DOI:10.2307/2986349 , see ../man/runmed.Rd
```
https://svn.r-project.org/R/trunk/src/library/stats/src/Trunmed.c


```
Algorithm AS 296
Optimal Median Smoothing
Haerdle und Steiger (1995)
```
https://rss.onlinelibrary.wiley.com/doi/10.2307/2986349

```
I've implemented a rolling median calculator in C here (Gist).

It uses a max-median-min heap structure: 
The median is at heap[0] (which is at the center of a K-item array).

There is a minheap starting at heap[1], and a maxheap (using negative indexing) at heap[-1].
It's not exactly the same as the Turlach implementation from the R source:

This one supports values being inserted on-the-fly, while the R version acts on a whole buffer at once.
But I believe the time complexity is the same. And it could easily be used to implement 
a whole buffer version (possibly with with the addition of some code to handle R's "endrules").
```
https://stackoverflow.com/questions/5527437/rolling-median-in-c-turlach-implementation


```
The idea is to maintain two sorted sets (minSet and maxSet) of Pair 
objects of length (k / 2) and (k / 2) + 1 depending on whether k is 
even or odd, minSet will always contain the first set of 
numbers (smaller) of window k and maxSet will contain the second set of numbers (larger).

As we move our window, we will remove elements from either 
of the sets (log n) and add a new element (log n) maintaining 
the minSet and maxSet rule specified above.
```
https://www.geeksforgeeks.org/median-of-sliding-window-in-an-array

## About std::multiset

std::multiset  
https://en.cppreference.com/w/cpp/container/multiset  

How is std::multiset implemented?
```
It's most often implemented as a threaded red-black tree, but almost any other form of balanced tree (e.g., AVL tree, B-tree) could be used instead (and implementations using both of these have been written). Regardless of the form the balancing takes, the tree does have to be threaded (i.e., incrementing or decrementing an iterator is required to have constant complexity, so you need some way to get from one node to its predecessor/successor without traversing an arbitrary number of nodes in between.
```
https://www.quora.com/How-is-std-multiset-implemented  

Tutorial: A Modern Approach To Implementing High Score Tables in C++ using STL and Boost  
https://katyscode.wordpress.com/2012/09/22/tutorial-a-modern-approach-to-implementing-high-score-tables-in-c-using-stl-and-boost/

## Red-Black Tree

Red-black trees in 4 minutes — The basics https://youtu.be/qvZGUFHWChY  
Red-black trees in 3 minutes — Rotations https://youtu.be/95s3ndZRGbk
Red-black trees in 5 minutes — Insertions (strategy) https://youtu.be/5IBxA-bZZH8
Red-black trees in 5 minutes — Insertions (examples) https://www.youtube.com/watch?v=A3JZinzkMpk  

Introduction to Algorithms (SMA 5503)  
Lecture 10: Red-black Trees, Rotations, Insertions, Deletions  
https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-046j-introduction-to-algorithms-sma-5503-fall-2005/video-lectures/lecture-10-red-black-trees-rotations-insertions-deletions/

Introduction to Algorithms 6.046J/18.401J  
https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-046j-introduction-to-algorithms-sma-5503-fall-2005/video-lectures/lecture-10-red-black-trees-rotations-insertions-deletions/lec10.pdf

## AVL-Trees

MIT 6.006 Introduction to Algorithms, Fall 2011  
6. AVL Trees, AVL Sort  
https://www.youtube.com/watch?v=FNeL18KsWPc

Introduction to Algorithms  
https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-006-introduction-to-algorithms-fall-2011/recitation-videos/recitation-6-avl-trees/

## About Circular Queue

In the LRU Cache we are using a Linked-List and here we are using a Circular Queue. Why?
Because here we now that the history always have "k" items.

In the LRU case we can have multiple items if we measure the cache by size.