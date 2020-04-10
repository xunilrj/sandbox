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

## About Circular Queue