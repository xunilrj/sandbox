# Concurrent Data Structures

## Memory Models

https://preshing.com/20120710/memory-barriers-are-like-source-control-operations  
https://preshing.com/20120625/memory-ordering-at-compile-time  
https://preshing.com/20120913/acquire-and-release-semantics  

https://www.nickwilcox.com/blog/arm_vs_x86_memory_model  

## Fifo

### Developing

### Testing

The hardest part for concurrent data structures is testing. There are SO many cases, and variations, that is almost certain, that we, developers, will forget something.

The standard approach is:

1 - Test the serial case. It must work with one thread;
2 - Create the concurrent test using threads. Run it million of times and hope to catch all possibilities.

Although these are nice approaches, and you CAN deliver running software like that. A lot of bright mings ind the field of Computer Science tried to think in better approaches. One of these minds was: Brian Norris and Brian Demsky.

They wrote a paper called: "CDSCHECKER: Checking Concurrent Data Structures Written with C/C++ Atomics"  
http://demsky.eecs.uci.edu/publications/c11modelcheck.pdf  

```
While it is possible to use a formal specification of the
C/C++ memory model [8] to prove code correct, experience
suggests that most software developers are unlikely to do so
(e.g., because they lack expertise or time).

We present a new approach for exhaustively exploring the
behaviors of code under the C/C++ memory model, based
on stateless model-checking.

Stateless model-checkers
typically explore a program’s possible behaviors—or state
space—by repeatedly executing the program under different
thread interleavings.

Thus, state-of-the-art model-checking rests on a class
of optimization techniques known as dynamic partial-order
reduction (DPOR) [20]. The DPOR algorithm can reduce
the explored state space (e.g., two
concurrent stores to the same object conflict, whereas two
loads do not).

Conflict points are recorded in a backtracking set, so
that the exploration can return (or backtrack) to
the recorded program point during a future execution and
attempt a different thread interleaving.

At runtime, CDSCHECKER schedules program
fragments sequentially and determines the values returned
by atomic memory operations
```

