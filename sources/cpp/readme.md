# Links and C/C++ Material

## Tutorials

[C++ Coroutines (even for WebAssembly)](coroutines/)  
[Command line Debugging](CommandLineDebug.md)  
[C++ Performance: Packed array and Valgrind](packedvsnode/readme.md)  
[WebAssembly Series](/sources/webassembly)  
[Linux Kernel Development](../unix/moddev/readme.md)  
[Bitfield](bitfield)

how to create C++ iterators  
https://stackoverflow.com/questions/38730479/iterator-over-non-existing-sequence/38772065#38772065  

Shoud Non-Copyable class have user conversion  
https://stackoverflow.com/questions/54963695/shoud-non-copyable-class-have-user-conversion#54963784  

## My Libraries

Header only functional lib (Currying, Partial Apply and Pipeline)
[Func.h](./func/Readme.md)  [.h](./func/func.h) [example](./func/main.cpp)  

### ECS

[Iteration Speed (WIP)](./iteration_speed/Readme.md)  
[Advantage of packed arrays (WIP)](./ecs/tutorials/ColvsRow.md)  
[ECS (WIP)](./ecs/readme.md)  

## Algorithms

### Linked List

Information Processing Language-V Manual
Year: 1962

### Heap

ALGORITHM 232: HEAPSORT  
https://cacm.acm.org/magazines/1964/6/13668-algorithms/abstract

Binary Heaps and Heapsort Algorithm  
http://theoryofprogramming.com/2015/02/01/binary-heaps-and-heapsort-algorithm  

You're Doing It Wrong  
https://cacm.acm.org/magazines/2010/7/95061-youre-doing-it-wrong/fulltext#F1  

### Cache LRU

[LRU Cache](./lru)  

## Performance

### Intel Manuals

Intel® 64 and IA-32 Architectures Optimization Reference Manual  
https://www.intel.co.uk/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-optimization-manual.pdf  

Performance Analysis Guide for Intel® Core™ i7 Processor and Intel® Xeon™ 5500 processors 
https://software.intel.com/sites/products/collateral/hpc/vtune/performance_analysis_guide.pdf

### CPU Cycles

![infographics cpu](http://ithare.com/wp-content/uploads/part101_infographics_v08.png)  
http://ithare.com/infographics-operation-costs-in-cpu-clock-cycles/  

### GCC specific

#### builtin_expect

https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-g_t_005f_005fbuiltin_005fexpect-4159  
https://stackoverflow.com/questions/7346929/what-is-the-advantage-of-gccs-builtin-expect-in-if-else-statements#7347175  
https://stackoverflow.com/questions/109710/how-do-the-likely-and-unlikely-macros-in-the-linux-kernel-work-and-what-is-t  

# Good links

https://kennykerr.ca/articles/  
https://docs.microsoft.com/en-us%5Carchive%5Cmsdn-magazine%5Cauthors%5CKenny_Kerr

## Concurrency

Concurrency Guide
https://www.kernel.org/doc/htmldocs/kernel-locking/index.html  
http://www.gotw.ca/resources/Software%20and%20Concurrency%20-%20OGDC.pdf  

	http://www.drdobbs.com/parallel/the-pillars-of-concurrency/200001985  
		[1] The elephant analogy and the pillar segmentation were created by David Callahan (www.microsoft.com/presspass/exec/de/Callahan/ default.mspx) in an unpublished work.  
			https://youtu.be/WxO0CrYFnKU  
				Good Example to Parallelize: Histogram  
		[2] H. Sutter. "The Free Lunch Is Over: A Fundamental Turn Toward Concurrency in Software" (www.ddj.com/dept/architect/184405990).  
		[3] H. Sutter. "The Trouble With Locks" (www.ddj.com/dept/cpp/184401930)  
		[4] H. Sutter and J. Larus. "Software and the Concurrency Revolution" (ACM Queue, September 2005). (gotw.ca/publications/concurrency-acm.htm).   
		[5] J. Duffy, http://www.bluebytesoftware.com/ blog/PermaLink,guid,81ca9c00-b43e-4860-b96b-4fd2bd735c9f.aspx.   
http://www.drdobbs.com/parallel/how-much-scalability-do-you-have-or-need/201202924  
https://web.archive.org/web/20071119065933/http://www.ddj.com/cpp/184401930  
https://web.archive.org/web/20071001205646/http://ddj.com/cpp/201804238  
https://web.archive.org/web/20071025043749/http://www.ddj.com/hpc-high-performance-computing/202401098  
https://web.archive.org/web/20071109100245/http://ddj.com/architect/202802983  
https://web.archive.org/web/20071214111431/http://www.ddj.com/hpc-high-performance-computing/204801163  
https://web.archive.org/web/20080123152912/http://www.ddj.com/cpp/205900309  
https://web.archive.org/web/20080203104023/http://www.ddj.com/hpc-high-performance-computing/206100542  
https://web.archive.org/web/20080314100403/http://www.ddj.com/hpc-high-performance-computing/206903306  
https://web.archive.org/web/20080414045618/http://ddj.com/architect/207100682  
https://web.archive.org/web/20080530004409/http://ddj.com/architect/208200273  
https://web.archive.org/web/20080701112517/http://www.ddj.com/hpc-high-performance-computing/208801371  
https://web.archive.org/web/20080810143822/http://www.ddj.com/hpc-high-performance-computing/209900973  
https://web.archive.org/web/20080915043701/http://www.ddj.com/cpp/210600279  
https://web.archive.org/web/20081103070701/http://www.ddj.com/cpp/211601363  
https://web.archive.org/web/20080930160117/http://www.ddj.com/hpc-high-performance-computing/210604448  
https://web.archive.org/web/20081106120425/http://www.ddj.com/cpp/211800538  
https://web.archive.org/web/20090114044325/http://www.ddj.com/hpc-high-performance-computing/212201163  
https://web.archive.org/web/20090112165430/http://www.ddj.com/hpc-high-performance-computing/212701484  
https://web.archive.org/web/20090217221213/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=214100002  
https://web.archive.org/web/20090417234245/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=216500409  
https://web.archive.org/web/20090519034857/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=217500206  
https://web.archive.org/web/20090620033058/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=217801299  
https://web.archive.org/web/20090719084031/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=218401447  
https://web.archive.org/web/20090816084836/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=219200099  
https://web.archive.org/web/20091017094027/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=220600388  
  
https://channel9.msdn.com/Events/PDC/PDC09/Patterns-of-Parallel-Programming  
https://channel9.msdn.com/Events/PDC/PDC09/FT52  
  
https://channel9.msdn.com/Events/PDC/PDC09/VTL32  
https://channel9.msdn.com/Events/PDC/PDC09/VTL04  
https://channel9.msdn.com/Events/PDC/PDC09/VTL02  
https://channel9.msdn.com/Events/PDC/PDC09/SVR17  
https://channel9.msdn.com/Events/PDC/PDC09/FT51  
https://channel9.msdn.com/Events/PDC/PDC09/FT21  
https://channel9.msdn.com/Events/PDC/PDC09/FT20  
https://channel9.msdn.com/Events/PDC/PDC09/FT19  
https://channel9.msdn.com/Events/PDC/PDC09/P09-17  
https://channel9.msdn.com/Events/PDC/PDC09/P09-09  
https://channel9.msdn.com/Events/PDC/PDC09/P09-01  
https://channel9.msdn.com/Events/PDC/PDC09/Patterns-of-Parallel-Programming  
  
https://web.archive.org/web/20091114062700/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=221601309  
https://web.archive.org/web/20100122071520/http://www.ddj.com/go-parallel/article/showArticle.jhtml?articleID=222301165  
  
http://igoro.com/archive/gallery-of-processor-cache-effects/  
https://web.archive.org/web/20090824005611/http://www.nwcpp.org/Downloads/2007/Machine_Architecture_-_NWCPP.pdf  
  
https://web.archive.org/web/20100516063612/http://www.drdobbs.com/go-parallel/article/showArticle.jhtml?articleID=224701827  
https://web.archive.org/web/20100728112646/http://www.drdobbs.com/go-parallel/article/showArticle.jhtml;jsessionid=JM3XD1KM22SCRQE1GHPSKH4ATMY32JVN?articleID=225700095  
https://web.archive.org/web/20100822030745/http://www.drdobbs.com/go-parallel/article/showArticle.jhtml?articleID=226700179  
https://web.archive.org/web/20100928235842/http://www.drdobbs.com/go-parallel/article/showArticle.jhtml?articleID=227500074  
  
https://channel9.msdn.com/Shows/Going+Deep/E2E-Herb-Sutter-and-Erik-Meijer-Perspectives-on-C  
  
https://herbsutter.com/welcome-to-the-jungle/  
https://www.facebook.com/Engineering/videos/10151029515183109/  
  
http://gec.di.uminho.pt/Discip/MIei/cpd1718/ESC/Material/Atomic%20Weapons%20-%20Memory%20Model.pdf  
https://channel9.msdn.com/Shows/Going+Deep/Cpp-and-Beyond-2012-Herb-Sutter-atomic-Weapons-1-of-2  
https://channel9.msdn.com/Shows/Going+Deep/Cpp-and-Beyond-2012-Herb-Sutter-atomic-Weapons-2-of-2  
C++ and Beyond 2012: Herb Sutter - atomic Weapons 1 of 2  
C++ and Beyond 2012: Herb Sutter - atomic Weapons 2 of 2  
CppCon 2016: Hans Boehm “Using weakly ordered C++ atomics correctly"  
22. History of Memory Models  

http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n2016.html  
  
  
https://herbsutter.com/2008/03/12/new-course-available-effective-concurrency/  
  
Fundamentals  
	• Define basic concurrency goals and requirements  
	• Understand applications’ scalability needs  
	• Key concurrency patterns  
Isolation: Keep Work Separate  
	• Running tasks in isolation and communicate via async messages  
	• Integrating multiple messaging systems, including GUIs and sockets  
	• Building responsive applications using background workers  
	• Threads vs. thread pools  
Scalability: Re-enable the Free Lunch  
	• When and how to use more cores   
	• Exploiting parallelism in algorithms   
	• Exploiting parallelism in data structures  
	• Breaking the scalability barrier  
Consistency: Don’t Corrupt Shared State  
	• The many pitfalls of locks–deadlock, convoys, etc.  
	• Locking best practices  
	• Reducing the need for locking shared data  
	• Safe lock-free coding patterns  
	• Avoiding the pitfalls of general lock-free coding  
	• Races and race-related effects  
Migrating Existing Code Bases to Use Concurrency  
Near-Future Tools and Features  
High Performance Concurrency  
	• Machine architecture and concurrency  
	• Costs of fundamental operations, including locks, context switches, and system calls  
	• Memory and cache effects  
	• Data structures that support and undermine concurrency  
	• Enabling linear and superlinear scaling  
  
From <https://herbsutter.com/category/concurrency/page/10/>   
  
  
  
https://stackoverflow.com/questions/13632344/understanding-c11-memory-fences  


# Interesting libs

https://github.com/dpilger26/NumCpp
  