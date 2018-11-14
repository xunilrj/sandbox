# Coroutines in C++

We know that C++ now have async/await:  

Bringing await to C++  
https://channel9.msdn.com/Events/GoingNative/2013/Bringing-await-to-Cpp  

CppCon 2016: Gor Nishanov “C++ Coroutines: Under the covers"
https://channel9.msdn.com/events/CPP/CppCon-2016/CppCon-2016-Gor-Nishanov-C-Coroutines-Under-the-covers?term=C%2B%2B%20Coroutines%3A%20Under%20the%20covers
  
C++ Coroutines: Understanding operator co_await  
https://lewissbaker.github.io/2017/11/17/understanding-operator-co-await  
  
And we even have some libraries that simulates coroutines:  
  
CO2 - A C++ await/yield emulation library for stackless coroutine   
https://github.com/jamboree/co2  
  
So I thought... I not making my own?!  
The first thing is to steal the idea from Keny Kerr:  
  
Windows with C++ - Lightweight Cooperative Multitasking with C++  
https://msdn.microsoft.com/magazine/jj553509.aspx  
  
His idea is very simple and works on the premises that switch in C works like a goto.  
This is actually valid C/C++ code. Give it a try: https://tbfleming.github.io/cib/  

    int f(int state) {
        switch(state)
        {
            case 0:
            return true;
            while(true){
                case 1:
                return true;
            }
            case 2:;
        }
        return false;
    }

Now we just have to manage the state of the function. That is where CoManager enters.  
But we also need to allocate the memory of the function state. There is where the allocators.h enters.  
  
I have implemented very simple allocators following Andrei talk:  
  
CppCon 2015: Andrei Alexandrescu “std::allocator...”  
https://www.youtube.com/watch?v=LIb3L4vKZ7U  
  
And I allow any coroutine to be started by its index. Which makes very easy to plug into a event loop like libuv or WebAssembly.  
  
If we plug a persistent buffer, saving to files for example, we can have persistent workflow.  

Here I am also implementing everything. Appart from main.cpp we do not use stdlib. I implemented:

    function_traits
    remove_ref
    tuple
    forward

# Using it

## Plain C/C++

We can easily use this coroutines in c/c++ like this:

    int main()
    {
        auto alloc = StackAllocator<1024>();

        auto comgr = CoManager(&alloc);
        auto result = comgr.make(average, 1, 3);
        auto coroutine = result.coroutine;

        auto r = comgr.step(coroutine.id());
        r = comgr.step(coroutine.id());

        comgr.free(coroutine.id());

        std::cout << "result is " << result.args().result;
    }

The advantage here is that you can "step" your coroutine with just its id. This allows
you to bind the "step" in any event loop. For example WebAssembly and JS.

## Using in WebAssembly and JS.

    struct readFileArgs{int id;ui64 pos;int readCount;Block buffer;};
    cocontinuation readFile(costate<readFileArgs> &args)
    {
        auto& [id, pos, readCount, buffer] = ARGS;
        START_COROUTINE
            buffer = allocator.allocate(1024);
            do
            {
                readAsync(args.id(), id, pos, 1024, &readCount, buffer.Pointer);
                YIELD;
                pos += readCount;
            } while(readCount > 0);
        END_COROUTINE
    }

In the JS file you could do:

    readAsync: function(coid, id, offset, size, readCountRef, bufferRef)
    {
        fetch(...).then(function(){
            //write readCount to readCountRef
            //write bytes to bufferRef
            webasm.exports.step(coid);
        })
    }

And you would have a complete coroutine that would completely read a file in 1024 bytes chunks.

With a little bit MACRO magic you could even simplify the function creation in one line.

# Improving It

Given that the proptotype worked. How about improving it a bit. The first task is off course reading existing material.

https://lewissbaker.github.io/2017/09/25/coroutine-theory

Working Draft, Technical Specification for
C++ Extensions for Coroutines
2017-03-03
http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2017/n4649.pdf

On unifying the coroutines and resumable
functions proposals
http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2016/p0073r1.pdf

Resumable Functions (revision 4) - Date: 2015-03-31  
https://isocpp.org/files/papers/N4402.pdf  
Resumable Functions (v2) - Date: 2014/10/10  
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4134.pdf  
2014-05-22  
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n3977.pdf  
2014-01-19  
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n3858.pdf  
2013-08-30  
https://isocpp.org/files/papers/N3722.pdf  

Stackful Coroutines and Stackless Resumable Functions  
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4232.pdf

https://stackoverflow.com/questions/23422889/rationale-behind-the-resumable-functions-restrictions  

A Portable C++ Library for Coroutine Sequencing
http://www.akira.ruc.dk/~keld/research/COROUTINE/COROUTINE-1.0/DOC/COROUTINE_REPORT.pdf


https://theboostcpplibraries.com/boost.coroutine


https://llvm.org/devmtg/2016-11/Slides/Nishanov-LLVMCoroutines.pdf

Coroutine Series  
https://kirit.com/How%20C%2B%2B%20coroutines%20work/My%20first%20coroutine  

https://kirit.com/How%20C++%20coroutines%20work/A%20more%20realistic%20coroutine  

https://kirit.com/How%20C%2B%2B%20coroutines%20work/Yielding%20Generators  

https://kirit.com/How%20C%2B%2B%20coroutines%20work/Generating%20Iterators  

https://kirit.com/How%20C%2B%2B%20coroutines%20work/Awaiting