# Coroutines in C++

We know that C++ now have async/await:  

Bringing await to C++  
https://channel9.msdn.com/Events/GoingNative/2013/Bringing-await-to-Cpp  
  
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
