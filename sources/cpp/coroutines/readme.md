# Coroutines in C++

We know that C++ now have async/await:  

Bringing await to C++  
https://channel9.msdn.com/Events/GoingNative/2013/Bringing-await-to-Cpp  
  
C++ Coroutines: Understanding operator co_await  
https://lewissbaker.github.io/2017/11/17/understanding-operator-co-await  
  
And there ar some libraries that simulates coroutines:  
  
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