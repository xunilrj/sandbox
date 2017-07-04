# It´s Free!

Maybe one of the biggest advantage of using c++ today is that we can have free abstractions.

I hope to demonstrate the precise meaning of this in this small article.

To really understand what is happenning we are goind to use the godbolt.org project, that allows us to see very easily the generated assembly.

If you do not understand or is scaried by assembly, do not worry, it is going to be much easier than you think.

## First step

Our first step is to see the generated assembly of a empty c++ application. So, go to http://godbolt.org, type:

    int main(int argc, char ** argv)
    {
        return 0;
    }

choose the x86-x64 gcc 7.1 and type on its arguments -O4.

If everything is fine you should see:

    main:
            xor     eax, eax
            ret

xor eax, eax is a instrunction to zero the register eax that is immediatelly returned as the result of the application.

To understand a little better why using xor see https://randomascii.wordpress.com/2012/12/29/the-surprising-subtleties-of-zeroing-a-register/ or directly on http://www.intel.com/content/www/us/en/architecture-and-technology/64-ia-32-architectures-optimization-manual.html section 3.5.1.8

    "So using zero idioms are preferable 
    than moving 0´s into the register"

If, for some reason, you are particularly interested in why use eax and the ret, read https://idea.popcount.org/2013-07-16-baby-steps-in-x86-assembly/

## Second Step

Our first free abstraction is going to be a simple sum. C++ compilers are smart enough to know that a static expression can be solved in compile time and generate the fastest possible assembly. For example:

    int main(int argc, char ** argv)
    {
        int result = 3 + 2 + 1;
        return result;
    }

generates the following assembly:

    mov    eax,0x6
    ret

We do not see any sums operation. So we can safely say that constant operations are free in c++. Runtime free, off course.

#Thirs Step

And what about strucs? Let us see...

    struct SomeData
    {
        int value;
        SomeData() : value(3+2+1)
        {
        }
    };

    int main(int argc, char ** argv)
    {
        auto data = SomeData();
        return data.value;
    }

Well... now we are obviously screwed! We are creating an object, callings its constructor, setting its member and returning it. I bet you are expecting dozens of instructions.

But the reality is that this code is exactly the same as the last one.

    mov    eax,0x6
    ret 

Surprisingly, or not, the compiler also know that they are the same, so the assembly generated is the same!

We are here at a very important position. In C++, objects can be free! Actually they not even exist in the generated assembly.

That is why it is impossible to regenerate C++ code com executable files.

## Step Four

Ok. But the last one was easy. Objects should ahve some kind of encapsulation. Well.... let us try a simple one here:

    struct SomeData
    {
        SomeData() : value(3+2+1)
        {
        }
        int getValue()
        {
            return value;
        }
    private:
        int value;
    };

    int main(int argc, char ** argv)
    {
        auto data = SomeData();
        return data.getValue();
    }

and the generated assembly is:

    mov    eax,0x6
    ret

hum... In C++, encapsulation can also be free!

## Step Five

OK. So let transform our "value" member to an array.

    struct SomeData
    {
        SomeData()
        {
            value[0] = 3+2+1;
        }

        int* getValue()
        {
            return value;
        }
    private:
        int value[6];
    };

    int main(int argc, char ** argv)
    {
        auto data = SomeData();
        return data.getValue()[0];
    }

Wel... now there is no possible optimization, right? After all we are allocating 6 ints in the stack. Besides that, I am returning a pointer.

Let us see...

    mov    eax,0x6
    ret 

Yes... array can also be free in C++. The compiler knows that we only use the first value and it knows that the first value is always 6. So it is very easy for it optimizes this code.

## Step Six - Dynamic Allocation

Now we are stepping in delicaded ground. Dynamic allocation is always more complicated. It is almost impossible to optimize because of its level of indirections.

Well, let us see our example:

    struct SomeData
    {
        SomeData(int size, int v)
        {
            value = new int[size];
            value[0] = v;
        }

        int* getValue()
        {
            return value;
        }
    private:
        int* value;
    };

    int main(int argc, char ** argv)
    {
        auto data = SomeData(6,6);
        return data.getValue()[0];
    } 

The first obvious thin here is that our class is leaking memory. We are asking machine resources and never returning them. For more about this see http://en.cppreference.com/w/cpp/language/raii

We will continue here because this leak is not important for us now.

Let us see the assembly code:

    sub    rsp,0x8
    mov    edi,0x18
    call   400460 <operator new[](unsigned long)@plt>
    mov    eax,0x6
    add    rsp,0x8
    ret

The first instruction reserves 8 bytes for local variables. It is a default behavior to reserve space with sub and release space with add. That is why we have

    sub    rsp,0x8
    ...
    add    rsp,0x8
    ret