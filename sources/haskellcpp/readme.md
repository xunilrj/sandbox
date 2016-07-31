# Introduction 

Interpretation of  
http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html  
to C++

## Functors

Functor is a typeclass. This is totally different thant C++ classes. Typeclasses are interfaces that data types implements. One can implement typeclasses verifiers in C++ using template metaprogramming.

So the typeclass

    class Functor f where
	    fmap :: (a -> b) -> f a -> f b

means that every data type that specifies a fmap compliant to the rule will be a functor. There rule is:
"(a->b)" = first parameter must be a function that receives one parameter and return something
"->fa" = second parameter must be a functor
"->fb" = the return type will be a functor

The first step is to constuct a dummy struct that will evolve to be a functor in C++ style.

    struct Just
    {
    };

and we will test in compile time (http://en.cppreference.com/w/cpp/language/static_assert) if Just is a functor:

    int main()
    {   
        static_assert(is_functor<Just>::value, "Just is not a functor!?");
        return 0;
    }

and will define a dummy is_functor validator that will fail for now.

    template <typename T>
    struct is_functor
    {
        static const bool value = false;
    };

Compiling this

    g++ -std=c++11 main.cpp && ./a.out
    main.cpp: In function 'int main()':
    main.cpp:14:5: error: static assertion failed: Just is not a functor!?
         static_assert(is_functor<Just>::value, "Just is not a functor!?");

http://coliru.stacked-crooked.com/a/17381ac0f6078dad

Well... it seems that Just is not a functor yet! Wich is fine since it really not is yet!
OK! The fist step is, we need a way to analyze in compile time if Just have a member function named "fmap". In C++ this is called "Member Detection Idiom" 

Today there are three ways to detect that a member exists:
Pre-C++11
https://en.wikibooks.org/wiki/More_C%2B%2B_Idioms/Member_Detector

Pre-C++17

and Pos-C++17
http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4436.pdf

But before diving in the deep waters of template metaprogramming let us first see how templates can help us to make some compilation times assert.

### Member Detection

For example. In the function:

    template <typename T1, typename T2>
    int canISum(T1 a, T2 b)
    {
    	return a+b;
    }

when I try

    CanISum(0,1);

the compiler will deduce T1 = int and T2 = int and both can be summed.

using some C++11 features I can rewrite the same function as

    template <typename T1, typename T2>
    auto canISum(T1 a, T2 b) -> int
    {
    	return a+b;
    }

and then

    template <typename T1, typename T2>
    auto canISum(T1 a, T2 b) -> decltype(a+b)
    {
    	return a+b;
    }

OK. Let us see
1 - auto in the return that it will have to compute the return type of the function;
2 - decltype receives a valid expression an return its type (http://en.cppreference.com/w/cpp/language/decltype);
3 - so this functionreturn type will depend on the return of (a+b).

but with 

    CanISum(Just(), Just()) 

    the compiler will raise an error:
    main.cpp: In function 'int main()':
    main.cpp:26:26: error: no matching function for call to 'canISum(Just, Just)'
         canISum(Just(),Just());
                              ^
    main.cpp:15:6: note: candidate: template<class T1, class T2> decltype ((a + b)) canISum(T1, T2)
    auto canISum(T1 a, T2 b) -> decltype(a+b)
          ^~~~~~~
    main.cpp:15:6: note:   template argument deduction/substitution failed:
    main.cpp: In substitution of 'template<class T1, class T2> decltype ((a + b)) canISum(T1, T2) [with T1 = Just; T2 = Just]':
    main.cpp:26:26:   required from here
    main.cpp:15:39: error: no match for 'operator+' (operand types are 'Just' and 'Just')
        auto canISum(T1 a, T2 b) -> decltype(a+b)
                                              ~^~

Because the compiler does not know how the sum two Just classes. Let us create a catch all function for this case.

    void CanISum(...){}

The compiler error is gone. Now the compiler have resolved CanISum(Just(), Just())  to the CanISum(...) function.

Now, all types that the compiler knows to sum are deduced to a function and all the others a deduced to another function.

Wonderful! Halfway there. Now we need a way to tell which function the compiler chose.

But first suit yourself because will get ugly!

The static_assert only allows me to use compile time expressions.

    static_assert(4==4, "Not ok!");

This wil test that if 4 is equal to 4. Not particularly useful... but...

    static_assert(sizeof(int)==4, "Not ok!");

will test if int, is in this particular machine int is 4 bytes long. hum... OK...

    so decltype(1) returns int
    decltype(1.0f) returns float

so I can rewrite the assert as

    static_assert(sizeof(decltype(1))==4, "Not ok!");

This still pass because 
    decltype(1) = int
    sizeof(int) = 4
    4 == 4

hum... This is something! With this I can know at compile time the type of any expression. Even compile deduced templates. How about doing:

    static_assert(sizeof(decltype(canISum(1,2)))==4, "Not ok!");

Hey! It worked! Why? Well... canISum when deduced to the templated version return the type returned when summing two values. The sum of two ints are an int. So:

    sizeof(decltype(canISum(1,2)))==4
    sizeof(int)==4
    4==4
    true

But when I Do

    static_assert(sizeof(decltype(canISum(Just(),Just())))==4, "Not ok!");

The compiler comlpain:

    main.cpp: In function 'int main()':
    main.cpp:31:58: warning: invalid application of 'sizeof' to a void type [-Wpointer-arith]
         static_assert(sizeof(decltype(canISum(Just(),Just())))==4, "Not ok!");
                                                              ^
    main.cpp:31:5: error: static assertion failed: Not ok!
         static_assert(sizeof(decltype(canISum(Just(),Just())))==4, "Not ok!");
         ^~~~~~~~~~~~~

Our CanISum is returning void, so the compiler is complaining about sizeof(void)... It is indeed a philosofical question... How is the size of Void!?

Putting philosophy aside let us change the return type to something easier: int!

    int canISum(...){}

This have become true:

    static_assert(sizeof(decltype(canISum(Just(),Just())))==4, "Not ok!");
    sizeof(decltype(canISum(Just(),Just())))==4
    sizeof(int)==4
    4==4

but this have not given advantage to us. But if we replace the return type to double.

    static_assert(sizeof(decltype(canISum(Just(),Just())))==4, "Not ok!");

would fail because the return size now is 8 bytes long. But the new version

    static_assert(sizeof(decltype(canISum(Just(),Just())))==8, "Not ok!");

now works!

and the main point is:

    sizeof(decltype(canISum(0,1))) == 4
    sizeof(decltype(canISum(Just(),Just())) == 8

all types that the compiler can sum will return sizeof(...) equal to 4 and all that it can�t will return sizeof(...) as 8. Very interesting!

Can we make it a little cleaner? Yes, we can!

First, let us create two types to help us:

    struct always_true
    {
        static const bool value = true;
    };

    struct always_false
    {
        static const bool value = false;
    };

And with them I can make

    static_assert(always_true::value, "Always true");
    static_assert(!always_false::value, "Always false");

Not very useful... but how about?

    always_true IsItTrue(int){}
    always_false IsItTrue(float){}

When passing a int I want that something would ne true. When passing a float I want taht this thing to be false. And I can assert as:

    static_assert(decltype(IsItTrue(1))::value, "Always true");
    static_assert(!decltype(IsItTrue(1.0f))::value, "Always false");

So It seems that I only need to change my functions to returns this structures...

    template <typename T1, typename T2>
    always_true canISum(T1 a, T2 b)
    {    
        auto c = a+b;
        return always_true();
    }

    always_false canISum(...){}

and the assertions

    static_assert(decltype(canISum(1,2))::value, "Not ok!");
    static_assert(decltype(canISum(Just(),Just()))::value, "Not ok!");

It seems the moment for glory! Bu the compiler raises an error:

    main.cpp: In instantiation of 'always_true canISum(T1, T2) [with T1 = Just; T2 = Just]':
    main.cpp:39:26:   required from here
    main.cpp:27:15: error: no match for 'operator+' (operand types are 'Just' and 'Just')
         auto c = a+b;
                  ~^~

Suddenly the compiler cannot use the ellipsis for the Just version anymore. It would be a dead end if we did not have another trick up to the sleaves!

We already now that this is OK!

    static_assert(sizeof(decltype(1)) == 4, "OK!");

but one could also do this:

    static_assert(sizeof(decltype(1.0, 1)) == 4, "OK!");

This is also true!! decltype evaluates all expressions and only return the type of the last one. So one cannot write non valid expressions on decltype. For example:

    static_assert(sizeof(decltype(idontexist, 1)) == 4, "OK!");

throws an error:

    main.cpp: In function 'int main()':
    main.cpp:44:35: error: 'idontexist' was not declared in this scope
         static_assert(sizeof(decltype(idontexist, 1)) == 4, "OK!");
                                       ^~~~~~~~~~

hum... what about...

    static_assert(sizeof(decltype(Just()+Just(), 1)) == 4, "OK!");

gives us the following error:

    main.cpp: In function 'int main()':
    main.cpp:44:41: error: no match for 'operator+' (operand types are 'Just' and 'Just')
         static_assert(sizeof(decltype(Just()+Just(), 1)) == 4, "OK!");

and

    static_assert(sizeof(decltype(1+1, 1)) == 4, "OK!");

gives us no error.
Hey! Now I know a easy way to test if a code compile and return a boolean.

    static_assert(decltype(1+1, always_true())::value, "This compile!");

This compiles fine! But this

    static_assert(decltype(Just()+Just(), always_true())::value, "This compile!");

give us an error:

    main.cpp: In function 'int main()':
    main.cpp:44:34: error: no match for 'operator+' (operand types are 'Just' and 'Just')
         static_assert(decltype(Just()+Just(), always_true())::value, "This compile!");
                                ~~~~~~^~~~~~~
    main.cpp:44:19: error: decltype evaluates to '<type error>', which is not a class or enumeration type
         static_assert(decltype(Just()+Just(), always_true())::value, "This compile!");
                       ^~~~~~~~

OK! But we don�t want a compile error, we want a simple boolean value. For this we just need to return to our functions and use this last technique:

    template <typename T1, typename T2>
    auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
    {    
        auto c = a+b;
        return always_true();
    }

    always_false canISum(...){}

and the statc_asserts:

    static_assert(decltype(canISum(1,2))::value, "Not ok!");
    static_assert(!decltype(canISum(Just(),Just()))::value, "Not ok!");

and everything compiles! Wow! Beautiful.
But can we make it a little cleaner? Yes, we can!
We can for example remove those constants and work directly with the types.

For this we will need to create a struct:

    struct can_i_sum
    {
        static const bool value = false;
    };

The C++ compiler allows me to put any compiler time value on a static const member. Well, we already know how to compute if a expressio is summable or not. So let us move our functions to inside this struct:

    struct can_i_sum
    {
        template <typename T1, typename T2>
        static auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
        {    
            auto c = a+b;
            return always_true();
        }

        static always_false canISum(...){}

        static const bool value = false;
    };

and we can still do:

    can_i_sum::canISum(1,2);

and

    static_assert(decltype(can_i_sum::canISum(1,2))::value, "Not ok!");

The next step is to change this static_assert expression to the value member.

    struct can_i_sum
    {
        template <typename T1, typename T2>
        static auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
        {    
            auto c = a+b;
            return always_true();
        }

        static always_false canISum(...){}

        static const bool value = decltype(can_i_sum::canISum(1,2))::value;
    };

and

    static_assert(can_i_sum::value, "Not ok!");

Almost there! Let us move the templates to the struct.

    template <typename T1, typename T2>
    struct can_i_sum
    {
        static auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
        {    
            auto c = a+b;
            return always_true();
        }

        static always_false canISum(...){}

        static const bool value = decltype(can_i_sum::canISum(1,2))::value;
    };

and

    can_i_sum<int,int>::canISum(1,2);

and

    static_assert(can_i_sum<int,int>::value, "Not ok!");

Well... we can say that the static_assert expression is done! It cannot get any better.
But the value member is still with 1 and 2 hardcoded.

This creates the problem that

    static_assert(can_i_sum<Just,Just>::value, "Not ok!");

does not compile anymore. We need to turn the line

    static const bool value = decltype(can_i_sum::canISum(1,2))::value;

to be the more generic as possible and depend only on the template parameters. We can do like this using another C++11 feature std::declval. declval is almost the cotnraty to decltype. Now, given a type you receive a reference to the type. 

http://en.cppreference.com/w/cpp/utility/declval

And it is better than just calling the default contructor of the type becuase declval works even with types that do not have default contructors.

And with 

    template <typename T1, typename T2>
    struct can_i_sum
    {
        private:
        static auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
        {    
            auto c = a+b;
            return always_true();
        }

        static always_false canISum(...){}
    public:
        static const bool value = decltype(canISum(std::declval<T1>(),std::declval<T2>()))::value;
    };

and with 

    static_assert(can_i_sum<int,int>::value, "Not ok!");
    static_assert(!can_i_sum<Just,Just>::value, "Not ok!");

we can compile and go to... wait! This gives another compile error:

    main.cpp: In instantiation of 'struct can_i_sum<Just, Just>':
    main.cpp:48:39:   required from here
    main.cpp:28:50: error: no match for 'operator+' (operand types are 'Just' and 'Just')
         static auto canISum(T1 a, T2 b) -> decltype(a+b, always_true())
                                                     ~^~
    main.cpp: In function 'int main()':
    main.cpp:48:5: error: static assertion failed: Not ok!
         static_assert(can_i_sum<Just,Just>::value, "Not ok!");
         ^~~~~~~~~~~~~

C++ compile are unbeatable... they nerver give up of throwing up errors!

The problem now is that we made the struct templated, but not the function. So the c++ compiler tried to create the fucntion with template parameters passed but it realized that this is impossible.

The solution is simple. We have to make the member function templated again!
And the (almost) gran finale!

    template <typename T1, typename T2>
    struct can_i_sum
    {
    private:
        template <typename A, typename B>
        static auto canISum(A a, B b) -> decltype(a+b, always_true())
        {    
            auto c = a+b;
            return always_true();
        }

        static always_false canISum(...){}
    public:
        static const bool value = decltype(canISum(std::declval<T1>(),std::declval<T2>()))::value;
    };

and 

    static_assert(can_i_sum<int,int>::value, "Not ok!");
    static_assert(!can_i_sum<Just,Just>::value, "Not ok!");

compiles with no error!

There is some improvements that we can do:
1 - The C++11 already gives me the always_true and always_false
http://en.cppreference.com/w/cpp/types/integral_constant

they are called
std::true_type and std::false_type and live in the type_traits header.

So our class now, is:

    template <typename T1, typename T2>
    struct can_i_sum
    {
    private:
        template <typename A, typename B>
        static auto canISum(A a, B b) -> decltype(a+b, std::true_type())
        {    
            auto c = a+b;
            return always_true();
        }

        static std::false_type canISum(...){}
    public:
        static const bool value = decltype(canISum(std::declval<T1>(),std::declval<T2>()))::value;
    };

2 - The code inside the member function will never run. It is totally inacessible. And the compiler does need it. So we can removed it.

    template <typename T1, typename T2>
    struct can_i_sum
    {
    private:
        template <typename A, typename B>
        static auto canISum(A a, B b) -> decltype(a+b, std::true_type());
        static std::false_type canISum(...);
    public:
        static const bool value = decltype(canISum(std::declval<T1>(),std::declval<T2>()))::value;
    };

3 - We can use more generic names in the member functions .

    template <typename T1, typename T2>
    struct can_i_sum
    {
    private:
        template <typename A, typename B>
        static auto check(A a, B b) -> decltype(a+b, std::true_type());
        static std::false_type check(...);
    public:
        static const bool value = decltype(check(std::declval<T1>(),std::declval<T2>()))::value;
    };

We can even make this a macro

    #define check_dyadic(name,op) template <typename T1, typename T2> \
    struct name \
    { \
    private: \
        template <typename A, typename B> \
        static auto check(A a, B b) -> decltype(op, std::true_type()); \
        static std::false_type check(...); \
    public: \
        static const bool value = decltype(check(std::declval<T1>(),std::declval<T2>()))::value; \
    };

and define operators checking like this:

    check_dyadic(can_i_sum,a+b)
    check_dyadic(can_i_take_difference,a-b)

and 

    static_assert(can_i_sum<int,int>::value, "Not ok!");
    static_assert(!can_i_sum<Just,Just>::value, "Not ok!");
    static_assert(can_i_take_difference<int,int>::value, "Not ok!");
    static_assert(!can_i_take_difference<Just,Just>::value, "Not ok!");

will all compile.
Woo! Macros! We wen too far!
Well... what a drigression! But we made it!

## Back to Functors

Going back... we do not give a **** if the class have a + operator. We really wnat to know if it is a functor. So let us return to the functor definition.

    class Functor f where
	    fmap :: (a -> b) -> f a -> f b

and our functor

    template <typename T>
    struct is_functor
    {
        static const bool value = false;
    };

Out last technique helped us to understand with some type have the operator +, but the reality was that we only checked if  some expression was compliable or not. We will do this again here. Every type that contains a member function named fmap that is compliant with the definition will be a functor. 

So let us go!
OK... another digression...

let us start with a simple global fmap and out macro:

    void fmap()
    {
    }

    check_dyadic(can_i_call_fmap,fmap())

remember that the macro will expando to:

    struct (can_i_call_fmap,fmap
    { 
    private: 
        template <typename A, typename B> 
        static auto check(A a, B b) -> decltype(fmap(), std::true_type()); 
        static std::false_type check(...); 
    public: 
        static const bool value = decltype(check(std::declval<T1>(),std::declval<T2>()))::value; 
    };

and the static_assert:

    static_assert(can_i_call_fmap<Just,Just>::value, "Not ok!");

will works, because we can call our current fmap with any type. Great!
But the real fmap receives parameters. It first parameter is a function that receives a parameter and return another value.

To better define this function we will use another C++11 feature http://en.cppreference.com/w/cpp/language/type_alias:

    template <typename A, typename B>
    using fmapArg1 = B(*)(A);

that defines a function that receives B and returns A. Now out fmap is:

    template <typename A, typename B>
    using fmapArg1 = B(*)(A);

    template <typename A, typename B>
    void fmap(fmapArg1<A,B> f)
    {
    }

in that notation we already have:

    fmap :: (a -> b) -> nothing

Including the next parameter and the return type we have:

    template <typename A, typename B, typename FA, typename FB>
    FB fmap(fmapArg1<A,B> f, FA a)
    {
    }

and now we have 

    fmap:: (a -> b) -> fa -> fb

Ok. It seems right, but we have a problem! This is not the same as the original notation. What we actually have now is this:

    original: fmap :: (a -> b) -> f a -> f b
    ours: fmap :: (a -> b) -> c -> d

which is too generic. We have bind the templates types to the function argument types. Like this:

    template <typename A, typename B, template <typename> typename FA, template <typename> typename FB>
    FB<B> fmap(fmapArg1<A,B> f, FA<A> a)
    {
    }

We have used another little feature of C++ 14 here.
Before C++ 14 templated template could not use the typename keyword. Altought it makes no difference, the syntax now is unified. See: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4051.html

OK! Now we have a compliant fmap.

Using our macro:

    check_dyadic(can_i_call_fmap,fmap(a,b))

we see that

    static_assert(can_i_call_fmap<Just,Just>::value, "Not ok!");

Just is not a functor yet! As expected!

But I can specialize a version of my template to my Just structure. I do not know how to generate generic fmaps, but for this struc i know.

    template <typename T>
    struct Just
    {    
    };

    template <typename A, typename B>
    using fmapArg1 = B(*)(A);

    template <typename A, typename B, template <typename> typename FA, template <typename> typename FB>
    FB<B> fmap(fmapArg1<A,B> f, FA<A> a)
    {
    }

    template <typename A, typename B>
    Just<B> fmap(fmapArg1<A,B> f, Just<A> a)
    {
        throw 0; // Not Implemented yet
    }

and the assertion now is:

    static_assert(can_i_call_fmap<fmapArg1<int,int>,Just<int>>::value, "Not ok!");

See that the old assert with <Just,Just> does not make any sense now. fmap is not a simetric operator anymore.
OK!

Now we will allow Just to carry some value and make the fmap actually work:

    template <typename T>
    struct Just
    {    
        T Value;

        Just(T value) : Value{value}
        {
        }
    };

    [...]

    template <typename A, typename B>
    Just<B> fmap(fmapArg1<A,B> f, Just<A> a)
    {
        return Just<B>(f(a.Value));
    }

and we can call it:

    int plus1 (int x )
    {
        return x + 1;
    }

    [...]

    auto just2 = fmap(plus1, Just<int>(1));
    std::cout << just2.Value;

Woo! Pleasure at last!
Now we see the "2" on the console.

Hey! But remember that we developers are lazy! We hate to type!
Out Just struct does nothing, it just hold values. This is so common that even has a name https://en.wikipedia.org/wiki/Algebraic_data_type.

In C++ 11 we do not need to type algebraic data types, we can use the std::tuple (http://en.cppreference.com/w/cpp/utility/tuple).

So let us change our fmap function to:

    template <typename A, typename B>
    std::tuple<B> fmap(fmapArg1<A,B> f, std::tuple<A> a)
    {
        return std::make_tuple(f(std::get<0>(a)));
    }

and call 

    static_assert(can_i_call_fmap<fmapArg1<int,int>,std::tuple<int>>::value, "Not ok!");
    
    auto just1 = std::make_tuple(1);
    auto just2 = fmap(plus1, just1);    
    
    std::cout << std::get<0>(just2);

Hey! We turned std::tuple<int> into a functor! Who would say...
But we can improve. Why limit us to a 1-tuple? Let us make fmap accept a n-tuple and use just the first value:

    template <typename A, typename B, typename C>
    std::tuple<B> fmap(fmapArg1<A,B> f, C a)
    {
        return std::make_tuple(f(std::get<0>(a)));
    }

and

    auto just123 = std::make_tuple(1, 2, 3);
    auto just2 = fmap(plus1, just123);

just works.

Althoug the syntax is pretty straightfowrad already we can copy the Haskell a little and crate the Just function.

and now we have.

    auto just1 = just(1);
    auto just2 = fmap(plus1, just1);
    std::cout << std::get<0>(just2);

Unfortunately this implementation still have a big problem. C++11 have accepted the lambda functions (http://en.cppreference.com/w/cpp/language/lambda).

But we cannot use them here:

    auto just1 = just(1);
    auto just2 = fmap(([](int x){return x+1;}), just1);
    std::cout << std::get<0>(just2);

the compiler will raise:

    main.cpp: In function 'int main()':
    main.cpp:67:54: error: no matching function for call to 'fmap(main()::<lambda(int)>, std::tuple<int>&)'
         auto just2 = fmap(([](int x){return x+1;}), just1);
                                                          ^
    main.cpp:26:7: note: candidate: template<class A, class B, template<class> class FA, template<class> class FB> FB<B> fmap(fmapArg1<A, B>, FA<A>)
    FB<B> fmap(fmapArg1<A,B> f, FA<A> a)
           ^~~~
    main.cpp:26:7: note:   template argument deduction/substitution failed:
    main.cpp:67:54: note:   mismatched types 'B (*)(A)' and 'main()::<lambda(int)>'
         auto just2 = fmap(([](int x){return x+1;}), just1);
                                                          ^
    main.cpp:31:15: note: candidate: template<class A, class B, class C> std::tuple<B> fmap(fmapArg1<A, B>, C)
    std::tuple<B> fmap(fmapArg1<A,B> f, C a)
                   ^~~~
    main.cpp:31:15: note:   template argument deduction/substitution failed:
    main.cpp:67:54: note:   mismatched types 'B (*)(A)' and 'main()::<lambda(int)>'
         auto just2 = fmap(([](int x){return x+1;}), just1);
                                                          ^

Yeah! 20 lines of error!!
We can make it work with a simple cast:

    auto just1 = just(1);
    auto just2 = fmap(static_cast<fmapArg1<int,int>>([](int x){return x+1;}), just1);
    std::cout << std::get<0>(just2);

off course the solution have totally degenerated the code. So is tehre anything we can do?