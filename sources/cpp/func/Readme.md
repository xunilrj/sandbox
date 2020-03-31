# Functional Composition

The history of functional compositions that somehow flows into computer programming started hundreads of years ago and it is merged with Western intellectual development. Some names are know, but some are not well known, unfortunately.

A good introduction is: https://publicdomainreview.org/essay/let-us-calculate-leibniz-llull-and-the-computational-imagination

In summary, we can go back to Majorca, Spain in 1316 to the life of Ramon Lull. A Christian philosopher thought the "perfect" way to convert Muslins to Christianity. He thought the problem was that we needed a more "algorithmic" way of thinking pursuing truth. His work really impressed Gottfried Leibniz four centuries later. Leibniz also believe we needed a more algorithmic way of solving problems. He thought that "everything which exists or which can be thought, must be compounded of parts". This idea he developed more in the now famous work "The Monadology", or the study of monads.

The Monadology
http://home.datacomm.ch/kerguelen/monadology/monadology.html

The first three items are enough for us here:

1. The Monad, of which we shall here speak, is nothing but a simple substance, which enters into compounds. By ‘simple’ is meant ‘without parts’.
2. And there must be simple substances, since there are compounds; for a compound is nothing but a collection or aggregatum of simple things. Go to next paragraph
3. Now where there are no parts, there can be neither extension nor form nor divisibility. These Monads are the real atoms of nature and, in a word, the elements of things. Go to next paragraph

The question is: are these monads atoms in the physics sense that we know today? are they dimensions of analysis as Lull intended? Or are they Platonic mini-ideals that would drive the real world?

For Leibniz probably the three, and more, together. Remember Leibniz is not proposing algorithms to solve mathematical questions, but all questions from all fields: law, teology, metaphysics, medicine etc... you named it, monads are going to solve it.

One of these monads was in the field of Combinatorics. Leibniz envisioned that if we can forms all words with a limited sets os characters, maybe we can do the same thing for every science. Combinatorics, off course, evolve much until 1924, Moses Shönfinkel wrote the paper "On the building blocks of mathematical logic" trying to formulate or find the "monads" in Mathematical logic.

He came to an incredible conclusion that we can avoid using variables if functions could be arguments and results of other function. With this idea we can build a "logic" or a "program" with only four "monads":

1 - One Constructor (the application of a function to an argument);
2 - High-Order Function C, a function which given any value x, returns the constant x valued function;
3 - High-Order Function S, a function which combines two functions, say f and g;
4 - High-Order Function U, it should be thought of as applying to two predicates and returns the universal quantification of the negated conjunction of the two predicates.

See more at:
https://www.rbjones.com/rbjpub/philos/bibliog/schonf24.htm

Ok, but why is this important?

If we go to other work of this era 20s/30s we will find the work of Haskell Curry. Curry said in his book "Combinatory Logic: Volume I", Chapter 3, Section 2: 

```
2. Need for a functional notation 
Curiously a systematic notation for functions is lacking in ordinary 
mathematics. 

3. The A-notation 
To have a systematic notation for the function in itself we must 
remember that a function is a law of correspondence, i.e., a class of 
ordered couples, 

If we abbreviate by 'M' an expression containing 
'x' which indicates the value of a function when the argument has the 
value x, we write 

(3) λx(M) 

to designate the function in itself.

Obvious abbreviations may be used: 'λxx^2' may be written instead of 
'λx(x^2)' as there is no ambiguity. Or we may use a dot instead of the round 
brackets, the scope of the dot extending to the end of the functional 
expression; hence 'λx.x + 2' is equivalent to 'λx(x + 2)'. 
```

And there you have the "lambda notation" and why it is called "Lambda Calculus".

The important part for us is a few paragraphs below:

```
2. Functions of several arguments 

For functions of several arguments we might similarly define an n-fold 
functional abstraction 

(1) λⁿx₁...xₙ.M 

with the intuitive sense of "the function whose value is M when the  
arguments are x₁,...,xₙ". However, a little thought will show that this 
operation can be denned by iteration of the operation of [application]. 

Take, for instance, addition, whose value, for the arguments x and y, is x + y. 
If we regard x as a fixed value, the function λy(x + y) will represent, 
according to our conventions, the operation of adding the argument to x. 

This correspondence, which according to our conventions is 
λx(λy(x + y)) or λx:λy.x + y, is then the intuitive equivalent of
addition; hence we can adopt the definition:

λ²xy.x + y [is equivalent to] λx:λy.x + y. 

In the general case we adopt the recursive definition 

(1) λ¹x.M [is equivalent to] λx.M, 
(2) λⁿ⁺ⁱx₁...Xₙy.M [is equivalent to] λⁿx₁...xₙ(λy.M). 

The exponent of the λ is usually omitted.

Thus we can express functions of any number of arguments by means 
of simple functional abstraction; and in a combinatorially complete 
system functions of any number of variables will be obs. 
```

And here we have the "Currying Law".

Ok, but now the question is, "why is this useful for us, developers?" And the question is, because it help us to write less and better code.

# Using currying in C++

Let us start with a very simple case.

```c++
int sum(int a, int b, int c)
{
    return a + b +c;
}

int times(int a, int b)
{
    return a * b;
}

int main()
{
    auto v = std::vector<int>{1, 2, 3}
    for(auto& x : v)
    {
        v = sum(1, 2, a);
        v = times(2, v);
    }
    return 0;
}
```

No tricks here. We just double every item in the list. This code is already simple, I grant,
but the pattern is recognizable but any developer.

Pretty soon C++ will allow us to develop this like:

```c++
int sum(int a, int b, int c)
{
    return a + b +c;
}

int times(int a, int b)
{
    return a * b;
}

int main()
{
    auto v = std::vector<int>{1, 2, 3}
    std::transform(v, [](auto v){
        v = sum(1, 2, a);
        v = times(2, v);
        return v;
    });    
    return 0;
}
```

Which is a little bit simpler, and safer, but also unrealistic. The more realistic approach would be:

```c++
int sum(int a, int b, int c)
{
    return a + b +c;
}

int times(int a, int b)
{
    return a * b;
}

int main()
{
    int param1 = parseFromSomewhere();
    auto v = std::vector<int>{1, 2, 3}
    std::transform(v, [&](auto v){
        v = sum(1, param1, a);
        v = times(2, v);
        return v;
    });    
    return 0;
}
```

Now we are capturing "param1" by reference using closure. Definitely more complex.
Would not be easier if we could do something like shell?

```
v | sum -a=1 -b=param1 | times -a=2
```

And it is possible. If we use "currying" we would transform "sum" from a function that 
receives three parameters, in:

```c++
int sum (int a, int b, int c)
{
    return a + b + c;
}

auto r = curry(sum)(1)(2)(3); // r = 6
```

with this we could do

```c++
int main()
{
    int param1 = parseFromSomewhere();
    auto step1 = curry(sum)(1)(param1);
    auto step2 = curry(times)(2);

    auto v = std::vector<int>{1, 2, 3}
    std::transform(v, [&](auto v){
        v = step1(v);
        v = step2(v);
        return v;
    });    
    return 0;
}
```

We have not "killed" the complex parts as closures, captures etc...
but the "lamda" body is definately simpler. It does not care for
some arguments, only for "v". Which passess as an item pass a "pipeline".

That is why we use another "function" called pipeline. It receives a list of functions,
and return a function that does exactly this. Call them in sequence, passing the result of one to the another.

```c++
int main()
{
    int param1 = parseFromSomewhere();
    auto step1 = curry(sum)(1)(param1);
    auto step2 = curry(times)(2);
    auto pipe = pipeline(step1, step2);

    auto v = std::vector<int>{1, 2, 3}
    std::transform(v, [&](auto v){
        return pipe(v);
    });    
    return 0;
}
```

Now this is much more pleasant. And the question is now, do we need the lamda? No. We can have.

```c++
int main()
{
    int param1 = parseFromSomewhere();
    auto step1 = curry(sum)(1)(param1);
    auto step2 = curry(times)(2);
    auto pipe = pipeline(step1, step2);

    auto v = std::vector<int>{1, 2, 3}
    std::transform(v, pipe);    
    return 0;
}
```

Wow! Much better. The question, can such thing be built in C++? The answer is yes, it can. This link has 
a header only, very simple "currying" library.

We can achieve this very easily.

First we "curry" method with the "$" (O jQuery, Where Art Thou?). But we can also specify any number of parameters.
When we reach the "right" number, the function is called.

```c++
int main (int argc, char** argv)
{
    auto f = $(sum);
    std::cout << "1: " << f(1)(2)(3) << std::endl;
    std::cout << "2: " << f(1,2)(3) << std::endl;
    std::cout << "3: " << f(1)(2,3) << std::endl;
    std::cout << "4: " << f(1,2,3) << std::endl;
    return 0;
}
```

But we can also bind using the "<<" and generate function that expects no arguments


```c++
int main (int argc, char** argv)
{
    auto f = $(sum);
    std::cout << "5: " << (f << 1 << 2 << 3)() << std::endl;
    std::cout << "6: " << (f << 1 << 2)(3) << std::endl;
    std::cout << "7: " << (f << 1)(2, 3) << std::endl;
}
```

We can use the "partially bound" function in any STL algorithms:

```c++
int main()
{
    auto v = std::vector<int>{1,2,3};
    std::transform(v, $(times) << 2);
    std::cout << "8: " << v << std::endl;
    
    std::transform(v, $(times)(2));
    std::cout << "9: " << v << std::endl;
    return 0;
}
```

And we can generate out pipeline with the function "$$", pretty much like we do in shell:

```c++
int main()
{
    auto p1 = $$($(sum) << 1 << 2, $(times) << 2);
    std::cout << "9: " << p1(10) << std::endl;

    std::transform(std::begin(v), std::end(v),
        std::begin(v),
        $$($(sum) << 1 << 2, $(times) << 2));
    std::cout << "11: " << v << std::endl;
    return 0;
}
```

# Func Performance

"Ok, but this MUST be slow!" I hear you saying. Look at all that crazy code.

The truth is that it isn't. 100% of the code can, and will, be scrapped by any
decent compiler in -O3. Let us see what godbolt tell us about that.

```c++
auto r2 = ($(sum) << rand() << rand() << rand())();
```

This generates:

```asm
call    rand
mov     ebx, eax # ebx = rand()

call    rand
mov     ebp, eax # ebp = rand()

call    rand     # eax = rand()

add     ebp, ebx
add     ebp, eax # ebp = ebp + ebx + eax
```

Which is an 100% equal a simple call to sum. The compiler kills everything. Wonderful beasts they are!

Regard of this we have a test that mandates the bind and call to never be 5% slower than just calling a function. 5% here is mainly for noise when running the test, because we expect the generated assembly to be similar.

# Stackoverflow Code Review

https://codereview.stackexchange.com/questions/238306/functional-utility-in-c-curry-partial-binding-and-pipeline

TLDR

I created a curry/partial binding "lib. My request for "code review" is, what possible improvements I need to achieve "release quality", if I wanted to release this as a lib?

You can see the end result here. https://godbolt.org/z/SnFnFt

Complete story and analyzing the code

This weekend I caught myself testing some new C++2x functionalities and come up with a "lib" that allow me to do this:

```
int sum(int a, int b, int c)
{
    return a+b+c;
}
...
auto f = $(sum);
std::cout << "1: " << f(1)(2)(3) << std::endl;
std::cout << "2: " << f(1,2)(3) << std::endl;
std::cout << "3: " << f(1)(2,3) << std::endl;
std::cout << "4: " << f(1,2,3) << std::endl;
std::cout << "5: " << (f << 1 << 2 << 3)() << std::endl;
std::cout << "6: " << (f << 1 << 2)(3) << std::endl;
std::cout << "7: " << (f << 1)(2, 3) << std::endl;
...
auto v = std::vector<int>{1,2,3};
std::transform(std::begin(v), std::end(v),
    std::begin(v),
    $$(
        $(sum) << 1 << 2,
        $(times) << 2)
    );
std::cout << "11: " << v << std::endl;
```

Complete code is at the end of this post. I will try to explain my rationale and the code as much as possible here.

The code relies heavily on "parameter pack" and "fold expressions" and very complex enable_if. Truly the only complex and hard-to-read part of the code, in my opinion. But let me try to explain it as much as possible.

It all start at the function $. TF here is meant to be callable. TF* is, of course, a pointer to this callable. In the example above, a pointer to sum. All I do here is create my wrapper class and return it. All the "magic" will happen in side this wrapper.

```
template <typename TF>
auto $(TF* f)
{
    return typename make_f<TF>::type
    {
        std::make_tuple(f)
    };
}
```

Here I have my first problem. To create my wrapper I need to know all TF arguments types, because I will use them to guarantee the binding is correct.

There is a "very easy way" to do this.

```
template<typename... TArgs> struct types { using type = std::tuple<TArgs...>; };

template<class T> struct args;
template<class TReturn, class... TArgs> struct args<TReturn     (TArgs...)> : types<TArgs...> {};
template<class TReturn, class... TArgs> struct args<TReturn  (*)(TArgs...)> : types<TArgs...> {};
template<class T> using args_t = typename args<T>::type;
```

So args_t<(int (*) (int,int)> returns std::tuple<int,int>.
See more here: https://godbolt.org/z/h4sbtW

With this I can build my wrapper type with:

using type = Func<TF, args_t<TF>, std::tuple<TF*>>;
This is the type of the wrapper completely unbound. Its template parameters are: 1 - function type;
2 - std:tuple of TF arguments, as we saw above'
3 - std:tuple of all bound parameters so far. Up until now just the function pointer.

The idea now is that everytime you give one more argument, I store in a "bigger" tuple with std::tuple_cat(old, new_argument), until this "catted" tuple matches the function definition.

Then I just call the target function.

The "heart" of the code that does that is:

```
template <typename TF,
    typename... TArgs,
    typename... TBounds>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TBounds...>>
{
...
// all args to the target function
using tuple_args = std::tuple<TArgs...>;        
// bound args so far
using tuple_bound = std::tuple<TF*, TBounds...>;
...
// bound args store in a tuple
tuple_bound bound;                              
...
// magic happen here: bind, partial apply on operator()
template <
        typename... TBinds,
        size_t QTD = sizeof...(TBinds),

        // avoid binding more arguments than possible
        typename = std::enable_if_t<QTD <= (sizeofArgs - sizeofBounds)>,

        // test if arguments types match, otherwise generate compiler error
        // more on this below
        typename = std::enable_if_t<
            types_match<
            sizeofBounds,
                tuple_args,
                std::tuple<TBinds...>,                
                std::make_index_sequence<sizeof...(TBinds)>
            >::type::value                  
        >
    >
    auto operator() (TBinds... binds)                       
    {                                       
        auto newf = Func<TF, tuple_args,    
            std::tuple<TF*, TBounds..., TBinds...>
        >
        {
            std::tuple_cat(bound, std::make_tuple(binds...))
        };
        // if we bound exactly the number of args, call target function
        // else returns a partial applied function
        if constexpr (QTD == (sizeofArgs - sizeofBounds))           
            return newf();
        else 
            return newf;                            
    }
}
```

I have a "Func" template specialization for when the bind is complete:

```
template <typename TF, typename... TArgs>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TArgs...>>
{
    std::tuple<TF*, TArgs...> bound;

    auto operator() ()
    {
        return std::apply(fwd, bound);
    }

    static auto fwd(TF* f, TArgs... args)
    {
        return f(args...);
    }
};
```

It guarantees that only the exact types are bound and it only allows you to call the function. In theory it represents a "auto (*) ()" function. If that was possible.

One nice feature of all of this, is that I can generate an (horrible) compile error when you try to bind arguments with the wrong type. This is done by the enable_if below that exists on the "operator ()" of the Func class.

```
template <
        // all new arguments that you are trying to bind
        typename... TBinds,
        // enabled only if the new binds match possible arguments
        typename = std::enable_if_t<
            types_match<
                sizeofBounds,
                tuple_args,
                std::tuple<TBinds...>,                
                std::make_index_sequence<sizeof...(TBinds)>
            >::type::value  
```

The "magic" here is:

1 - tuple_args would be, for example, std:tuple<int,int>;
2 - We have not bound anything yet, so sizeofBounds is zero, and we are passing TBinds... new arguments. So, I need to check if these new types match what is expected. I do this with the types_match type trait. It receives two std::tuple and an offset, and check if their types match.

Something like this.

```
std::is_same<
    decltype(std::get<0>(tuple1),
    decltype(std::get<0 + OFFSET>(tuple2)
> && std::is_same<
    decltype(std::get<1>(tuple1),
    decltype(std::get<1 + OFFSET>(tuple2)
> && ...
```
See more here: https://godbolt.org/z/wG8JYr

This allows you to bind any number of arguments at any time. The only constraints are that the types must match, and you cannot bind more than needed.

All of this seems like a huge burden to a simple function call, but "Godbolting" this with:

```
($(sum) << rand() << rand() << rand())();
```

generates:

```
call    rand
mov     ebx, eax # ebx = rand()

call    rand
mov     ebp, eax # ebp = rand()

call    rand     # eax = rand()

add     ebp, ebx
add     ebp, eax # ebp = ebp + ebx + eax
```

Which I consider a wonderful result. The compiler optimization killed everything. Wonderful beasts they are!

I even did a small performance test to assert this. The performance is pretty much identical to normally calling the function.

https://github.com/xunilrj/sandbox/blob/master/sources/cpp/func/main.cpp#L87

```
TEST_CASE("Func.Performance.Should not be slower than manual code", "[ok]")
{
    using namespace std;

    // I will generate some random numbers below

    random_device rnd_device;
    mt19937 mersenne_engine{ rnd_device() };
    uniform_int_distribution<int> dist{ 1, 52 };
    auto gen = [&dist, &mersenne_engine]() { return dist(mersenne_engine); };
    vector<int> vec(3);

    std::clock_t    start;
    start = std::clock();

    // Benchmark. Manual calling sum with tree random numbers
    /* MANUAL CODE */
    auto r = true;
    for (int i = 0; i < 10000000; ++i)
    {
        generate(begin(vec), end(vec), gen);
        auto expected = sum(vec[0], vec[1], vec[2]);

        //using this just to guarantee that the compiler will not drop my code.
        r &= (sum(vec[0], vec[1], vec[2]) == expected);
    }
    /* MANUAL CODE */
    auto manualTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);


    start = std::clock();
    // Now we are using the "lib" code
    /* FUNC CODE */
    auto rr = true;
    for (int i = 0; i < 10000000; ++i)
    {
        generate(begin(vec), end(vec), gen);
        auto expected = sum(vec[0], vec[1], vec[2]);

        auto f = $(sum) << vec[0] << vec[1] << vec[2];

        // again just to guarantee nothing is dropped.
        rr &= (f() == expected);
    }
    /* FUNC CODE */
    auto funcTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);


    std::cout << "manual: " << manualTime 
        << ", func: " << funcTime 
        << " (func/manual = " << (float)funcTime / (float)manualTime << ")" << std::endl;
    REQUIRE(r == rr);

    // Assert that we are inside a "noise threshold" in RELEASE
    #ifdef NDEBUG
        REQUIRE(funcTime < (manualTime * 1.05)); // Func cannot be 5% slower than manual code
    #endif
}
```

Complete code using the "lib":
https://github.com/xunilrj/sandbox/blob/master/sources/cpp/func/main.cpp

Possible steps would be:

1 - Test with member function;
2 - Test functions with (references, pointers, moves etc...);
3 - Test with Polymorphism;
4 - Test with unmaterialized templates;
5 - The pipeline function creates a tuple of "constructed" objects. Is this avoidable?
6 - Test bounding High-Order-Functions with other function and "partial applied" function.
7 - Test with more callback, events, observers etc... systems.

Complete "lib" code:
https://github.com/xunilrj/sandbox/blob/master/sources/cpp/func/func.h

Me going through what/why and how:
https://github.com/xunilrj/sandbox/tree/master/sources/cpp/func

Complete code

```
#include <tuple>
#include <iostream>
#include <type_traits>
#include <algorithm>
#include <vector>

template <typename TF,
    typename TArgs,
    typename TBound>
struct Func{};

template <typename TF, typename... TArgs>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TArgs...>>
{
    std::tuple<TF*, TArgs...> bound;

    auto operator() ()
    {
        return std::apply(fwd, bound);
    }

    static auto fwd(TF* f, TArgs... args)
    {
        return f(args...);
    }
};

template <typename TF,
    typename... TArgs,
    typename... TBounds>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TBounds...>>
{
    using result_of = std::invoke_result_t<TF, TArgs...>;

    using tuple_args = std::tuple<TArgs...>;
    using tuple_bound = std::tuple<TF*, TBounds...>;
```
