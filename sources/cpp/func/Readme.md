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

