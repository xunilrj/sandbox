# Can OO Strike Back?

Theses days is almost impossible to go to kindergarten playground and do not listen kids talking about monads and categories.

But it is true that OO has reached its maturity point and we can expect OO to die? It is true that all the usefulness from functional languages are really functional?

First, let us try to rebuild the OO history starting from a mature project done by Martin Abadi and Luca Cardelli in the book "The Theory of Objects" [TTO]. 

Their idea was to create the ς-calculus. A object oriented competitor of the λ-calculus. Lambda calculus was created as a computation model by Lorenzo Church in 1936.

For our very fast introduction to -calculus we will use the book "The Lambda Calculus: Its Syntax and Semantics" [TLC].

## λ-Calculus

### History

#### From Mathematics Point-of-View

To understand what the λ-calculus creators were trying to achieve we must understand Mathematics on the beginning of the 20th century.

One option is to read "Mathematics and Its History" Chapter 24 by John Stillwell (http://www.springer.com/gb/book/9781441960528), see N. J. Wildberg video https://youtu.be/5LsdsnjXT_Y?list=PL55C7C83781CF4316 at 19:28-23:00)
and read "History of Lambda-calculus and
Combinatory Logic" by Felice Cardone and J. Roger Hindley (see http://www.users.waitrose.com/~hindley/SomePapers_PDFs/2006CarHin,HistlamRp.pdf).

The following quotes are from this paper as a summary of the λ-calculus.

    Seen in outline, the history of λ and CL splits into three main periods:
    first, several years of intensive and very fruitful study in the 1920s
    and ’30s;
    next, a middle period of nearly 30 years of relative quiet;
    then in the late 1960s an upsurge of activity stimulated by developments
    in higher-order function theory, by connections with programming 
    languages, and by new technical discoveries.

The real start of λ-calculus can be described as:

    Around 1928 [Alonzo Church] began to build a formal system with the aim
    of providing a foundation for logic which would be more natural than
    Russell’s type theory or Zermelo’s set theory, and would not contain free
    variables (for reasons he explained in [Church, 1932, pp. 346–347]). He
    chose to base it on the concept of function rather than set, and his
    primitives included abstraction λx[M] and application {F}(X), which we
    shall call here “λx.M” and “(F X)”.

    Church was not the first to introduce an explicit notation for
    function-abstraction. But he was the first to state explicit formal
    conversion rules for the notation, and to analyse their consequences in
    depth.

    At the end of his 1933 paper, Church introduced the representation of the
    positive integers by the λ-terms now known as the Church numerals [Church,
    1933, §9]:

    From 1931 to 1934 in Princeton University, Church received the help of
    two outstanding graduate students, Stephen Kleene and Barkley Rosser, and
    in a remarkable four years of collaboration this group made a series of
    major discoveries about both Church’s 1933 logic and the underlying pure
    λ-calculus. Unfortunately for the 1933 logic, one of these discoveries
    was its inconsistency, as a variant of the Richard paradox was proved in
    the system, [Kleene and Rosser, 1935].

     In fact in the later 1930s Church retreated from the task of designing a
     general type-free logic to the much less ambitious one of re-formulating
     simple type theory on a λ-calculus base. In this he succeeded, publishing a
     smooth and natural system in [Church, 1940] that has been
     the foundation for much type-theoretic work since (see §5.1 and §8 below).

     For many years after the mid-1930s, neither λ nor CL attracted much interest
     among logicians in general. 

     However, on the expository side, an advance in opening up λ to non-specialists
     was made by Church with his readable introductory textbook [Church, 1941].

    Returning to the 1960s: at about this time,λ and CL began to attract the interest
    of a new group outside the community of logicians, namely computer scientists
    involved in the theory and practice of programming languages

    From 1956 to ’60, John McCarthy in the U.S.A. developed the computer language
    LISP, which was a list-processing language with a function-abstraction facility,
    [..] and to encourage the style of program-organization that is nowadays called
    functional programming.

#### From Computer Science Point-of-View

    LISP was not directly based on λ-calculus, although it owed something to λ (and
    its abstraction notation was even called “LAMBDA”). LISP’s substitution procedure
    was not defined exactly the same as in λ, in particular identifiers were handled
    according to so-called “dynamic binding” rules. This choice simplified LISP’s
    implementation by means of interpreters, but it greatly complicated the use of
    bound variables by the programmer.

    In the early 1960s in England, Peter Landin proposed the use of λ-terms to code
    constructs of the programming language Algol 60, see [Landin, 1965]. Landin
    described in 1963 an abstract machine for reducing λ-terms considered as programs,
    the SECD-machine of [Landin, 1964, pp. 316–318] and [Landin, 1966a]. This
    consisted of a transition system whose states were made of four components:
    • a stack S for storing intermediate results of evaluation,
    • an environment E, that associates values to free identifiers,
    • a control C consisting of a list of expressions that drives the evaluation
    process,
    • a dump D that records a complete state of the machine, of the form
    (S', E', C', D').

#### λ-calculus on LISP

To better understand how LISP started using -calculus let us understand a little better LISP history using the famous 1978 book "History of Programming Languages" from  Richard L. Wexelblat (see https://dl.acm.org/citation.cfm?id=800025).

This book contains papers based on famous sessions about the history and development of languages that existed on 1978. This particular session about LISP had two giants of computation: Barbara Liskov as chairman and John McCarthy as speaker.

On the second paragraph of the paper, John McCarthy explains LISP philosophy:

    As a programming language, LISP is characterized by the following ideas: 
    
    - computing with symbolic expressions rather than numbers,
    - representation of symbolic expressions and other information by list structure
    in the memory of a computer,
    - representation of information in external media mostly by multilevel lists and 
    sometimes by S-expressions,
    - a small set of selector and constructor operations expressed as functions,
    - composition of functions as a tool for forming more complex functions,
    - the use of conditional expressions for getting branching into function 
    definitions,
    - the recursive use of conditional expressions as a sufficient tool for building 
    computable functions,
    - the use of λ-expressions for naming functions,
    - the representation of LISP programs as LISP data,
    - the conditional expression interpretation of Boolean connectives,
    - the LISP function eval that serves both as a formal definition of the language
    and as an interpreter,
    - and garbage collection as a means of han­dling the erasure problem.
    LISP statements are also used as a command language when LISP is used in a 
    time-sharing environment.

Obviously a lot of these decisions seems trivial and even obvious to us now, but we must remember that they had to be created by someone and even the risk of choosing new ideas.

#### XIF Digression - Feel free to skip

Digressing a little we cn read how the IF came to existence on LISP:

    I invented conditional expressions in connection with a set of chess legal move
    routines. I wrote in FORTRAN for the IBM 704 at MIT during 1957-1958. This
    program did not use list processing. The IF statement provided in FORTRAN I and
    FORTRAN II was very awkward to use, and it was natural to invent a function XIF(M,
    Nl,N2) whose value was Nl or N2 according to whether the expression M was zero or
    not. The function shortened many programs and made them easier to understand, but
    it had to be used sparingly, be­cause all three arguments had to be evaluated
    before XIF was entered, since XIF was called as an ordinary FORTRAN function
    though written in machine language. This led to the invention of the true
    conditional expression which evaluates only one of Nl and N2 according to whether
    M is true or false and to a desire for a programming language that would allow
    its use.

Today we can say that this function is known as IIF https://en.wikipedia.org/wiki/IIf)
and it exists in Visual Basic, ColdFusion, SQL and even on Excel. And, until today it
still have the problem stated by John McCarth of eagerly evaluating all of its 
arguments.

For example, let us analyze a very simple, and probably the most common implementation of IIF, using javascript:

    const iif = (cond, ok, nok) => {
        if(cond) return ok; else return nok;  
    }
    const x = undefined;
    console.log(iif(x !== undefined,x.id,2));

If we ran this we will get:

    "TypeError: Cannot read property 'id' of undefined

The problem is as John McCarthy specified: "be­cause all three arguments had to be
evaluated before XIF was entered". And in this case, the second parameter was not
defined. A possibility of course is to thunk both "ok" and "nok" arguments.

    const iif = (cond, ok, nok) => {
    if(cond) return ok(); else return nok();  
    }
    const x = undefined;    
    console.log(iif(x !== undefined, () => x.id, () => 2));

Unfortunately, javascript does not have a simple operator to turn a value on a thunk,
so we had to make it manually. The same does not happen when using the ternary
operator:

    const x = undefined;
    console.log((x !== undefined) ? (x.id) : (2));

Where we get the expected value:

    > 2

This problem is solved, for example in Haskell:

    main = do
        let iif cond ok nok = if cond then ok else nok
        let items = []
        putStrLn $ show $ iif (length items > 0) (head items) (0)

Where we get

    > 0

The problematic expression "head []" is never ran. That is one advantage of having
everything "lazy by default". Haskell is a pure functional language, but "lazy by
default" or a "call as lazy" is not a functional trait. OO languages could (should?)
have this feature.

### Maplist

Another set of inovations brought by LISP in the words of John McCarthy in person:

    I spent the summer of 1958 at the IBM Information Research Department at the
    invita­tion of Nathaniel Rochester and chose differentiating algebraic
    expressions as a sample problem. It led to the following innovations beyond FLPL:
    - (b) The maplist function that forms a list of applications of a functional
    argument to the elements of a list. This was obviously wanted for differentiating
    sums of arbitrarily many terms, and with a slight modification, it could be 
    applied to differentiating products. (The original form was what is now called 
    mapcar).

To apply a function to each element of a list is almost the default way of working
with lists in all majors languages now.

- C++: http://en.cppreference.com/w/cpp/algorithm/transform
- C#: https://msdn.microsoft.com/en-us/library/bb548891(v=vs.110).aspx
- Javascript:  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map
- Scala: http://www.scala-lang.org/api/current/scala/collection/Traversable.html#map[B](f:A=>B):Traversable[B]

### Back to λ-Calculus

The following paragraph, still on LISP innovations John McCarthy speaks how λ-calculus entered in the world of programming languages. Not as glamorous as you would expect, but still:

    (c) To use functions as arguments, one needs a notation for functions, and it
    seemed natural to use the λ-notation of Church (1941). I didn't understand the
    rest of his book, so I wasn't tempted to try to implement his more general
    mechanism for defining functions. Church used higher order functionals instead of
    using conditional expressions. Condi­tional expressions are much more readily
    implemented on computers.

To be Continued...