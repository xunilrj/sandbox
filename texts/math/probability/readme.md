# Probability as a Measure Space

In Mathematics there is a very simple, but powerful concept called "Measure", that is simple a function that maps a mathematical object to a non-negative real number:

    f: S -> R

In this case "f" is a "measure" that maps some "element" of S to a non-negative Real Number (R). When this function "f" systematic follows some properties we have a "measure space".

# Probability as Set Measure

F# is a functional language for .NET of the OCamML family. Even if you have never seen F#, I bet you will be able to understand this code;

Let us get more concrete with a simple example. Our "element" will be sets, and out "measure" will be "length of the set". To illustrate we will have a "population" of six people. defined as:

    > type Person = {Name:string; Sex:int}
    > let population = [ 
        {Name="Stephen";Sex=0};
        {Name="Conrad";Sex=0};
        {Name="Michael";Sex=0};
        {Name="Carlos";Sex=0};
        {Name="Maeve";Sex=1};
        {Name="Dawn";Sex=1}
    ]

To "measure" this, and all of its "subsets" we will use the very simple "measure":

    let setSize = List.length

And applying our "measure" to the population we get:

    > printf "Population size %d\n" (setSize population)
    Population size 6

As expected.

Before continuing let us pull another trick from our toolbelt. First we can filter the "population" in F# doing:

    > let isMale x = x.Sex = 0
    > let filterMales = List.filter isMale
    > let males = filterMales population
    > printf "Males size %d\n" (setSize males)
    Males size 4

Interesting. What if we divide the male set measure by the population set measure.

    > let populationSize = setSize population
    > let malesSize = setSize males
    > printf "MALES/POPULATION %f\n" (malesSize ./ populationSize)
    MALES/POPULATION 0.666667

What is interesting about this number is that this is the Probability of choosing a male from our population. Is that true? How can we test it? Let us randomly choose someone from our "population" and see the percentage of males that comes from.

    > let r = System.Random()
    > let rnd min max = Seq.initInfinite (fun _ -> r.Next(min, max))
    > let rndn min max n = rnd min max |> Seq.take n |> Seq.toList
    > let getPerson i = population.[i]
    > let percentageOfMale n = (rndn 0 6 n |> List.map getPerson |> List.filter isMale |> List.length) ./ n
    > printf "Convergence"
    > printf "-----"
    > printf "%A\n" (percentageOfMale 10)
    > printf "%A\n" (percentageOfMale 100)
    > printf "%A\n" (percentageOfMale 1000)
    > printf "%A\n" (percentageOfMale 10000)
    > printf "%A\n" (percentageOfMale 100000)
    > printf "%A\n" (percentageOfMale 1000000)
    Convergence
    -----
    0.8
    0.65
    0.677
    0.6704
    0.66569
    0.666594

We can see that as we increase the "sample size" it converges to the calculated probability. So we can say that Probability is a "measure" of sets.

## Conditional Probability

But let us advance and say that, I have randomly chosen someone with a name starting with a M. But, I have already randomly chosen that I would only sample from the set of females. Does that change the probability?

Well, let us see. First how many people do we have in our "population" with names that starts with "M".

    > let startsWithM x = x.Name.StartsWith("M")
    > let filterStartM = List.filter startsWithM
    > let withM = filterStartM population
    > printf "WithM size %d\n" (setSize withM)
    WithM size 2

So we have a 2/6 = 0.333 probability of choosing someone with a name that stars with "M". Two because we have two people with names starting with "M", over six, because our population have six people. This is called the "absolute probability". Absolute because the denominator is our population. But, our problem stated that we had already chosen the female "subset". This changes our population. Instead of focusing on all six, we are now focusing only on the females. So we have to recalculate.

    > let isFemale x = x.Sex = 1
    > let filterFemales = List.filter isFemale
    > let females = filterFemales population
    > let femalesWithM = filterStartM females
    > printf "STARTS M / FEMALE %f\n" (setSize femalesWithM)
    STARTS M / FEMALE 0.500000

So the probability is completely differente, as expected. This is called "Conditional Probability". It is denoted and read as:

    P( WITHM | FEMALE ) = Probability of WITHM given FEMALE = P(WITHM AND FEMALE)/P(FEMALE)

or more generally

    P(A|B) = Probability of A given B = P(A AND B)/P(B)

So far so good.

## TODO
BINOMIAL
BINOMIAL PROCESS
POISSON
POISSON PROCESS
EXPONENTIAL
MARKOV PROPERTY
CHAPMAN KOLMOGOROV
MM1




