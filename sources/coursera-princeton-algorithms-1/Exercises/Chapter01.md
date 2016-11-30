# Exercises

## 1.1.1 Give the value of each of the following expressions:
1. ( 0 + 15 ) / 2
2. 2.0e-6 * 100000000.1
3. true && false || true && true

### Answers

1. ( 0 + 15 ) / 2  
15 / 2  
7

2. 2.0e-6 * 100000000.1  
0.0000002 * 100000000.1

3. true && false || true && true  
false || true
true

## 1.1.2 Give the type and value of each of the following expressions:
1. (1 + 2.236)/2
2. 1 + 2 + 3 + 4.0
3. 4.1 >= 4
4. 1 + 2 + "3"

### Answers

1. double  
3.236 / 2  
1.618

2. double  
10.0

3. boolean  
true

4. String
"33"

## 1.1.3 Write a program that takes three integer command-line arguments and prints equal if all three are equal, and not equal otherwise.

### Answers

    public static void Main(string[] args)
    {
        var num1 = int.Parse(args[0]);
        var num2 = int.Parse(args[1]);
        var num3 = int.Parse(args[2]);

        Console.WriteLine((num1 == num2) && (num2 == num3));
    }

## 1.1.4 What (if anything) is wrong with each of the following statements?
1. if (a > b) then c = 0;
2. if a > b { c = 0; }
3. if (a > b) c = 0;
4. if (a > b) c = 0 else b = 0;

### Answers

1. NOK. "then" is not a keyword;
2. NOK. Missing parenthesis;
3. OK;
4. NOK. Missing a semicolon after "c = 0".

## 1.1.5 Write a code fragment that prints true if the double variables x and y are both strictly between 0 and 1 and false otherwise.

### Answers

    bool BetweenZeroOne(double x, double y)
    {
        return Between(x, 0, 1) && Between(y, 0, 1);
    }

    bool Between(double x, double start, double end)
    {
        return (x > start) && (x < end);
    }

## 1.1.6 What does the following program print?

    int f = 0;
    int g = 1;
    for (int i = 0; i <= 15; i++)
    {
        StdOut.println(f);
        f = f + g;
        g = f - g;
    }

### Answers

It is the Fibonacci sequence

## 1.1.7 Give the value printed by each of the following code fragments:

1.

    double t = 9.0;
    while (Math.abs(t - 9.0/t) > .001)
        t = (9.0/t + t) / 2.0;
    StdOut.printf("%.5f\n", t);

2.

    int sum = 0;
    for (int i = 1; i < 1000; i++)
        for (int j = 0; j < i; j++)
            sum++;
    StdOut.println(sum);

3.

    int sum = 0;
    for (int i = 1; i < 1000; i *= 2)
        for (int j = 0; j < 1000; j++)
            sum++;
    StdOut.println(sum);

### Answers

1.

Aproximation of the squared root of 9.
3.00009

2.

Sum of the arithmetic progression starting between 1 and 999
999(1+999)/2 = 499,500

3.

log_2 {1000} = (log 1000) / (log 2)  
             = 3 / 0.30102999566398119521373889472449  
             = 9.9657842846620870436109582884683  
             = 9 <- Max integer power lower than 1000  

Starting in zero and finishing in 9, we have  
= (9-0)+1  
= 9+1  
= 10 items  

Just to be sure

0 1 2 3 4  5  6  7   8   9  
1 2 4 8 16 32 64 128 256 512  

OK!

So the algorithm will print the sum:

= 1000 + 1000 + 1000 + ... + 1000 <10 times>  
= 1000 * 10  
= 10,000  

## 1.1.8 What do each of the following print?
1. System.out.println('b');
2. System.out.println('b' + 'c');
3. System.out.println((char) ('a' + 4));
Explain each outcome.

###Answers
1. b
2. 197
3. 'a' = 61
61 + 4 = 65 = 'e'
so
e

## 1.1.9 Write a code fragment that puts the binary representation of a positive integer N into a String s .
Solution: Java has a built-in method Integer.toBinaryString(N) 
for this job, but the point of the exercise is to see how
such a method might be implemented. Here is a particularly
concise solution:

    String s = "";
    for (int n = N; n > 0; n /= 2)
    s = (n % 2) + s;

### Answers

    int x = 7;
    uint mask = (uint)1 << 31;
    while(mask > 0)
    {
        var r = x & mask;
        Console.Write(r > 0 ? "1" : "0");
        mask = mask >> 1;
    }

## 1.1.10 What is wrong with the following code fragment?

    int[] a;
    for (int i = 0; i < 10; i++)
        a[i] = i * i;

Solution: It does not allocate memory for a[] with new.
This code results in a variable a might not have been
initialized compile-time error.

## 1.1.11 Write a code fragment that prints the contents of a two-dimensional boolean array, using * to represent true and a space to represent false . Include row and column numbers.

### Answers

    private void Print(bool[,] matrix)
    {
        var ysize = matrix.GetLength(0);
        var xsize = matrix.GetLength(1);

        var ychars = (int)Math.Log10(ysize) + 1;
        var xchars = (int)Math.Log10(xsize) + 1;

        var ycolumnsize = ychars + 4;

        Console.Write(new string(' ', ycolumnsize));

        //Print Header
        for (int i = 0; i < matrix.GetLength(1); i++)
        {
            Console.Write("[,");
            Console.Write((i).ToString().PadLeft(xchars));
            Console.Write("] ");
        }

        Console.WriteLine();

        for (int y = 0; y < matrix.GetLength(0); y++)
        {
            Console.Write("[");
            Console.Write(y.ToString().PadLeft(ychars));
            Console.Write(",] ");

            for (int x = 0; x < matrix.GetLength(1); x++)
            {
                int lchars = xchars / 2;
                int lmod = xchars % 2;
                Console.Write(" ".PadLeft(lchars + lmod + 1));
                Console.Write(matrix[y, x] ? ("*") : (" "));
                Console.Write(" ".PadRight(lchars + 2));
            }

            Console.WriteLine();
        }
    }

## 1.1.12 What does the following code fragment print?

    int[] a = new int[10];
    for (int i = 0; i < 10; i++)
        a[i] = 9 - i;
    for (int i = 0; i < 10; i++)
        a[i] = a[a[i]];
    for (int i = 0; i < 10; i++)
        System.out.println(a[i]);

### Asnwers

0 1 2 3 4 4 3 2 1 0

## 1.1.13 Write a code fragment to print the transposition (rows and columns changed) of a two-dimensional array with M rows and N columns.

### Answers

little modification to the print method
to choose who is the x and y axis.

    private void PrintTransposition(bool[,] matrix)
    {
        Print(matrix, 1, 0);
    }
    
    private void Print(bool[,] matrix, int yaxis, int xaxis)
    {
        var ysize = matrix.GetLength(yaxis);
        var xsize = matrix.GetLength(xaxis);

        var ychars = (int)Math.Log10(ysize) + 1;
        var xchars = (int)Math.Log10(xsize) + 1;

        var ycolumnsize = ychars + 4;

        Console.Write(new string(' ', ycolumnsize));

        //Print Header
        for (int i = 0; i < matrix.GetLength(xaxis); i++)
        {
            Console.Write("[,");
            Console.Write((i).ToString().PadLeft(xchars));
            Console.Write("] ");
        }

        Console.WriteLine();

        for (int y = 0; y < matrix.GetLength(yaxis); y++)
        {
            Console.Write("[");
            Console.Write(y.ToString().PadLeft(ychars));
            Console.Write(",] ");

            for (int x = 0; x < matrix.GetLength(xaxis); x++)
            {
                int lchars = xchars / 2;
                int lmod = xchars % 2;
                Console.Write(" ".PadLeft(lchars + lmod + 1));
                var indices = new[] { y, x }
                    .Zip(new[] { yaxis, xaxis }, Tuple.Create)
                    .OrderBy(item => item.Item2)
                    .Select(item => item.Item1)
                    .ToArray();
                Console.Write((bool)matrix.GetValue(indices) ? ("*") : (" "));
                Console.Write(" ".PadRight(lchars + 2));
            }

            Console.WriteLine();
        }
    }

## 1.1.14 Write a static method lg() that takes an int value N as argument and returns the largest int not larger than the base-2 logarithm of N . Do not use Math .

### Answers

    static int Lg(int x)
    {
        int lg = x;
        int i = 0;

        while (lg > 1)
        {
            lg = lg >> 1;
            i++;
        }

        return i;
    }

## 1.1.15 Write a static method histogram() that takes an array a[] of int values and an integer M as arguments and returns an array of length M whose i th entry is the number of times the integer i appeared in the argument array. If the values in a[] are all between 0 and M–1 , the sum of the values in the returned array should be equal to a.length.

### Asnwers

    private int[] Histogram(int size, int[] numbers)
    {
        var hist = new int[size];

        for (int i = 0; i < numbers.Length; i++)
        {
            hist[numbers[i]]++;
        }

        return hist;
    }


## 1.1.16 Give the value of exR1(6) :

    public static String exR1(int n)
    {
        if (n <= 0) return "";
        return exR1(n-3) + n + exR1(n-2) + n;
    }

### Answers

exR1(6) = exR1(3) + 6 + exR1(4) + 6
        = (exR1(0) + 3 + exR1(1) + 3) 
            + 6 
            + (exR1(1) + 4 + exR1(2) + 4)
            + 6
        = ("" + 3 + (exR1(-2) + 1 + exR1(-1) + 1) + 3)
            + 6
            + (exR1(1) + 4 + exR1(2) + 4)
            + 6
        = "3" + ("" + 1 + "" + 1) + 3
            + 6
            + (exR1(1) + 4 + exR1(2) + 4)
            + 6
        = "31136" + (exR1(1) + 4 + exR1(2) + 4) + 6
        = "31136 + ("114" + "22" + 4) + 6
        = "311361142246"

## 1.1.17 Criticize the following recursive function:
    
    public static String exR2(int n)
    {
        String s = exR2(n-3) + n + exR2(n-2) + n;
        if (n <= 0) return "";
        return s;
    }

Answer : The base case will never be reached. A call to exR2(3) will result in calls to exR2(0) , exR2(-3) , exR3(-6) , and so forth until a StackOverflowError occurs.

### Answers

The base case return must always be before the recursion call.

## 1.1.18 Consider the following recursive function:

    public static int mystery(int a, int b)
    {
        if (b == 0) return 0;
        if (b % 2 == 0) return mystery(a+a, b/2);
        return mystery(a+a, b/2) + a;
    }

What are the values of mystery(2, 25) and mystery(3, 11)?  
Given positive integers a and b , describe what value mystery(a, b) computes. Answer the same question, but replace the three + operators with * and replace return 0 with return 1 .

### Answers

mystery(2,25) = 50
mystery(3,11) = 33

It is the multiplication operator!

Changing it we got

mystery2(2,4) = 16

It is the power operator!

### 1.1.19 Run the following program on your computer:

    public class Fibonacci
    {
        public static long F(int N)
        {
            if (N == 0) return 0;
            if (N == 1) return 1;
            return F(N-1) + F(N-2);
        }
        public static void main(String[] args)
        {
            for (int N = 0; N < 100; N++)
                StdOut.println(N + " " + F(N));
        }
    }

What is the largest value of N for which this program takes less than 1 hour to compute
the value of F(N) ? Develop a better implementation of F(N) that saves computed values
in an array.  

### Answers

The following table shows that the time increases by 1.61 in each round.

00:00:00.01	19	4181	1.33571597633136  
00:00:00.01	20	6765	1.49151220895205  
00:00:00.01	21	10946	1.59738392814713  
00:00:00.02	22	17711	1.79665315533078  
00:00:00.04	23	28657	1.47935372502266  
00:00:00.06	24	46368	1.70212890985712  
00:00:00.09	25	75025	1.55188785483073  
00:00:00.16	26	121393	1.6538437499669  
00:00:00.25	27	196418	1.60038354939479  
00:00:00.41	28	317811	1.64444133150292  
00:00:00.66	29	514229	1.61265742867671  
00:00:01.06	30	832040	1.60300746932154  
00:00:01.73	31	1346269	1.62542306210535  
00:00:02.79	32	2178309	1.61393130753698  
00:00:04.53	33	3524578	1.62574927494153  
00:00:07.33	34	5702887	1.61805214194821  
00:00:11.90	35	9227465	1.62367009149957  
00:00:19.27	36	14930352	1.61897180898301  
*Prediction:*  
00:00:31.18	37  
00:00:50.46	38  
00:01:21.66	39  
00:02:12.15	40  
00:03:33.87	41  
00:05:46.11	42  
00:09:20.12	43  
00:15:06.46	44  
00:24:26.94	45  
00:39:34.00	46  
01:04:01.91	47  

With this pattern 46 will be the last number that the code run for less than one hour.

After the modification almost all are instantaneous.

00:00:00.0005966 - 0 - 0  
00:00:00.0000013 - 1 - 1  
00:00:00.0000041 - 2 - 1  
00:00:00.0000020 - 3 - 2  
00:00:00.0000020 - 4 - 3  
00:00:00.0000013 - 5 - 5  
00:00:00.0000013 - 6 - 8  
00:00:00.0000013 - 7 - 13  
00:00:00.0000013 - 8 - 21  
00:00:00.0000020 - 9 - 34  
00:00:00.0000020 - 10 - 55  
00:00:00.0000020 - 11 - 89  
00:00:00.0000020 - 12 - 144  
00:00:00.0000020 - 13 - 233  
00:00:00.0000013 - 14 - 377  
00:00:00.0000020 - 15 - 610  
00:00:00.0000020 - 16 - 987  
00:00:00.0000013 - 17 - 1597  
00:00:00.0000013 - 18 - 2584  
00:00:00.0000013 - 19 - 4181  
00:00:00.0000013 - 20 - 6765  
00:00:00.0000013 - 21 - 10946  
00:00:00.0000020 - 22 - 17711  
00:00:00.0000020 - 23 - 28657  
00:00:00.0000013 - 24 - 46368  
00:00:00.0000013 - 25 - 75025  
00:00:00.0000013 - 26 - 121393  
00:00:00.0000013 - 27 - 196418  
00:00:00.0000013 - 28 - 317811  
00:00:00.0000020 - 29 - 514229  
00:00:00.0000020 - 30 - 832040  
00:00:00.0000020 - 31 - 1346269  
00:00:00.0000020 - 32 - 2178309  
00:00:00.0000020 - 33 - 3524578  
00:00:00.0000013 - 34 - 5702887  
00:00:00.0000013 - 35 - 9227465  
00:00:00.0000013 - 36 - 14930352  
00:00:00.0000013 - 37 - 24157817  
00:00:00.0000013 - 38 - 39088169  
00:00:00.0000013 - 39 - 63245986  
00:00:00.0000013 - 40 - 102334155  
00:00:00.0000013 - 41 - 165580141  
00:00:00.0000013 - 42 - 267914296  
00:00:00.0000013 - 43 - 433494437  
00:00:00.0000013 - 44 - 701408733  
00:00:00.0000020 - 45 - 1134903170  
00:00:00.0000013 - 46 - 1836311903  
00:00:00.0000020 - 47 - 2971215073  
00:00:00.0000013 - 48 - 4807526976  
00:00:00.0000013 - 49 - 7778742049  
00:00:00.0000020 - 50 - 12586269025  
00:00:00.0000013 - 51 - 20365011074  
00:00:00.0000013 - 52 - 32951280099  
00:00:00.0000013 - 53 - 53316291173  
00:00:00.0000013 - 54 - 86267571272  
00:00:00.0000020 - 55 - 139583862445  
00:00:00.0000020 - 56 - 225851433717  
00:00:00.0000020 - 57 - 365435296162  
00:00:00.0000013 - 58 - 591286729879  
00:00:00.0000013 - 59 - 956722026041  
00:00:00.0000013 - 60 - 1548008755920  
00:00:00.0000013 - 61 - 2504730781961  
00:00:00.0000020 - 62 - 4052739537881  
00:00:00.0000020 - 63 - 6557470319842  
00:00:00.0000020 - 64 - 10610209857723  
00:00:00.0000020 - 65 - 17167680177565  
00:00:00.0000013 - 66 - 27777890035288  
00:00:00.0000013 - 67 - 44945570212853  
00:00:00.0000013 - 68 - 72723460248141  
00:00:00.0000013 - 69 - 117669030460994  
00:00:00.0000013 - 70 - 190392490709135  
00:00:00.0000013 - 71 - 308061521170129  
00:00:00.0000013 - 72 - 498454011879264  
00:00:00.0000013 - 73 - 806515533049393  
00:00:00.0000020 - 74 - 1304969544928657  
00:00:00.0000013 - 75 - 2111485077978050  
00:00:00.0000013 - 76 - 3416454622906707  
00:00:00.0000020 - 77 - 5527939700884757  
00:00:00.0000020 - 78 - 8944394323791464  
00:00:00.0000013 - 79 - 14472334024676221  
00:00:00.0000020 - 80 - 23416728348467685  
00:00:00.0000020 - 81 - 37889062373143906  
00:00:00.0000020 - 82 - 61305790721611591  
00:00:00.0000013 - 83 - 99194853094755497  
00:00:00.0000020 - 84 - 160500643816367088  
00:00:00.0000013 - 85 - 259695496911122585  
00:00:00.0000013 - 86 - 420196140727489673  
00:00:00.0000013 - 87 - 679891637638612258  
00:00:00.0000013 - 88 - 1100087778366101931  
00:00:00.0000013 - 89 - 1779979416004714189  
00:00:00.0000013 - 90 - 2880067194370816120  
00:00:00.0000013 - 91 - 4660046610375530309  
00:00:00.0000013 - 92 - 7540113804746346429  
00:00:00.0000013 - 93 - -6246583658587674878  
00:00:00.0000020 - 94 - 1293530146158671551  
00:00:00.0000013 - 95 - -4953053512429003327  
00:00:00.0000013 - 96 - -3659523366270331776  
00:00:00.0000013 - 97 - -8612576878699335103  
00:00:00.0000013 - 98 - 6174643828739884737  
00:00:00.0000013 - 99 - -2437933049959450366  

## 1.1.20 Write a recursive static method that computes the value of ln(N!).

### Answers

log_e{N!} = log_e{N*(N-1)!} = log_e{N} + log_e{N-1!}

    public static double lnf(int N)
    {
        if (N == 0) return 0;
        return Math.Log(N) + lnf(N - 1);
    }

## 1.1.21 Write a program that reads in lines from standard input with each line containing a name and two integers and then uses printf() to print a table with a column of the names, the integers, and the result of dividing the first by the second, accurate to three decimal places. You could use a program like this to tabulate batting averages for baseball players or grades for students.

## Answers

    private void Print(StringReader reader)
    {
        string line = reader.ReadLine();
        while (line != null)
        {
            var data = line.Split(' ');
            var result = double.Parse(data[1]) / double.Parse(data[2]);
            Console.WriteLine($"{data[0]} {data[1]} {data[2]} {result:N3}");
            line = reader.ReadLine();
        }
    }

## 1.1.22 Write a version of BinarySearch that uses the recursive rank() given on page 25 and traces the method calls. Each time the recursive method is called, print the argument values lo and hi , indented by the depth of the recursion. Hint: Add an argument to the recursive method that keeps track of the depth.

### Arnswers

    public static int TracedRank(int key, int[] a)
    {
        return TracedRank(key, a, 0, a.Length - 1, 0);
    }

    public static int TracedRank(int key, int[] a, int lo, int hi, int depth)
    {
        // Index of key in a[], if present, is not smaller than lo
        // and not larger than hi.
        Console.WriteLine($"{new String(' ', depth * 4)} {lo} {hi}");

        if (lo > hi) return -1;

        int mid = lo + (hi - lo) / 2;

        if (key < a[mid])
        {
            return TracedRank(key, a, lo, mid - 1, ++depth);
        }
        else if (key > a[mid])
        {
            return TracedRank(key, a, mid + 1, hi, ++depth);
        }
        else
        {
            return mid;
        }
    }

## 1.1.23 Add to the BinarySearch test client the ability to respond to a second argument: + to print numbers from standard input that are not in the whitelist, - to print numbers that are in the whitelist.

### Answers

    public static void BlackWhiteList(int[] numbers, string op, TextReader reader)
    {
        string line = reader.ReadLine();
        while(line != null)
        {
            var n = int.Parse(line);
            var rank = BinarySearch.Rank(n, numbers);

            if (op == "+")
            {
                if(rank == -1)
                {
                    Console.WriteLine(n);
                }
            }
            else
            {
                if (rank != -1)
                {
                    Console.WriteLine(n);
                }
            }

            line = reader.ReadLine();
        }
    }

## 1.1.24 Give the sequence of values of p and q that are computed when Euclid’s algorithm is used to compute the greatest common divisor of 105 and 24. Extend the code given on page 4 to develop a program Euclid that takes two integers from the command line and computes their greatest common  divisor, printing out the two arguments for each call on the recursive method. Use your program to compute the greatest common divisor of 1111111 and 1234567.

### Answers

    private void TracedGCD(TextReader reader)
    {
        var numbers = reader.ReadLine();
        var data = numbers.Split(' ').Select(x => int.Parse(x)).ToArray();

        var gcd = Euclid.TracedGCD(data[0], data[1]);
        Console.WriteLine(gcd);
    }

    public static int TracedGCD(int p, int q)
    {
        Console.WriteLine($"{p} {q}");

        if (q == 0) return p;
        int r = p % q;
        return TracedGCD(q, r);
    }

## 1.1.25 Use mathematical induction to prove that Euclid’s algorithm computes the greatest common divisor of any pair of nonnegative integers p and q.

gcd(a,b) = g
g|a
g|b

require a > b
gcd(a,b) =  
    1. a                if b == 0  
    2. gcd(b, a MOD b)  else

Induction on N = a*b:
Induction Step = gcd(b, a MOD b) = gcd(a, b)

Base When N = 0
b = 0
gcd(a,0) = a
a|a => OK
a|0 => OK
a = 1*a + 0*0 => OK

Assume that 
gcd(b, a MOD b) = g
g|b
g|(a MOD b)
g = g_1*a + g_2*b

So we have to prove that:
1. g|a
2. g|b
3. g = g_1*a + g_2*b

Immediately
2. g|b => OK (by assumption)

b = b'g
a MOD b = c'g

a = bq + r
a = bq + (a mod b)
a = (b'g)q + c'g
a = b'qg + c'g
a = (b'q + c')g
a = a'g
or g|a
1. g|a => OK

Imagine that exist
g' = gcd(a,b)
where g' > g
in this case
g'|a
g'|b
=> g'|(a MOD b).

But the greatest value that divides both b and (a MOD b) is g.
So does not exist g' > g.

QED    

## 1.1.26 Sorting three numbers. Suppose that the variables a , b , c , and t are all of the same numeric primitive type. Show that the following code puts a , b , and c in ascending order:

    if (a > b) { t = a; a = b; b = t; }
    if (a > c) { t = a; a = c; c = t; }
    if (b > c) { t = b; b = c; c = t; }

### Answers

1 2 3
1. a > b = false
2. a > c = false
3. b > c = false  
1 2 3 OK

1 3 2
1. a > b = false
2. a > c = false
3. b > c = true  
    t = 3  
    b = 2  
    c = 3    
1 2 3 => OK

2 1 3
1. a > b = true  
    t = 2  
    a = 1  
    b = 2    
1 2 3  
2. a > c = false  
3. b > c = false  
1 2 3 => OK

3 2 1
1. a > b = true  
    t = 3  
    a = 2  
    b = 3  
2 3 1
2. a > c = true  
    t = 2  
    a = 1  
    c = 2  
1 3 2
3. b > c = true  
    t = 3  
    b = 2  
    c = 3  
1 2 3 => OK

## 1.1.27 Binomial distribution. Estimate the number of recursive calls that would be used by the code to compute binomial(100, 50, 0.25) . Develop a better implementation that is based on saving computed values in an array.
    
    public static double binomial(int N, int k, double p)
    {
        if ((N == 0) && (k == 0)) return 1.0;
        if ((N < 0) || (k < 0)) return 0.0;
        return (1 - p)*binomial(N-1, k, p) + p*binomial(N-1, k-1, p);
    }


### Answers

Call Tree:  

.1 binomial(4,2).  
.2 binomial(3,2).  
.3 binomial(2,2) = 1.  
.3 binomial(2,1).  
.4 binomial(1,1) = 1.  
.4 binomial(1,0) = 1.  
.2 binomial(3,1).  
.3 binomial(2,1).  
.4 binomial(1,1) = 1.  
.4 binomial(1,0) = 1.  
.3 binomial(2,0) = 1.  

All leafs always have value 1. So binomial(n,k) = 1 + 1 + ... + 1 + 1. In this case the call tree have:
11 nodes =   
6 leafs  
5 aggregators  

Every aggregator comes from a recursive call. So  
binomial(4,2) makes 5 recursives calls.  

\#binomial(n,k) = \#binomial(n-1,k) + \#binomial(n-1,k-1) + 1  

Thesis:  
\#binomial(n,k) = binomial(n,k) - 1  

Bases:  
\#binomial(1,0) = binomial(1,0) - 1 = 1 - 1 = 0 (OK)  

Step:  
\#binomial(n,k) = binomial(n,k) - 1  
\#binomial(n,k) + binomial(n,k+1) = binomial(n,k) - 1 + binomial(n,k+1)  
\#binomial(n,k) + \#binomial(n,k+1) + 1 = binomial(n+1,k+1) - 1  
\#binomial(n+1,k+1) = binomial(n+1,k+1) - 1  
QED  

so
binomial(100, 50, p) will have binomial(100,50) - 1 calls  
binomial(100,50) = 100!/(50!(100-50)!)  
binomial(100,50) = (100*99*...*51*50!)/(50!(100-50)!)  
binomial(100,50) = (100*99*...*51)/50!  

which is very hard to calculate. A more simple and elegant
way is  
binomial(N,k) = (n/k)*binomial(n-1,k-1)  
binomial(N,k) = (n/k)*(n-1/k-1)*binomial(n-2,k-2)  
...  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/k-(k-2))*(n-(k-1)/k-(k-1))*binomial(n-k, k-k)  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/k-(k-2))*(n-(k-1)/k-(k-1))*binomial(n-k, 0)  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/k-(k-2))*(n-(k-1)/k-(k-1))*1  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/k-k+2))*(n-(k-1)/k-k+1))*1  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/k-k+2))*(n-(k-1)/k-k+1))*1  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/0+2))*(n-(k-1)/0+1))*1  
binomial(N,k) = (n/k)*(n-1,k-1)*...*(n-(k-2)/2))*(n-(k-1)/1))*1  
  
or:  
binomial(100,50) = 1*(100*(50-1))*(100*(50-2)/2)*...*(100*2/49)*(100*1/50)  
binomial(100,50) = 1*(100*49)*(100*48/2)*...*(100*2/49)*(100*1/50)  
binomial(100,50) = 100891344545564193334812497256  

\#binomial(100,50) = 100891344545564193334812497256 - 1  
\#binomial(100,50) = 100891344545564193334812497255

1.1.28 Remove duplicates. Modify the test client in BinarySearch to remove any du-
plicate keys in the whitelist after the sort.
1.1.29 Equal keys. Add to BinarySearch a static method rank() that takes a key and
a sorted array of int values (some of which may be equal) as arguments and returns the
number of elements that are smaller than the key and a similar method count() that
returns the number of elements equal to the key. Note : If i and j are the values returned
by rank(key, a) and count(key, a) respectively, then a[i..i+j-1 ] are the values in
the array that are equal to key .
1.1.30 Array exercise. Write a code fragment that creates an N-by-N boolean array
a[][] such that a[i][j] is true if i and j are relatively prime (have no common fac-
tors), and false otherwise.
1.1.31 Random connections. Write a program that takes as command-line arguments
an integer N and a double value p (between 0 and 1), plots N equally spaced dots of size
.05 on the circumference of a circle, and then, with probability p for each pair of points,
draws a gray line connecting them.
59 1.1
n Basic Programming Model
1.1.32 Histogram. Suppose that the standard input stream is a sequence of double
values. Write a program that takes an integer N and two double values l and r from the
command line and uses StdDraw to plot a histogram of the count of the numbers in the
standard input stream that fall in each of the N intervals defined by dividing (l , r) into
N equal-sized intervals.
1.1.33 Matrix library. Write a library Matrix that implements the following API:
public class Matrix
static double dot(double[] x, double[] y) vector dot product
static double[][] mult(double[][] a, double[][] b) matrix-matrix product
static double[][] transpose(double[][] a) transpose
static double[] mult(double[][] a, double[] x) matrix-vector product
static double[] mult(double[] y, double[][] a) vector-matrix product
Develop a test client that reads values from standard input and tests all the methods.
1.1.34 Filtering. Which of the following require saving all the values from standard
input (in an array, say), and which could be implemented as a filter using only a fixed
number of variables and arrays of fixed size (not dependent on N)? For each, the input
comes from standard input and consists of N real numbers between 0 and 1.
n Print the maximum and minimum numbers.
n Print the median of the numbers.
n Print the k th smallest value, for k less than 100.
n Print the sum of the squares of the numbers.
n Print the average of the N numbers.
n Print the percentage of numbers greater than the average.
n Print the N numbers in increasing order.
n Print the N numbers in random order.
crEAtivE problEms (continued)
60 Chapter 1
n Fundamentals
ExpErimENts
1.1.35 Dice simulation. The following code computes the exact probability distribu-
tion for the sum of two dice:
int SIDES = 6;
double[] dist = new double[2*SIDES+1];
for (int i = 1; i <= SIDES; i++)
for (int j = 1; j <= SIDES; j++)
dist[i+j] += 1.0;
for (int k = 2; k <= 2*SIDES; k++)
dist[k] /= 36.0;
The value dist[k] is the probability that the dice sum to k . Run experiments to vali-
date this calculation simulating N dice throws, keeping track of the frequencies of oc-
currence of each value when you compute the sum of two random integers between 1
and 6. How large does N have to be before your empirical results match the exact results
to three decimal places?
1.1.36 Empirical shuffle check. Run computational experiments to check that our
shuffling code on page 32 works as advertised. Write a program ShuffleTest that
takes command-line arguments M and N, does N shuffles of an array of size M that is
initialized with a[i] = i before each shuffle, and prints an M-by-M table such that row
i gives the number of times i wound up in position j for all j . All entries in the table
should be close to N/M.
1.1.37 Bad shuffling. Suppose that you choose a random integer between 0 and N-1
in our shuffling code instead of one between i and N-1 . Show that the resulting order is
not equally likely to be one of the N ! possibilities. Run the test of the previous exercise
for this version.
1.1.38 Binary search versus brute-force search. Write a program BruteForceSearch
that uses the brute-force search method given on page 48 and compare its running time
on your computer with that of BinarySearch for largeW.txt and largeT.txt .
61 1.1
n Basic Programming Model
1.1.39 Random matches. Write a BinarySearch client that takes an int value T as
command-line argument and runs T trials of the following experiment for N = 10 3 , 10 4 ,
10 5 , and 10 6 : generate two arrays of N randomly generated positive six-digit int values,
and find the number of values that appear in both arrays. Print a table giving the average
value of this quantity over the T trials for each value of N.