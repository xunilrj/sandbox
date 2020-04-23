#include <iostream>
#include <sstream>
#include <omp.h>
#include <atomic>

void runIncrement()
{
    const int LOOPSIZE = 1000000;

    int A = 0;
    int B = 0;
    int C = 0;
    std::atomic<int> D {0}; //https://stackoverflow.com/questions/21554099/can-stdatomic-be-safely-used-with-openmp

    std::cout << "Increment ------------------------------" << std::endl;
    #pragma omp parallel for shared(A,B,C,D)
    for (int i = 0;i < LOOPSIZE; ++i)
    {
        ++A;

        #pragma omp critical
        {
            ++B;
        }

        #pragma omp atomic
        ++C;

        ++D;
    }

    std::cout << "correct: " << LOOPSIZE << std::endl;
    std::cout << "A: " << A << " [" << (A == LOOPSIZE) << "]" << std::endl;
    std::cout << "B: " << B << " [" << (B == LOOPSIZE) << "]" << std::endl;
    std::cout << "C: " << C << " [" << (C == LOOPSIZE) << "]" << std::endl;
    std::cout << "D: " << D << " [" << (D == LOOPSIZE) << "]" << std::endl;
}

void runBinaryOperatorEqual()
{
    const int LOOPSIZE = 1000000;

    srand(time(NULL));
    int step = rand() % 9 + 1;

    int A = 0;
    int B = 0;
    int C = 0;
    std::atomic<int> D {0}; //https://stackoverflow.com/questions/21554099/can-stdatomic-be-safely-used-with-openmp

    std::cout << "BinaryOperatorEqual ------------------------------" << std::endl;
    #pragma omp parallel for shared(A,B,C,D)
    for (int i = 0;i < LOOPSIZE; ++i)
    {
        A+=step;

        #pragma omp critical
        {
            B+=step;
        }

        #pragma omp atomic
        C+=step;

        D+=step;
    }

    std::cout << "correct: " << LOOPSIZE*step << std::endl;
    std::cout << "A: " << A << " [" << (A == LOOPSIZE*step) << "]" << std::endl;
    std::cout << "B: " << B << " [" << (B == LOOPSIZE*step) << "]" << std::endl;
    std::cout << "C: " << C << " [" << (C == LOOPSIZE*step) << "]" << std::endl;
    std::cout << "D: " << D << " [" << (D == LOOPSIZE*step) << "]" << std::endl;
}

void runReadModifyWrite()
{
    const int LOOPSIZE = 1000000;

    srand(time(NULL));
    int step = rand() % 9 + 1;

    int A = 0;
    int B = 0;
    int C = 0;
    std::atomic<int> D {0}; //https://stackoverflow.com/questions/21554099/can-stdatomic-be-safely-used-with-openmp
    std::atomic<int> E {0};
    std::atomic<int> ECount {0};

    std::cout << "ReadModifyWrite ------------------------------" << std::endl;
    #pragma omp parallel for shared(A,B,C,D)
    for (int i = 0;i < LOOPSIZE; ++i)
    {
        A = A + step + 10;

        #pragma omp critical
        {
            B = B + step + 10;
        }

        //omp003.cpp:99:24: error: the statement for 'atomic' must be an 
        //expression statement of form '++x;', '--x;', 'x++;', 'x--;', 
        //'x binop= expr;', 'x = x binop expr' or 'x = expr binop x',
        // where x is an l-value expression with scalar type
        //#pragma omp atomic
        //C = (C + step) * 2;

        // Incorrect use of atomic in this case
        // others are fine.
        // See correcy way below.
        D = D + step + 10;

        //https://stackoverflow.com/questions/25199838/understanding-stdatomiccompare-exchange-weak-in-c11
        //https://preshing.com/20150402/you-can-do-any-kind-of-atomic-read-modify-write-operation/
        int oldE = E.load();
        int newE;
        do
        {
            ++ECount; // <- measure how many times compare_exchange_weak will fail
            newE = oldE + step + 10; // <- what we really want to do
        } while(!E.compare_exchange_weak(oldE, newE));
    }

    std::cout << "correct: " << LOOPSIZE*(step+10) << std::endl;
    std::cout << "A: " << A << " [" << (A == LOOPSIZE*(step+10)) << "]" << std::endl;
    std::cout << "B: " << B << " [" << (B == LOOPSIZE*(step+10)) << "]" << std::endl;
    std::cout << "C: " << C << " [" << (C == LOOPSIZE*(step+10)) << "]" << std::endl;
    std::cout << "D: " << D << " [" << (D == LOOPSIZE*(step+10)) << "]" << std::endl;
    std::cout << "E: " << E << " [" << (E == LOOPSIZE*(step+10)) << "]" << std::endl;
    std::cout << "E conflicts: " << ECount - LOOPSIZE << "(" << (ECount - LOOPSIZE) * 100.0 / LOOPSIZE  << "%)" <<  std::endl;
}

int main(int argc, char** argv)
{
    if(argc > 1)
    {
        std::stringstream ss;
        ss << argv[1];
        
        int numThreads;
        ss >> numThreads;
        omp_set_num_threads(numThreads);
    }

    runIncrement();
    runBinaryOperatorEqual();
    runReadModifyWrite();
}