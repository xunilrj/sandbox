#include <iostream>
#include <sstream>
#include <omp.h>
#include <vector>
#include <chrono>

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

    #pragma omp parallel
    {
        #pragma omp for ordered
        for (int n = 0; n < 10; ++n)
        {
            printf("u1 = %d\n",n);
            #pragma omp ordered
            printf("o = %d\n",n);
            printf("u2 = %d\n",n);
        }
    }
    return 0;
}