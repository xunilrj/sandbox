#include <iostream>
#include <sstream>
#include <omp.h>

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
        int ID = omp_get_thread_num();
        printf(" hello(%d) ", ID);
        printf(" world(%d) \n", ID);
    }
}