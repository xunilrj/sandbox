#include <iostream>
#include <sstream>
#include <omp.h>
#include <vector>
#include <chrono>

class Timer
{
public:
    Timer() : beg_(clock_::now()) {}
    void reset() { beg_ = clock_::now(); }
    double elapsed() const { 
        return std::chrono::duration_cast<second_>
            (clock_::now() - beg_).count(); }

private:
    typedef std::chrono::high_resolution_clock clock_;
    typedef std::chrono::duration<double, std::ratio<1> > second_;
    std::chrono::time_point<clock_> beg_;
};


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

    const int LOOPSIZE = 1000000000;
    std::vector<int> v (LOOPSIZE, 3);

    auto timer = Timer{};
    int sum = 0;
    for (int i = 0;i < LOOPSIZE; ++i)
    {
        sum += v[i];
    }
    std::cout << "elapsed: " << timer.elapsed() << std::endl;
    std::cout << sum << std::endl;
    
    timer.reset();
    sum = 0;
    #pragma omp parallel for reduction (+:sum)
    for (int i = 0;i < LOOPSIZE; ++i)
    {
        sum += v[i];
    }
    std::cout << "elapsed: " << timer.elapsed() << std::endl;
    std::cout << sum << std::endl;
}