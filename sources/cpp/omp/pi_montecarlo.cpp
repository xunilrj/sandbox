#include <iostream>
#include <sstream>
#include <omp.h>
#include <vector>
#include <chrono>
#include <cmath>
#include <numeric>
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

struct pi_montecarlo_worker
{
    bool ran;
    int qtd;
    int inside;
};

const int ITERATIONS = 100000000;

// rand() inside OMP
// https://www.viva64.com/en/b/0012/

void runSerial()
{
    std::cout << "Serial -------------------" << std::endl;
    int numThreads = 1;
    std::vector<pi_montecarlo_worker> workers (numThreads);

    auto timer = Timer{};
    
    for(int i = 0; i <ITERATIONS; ++i)
    {
        auto tid = omp_get_thread_num();
        workers[tid].ran = true;

        auto x = rand() % 1000 / 1000.0f * 2.0f - 1.0f;
        auto y = rand() % 1000 / 1000.0f * 2.0f - 1.0f;

        auto inside = std::sqrt(x*x+y*y);
        ++workers[tid].qtd;
        if(inside < 1.0f)
            ++workers[tid].inside;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

struct my_random
{
    unsigned short lfsr = 0xACE1u;
    unsigned bit;

    my_random() : bit{rand()}
    {
        lfsr = (unsigned short)rand() % 100000;
    }

    unsigned rand()
    {
        bit  = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
        return lfsr =  (lfsr >> 1) | (bit << 15);
    }
};

void runParallel_1(int numThreads)
{
    std::cout << "Parallel_1 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);

    auto r = my_random{};
    auto timer = Timer{};
    
    // Maybe creating
    // too many very small tasks.
    #pragma omp parallel for
    for(int i = 0; i <ITERATIONS; ++i)
    {
        auto tid = omp_get_thread_num();
        workers[tid].ran = true;

        auto x = rand() % 1000 / 1000.0f * 2.0f - 1.0f;
        auto y = rand() % 1000 / 1000.0f * 2.0f - 1.0f;

        auto inside = std::sqrt(x*x+y*y);
        ++workers[tid].qtd;
        if(inside < 1.0f)
            ++workers[tid].inside;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

void runParallel_2(int numThreads)
{
    std::cout << "Parallel_2 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);

    auto r = my_random{};
    auto timer = Timer{};
    
    // Better batching
    // But still everyone using the same memory around "workers"
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        workers[tid].ran = true;

        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = rand() % 1000 / 1000.0f * 2.0f - 1.0f;
            auto y = rand() % 1000 / 1000.0f * 2.0f - 1.0f;

            auto inside = std::sqrt(x*x+y*y);
            ++workers[tid].qtd;
            if(inside < 1.0f)
                ++workers[tid].inside;
        }
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

void runParallel_3(int numThreads)
{
    std::cout << "Parallel_3 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);

    auto r = my_random{};
    auto timer = Timer{};
    
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        
        //Write to local memory
        auto w = pi_montecarlo_worker{};
        w.ran = true;

        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = rand() % 1000 / 1000.0f * 2.0f - 1.0f;
            auto y = rand() % 1000 / 1000.0f * 2.0f - 1.0f;

            auto inside = std::sqrt(x*x+y*y);
            ++w.qtd;
            if(inside < 1.0f)
                ++w.inside;
        }

        // Update global just at the end.
        workers[tid] = w;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

#include <random>

void runParallel_4(int numThreads)
{
    std::cout << "Parallel_4 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);
    
    auto timer = Timer{};
    
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        
        //Write to local memory
        auto w = pi_montecarlo_worker{};
        w.ran = true;

        auto r = my_random{};

        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = r.rand() % 1000 / 1000.0f * 2.0f - 1.0f;
            auto y = r.rand() % 1000 / 1000.0f * 2.0f - 1.0f;

            auto inside = std::sqrt(x*x+y*y);
            ++w.qtd;
            if(inside < 1.0f)
                ++w.inside;
        }

        // Update global just at the end.
        workers[tid] = w;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

void runParallel_5(int numThreads)
{
    std::cout << "Parallel_5 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);
    
    auto timer = Timer{};
    
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        
        //Write to local memory
        auto w = pi_montecarlo_worker{};
        w.ran = true;

        auto r = my_random{};

        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = r.rand() % 1000 / 1000.0f * 2.0f - 1.0f;
            auto y = r.rand() % 1000 / 1000.0f * 2.0f - 1.0f;

            auto inside = std::sqrt(x*x+y*y);
            ++w.qtd;
            if(inside < 1.0f)
                ++w.inside;
        }

        // Update global just at the end.
        workers[tid] = w;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}


void runParallel_6(int numThreads)
{
    std::cout << "Parallel_6 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);
    
    auto timer = Timer{};
    
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        
        //Write to local memory
        auto w = pi_montecarlo_worker{};
        w.ran = true;

        std::random_device r;
        std::default_random_engine e1(r());
        std::uniform_real_distribution<float> d(-1, 1);
        
        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = d(e1);
            auto y = d(e1);

            auto inside = std::sqrt(x*x+y*y);
            ++w.qtd;
            if(inside < 1.0f)
                ++w.inside;
        }

        workers[tid] = w;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

//https://stackoverflow.com/questions/35358501/what-is-performance-wise-the-best-way-to-generate-random-bools
struct my_random_2
{
    uint64_t s[2];
    uint64_t curRand;
    uint8_t bit = 63;

    my_random_2(std::random_device& rd)
    {
        s[0] = (uint64_t(rd()) << 32) ^ (rd());
        s[1] = (uint64_t(rd()) << 32) ^ (rd());
    }

    uint64_t next()
    {
        uint64_t x = s[0];
        uint64_t const y = s[1];
        s[0] = y;
        x ^= x << 23; // a
        s[1] = x ^ y ^ (x >> 17) ^ (y >> 26); // b, c
        return s[1] + y;
    }

    float next_float()
    {
        return next() / (float)std::numeric_limits<uint64_t>::max();
    }
};

void runParallel_7(int numThreads)
{
    std::cout << "Parallel_7 -------------------" << std::endl;
    std::vector<pi_montecarlo_worker> workers (numThreads);
    
    auto timer = Timer{};
    
    #pragma omp parallel
    {
        auto tid = omp_get_thread_num();
        
        //Write to local memory
        auto w = pi_montecarlo_worker{};
        w.ran = true;

        std::random_device rd;
        auto r = my_random_2{rd};
        
        const int ITER = ITERATIONS / omp_get_num_threads();
        for(int i = 0; i <ITER; ++i)
        {
            auto x = r.next_float() * 2.0f - 1.0f;
            auto y = r.next_float() * 2.0f - 1.0f;

            auto inside = std::sqrt(x*x+y*y);
            ++w.qtd;
            if(inside < 1.0f)
                ++w.inside;
        }

        workers[tid] = w;
    }

    auto qtd = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.qtd : r; });
    auto inside = std::accumulate(workers.begin(), workers.end(), 0, [](auto r,auto&w) { return w.ran ? r + w.inside : r; });

    // Area = pi*r^2
    // r = 1
    float area_boundingbox = 4;
    float pi = (float)inside / qtd;
    std::cout << pi * area_boundingbox << " " << qtd << std::endl;
    std::cout << numThreads << " threads, elapsed: " << timer.elapsed() << std::endl;
}

int main(int argc, char** argv)
{
    int numThreads = omp_get_num_procs();
    if(argc > 1)
    {
        std::stringstream ss;
        ss << argv[1];
        
        ss >> numThreads;
        omp_set_num_threads(numThreads);
    }

    runSerial();
    runParallel_1(numThreads);
    runParallel_2(numThreads);
    runParallel_3(numThreads);
    runParallel_4(numThreads);
    runParallel_5(numThreads);
    runParallel_6(numThreads);
    runParallel_7(numThreads);
}