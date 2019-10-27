#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include <iostream> 
#include <chrono> 

#include <mutex>
#include <queue>
#include <thread>
template <typename T>
class MPMCQueue
{
public:
    void enqueue(const T& item) &
    {
        std::lock_guard<std::mutex> lk(m);
        items.push(item);
    }

    T dequeue() &
    {
        std::lock_guard<std::mutex> lk(m);
        auto item = items.front();
        items.pop();
        return item;
    }

    size_t size() &
    {
        std::lock_guard<std::mutex> lk(m);
        return items.size();
    }
private:
    std::mutex m;
    std::queue<T> items;
};

TEST_CASE( "Must FIFO", "[MPMC]" )
{
    MPMCQueue<int> q;
    q.enqueue(1);
    q.enqueue(2);
    q.enqueue(3);

    REQUIRE(q.dequeue() == 1);
    REQUIRE(q.dequeue() == 2);
    REQUIRE(q.dequeue() == 3);
}

template <typename F>
auto measure_time(F f)
{
    auto start = std::chrono::high_resolution_clock::now();

    f();

    auto finish = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = finish - start;
    return elapsed.count();
}

#include <Windows.h>
class W32Event
{
public:
    W32Event () { _handle = CreateEvent(0, TRUE, FALSE, 0); }
    ~W32Event () { CloseHandle (_handle); }

    void Release () { SetEvent (_handle); }
    void Wait () { WaitForSingleObject (_handle, INFINITE); }
private:
    HANDLE _handle;
};

#define TESTSIZE 10000000
TEST_CASE("Contention cost", "[MPMC]" )
{
    auto time = measure_time([](){
        MPMCQueue<int> q;
        for(int i = 0;i < TESTSIZE; ++i)
        {
            q.enqueue(i);
        }
        REQUIRE(q.size() == TESTSIZE);
    });

    W32Event e;
    MPMCQueue<int> q;
    auto insert_function = [&](unsigned int qtd) {
        e.Wait();
        for(int i = 0;i < qtd; ++i)
        {
            q.enqueue(i);
        }
    };
    std::vector<std::thread> threads;
    threads.emplace_back(insert_function, TESTSIZE / 4);
    threads.emplace_back(insert_function, TESTSIZE / 4);
    threads.emplace_back(insert_function, TESTSIZE / 4);
    threads.emplace_back(insert_function, TESTSIZE / 4);
    auto time2 = measure_time([&](){
        e.Release();
        for (auto &thread : threads) thread.join();
    });

    std::cout << "One thread: " << time << std::endl;
    std::cout << "4 threads. Expected: " << time/4.0 << " Actual: " << time2 << std::endl;
}