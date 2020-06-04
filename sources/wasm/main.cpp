// #include <iostream>
// #include <future>
#include <experimental/coroutine>
#include "cppcoro/task.hpp"
//#include "../../../cppcoro/include/cppcoro/sync_wait.hpp"
#include "cppcoro/sync_wait.hpp"

cppcoro::task<void> print_and_sleep()
{
    // std::cout << "sleeping..." << std::endl;
    co_await std::experimental::suspend_always();
    // std::cout << " done!" << std::endl;
}