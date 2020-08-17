#include <errno.h>

#include <unistd.h>
#include <signal.h>

#include <sys/epoll.h>
#include <sys/signalfd.h>
#include <sys/timerfd.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <array>
#include <iostream>

#include "unique_handle.h"

#include <experimental/coroutine>

struct epoll_event_awaiter
{
    epoll_event &e;

    bool await_ready() noexcept { return false; }
    void await_suspend(std::experimental::coroutine_handle<> h) noexcept
    {
        std::cout << std::to_address(this) << "await_transform epoll_event::awaiter e.data.ptr updated \n";
        std::cout << std::to_address(this) << "await_transform epoll_event::awaiter " << h.address() << std::endl;
        e.data.ptr = h.address();
    }
    void await_resume() noexcept
    {
        std::cout << std::to_address(this) << "await_transform epoll_event::awaiter resume \n";
    }
};

template <typename T, typename TInitialSuspend>
struct task_t
{
    struct promise_type
    {
        T value;

        task_t<T, TInitialSuspend> get_return_object() noexcept
        {
            std::cout << std::to_address(this) << "promise_type::get_return_object " << std::endl;

            return {std::experimental::coroutine_handle<promise_type>::from_promise(*this)};
        }

        TInitialSuspend initial_suspend() noexcept
        {
            std::cout << std::to_address(this) << "promise_type::initial_suspend " << std::endl;

            return {};
        }

        std::experimental::suspend_never final_suspend() noexcept
        {
            std::cout << std::to_address(this) << "promise_type::final_suspend " << std::endl;
            return {};
        }

        void unhandled_exception() { std::terminate(); }

        void return_value(T v) noexcept
        {
            std::cout << std::to_address(this) << "promise_type::return_value " << v << "\n";
            value = v;
        }
    };

    bool await_ready() noexcept
    {
        return false;
    }

    void await_suspend(std::experimental::coroutine_handle<promise_type> h) noexcept
    {
        std::cout << "handle addr: " << handle.address() << std::endl;
        handle.resume();
        handle = h;
    }

    T await_resume() noexcept
    {
        return handle.promise().value;
    }

    template <typename A, typename B>
    friend std::ostream &operator<<(std::ostream &out, const task_t<A, B> &t);

    auto operator co_await() &noexcept
    {
        poll();
        return *this;
    }

    auto operator co_await() const &&noexcept
    {
        poll();
        return *this;
    }

    void poll()
    {
        std::cout << std::to_address(this) << "task_t::poll " << *this << std::endl;
        handle();
    }

    std::experimental::coroutine_handle<promise_type> handle;
};

template <typename T>
using task = task_t<T, std::experimental::suspend_always>;
template <typename T>
using eager_task = task_t<T, std::experimental::suspend_never>;

template <typename T>
std::ostream &operator<<(std::ostream &out, const task<T> &t)
{
    return out << "task@" << std::to_address(&t) << "[handle=" << t.handle.address() << "]";
}

class runtime
{
public:
    template <typename T>
    void spawn(T &&t)
    {
        std::cout << "runtime::spawn" << std::endl;
        t.poll();
    }
};

namespace sys
{
    struct fd_trait
    {
        static auto invalid() noexcept { return -1; }
        static auto close(int value) noexcept { ::close(value); }
    };
} // namespace sys

namespace sys::socket
{
    using socket_handle = unique_handle<int, fd_trait>;

    socket_handle socket(int __domain, int __type, int __protocol)
    {
        auto sfd = ::socket(__domain, __type, __protocol);
        return socket_handle{sfd};
    }

    struct socket_t
    {
        socket_handle handle;
        epoll_event e;

        socket_t(int __domain, int __type, int __protocol) : handle{socket(__domain, __type, __protocol)}
        {
        }

        task<int> accept()
        {
            int i = 0;
            while (true)
            {
                std::cout << "socket_t::accept " << i << std::endl;
                sockaddr_in client = {0};
                socklen_t clientlen;
                auto r3 = accept4(handle, (struct sockaddr *)&client, &clientlen, SOCK_NONBLOCK);
                if (r3 > 0)
                    co_return r3;
                else if (errno == EWOULDBLOCK)
                    co_await epoll_event_awaiter{e};
                else
                    break;
                ++i;
            }

            co_return 0;
        }

        operator const int &() const { return handle; }
    };
} // namespace sys::socket

namespace sys::epoll
{
    using epoll_handle = unique_handle<int, fd_trait>;

    epoll_handle epoll_create1(int flags)
    {
        auto poll_fd = ::epoll_create1(flags);
        return epoll_handle{poll_fd};
    }

    class epoll_t
    {
        epoll_handle handle;

    public:
        epoll_t() : handle{epoll_create1(0)}
        {
        }

        epoll_t &add(sys::socket::socket_t &fd)
        {
            fd.e = {0}; //TODO local
            fd.e.events = EPOLLIN;
            fd.e.data.ptr = &fd.e;
            auto r4 = epoll_ctl(handle, EPOLL_CTL_ADD, fd, &fd.e);

            return *this;
        }

        operator const int &() const { return handle; }
    };
} // namespace sys::epoll

struct event_tag
{
    int fd;
    int tag;
};

int epoll_add_in(int epoll, int fd)
{
    epoll_event e = {0};
    e.events = EPOLLIN;
    e.data.ptr = new event_tag{fd, 127};
    return epoll_ctl(epoll, EPOLL_CTL_ADD, fd, &e);
}

sigset_t epoll_add_signal(int epoll, int signo)
{
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGTERM);
    auto signal_fd = signalfd(-1, &mask, 0);

    epoll_event e = {0};
    e.events = EPOLLIN;
    e.data.ptr = new event_tag{signal_fd, SIGTERM};

    epoll_ctl(epoll, EPOLL_CTL_ADD, signal_fd, &e);

    return mask;
}

int epoll_add_interval(int epoll, uint64_t interval)
{
    int fd = timerfd_create(CLOCK_MONOTONIC, 0);
    std::cout << fd;
    itimerspec timer = {0};

    auto interval_nanoseconds = interval * 1000000;

    auto seconds = interval_nanoseconds / (1000 * 1000000);
    auto nano_seconds = interval_nanoseconds - (seconds * (1000 * 1000000));

    timer.it_value.tv_sec = seconds;
    timer.it_value.tv_nsec = nano_seconds;
    timer.it_interval.tv_sec = seconds;
    timer.it_interval.tv_nsec = nano_seconds;

    auto r = timerfd_settime(fd, 0, &timer, NULL);
    std::cout << r;

    epoll_event e = {0};
    e.events = EPOLLIN;
    e.data.ptr = new event_tag{fd, 777};

    return epoll_ctl(epoll, EPOLL_CTL_ADD, fd, &e);
}

task<int> f(sys::socket::socket_t &f)
{
    std::cout << "before f " << std::endl;
    auto t = f.accept();
    auto newf = co_await t;
    std::cout << "newf " << newf << std::endl;
    co_return 1;
}

int main(int argc, char **argv)
{
    using namespace sys::epoll;
    using namespace sys::socket;
    auto poll_fd = epoll_t{};

    auto sfd = socket_t{AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0};

    sockaddr_in server = {0};
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons(8888);
    auto r1 = bind(sfd, (sockaddr *)&server, sizeof(server));
    // std::cout << r1 << std::endl;
    auto r2 = listen(sfd, 100);
    // std::cout << r2 << std::endl;

    // epoll_event acceptevent = {0};
    // acceptevent.events = EPOLLIN;
    // auto r4 = epoll_ctl(poll_fd, EPOLL_CTL_ADD, sfd, &acceptevent);
    // std::cout << r4 << std::endl;
    poll_fd.add(sfd);

    // sockaddr_in client = {0};
    // socklen_t clientlen;
    // auto r3 = accept4(sfd, (struct sockaddr *)&client, &clientlen, SOCK_NONBLOCK);
    // std::cout << r3 << " " << (r3 == EWOULDBLOCK) << std::endl;
    // std::cout << errno << " " << (errno == EWOULDBLOCK) << std::endl;

    //await f(sfd);
    auto rt = runtime{};
    auto t = f(sfd);
    std::cout << t << std::endl;
    rt.spawn(t);

    std::array<epoll_event, 1> events;
    int timeout = 10000;
    while (true)
    {
        auto r5 = epoll_wait(poll_fd, events.data(), events.size(), timeout);
        if (r5 == 1)
        {
            epoll_event *e = (epoll_event *)events[0].data.ptr;
            auto h = std::experimental::coroutine_handle<>::from_address(e->data.ptr);
            std::cout << "jmp to " << e->data.ptr << std::endl;
            h.resume();
        }
    }

    // epoll_add_in(poll_fd, 0);
    // auto mask = epoll_add_signal(poll_fd, SIGTERM);
    // epoll_add_interval(poll_fd, 1000);

    // std::array<epoll_event, 1> events;
    // int timeout = 10000;

    // std::array<uint8_t, 256> buffer;
    // sigprocmask(SIG_SETMASK, &mask, nullptr);

    // std::cout << "waiting..." << std::endl;
    // bool cont = true;
    // while (cont)
    // {
    //     auto qtd = epoll_pwait(poll_fd, events.data(), events.size(), timeout, &mask);
    //     std::cout << qtd << std::endl;

    //     for (auto i = 0; i < qtd; i++)
    //     {
    //         auto &current = events[i];
    //         auto &data = *(event_tag *)current.data.ptr;
    //         std::cout << data.fd << std::endl;
    //         auto bytes = read(data.fd, buffer.data(), buffer.size());
    //         std::cout << bytes << " " << data.tag << std::endl;

    //         if (data.tag == SIGTERM)
    //         {
    //             cont = false;
    //         }
    //     }
    // }

    // close(poll_fd);

    return 0;
}