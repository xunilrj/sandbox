#include <unistd.h>
#include <signal.h>

#include <sys/epoll.h>
#include <sys/signalfd.h>
#include <sys/timerfd.h>

#include <array>
#include <iostream>

struct event_tag
{
    int fd;
    int tag;
};

int epoll_add_in(int epoll, int fd)
{
    epoll_event e = {0};
    e.events = EPOLLIN;
    e.data.ptr = new event_tag { fd, 127};
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
    e.data.ptr = new event_tag { signal_fd, SIGTERM };

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
    e.data.ptr = new event_tag { fd, 777 };

    return epoll_ctl(epoll, EPOLL_CTL_ADD, fd, &e);
}

int main(int argc, char** argv)
{
    auto poll_fd = epoll_create1(0);
    std::cout << "epoll_create: " << poll_fd << std::endl;

    epoll_add_in(poll_fd, 0);
    auto mask = epoll_add_signal(poll_fd, SIGTERM);
    epoll_add_interval(poll_fd, 1000);

    std::array<epoll_event, 1> events;
    int timeout = 10000;

    std::array<uint8_t, 256> buffer;
    sigprocmask(SIG_SETMASK, &mask, nullptr);  

    std::cout << "waiting..." << std::endl;
    bool cont = true;
    while(cont)
    {
        auto qtd = epoll_pwait(poll_fd, events.data(), events.size(), timeout, &mask);
        std::cout << qtd << std::endl;

        for(auto i = 0; i < qtd; i++)
        {
            auto& current = events[i];
            auto& data = * (event_tag*) current.data.ptr;
            std::cout << data.fd << std::endl;
            auto bytes = read(data.fd, buffer.data(), buffer.size());
            std::cout << bytes << " " << data.tag << std::endl;

            if(data.tag == SIGTERM)
            {
                cont = false;
            }
        }
    }

    close(poll_fd);

    return 0;
}