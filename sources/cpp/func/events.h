#pragma once

#include <vector>
#include <random>
#include <algorithm>
#include <iterator>
#include <iostream>
#include <functional>

struct DelegateRecord
{
    bool embedded;
    union
    {
        uint8_t data[20];
        void* ptr;
    };
    void (*fptr) (void*, void*);
};

struct Delegate
{
    std::vector<DelegateRecord> handlers;
};

template <typename... TArgs>
struct TypedDelegate : Delegate
{
    template <
        typename F,
        typename = F::can_invoke_with<TArgs...>
    >
    void subscribe(const F& f)
    {
        auto record = DelegateRecord{};

        if (sizeof(f) > sizeof(record.data))
        {
            record.embedded = false;
            record.ptr = new uint8_t[sizeof(f)]; //TODO how to deallocate
            std::memcpy(record.ptr, &f, sizeof(f));
        }
        else
        {
            record.embedded = true;
            std::memcpy(record.data, &f, sizeof(f));
        }

        auto ptr = &magic_invoke<F,TArgs...>;
        record.fptr = (void (*) (void*, void*))(ptr);

        handlers.emplace_back(record);
    }

    void operator() (TArgs... args) const
    {
        auto t = std::tuple{ args... };
        
        for (auto&& f : handlers)
        {
            if(f.embedded)
                f.fptr((void*)&f.data, &t);
            else
                f.fptr(f.ptr, &t);
        }
    }
};

class EventSystem
{
    size_t count;
    std::unordered_map<size_t, Delegate> events;
public:
    EventSystem() : count{ 0 }
    {
    }

    template <typename... TArgs>
    TypedDelegate<TArgs...> make()
    {
        auto d = TypedDelegate<TArgs...>{};
        return d;
    }

    template <typename... TArgs>
    void raise(size_t id, TArgs... args)
    {
        auto it = events.find(id);
        if (it == events.end())
        {
            events[id] = TypedDelegate<TArgs...>{};
        }

        auto& d = *(TypedDelegate<TArgs...>*)&events[id];
        d(args...);
    }

    template <
        typename... TArgs,
        typename F,
        typename = F::can_invoke_with<TArgs...>>
    void subscribe(size_t id, const F& f)
    {
        auto it = events.find(id);
        if (it == events.end())
        {
            //TODO early subscription
            return;
        }

        auto& d = *(TypedDelegate<TArgs...>*) &(it->second);
        d.subscribe(f);
    }
};