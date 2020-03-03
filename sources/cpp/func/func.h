#pragma once

#include <tuple>
#include <type_traits>
#include <algorithm>

template <typename TF,
    typename TArgs,
    typename TBound>
struct Func{};

template <typename TF, typename... TArgs>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TArgs...>>
{
    std::tuple<TF*, TArgs...> bound;

    auto operator() ()
    {
        return std::apply(fwd, bound);
    }

    static auto fwd(TF* f, TArgs... args)
    {
        return f(args...);
    }
};

template <typename TF,
    typename... TArgs,
    typename... TBounds>
struct Func<TF, std::tuple<TArgs...>, std::tuple<TF*, TBounds...>>
{
    using result_of = std::invoke_result_t<TF, TArgs...>;
    
    using tuple_args = std::tuple<TArgs...>;
    using tuple_bound = std::tuple<TF*, TBounds...>;

    constexpr static size_t sizeofArgs = sizeof...(TArgs);
    constexpr static size_t sizeofBounds = sizeof...(TBounds);

    using next_argument = std::tuple_element_t<sizeofBounds, tuple_args>;

    tuple_bound bound;

    template <size_t OFFSET,
        typename TAs,
        typename TBs,
        typename SQ>
    struct types_match  {};
    template <size_t OFFSET,
        typename TAs,
        typename TBs,
        size_t... IA>
    struct types_match<OFFSET, 
        TAs, 
        TBs,
        std::index_sequence<IA...>
    > 
    {
        using type = std::conjunction<
            std::is_same<
                std::tuple_element_t<OFFSET + IA, TAs>,
                std::tuple_element_t<IA, TBs>
            >...
        >;
    };

    template <
        typename... TBinds,
        size_t QTD = sizeof...(TBinds),
        typename = std::enable_if_t<QTD <= (sizeofArgs - sizeofBounds)>,
        typename = std::enable_if_t<
            types_match<
            sizeofBounds,
                tuple_args,
                std::tuple<TBinds...>,                
                std::make_index_sequence<sizeof...(TBinds)>
            >::type::value
        >
    >
    auto operator() (TBinds... binds)
    {
        auto newf = Func<TF, tuple_args,
            std::tuple<TF*, TBounds..., TBinds...>
        >
        {
            std::tuple_cat(bound, std::make_tuple(binds...))
        };
        if constexpr (QTD == (sizeofArgs - sizeofBounds))
            return newf();
        else 
            return newf;
    }

    template <
        typename... TBinds,
        size_t QTD = sizeof...(TBinds),
        typename = std::enable_if_t<QTD <= (sizeofArgs - sizeofBounds)>,
        typename = std::enable_if_t<
            types_match<
            sizeofBounds,
                tuple_args,
                std::tuple<TBinds...>,                
                std::make_index_sequence<sizeof...(TBinds)>
            >::type::value
        >
    >
    auto operator << (TBinds... binds)
    {
        auto newf = Func<TF, tuple_args,
            std::tuple<TF*, TBounds..., TBinds...>
        >
        {
            std::tuple_cat(bound, std::make_tuple(binds...))
        };
        return newf;
    }
};


template <typename TF>
struct make_f
{
    template<typename... TArgs> struct types { using type = std::tuple<TArgs...>; };

    template<class Sig>
    struct args;
    
    template<class R, class...Args>
    struct args<R (Args...)> : types<Args...>
    {        
    };

    template<class R, class...Args>
    struct args<R (*)(Args...)> : types<Args...>
    {        
    };

    template<class Sig> using args_t = typename args<Sig>::type;

    using type = Func<TF, args_t<TF>, std::tuple<TF*>>;
};

template <typename Fs1, typename... Fs>
struct pipeline
{
    using functions_type = std::tuple<
        Fs1,
        Fs...
    >;
    functions_type fs;
    
    using data_type = std::tuple<
        typename Fs1::next_argument,
        typename Fs1::result_of,
        typename Fs::result_of...
    >;

    data_type t;

    pipeline(Fs1 fs1, Fs... fs) :  fs{fs1, fs...}, t{}
    {
    }

    auto operator() (typename Fs1::next_argument arg)
    {
        std::get<0>(t) = arg;
        call_pipe(t, fs,
            std::make_index_sequence<sizeof...(Fs) + 1>{}
        );
        return std::get<std::tuple_size_v<data_type> - 1>(t);
    }

    template <size_t... Is>
    void call_pipe(data_type& t,
        functions_type& fs,
        std::index_sequence<Is...> s)
    {
        ((
            std::get<Is + 1>(t) = 
                std::get<Is>(fs)
                    ( std::get<Is>(t) )
        ),...);
    }
};

#ifdef FUNC_BODY

    template <typename TF>
    auto $(TF* f)
    {
        return typename make_f<TF>::type
        {
            std::make_tuple(f)
        };
    }

    template <typename Fs1, typename... Fs>
    auto $$(Fs1 fs1, Fs... fs)
    {
        return pipeline{fs1, fs...};
    }

#else

    template <typename TF> 
    auto $(TF* f);

    template <typename Fs1, typename... Fs>
    auto $$(Fs1 fs1, Fs... fs);

#endif