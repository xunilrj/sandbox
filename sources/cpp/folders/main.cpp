#include <functional>
#include <limits>
#include <vector>
#include <iostream>

template <typename T, typename TCompare, T initial>
struct folder
{
    T value = initial;
};

template <typename T> using maximum =
    folder<T, std::greater<T>, std::numeric_limits<T>::min()>;
template <typename T> using minimum = 
    folder<T, std::less<T>, std::numeric_limits<T>::max()>;

struct folders
{
    struct maximum_t
    {
        template <typename T> using compare_type = std::greater<T>;
        template <typename T> static inline T initial = std::numeric_limits<T>::min();
    };

    static maximum_t max;
};

template <typename T, typename TCompare, T initial>
const T& operator >> (const T& v, folder<T,TCompare,initial>& f)
{
    if(TCompare{}(v, f.value))
        f.value = v;
    return v;
}

struct runner_now
{
};

template <typename TCol, typename... TFolders>
struct runner
{
    const TCol& col;
    auto run() const
    {
        return _run(TFolders{}...);
    }
private:
    auto _run(TFolders... folders) const
    {
        for(auto&&x : col)
        {
            (x >> ... >> folders);
        }
        return std::tuple{(folders.value)...};
    }
};

template <typename T>
inline constexpr bool has_begin_v = 
std::is_same_v<
    decltype( std::cbegin(std::declval<T>()) ),
    typename T::const_iterator    
>;

template <typename T, 
    typename TCompare,
    typename T::value_type initial,
    typename = std::enable_if_t<has_begin_v<T>>
>
runner<T, folder<typename T::value_type, TCompare, initial>>
operator >> (const T& v, const folder<typename T::value_type, TCompare, initial>& f)
{
    return {v};    
}

template <typename T, typename TFolder>
runner<T, folder<typename T::value_type, typename TFolder::compare_type<T::value_type>, TFolder::initial<T::value_type>>>
operator >> (const T& v, const TFolder& f)
{
    return {v};    
}

template <typename T, typename... TFolders, typename TCompare, typename T::value_type initial>
runner<T, TFolders..., folder<typename T::value_type, TCompare, initial>>
operator >> (
    const runner<T, TFolders...>& v,
    const folder<typename T::value_type, TCompare, initial>& f)
{    
    return {v.col};
}




int main()
{
    std::vector<int> v = {0,1,2,3};
    
    auto f = v >> folders::max >> minimum<int> {};
    auto r = f.run();
    std::cout << std::get<0>(r) << " " << std::get<1>(r) << std::endl;
    return 0;
}