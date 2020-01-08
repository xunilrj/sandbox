#include <iostream>
#include <vector>
#include <deque>
#include <set>
#include <algorithm>
#include <tuple>
#include <stdlib.h>
#include "../deltaTime/deltaTime.h"

template <typename T, typename F>
void time_for_each(const T& items, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;
    deltaTime<double> dt{ true };

    while(ntimes > 0) {
        for(auto&& x : items) f(x);
        --ntimes;
    }
    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "For Each: " << elapsed << "ms";
}

template <typename T, typename F>
void time_begin_end(const T& items, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;

    deltaTime<double> dt{ true };
    while(ntimes > 0) {
        auto it = items.cbegin();
        auto end = items.cend();
        while(it != end)
        {
            f(*it);
            ++it;
        }
        --ntimes;
    }
    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "Begin/End: " << elapsed << "ms";
}

template <typename T, typename F>
void time_by_index(const T& items, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;
    typename T::value_type accum = 0;

    deltaTime<double> dt{ true };
    while(ntimes > 0) {
        auto size = items.size();
        for(int i = 0; i < size; ++i)
        {
            f(items[i]);
        }
        --ntimes;
    }
    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "Indices: " << elapsed << "ms";
}

template <typename T, typename F>
void time_data_directly(const T& items, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;

    deltaTime<double> dt{ true };
    while(ntimes > 0) {
        auto size = items.size();
        auto* item = items.data();
        for(int i = 0; i < size; ++i, ++item)
        {
            f(*item);
        }
        --ntimes;
    }
    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "Data: " << elapsed << "ms";
}

template <typename S, typename T, typename F>
void time_foreach_index(const S& set, const T& items, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;

    deltaTime<double> dt{ true };
    while(ntimes > 0) {
        for(auto&& i : set)
        {
            f(items[i]);
        }
        --ntimes;
    }
    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "ForEach/Index: " << elapsed << "ms";
}

template <typename T, typename S, typename F>
void time_iterators_iterators(const T& items, const S& set, const F& f, uint64_t ntimes = 1000)
{
    double ontimes = ntimes;

    deltaTime<double> dt{ true };
    while(ntimes > 0) {
        size_t slast = 0;
        auto sit = set.begin();
        auto send = set.end();
        auto dit = items.begin();
        while(sit != send)
        {
            auto scurrent = *sit;
            dit += (scurrent - slast);

            slast = scurrent;
            ++sit;

            f(*dit);
        }
        --ntimes;
    }

    auto elapsed = dt.elapsed() / ontimes;
    std::cout << "It/It: " << elapsed << "ms";
}

template <typename T, typename S>
void time_foreach_index_index(const S& set, const T& items1, const T& items2)
{
    typename T::value_type accum = 0;

    deltaTime<double> dt{ true };
    for(auto&& i : set)
    {
        accum += items1[i];
        accum += items2[i];
    }
    auto elapsed = dt.elapsed();
    std::cout << "ForEach/Index: " << elapsed << "ms. [" << accum << "]" << std::endl;
}

template <typename S, typename F, typename... TArgs>
void time_join_nadic(const S& set, const F& f, const TArgs&... items)
{
    deltaTime<double> dt{ true };
    for(auto&& i : set)
    {
        f(items[i]...);
    }
    auto elapsed = dt.elapsed();
    std::cout << "n-adic: " << elapsed << "ms";
}

struct data_with_flag
{
    bool enabled;
    int x, y, z;
};

template <typename T, class TCompare = std::less<T>>
class sorted_vector
{
public:
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;
 
    iterator begin() { return data.begin(); }
    iterator end() { return data.end(); }
    const_iterator begin() const { return data.begin(); }
    const_iterator end() const { return data.end(); }

    const_iterator cbegin() { return data.cbegin(); }
    const_iterator cend() { return data.cend(); }

    iterator insert(const T& t) 
    {
        iterator i = std::lower_bound(begin(), end(), t, cmp);
        if (i == end() || cmp(t, *i))
            data.insert(i, t);
        return i;
    }
private:
    TCompare cmp;
    std::vector<T> data;
};

template <typename T>
struct sparse_vector_node
{
    bool enabled;
    T data;
};

template <typename T, uint64_t SIZE>
class const_sparse_vector_iterator : public std::iterator<std::forward_iterator_tag, T>
{
public:
    using TMe = const_sparse_vector_iterator<T,SIZE>;

    const_sparse_vector_iterator() : 
        pos{-1}, chunk_qtd{nullptr}, data{nullptr}
    {
    }

    const_sparse_vector_iterator(int64_t first, const std::vector<uint16_t>* qtd, const std::vector<sparse_vector_node<T>>* d) :
        pos{first}, chunk_qtd{qtd}, data{d}
    {
    }

    const TMe& operator++() {
        advance();
        return *this;
    }

    const T& operator* () {
        return (*data)[pos].data;
    }

    int operator == (const TMe& other) const noexcept
    {
        if (data == nullptr && other.data == nullptr) return 1;
        else if ((pos == -1) && (other.data == nullptr)) return 1;
        return 0;
    }

    int operator!= (const TMe& other) const noexcept
    {
        return !(*this == other);
    }
private:
    void advance()
    {
        if(current_chunk_qtd > 0) {
            ++pos;
            --current_chunk_qtd;
            return;
        }

        while(current_chunk_qtd <= 0)
        {
            auto chunk_pos = pos / SIZE;
            if(chunk_pos >= chunk_qtd->size()) {
                chunk_qtd = nullptr;
                data = nullptr;
                return;
            } else {
                current_chunk_qtd = (*chunk_qtd)[chunk_pos];
                if(current_chunk_qtd == 0)
                    pos += SIZE - 1;
            }
        }

        if(pos >= data->size())
        {
            chunk_qtd = nullptr;
            data = nullptr;
            pos = -1;
            current_chunk_qtd = -1;
            return;
        }
    }

    int64_t pos;
    int64_t current_chunk_qtd;
    const std::vector<uint16_t>* chunk_qtd;
    const std::vector<sparse_vector_node<T>>* data;
};

// template <typename T, uint64_t SIZE>
// class sparse_vector_iterator : public std::iterator<std::forward_iterator_tag, T>
// {
// public:
//     using TMe = sparse_vector_iterator<T,SIZE>;

//     sparse_vector_iterator() : 
//         pos{-1}, chunk_qtd{nullptr}, data{nullptr}
//     {
//     }

//     sparse_vector_iterator(int64_t first, std::vector<uint16_t>* qtd, std::vector<sparse_vector_node<T>>* d) :
//         pos{first}, chunk_qtd{qtd}, data{d}
//     {
//     }

//     TMe& operator++() {
//         ++pos;

//         auto chunk_pos = pos / SIZE;
//         if(chunk_pos >= chunk_qtd->size()) {
//             chunk_qtd = nullptr;
//             data = nullptr;
//             pos = -1;
//         } else {
//             auto qtd = (*chunk_qtd)[chunk_pos];
//             if(qtd == 0)
//                 pos += SIZE - 1;
//         }

//         if(pos >= data->size())
//         {
//             chunk_qtd = nullptr;
//             data = nullptr;
//             pos = -1;
//         }

//         return *this;
//     }

//     T& operator* () { return (*data)[pos].data; }

//     int operator == (const TMe& other) const noexcept
//     {
//         if (data == nullptr && other.data == nullptr) return 1;
//         else if ((pos == -1) && (other.data == nullptr)) return 1;
//         return 0;
//     }

//     int operator!= (const TMe& other) const noexcept
//     {
//         return !(*this == other);
//     }
// private:
//     int64_t pos;
//     std::vector<uint16_t>* chunk_qtd;
//     std::vector<sparse_vector_node<T>>* data;
// };

template <typename T, uint64_t SIZE, class TCompare = std::less<T>>
class sparse_vector
{
public:
    using value_type = T;
    // using iterator = sparse_vector_iterator<T, SIZE>;
    using const_iterator = const_sparse_vector_iterator<T, SIZE>;

    sparse_vector() : next_pos{ 0 }, size_enabled{ 0 }, first{ -1 } {}
    sparse_vector(int64_t size) :
        chunk_qtd(size/SIZE), data(size), 
        next_pos{ 0 }, size_enabled{ 0 }, first{ -1 }
    {
        std::fill(chunk_qtd.begin(), chunk_qtd.end(), 0);
        std::fill(data.begin(), data.end(), sparse_vector_node<T>{false, {}});
    }

    // iterator begin() { return {first, &chunk_qtd, &data}; }
    // iterator end() { return {}; }

    const_iterator begin() const { return {first, &chunk_qtd, &data}; }
    const_iterator end() const { return {}; }

    size_t size() { return size_enabled; }
    size_t capacity() { return data.capacity(); }

    void enable(size_t pos)
    {
        if(!data[pos].enabled) {
            data[pos].enabled = true;
            inc_count(pos);
        }

        if (first == -1) first = pos;
        if (pos < first) first = pos;
    }

    void disable(size_t pos)
    {
        if(data[pos].enabled) {
            data[pos].enabled = false;
            dec_count(pos);
        }

        if (first == pos) {
            while(!data[first].enabled && first <=data.size()) ++first;
            if(first >= data.size()) first = -1;
        }
    }

    void push_back(const T& item)
    {
        auto pos = find_pos();
        inc_count(pos);

        data[pos] = sparse_vector_node<T>{true, item};
    }
private:
    void inc_count(size_t pos)
    {
        auto chunk_pos = pos / SIZE;
        if(chunk_pos >= chunk_qtd.size()) {
            chunk_qtd.push_back(1);
        } else {
            ++chunk_qtd[chunk_pos];
        }

        ++size_enabled;
    }

    void dec_count(size_t pos)
    {
        auto chunk_pos = pos / SIZE;
        if(chunk_pos >= chunk_qtd.size()) {
            chunk_qtd.push_back(0);
        } else {
            --chunk_qtd[chunk_pos];
        }

        --size_enabled;
    }

    size_t find_pos()
    {
        if(next_pos >= data.size()) {
            data.push_back({false, {}});
            next_pos = data.size() - 1;
        }
        
        auto np = next_pos;
        ++next_pos;

        return np;
    }

    size_t next_pos;
    size_t size_enabled;
    int64_t first;
    std::vector<uint16_t> chunk_qtd;
    std::vector<sparse_vector_node<T>> data;
};

int main()
{
    std::vector<int> numbers (1000000);
    std::fill(numbers.begin(), numbers.end(), 1);

    std::vector<int> numbers2 (1000000);
    std::fill(numbers2.begin(), numbers2.end(), 0);
    
    int accum = 0;
    std::cout << "------------------------Linear run - vector" << std::endl;

    accum = 0; time_for_each (numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
    accum = 0; time_begin_end(numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
    accum = 0; time_by_index (numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
    accum = 0; time_data_directly(numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;

    // std::cout << "------------------------Linear run - deque" << std::endl;
    // std::deque<int> dnumbers (1000000);
    // std::fill(dnumbers.begin(), dnumbers.end(), 0);

    // accum = 0; time_for_each(dnumbers, [&](auto&& x){ accum += x; }); std::cout << accum << std::endl;
    // time_begin_end(dnumbers);
    // time_by_index(dnumbers);

    std::cout << "------------------------From sorted Set" << std::endl;
    std::set<size_t> selected;
    uint64_t last = 0;
    for(int i = 0; i < 1000000; ++i)
    {
        selected.insert(last);
        ++last;
    }

    accum = 0; time_foreach_index(selected, numbers, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;
    accum = 0; time_iterators_iterators(numbers, selected, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;

    std::cout << "------------------------Linear run - set" << std::endl;

    accum = 0; time_for_each(selected, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;

    std::cout << "------------------------From sorted vector" << std::endl;
    sorted_vector<size_t> selected2;
    last = 0;
    for(int i = 0; i < 1000000; ++i)
    {
        selected2.insert(last);
        ++last;
    }
    
    accum = 0; time_foreach_index(selected2, numbers, [&](auto&& x){ accum += x; }); std::cout << accum << std::endl;
    // time_iterators_iterators(numbers, selected2);

    // std::cout << "------------------------Join" << std::endl;
    // time_foreach_index_index(selected2, numbers, numbers2);
    
    // time_join_nadic(selected2, [&](auto&&a, auto&& b) {
    //    accum += a + b;
    // }, numbers, numbers2);
    // std::cout << accum << std::endl;


    // std::vector<data_with_flag> positions(1000000);
    // std::fill(positions.begin(), positions.end(), data_with_flag{true, 0, 0, 0});
    // std::cout << "------------------------data with flag - all objects" << std::endl;

    // accum = 0; time_for_each(positions, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;
    // accum = 0; time_foreach_index(selected2, positions, [&](auto&& x) { accum += x.x; }); std::cout << accum << std::endl;
    // accum = 0; time_foreach_index(selected2, positions, [&](auto&& x) { 
    //    if(x.enabled)
    //        accum += x.x;
    // }); std::cout << accum << std::endl;
    // // time_begin_end(positions);
    // // time_by_index(positions);
    // // time_data_directly(positions);

    // std::cout << "------------------------data with flag - only 50% branch prediction - interleaved" << std::endl;
    // for(auto it = positions.begin(); it != positions.end(); it+=2) {
    //    it->enabled = false;
    // }

    // accum = 0; time_foreach_index(selected2, positions, [&](auto&& x) { 
    //    if(x.enabled)
    //        accum += x.x;
    // }); std::cout << accum << std::endl;

    // std::cout << "------------------------data with flag - only 50% branch prediction - first half" << std::endl;
    // for(auto it = positions.begin(); it != positions.end(); ++it) { it->enabled = false; }
    // for(auto it = positions.begin(); it != positions.begin() + (positions.size() / 2); ++it) { it->enabled = true; }

    // accum = 0; time_foreach_index(selected2, positions, [&](auto&& x) { 
    //    if(x.enabled)
    //        accum += x.x;
    // }); std::cout << accum << std::endl;


    // std::cout << "------------------------only 25% branch prediction" << std::endl;
    // for(auto it = positions.begin(); it != positions.end(); ++it) { it->enabled = true; }
    // for(auto it = positions.begin(); it != positions.end(); it+=4) { it->enabled = false; }

    // accum = 0; time_foreach_index(selected2, positions, [&](auto&& x) { 
    //    if(x.enabled)
    //        accum += x.x;
    // }); std::cout << accum << std::endl;

    // std::cout << "------------------------sparse vector - empty" << std::endl;
    // sparse_vector<data_with_flag, 100> positions_sparse25(1000000);    
    // accum = 0; time_for_each(positions_sparse25, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;

    // std::cout << "------------------------sparse vector - full" << std::endl;
    // for(size_t i = 0; i < positions_sparse25.capacity(); ++i)
    //     positions_sparse25.enable(i);
    // accum = 0; time_for_each(positions_sparse25, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;

    // std::cout << "------------------------sparse vector - 50% - interleaved" << std::endl;
    // for(size_t i = 0; i < positions_sparse25.capacity(); ++i) {
    //     if(i%2 == 0)
    //         positions_sparse25.enable(i);
    //     if(i%2 == 1)
    //         positions_sparse25.disable(i);
    // }
    // accum = 0; time_for_each(positions_sparse25, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;

    // std::cout << "------------------------sparse vector - 50% - first half" << std::endl;
    // for(size_t i = 0; i < positions_sparse25.capacity(); ++i) {
    //     positions_sparse25.disable(i);
    // }
    // for(size_t i = 0; i < positions_sparse25.capacity() / 2; ++i) {
    //     positions_sparse25.enable(i);
    // }
    // accum = 0; time_for_each(positions_sparse25, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;

    // std::cout << "------------------------sparse vector - 20% - interleaved" << std::endl;
    // for(size_t i = 0; i < positions_sparse25.capacity(); ++i) {
    //     if(i%5 == 0)
    //         positions_sparse25.enable(i);
    //     if(i%5 > 0)
    //         positions_sparse25.disable(i);
    // }
    // accum = 0; time_for_each(positions_sparse25, [&](auto&& x){ accum += x.x; }); std::cout << accum << std::endl;
    return 0;
}