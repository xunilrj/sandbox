#include <vector>
#include <iostream>
#include <algorithm>
#include <climits>
#include <numeric>
#include <set>
#include <tuple>
#include <cmath>
using namespace std;

template <typename T, class TCompare = std::less<T>>
class sorted_vector
{
public:
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;

    sorted_vector(size_t qtd) : data(qtd) {}

    iterator begin() { return data.begin(); }
    iterator end() { return data.end(); }
    const_iterator begin() const { return data.begin(); }
    const_iterator end() const { return data.end(); }

    const_iterator cbegin() { return data.cbegin(); }
    const_iterator cend() { return data.cend(); }

    iterator insert(const T &t)
    {
        iterator i = std::lower_bound(begin(), end(), t, cmp);
        if (i == end() || cmp(t, *i))
            data.insert(i, t);
        return i;
    }

    void erase(iterator it)
    {
        data.erase(it);
    }

private:
    TCompare cmp;
    std::vector<T> data;
};

struct agg
{
    int count;
    size_t min;

    static agg make(int v, size_t start, size_t end)
    {
        return {v, start};
    }

    static agg merge(const agg &a, const agg &b)
    {
        if (a.count == 0)
            return b;
        if (b.count == 0)
            return a;
        return {a.count + b.count, std::min(a.min, b.min)};
    }

    static agg empty(size_t start, size_t end)
    {
        return {0, start};
    }
};

template <typename T>
struct segment_tree
{
    struct segment_tree_node
    {
        bool active;
        size_t start;
        size_t end; // +1 exclusive

        T agg;
    };

    std::vector<int> &arr;
    std::vector<segment_tree_node> seg_tree;

    segment_tree(std::vector<int> &v) : arr{v}
    {
        auto depth = std::log2(arr.size());
        depth = std::ceil(depth) + 1;

        auto tree_size = std::pow(2, depth);
        seg_tree = vector<segment_tree_node>(tree_size, segment_tree_node{});
    }

    void build()
    {
        build(0, 0, arr.size());
    }

    auto get(size_t start, size_t end)
    {
        return get_max(seg_tree, 0, start, end);
    }

    void update(size_t i, int new_value)
    {
        arr[i] = new_value;
        update_tree(seg_tree, 0, 0, arr.size(), i);
    }

private:
    void build(size_t i, size_t start, size_t end)
    {
        if (end - start == 1)
        {
            seg_tree[i] = {true, start, end, T::make(arr[start], start, end)};
        }
        else
        {
            auto mid = (end - start) / 2;
            auto li = 2 * i + 1;
            auto ri = 2 * i + 2;
            build(li, start, start + mid);
            build(ri, start + mid, end);

            seg_tree[i] = {true, start, end, T::merge(seg_tree[li].agg, seg_tree[ri].agg)};
        }
    }

    T get_max(const vector<segment_tree_node> &tree, size_t i, size_t start, size_t end)
    {
        auto &current = tree[i];
        if (!current.active)
            return T::empty(start, end);

        if (current.start >= start && current.end <= end)
            return current.agg;
        else if (current.end <= start || current.start >= end)
            return T::empty(start, end);
        else
        {
            auto l = get_max(tree, 2 * i + 1, start, end);
            auto r = get_max(tree, 2 * i + 2, start, end);
            return T::merge(l, r);
        }
    }

    void update_tree(const vector<segment_tree_node> &tree, size_t i, size_t start, size_t end, size_t updated_i)
    {
        if (end - start == 1)
        {
            seg_tree[i] = {true, start, end, T::make(arr[i], start, end)};
        }
        else
        {
            auto mid = (end - start) / 2;
            auto li = 2 * i + 1;
            auto ri = 2 * i + 2;

            if (updated_i < mid)
                update_tree(seg_tree, li, start, start + mid, updated_i);
            else
                update_tree(seg_tree, ri, start + mid, end, updated_i);

            seg_tree[i] = {true, start, end, T::merge(seg_tree[li].agg, seg_tree[ri].agg)};
        }
    }
};

class Solution
{
public:
    int maxEvents(vector<vector<int>> &events)
    {
        std::sort(events.begin(), events.end(), [](const auto &a, const auto &b) {
            return a[1] < b[1];
        });

        int minn = INT_MAX;
        int maxx = INT_MIN;
        for (auto &&x : events)
        {
            minn = std::min(minn, x[0]);
            maxx = std::max(maxx, x[1]);
        }

        std::vector<int> days(maxx - minn + 1, 1);
        auto t = segment_tree<agg>{days};
        t.build();

        int ans = 0;
        for (const auto &e : events)
        {
            auto q = t.get(e[0] - minn, e[1] - minn);

            if (q.count == 0)
                continue;

            t.update(q.min, 0);
            ++ans;
        }
        return ans;
    }
};

int main()
{
    char c;
    std::cin >> c;

    vector<vector<int>> events;

    while (true)
    {
        std::cin >> c;
        if (c == '[')
        {
            int a, b;
            std::cin >> a >> c >> b >> c;
            events.push_back({a, b});
        }
        else if (c == ',')
            continue;
        else if (c == ']')
            break;
        else
        {
            exit(1);
        }
    }

    std::cout << "running..." << events.size() << std::endl;
    auto s = Solution{};
    std::cout << "solution" << s.maxEvents(events) << std::endl;
}
