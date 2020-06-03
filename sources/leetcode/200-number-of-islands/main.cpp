#include <stack>
#include <queue>
#include <tuple>
#include <algorithm>

using grid = vector<vector<char>>;

bool dfs(grid &g, uint64_t i, uint64_t j)
{
    if (g[i][j] == '0')
        return false;

    std::queue<std::tuple<uint64_t, uint64_t>> q{};
    q.push({i, j});

    while (q.size() > 0)
    {
        auto [i, j] = q.front();
        q.pop();

        if (i < 0)
            continue;
        if (j < 0)
            continue;
        if (i >= g.size())
            continue;
        if (j >= g[i].size())
            continue;

        if (g[i][j] == '0')
            continue;

        g[i][j] = '0';

        q.push({i - 1, j + 0});
        q.push({i + 0, j - 1});
        q.push({i + 1, j + 0});
        q.push({i + 0, j + 1});
    }

    return true;
}

class DisjointSet
{
    std::vector<int> root;
    uint64_t width;
    uint64_t height;

    uint64_t find_root(uint64_t idx) const
    {
        auto v = root[idx];
        if (v == idx)
            return idx;
        if (v == -2)
            return idx;
        if (v == -1)
            return idx;
        else
            return find_root(v);
    }

public:
    DisjointSet(grid &g) : root(g.size() * g[0].size(), -2),
                           width(g[0].size()),
                           height(g.size())
    {
    }

    void set(uint64_t i, uint64_t j)
    {
        if (i >= width)
            return;
        if (j >= height)
            return;

        auto idx = j * width + i;
        if (root[idx] == -2)
            root[j * width + i] = j * width + i;
    }

    void unite(grid &g, uint64_t ia, uint64_t ja, uint64_t ib, uint64_t jb)
    {
        if (ia >= width)
            return;
        if (ja >= height)
            return;
        if (ib >= width)
            return;
        if (jb >= height)
            return;

        if (g[jb][ib] == '0')
            return;

        auto aroot = find_root(ja * width + ia);
        auto broot = find_root(jb * width + ib);

        root[aroot] = broot;
    }

    uint64_t count_roots() const
    {
        auto count = 0;
        for (auto i = 0; i < root.size(); ++i)
        {
            if (root[i] == i)
                ++count;
        }

        return count;
    }
};

class Solution
{
public:
    int numIslands(vector<vector<char>> &g)
    {
        if (g.size() == 0)
            return 0;
        if (g[0].size() == 0)
            return 0;

        DisjointSet sets{g};
        for (auto j = 0; j < g.size(); ++j)
        {
            auto &row = g[j];
            for (auto i = 0; i < row.size(); ++i)
            {

                if (g[j][i] == '0')
                    continue;

                sets.set(i, j);
                sets.unite(g, i, j, i - 1, j + 0);
                sets.unite(g, i, j, i + 0, j - 1);
                sets.unite(g, i, j, i + 1, j + 0);
                sets.unite(g, i, j, i + 0, j + 1);
            }
        }

        return sets.count_roots();
    }
};