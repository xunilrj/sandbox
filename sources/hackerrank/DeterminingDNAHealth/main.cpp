#include <algorithm>
#include <bits/c++config.h>
#include <bits/stdc++.h>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <limits>
#include <list>

using namespace std;

vector<string> split_string(string);

struct trieNode
{
    trieNode* children[26] = {0};
    std::vector<size_t> tags;
};

struct trieCtx
{
    trieNode* it;
};

template <typename T, typename TCompare, T initial>
struct folder
{
    T value = initial;
};

template <typename T, typename TCompare, T initial>
const T& operator >> (const T& v, folder<T,TCompare,initial>& f)
{
    if(TCompare{}(v, f.value))
        f.value = v;
    return v;
}

template <typename T> using maximum =
    folder<T, std::greater<T>, std::numeric_limits<T>::min()>;
template <typename T> using minimum = 
    folder<T, std::less<T>, std::numeric_limits<T>::max()>;

class trie
{
    trieNode root;
    std::list<trieCtx> ctxs;

    void insert(const std::string& str, size_t i, trieNode* it, size_t tag)
    {
        while(i < str.length())
        {
            auto c = str[i];
            auto cv = c - 97;

            auto* child_it = it->children[cv];
            if(child_it == nullptr)
                child_it = it->children[cv] = new trieNode{};

            it = child_it;
            ++i;
        }

        it->tags.push_back(tag);
    }

    template <typename F>
    bool step(trieNode*& it, char c, F f)
    {
        if(it == nullptr) return true;

        auto cv = c - 97;
        auto* child_it = it->children[cv];
        if(child_it == nullptr) return true;

        for(auto tag : child_it->tags) f(tag);
        // std::copy(
        //     child_it->tags.begin(),
        //     child_it->tags.end(),
        //     std::back_inserter(tags));

        it = child_it;
        return false;
    }
public:
    void push(const std::string& str, size_t tag)
    {        
        insert(str, 0, &root, tag);
    }

    void clearAll()
    {
        ctxs.clear();
    }

    template <typename F>
    void stepAll(char c, F f)
    {
        ctxs.push_back({&root});

        auto it = ctxs.begin();
        while(it != ctxs.end())
        {
            auto died = step(it->it, c, f);

            if(died) it = ctxs.erase(it);
            else ++it;
        }
    }
};

int main()
{
    size_t n;
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    string genes_temp_temp;
    getline(cin, genes_temp_temp);

    vector<string> genes_temp = split_string(genes_temp_temp);
    vector<string> genes(n);

    for (size_t i = 0; i < n; i++)
    {
        string genes_item = genes_temp[i];
        genes[i] = genes_item;
    }

    string health_temp_temp;
    getline(cin, health_temp_temp);

    vector<string> health_temp = split_string(health_temp_temp);
    vector<int> health(n);

    for (size_t i = 0; i < n; i++)
    {
        int health_item = stoi(health_temp[i]);
        health[i] = health_item;
    }

    size_t s;
    cin >> s;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    // build trie
    trie t;
    for (size_t i = 0; i < n; i++)
        t.push(genes[i], i);

    maximum<uint64_t> M;
    minimum<uint64_t> m;
    
    for (size_t s_itr = 0; s_itr < s; s_itr++)
    {
        string firstLastd_temp;
        getline(cin, firstLastd_temp);

        vector<string> firstLastd = split_string(firstLastd_temp);

        size_t first = stoi(firstLastd[0]);
        size_t last = stoi(firstLastd[1]);
        string d = firstLastd[2];

        t.clearAll();

        // step trie
        uint64_t score = 0;
        for(size_t string_char = 0;string_char < d.size(); ++string_char)
        {
            t.stepAll(d[string_char], [&](auto tag){
                if(tag >= first && tag <= last)
                    score += health[tag];
            });

            // for(auto tag : tags)
            //     if(tag >= first && tag <= last)
            //         score += health[tag];
        }

        score >> M >> m;
    }

    std::cout << m.value << " " << M.value << std::endl;

    return 0;
}

vector<string> split_string(string input_string) {
    string::iterator new_end = unique(input_string.begin(), input_string.end(), [] (const char &x, const char &y) {
        return x == y and x == ' ';
    });

    input_string.erase(new_end, input_string.end());

    while (input_string[input_string.length() - 1] == ' ') {
        input_string.pop_back();
    }

    vector<string> splits;
    char delimiter = ' ';

    size_t i = 0;
    size_t pos = input_string.find(delimiter);

    while (pos != string::npos) {
        splits.push_back(input_string.substr(i, pos - i));

        i = pos + 1;
        pos = input_string.find(delimiter, i);
    }

    splits.push_back(input_string.substr(i, min(pos, input_string.length()) - i + 1));

    return splits;
}
