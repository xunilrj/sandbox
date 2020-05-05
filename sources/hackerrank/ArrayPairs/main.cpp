#include <bits/stdc++.h>
#include <cmath>
#include <pthread.h>
#include <string>

using namespace std;

vector<string> split_string(string);

struct segment_tree_node
{
    bool active;

    uint64_t max;
    uint64_t min;

    size_t start;
    size_t end; // +1 exclusive
};

std::string dot(const vector<segment_tree_node>& max_tree, size_t i)
{
    auto& current = max_tree[i];
    return "\"["s + std::to_string(current.start) + "," + std::to_string(current.end) + "];" + std::to_string(current.max) + ";" + std::to_string(current.min) + "\"";
}

void printTree(const vector<segment_tree_node>& max_tree)
{
    std::cout << "digraph G {" << std::endl;
    for(auto i = 0;i < max_tree.size(); ++i)
    {
        auto& current = max_tree[i];
        if(current.active)
        {
            auto& ln = max_tree[2*i+1];
            auto& rn = max_tree[2*i+2];
            if(ln.active)
                std::cout << dot(max_tree, i) << "->" << dot(max_tree, 2*i+1) << std::endl;
            if(rn.active)
                std::cout << dot(max_tree, i) << "->" << dot(max_tree, 2*i+2) << std::endl;
        }
    }
    std::cout << "}" << std::endl;
}

//////////////////////////////////////////////// SEGMENT TREE
void agg_tree(const vector<int>& arr,
    size_t i,
    size_t start, size_t end,
    vector<segment_tree_node>& max_tree
)
{
    if(end - start == 1)
    {
        //std::cout << "arr[" << start << "]=" << arr[start] << " " << start << " " << end << std::endl;
        max_tree[i] = { true, (uint64_t)arr[start], (uint64_t) arr[start], start, end};
    }
    else
    {
        auto mid = (end - start) / 2;
        auto li = 2*i+1;
        auto ri = 2*i+2;
        agg_tree(arr, li, start, start + mid, max_tree);
        agg_tree(arr, ri, start + mid, end, max_tree);

        auto max = std::max(max_tree[li].max, max_tree[ri].max);
        auto starti = start*2;
        auto endi = end*2;
        auto min = std::min(max_tree[li].min, max_tree[ri].min);
        max_tree[i] = { true, max, min, start, end };
    }    
}

int get_max(const vector<segment_tree_node>& tree, size_t i, size_t start, size_t end)
{
    auto& current = tree[i];
    if(!current.active) return 0;

    // auto& ln = tree[2*i+1];
    // auto& rn = tree[2*i+2];
    // if(ln.active)
    //     std::cout << dot(tree, i) << "->" << dot(tree, 2*i+1) << std::endl;
    // if(rn.active)
    //     std::cout << dot(tree, i) << "->" << dot(tree, 2*i+2) << std::endl;

    if(current.start >= start && current.end <= end)
    {
        //std::cout << "within:" << current.max << std::endl;
        return current.max;
    }
    else if(current.end <= start || current.start >= end)
        return 0;
    else
    {
        auto l = get_max(tree, 2*i+1, start, end);
        auto r = get_max(tree, 2*i+2, start, end);   

        return std::max(l, r);     
    }
}


// Complete the solve function below.
long solve(vector<int> arr)
{
    auto depth = std::log2(arr.size());
    depth = std::ceil(depth) + 1;

    auto tree_size = std::pow(2, depth);
    auto max_tree = vector<segment_tree_node>(tree_size, {false,0,0,0});

    agg_tree(arr, 0, 0, arr.size(), max_tree);
    printTree(max_tree);

    auto arrSize = arr.size();
    //arrSize = 2;

    size_t count = 0;
    for(size_t i = 0;i < arrSize; ++i)
    {
        auto m = arr[i];
        for(size_t j = i + 1;j < arrSize; ++j)
        {
            //auto m = get_max(max_tree, 0, i, j+1);
            if(arr[j] > m) m = arr[j];
            auto mul = (uint64_t)arr[i] * (uint64_t)arr[j];
            std::cout << arr[i]  << " " << arr[j]<< " " << mul << "<=" << m << std::endl;
            if(mul <= m)
                ++count;
        }
    }
    std::cout << count << std::endl;
    return count;
}

int main()
{
    ofstream fout(getenv("OUTPUT_PATH"));

    int arr_count;
    cin >> arr_count;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    string arr_temp_temp;
    getline(cin, arr_temp_temp);

    vector<string> arr_temp = split_string(arr_temp_temp);

    vector<int> arr(arr_count);

    for (int i = 0; i < arr_count; i++)
    {
        int arr_item = stoi(arr_temp[i]);
        arr[i] = arr_item;
    }

    long result = solve(arr);

    fout << result << "\n";

    fout.close();

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
