#include <bits/stdc++.h>
#include <cmath>
#include <pthread.h>
#include <string>

#define BACKWARD_HAS_DWARF 1
#include "backward.hpp"
namespace backward
{
    backward::SignalHandling sh;
}

using namespace std;

vector<string> split_string(string);

// struct segment_tree_node
// {
//     bool active;

//     uint64_t max;
//     uint64_t min;

//     size_t maxi;
//     size_t mini;

//     size_t start;
//     size_t end; // +1 exclusive
// };

// std::string dot(const vector<segment_tree_node>& max_tree, size_t i)
// {
//     auto& current = max_tree[i];
//     return "\"["s + std::to_string(current.start) + "," + std::to_string(current.end) + "];" + std::to_string(current.max) + ";" + std::to_string(current.min) + "\"";
// }

// void printTree(const vector<segment_tree_node>& max_tree)
// {
//     std::cout << "digraph G {" << std::endl;
//     for(auto i = 0;i < max_tree.size(); ++i)
//     {
//         auto& current = max_tree[i];
//         if(current.active)
//         {
//             auto& ln = max_tree[2*i+1];
//             auto& rn = max_tree[2*i+2];
//             if(ln.active)
//                 std::cout << dot(max_tree, i) << "->" << dot(max_tree, 2*i+1) << std::endl;
//             if(rn.active)
//                 std::cout << dot(max_tree, i) << "->" << dot(max_tree, 2*i+2) << std::endl;
//         }
//     }
//     std::cout << "}" << std::endl;
// }

// //////////////////////////////////////////////// SEGMENT TREE
// void agg_tree(const vector<int>& arr,
//     size_t i,
//     size_t start, size_t end,
//     vector<segment_tree_node>& max_tree
// )
// {
//     if(end - start == 1)
//     {
//         //std::cout << "arr[" << start << "]=" << arr[start] << " " << start << " " << end << std::endl;
//         max_tree[i] = { true, 
//             (uint64_t)arr[start],
//             (uint64_t) arr[start],
//             start,
//             start,
//             start, end};
//     }
//     else
//     {
//         auto mid = (end - start) / 2;
//         auto li = 2*i+1;
//         auto ri = 2*i+2;
//         agg_tree(arr, li, start, start + mid, max_tree);
//         agg_tree(arr, ri, start + mid, end, max_tree);

//         auto max = std::max(max_tree[li].max, max_tree[ri].max);
//         auto min = std::min(max_tree[li].min, max_tree[ri].min);
//         max_tree[i] = { true, 
//             max, min, 
//             max == max_tree[li].max ? max_tree[li].maxi : max_tree[ri].maxi,
//             min == max_tree[li].min ? max_tree[li].mini : max_tree[ri].mini,
//             start, end };
//     }    
// }

// std::tuple<uint64_t,size_t> get_max(const vector<segment_tree_node>& tree, size_t i, size_t start, size_t end)
// {
//     auto& current = tree[i];
//     if(!current.active) return {0,0};

//     // auto& ln = tree[2*i+1];
//     // auto& rn = tree[2*i+2];
//     // if(ln.active)
//     //     std::cout << dot(tree, i) << "->" << dot(tree, 2*i+1) << std::endl;
//     // if(rn.active)
//     //     std::cout << dot(tree, i) << "->" << dot(tree, 2*i+2) << std::endl;

//     if(current.start >= start && current.end <= end)
//     {
//         //std::cout << "within:" << current.max << std::endl;
//         return {current.max, current.maxi};
//     }
//     else if(current.end <= start || current.start >= end)
//         return {0, 0};
//     else
//     {
//         auto [lmax, lmaxi] = get_max(tree, 2*i+1, start, end);
//         auto [rmax, rmaxi] = get_max(tree, 2*i+2, start, end);   
//         auto m = std::max(lmax, rmax);
//         return { m, m == lmax ? lmaxi : rmaxi };
//     }
// }

// size_t stride;
// void printStep(size_t* checks)
// {
//     for(size_t i = 0;i < stride; ++i)
//     {
//         for(size_t j = 0;j < stride; ++j)    
//             std::cout << checks[i*stride+j] << " ";
//         std::cout << std::endl;
//     }
// }

// size_t get_count(const vector<int>& arr,
//     const vector<segment_tree_node>& tree,
//     size_t start, size_t end, size_t* checks)
// {
//     std::cout << start << " " << end << std::endl;

//     if((end - start) <= 1) return 0;
//     auto [max, maxi] = get_max(tree, 0, start, end);
//     std::cout << "max " << max << "[" << maxi << "]" << std::endl;

//     size_t count = 0;
//     if(maxi == start)
//     {
//         for (size_t j = maxi + 1;j < end; ++j)
//         {
//             ++checks[start*stride+j];
//             std::cout << "check " << start << " " << j << std::endl;
//             //if(arr[i]*arr[j] <= max) ++count;
//         }
//     }
//     else if(maxi == end - 1)
//     {
//         for (size_t i = start;i < end - 2; ++i)
//         {
//             ++checks[i*stride+(end-1)];
//             std::cout << "check " << i << " " << (end-1) << std::endl;
//             //if(arr[i]*arr[j] <= max) ++count;
//         }
//     }
//     else
//     {        
//         for (size_t i = start;i < maxi; ++i)        
//             for (size_t j = maxi + 1;j < end; ++j)
//             {
//                 ++checks[i*stride+j];
//                 std::cout << "check " << i << " " << j << std::endl;
//                 //if(arr[i]*arr[j] <= max) ++count;
//             }
//     }

//     printStep(checks);
//     std::cout << "-----------------------------------------------------" << std::endl;

//     count += get_count(arr, tree, start, maxi, checks);
//     count += get_count(arr, tree, maxi+1, end, checks);
//     // std::cout << "call " << start << " " << end << " " << count << std::endl;
//     return count;
// }

// void print(auto start, auto end)
// {
//     while(start != end)
//     {
//         std::cout << *start << " ";
//         ++start;
//     }
//     std::cout << std::endl;
// }

// size_t c(auto max, auto a, auto end)
// {
//     while(a > end)
//     {
//         if(*a**(a-1) > *max) --a;
//         else break;
//     }

//     if(a == end)
//     {
//         if(*a**(a-1) <= *max) return 1;
//         else return 0;
//     }
//     else
//     {
//         return std::distance(end, a) + 2;    
//     }
// }

// std::tuple<size_t,size_t> c2(auto max, auto a, auto aend, auto b, auto bend)
// {
//     int i = 0;
//     while(a > aend || b > bend)
//     {
//         if(*a**b > *max)
//         {
//             ++i;
//             if(i % 2 == 0 && a > aend) --a;
//             else if (b > bend) --b;
//         }
//         else
//             break;
//     }

//     if(a == aend && b == bend)
//     {
//         if(*a**b <= *max) return std::tuple {(size_t)1,(size_t)1};
//         else return std::tuple {(size_t)0,(size_t)0}; 
//     }
    
//     auto l = std::distance(aend, a) + 1;
//     auto r = std::distance(bend, b) + 1;
//     return std::tuple{l,r};
// }

using TCIT = std::vector<uint64_t>::const_iterator;
using TIT = std::vector<uint64_t>::iterator;

enum class strategy
{
    sorted,
    normal,
    rle
};

std::tuple<std::vector<size_t>, uint64_t, uint64_t> count_distincts (const TCIT start, const TCIT end)
{
    if(start == end)
        return {{}, 0, 0};

    auto min_max = std::minmax_element(start, end);
    auto min = *min_max.first;
    auto max = *min_max.second;
    
    std::vector<size_t> count((max - min) + 1, 0u);
    for (auto i = start; i != end; ++i)
        ++count[*i - min];

    return {count, min, max};
}

uint64_t count_left_right_cross(const TCIT start, const TCIT end, const TCIT max, const size_t maxi, strategy st)
{
    auto count = 0;
    TCIT nstart, nend, nmax, nendleft, nstartright;

    if (st == strategy::rle)
    {
        nstart = start;
        nend = end;
        nmax = nstart + maxi;
        nendleft = nmax;
        nstartright = nmax + 1;

        // std::cout << "count_distincts" << std::endl;

        auto [cl, minl, maxl] = count_distincts(nstart, nendleft);
        auto [cr, minr, maxr] = count_distincts(nstartright, nend);

        // std::cout << *start << " " << *(end-1) << " " << *max << " ";
        // std::cout << " minl:" << minl << " maxl:" << maxl;
        // std::cout << " minr:" << minr << " maxr:" << maxr;
        // std::cout << std::endl;

        auto la = 0, ra = 0;
        if(minl == 1) la = cl[0];
        if(minr == 1) ra = cr[0];
        
        // std::cout << " la:" << la << " ra:" << ra;
        // std::cout << std::endl;

        if((minl == 0) || (minr == 0)) return la + ra;

        auto l = minl;
        auto r = minr;
        auto rcount = 0;


        while((r <= maxr) && (l*r <= *max)) { rcount += cr[r - minr]; ++r; }
        // std::cout << "+1 " << cl[l - minl] << " " << rcount << std::endl;
        count += cl[l - minl] * rcount;
        if(r > maxr) {  r = maxr; }
        while(l <= maxl)
        {
            ++l;
            if(l > maxl) break;

            // std::cout << "t2 " << r << " " << minr << " " << l << " " << r << " " << *max << std::endl;
            while((r >= minr) && (l*r > *max)) { rcount -=  cr[r - minr]; --r;  }
            
            if(r < minr) break;
            else
            {
                count += cl[l - minl] * rcount;
                // std::cout << "+2 " << cl[l - minl] << " " << rcount << std::endl;
            }
        }

        // std::cout << "cross:" << count << std::endl;

        return count + la + ra;
    }

    auto arr = vector<uint64_t>();
    
    if(st == strategy::sorted)
    {
        nstart = start;
        nend = end;
        nmax = nstart + maxi;
        nendleft = nmax;
        nstartright = nmax + 1;
    }
    else if (st == strategy::normal)
    {
        arr = vector<uint64_t>(start, end);
        auto nnstart = arr.begin();
        auto nnend = arr.end();
        auto nnmax = nnstart + maxi;
        auto nnendleft = nnmax;
        auto nnstartright = nnmax + 1;

        auto lsorted = std::is_sorted(nnstart, nnendleft);
        auto rsorted = std::is_sorted(nnstartright, nnend);

        if(!lsorted)
            std::sort(nnstart, nnendleft);
        if(!rsorted)
            std::sort(nnstartright, nnend);

        nstart = nnstart;
        nend = nnend;
        nmax = nnmax;
        nendleft = nnendleft;
        nstartright = nnstartright;
    }

    auto la = std::distance(nstart, std::upper_bound(nstart, nendleft, 1));
    auto ra = std::distance(nstartright, std::upper_bound(nstartright, nend, 1));

    auto lsize = std::distance(nstart, nendleft);
    auto rsize = std::distance(nstartright, nend);

    if((lsize > 0) && (rsize > 0))
    {        
        auto l = nstart;
        auto r = nstartright;

        bool skip = false;
        if(*l**r > *max) skip = true;

        if(!skip)
        {
            while((r < nend) && (*l**r <= *max)) ++r;
            count += std::distance(nstartright, r);
            if(r >= nend) r = nend - 1;
            // std::cout << *l << " " << *r << " " << *max << std::endl;
            while(l < nendleft)
            {
                ++l;
                if(l >= nendleft) break;

                while((r >= nstartright) && (*l**r > *max)) --r;
                
                if(r < nstartright) break;
                else
                    count += std::distance(nstartright, r) + 1;            
            }
        }
    }

    return count + la + ra;
}

uint64_t recursive_solve(const vector<uint64_t>& arr, TCIT start, TCIT end, strategy st)
{
    if(start >= end) return 0;
    if(std::distance(start, end) == 1) return 0;
    
    auto max = std::max_element(start, end);
    auto nextmax = max;
    for(int j = 0;j < 1000;++j)
    {
        if((nextmax < (end - 1)) && (*nextmax == *max))
            max = nextmax;
        ++nextmax; 
    } 
    auto maxi = std::distance(start, max);
    auto endleft = start + maxi;
    auto startright = start + maxi + 1;

    auto count = count_left_right_cross(start, end, max, maxi, st);
    count += recursive_solve(arr, start, endleft, st);
    count += recursive_solve(arr, startright, end, st);

    return count;
}

// Complete the solve function below.
long solve(const vector<uint64_t>& arr)
{
    auto min_max = std::minmax_element(arr.begin(), arr.end());
    // std::cout << *min_max.second << " " << *min_max.first << std::endl;

    strategy st = strategy::normal;
    
    auto spread =  *min_max.second - *min_max.first;
    if(spread < 3) st = strategy::rle;

    auto count = recursive_solve(arr, arr.begin(), arr.end(), st);
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

    vector<uint64_t> arr(arr_count);

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
