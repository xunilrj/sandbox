#include <bits/stdc++.h>
#include <algorithm>

using namespace std;

#include <set>
#include <functional>
#include <algorithm>
#include <queue>

template <typename T>
struct reversion_wrapper { T& iterable; };

template <typename T>
auto begin (reversion_wrapper<T> w) { return std::rbegin(w.iterable); }

template <typename T>
auto end (reversion_wrapper<T> w) { return std::rend(w.iterable); }

template <typename T>
reversion_wrapper<T> iter_reverse (T&& iterable) { return { iterable }; }

template <typename T>
std::ostream& operator << (std::ostream& out, const std::vector<T>& vector)
{
    for(auto& v : vector)
    {
        out << v << " ";
    }

    return out;
}

class circularQueue
{
    std::vector<int> history;
    size_t hstart;
    size_t hend;
    size_t qtd;
public:
    
    circularQueue(size_t k) : 
        history(k), hstart{0}, hend{0}, qtd{0}
    {
    }
    
    std::tuple<bool,int> push_back(int v)
    {
        if(isFull())
        {
            auto remove = history[hstart];            
            history[hstart] = v;
            (++hstart) %= history.size();
            (++hend) %= history.size();
            
            return {true, remove};
        }
        else
        {        
            history[hend] = v;
            (++hend) %= history.size();
            ++qtd;
            
            return {false, 0};
        }
    }
    
    bool isFull() const { return qtd == history.size(); }
    size_t size() const { return qtd; }
};


class slidingWindowMedian
{
    circularQueue q;
    size_t k;
    std::multiset<int, std::greater<int>> smallers;
    std::multiset<int, std::less<int>> biggers;
 
    template <typename TContainer>
    bool remove(TContainer& multiset, int value)
    {
        auto itr = multiset.find(value);
        if(itr != multiset.end())
        { 
            multiset.erase(itr);
            return true;
        }
        return false;
    }
    
    template <typename TA, typename TB>
    void move(TA&a, TB&b)
    {
        auto it = a.begin();
        b.insert(*it);
        a.erase(it);                
    }
public:
    using value_type = int;
    
    slidingWindowMedian(size_t k) : k{k}, q{k}
    {        
    }
    
    void push_back(int v)
    {
        auto t = q.push_back(v);
        auto popped = std::get<0>(t);
        auto popped_value = std::get<1>(t);
        
        auto median = get_median(); 
        auto insertBigger = v >= median;
        if(popped)
        {
            auto removeBigger = popped_value >= median;
            
            if(insertBigger)
            {
                biggers.insert(v);
                if(removeBigger)
                {
                    remove(biggers, popped_value);
                }
                else
                {
                    remove(smallers, popped_value);
                    move(biggers,smallers);
                }
            }
            else
            {
                smallers.insert(v);
                if(removeBigger)
                {
                    remove(biggers, popped_value);
                    move(smallers,biggers);
                }
                else
                {
                    remove(smallers, popped_value);
                }
            }
        }
        else
        {
            if(insertBigger)
                biggers.insert(v);
            else
                smallers.insert(v);
            
            auto qsize = q.size();
            auto qhalfsize = qsize / 2;
            while(smallers.size() > qhalfsize)
                move(smallers, biggers);
            while(smallers.size() < qhalfsize)
                 move(biggers, smallers);
        }
    }
    
    double get_median() const
    {
        size_t biggers_size = biggers.size();
        size_t smallers_size = smallers.size();
        
        //if(biggers_size == 0 && smallers_size == 0)
        //  return 0;
        
        //if we have items
        //balance dictates that we have
        //one at biggers
        double smallest_biggers = *biggers.begin();
        if(k % 2 == 1)
        {
            return smallest_biggers;
        }
        else
        {
            //if(smallers_size == 0)
            //  return smallest_biggers;
            
            double bigger_smallest = *smallers.begin();
            return (smallest_biggers + bigger_smallest)/2.0;
        }        
    }
    
    void dump()
    {
        std::cout << "slidingWindowMedian ";            
        for(auto& x : iter_reverse(smallers))
            std::cout << x << " ";
        std::cout << " | ";
        for(auto& x : biggers)
            std::cout << x << " ";
        std::cout << std::endl;            
        
    }
};


vector<string> split_string(string);

struct vi { int v; int i;};

std::ostream& operator << (std::ostream& out, const vi& v)
{
    return out << "{" << v.v << "," << v.i << "} " ;
}

int activityNotifications(vector<int>& expenditure, int d) {
    auto w = slidingWindowMedian{(size_t)d};
    std::copy_n(expenditure.begin(), d, std::back_inserter(w));

    auto notifications = 0;
    for(auto it = expenditure.begin() + d;it != expenditure.end(); ++it)
    {
        double median = w.get_median();
        double current = *it;

        if(current >= 2.0 * median)
            ++notifications;

        w.push_back(*it);
    }

    return notifications;
}

int main()
{
    ofstream fout(getenv("OUTPUT_PATH"));

    string nd_temp;
    getline(cin, nd_temp);

    vector<string> nd = split_string(nd_temp);

    int n = stoi(nd[0]);

    int d = stoi(nd[1]);

    string expenditure_temp_temp;
    getline(cin, expenditure_temp_temp);

    vector<string> expenditure_temp = split_string(expenditure_temp_temp);

    vector<int> expenditure(n);

    for (int i = 0; i < n; i++) {
        int expenditure_item = stoi(expenditure_temp[i]);

        expenditure[i] = expenditure_item;
    }

    int result = activityNotifications(expenditure, d);

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
