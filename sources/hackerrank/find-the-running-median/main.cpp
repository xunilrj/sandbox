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
    size_t qtd;
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
    
    slidingWindowMedian() : qtd{0}
    {        
    }
    
    void push_back(int v)
    {
        if(qtd == 0)
        {
            biggers.insert(v);
            ++qtd;
            return;
        }

        auto median = get_median(); 
        auto insertBigger = v >= median;
       
        if(insertBigger)
            biggers.insert(v);
        else
            smallers.insert(v);

        ++qtd;
        
        auto qtdhalfsize = qtd / 2;
        while(smallers.size() > qtdhalfsize)
            move(smallers, biggers);
        while(smallers.size() < qtdhalfsize)
                move(biggers, smallers);
    }
    
    double get_median() const
    {
        if(qtd == 0) return 0;

        double smallest_biggers = *biggers.begin();
        if(qtd == 1) return smallest_biggers;

        size_t smallers_size = smallers.size();
        
        //if we have items
        //balance dictates that we have
        //one at biggers
        
        if(qtd % 2 == 1)
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


/*
 * Complete the runningMedian function below.
 */
vector<double> runningMedian(vector<int>& a) {
    slidingWindowMedian w;

    std::vector<double> medians;
    for (auto v : a)
    {
        w.push_back(v);
        medians.push_back(w.get_median());
    }
    return medians;
}

int main()
{
    ofstream fout(getenv("OUTPUT_PATH"));

    int a_count;
    cin >> a_count;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<int> a(a_count);

    for (int a_itr = 0; a_itr < a_count; a_itr++) {
        int a_item;
        cin >> a_item;
        cin.ignore(numeric_limits<streamsize>::max(), '\n');

        a[a_itr] = a_item;
    }

    vector<double> result = runningMedian(a);

    for (int result_itr = 0; result_itr < result.size(); result_itr++) {
        fout << result[result_itr];

        if (result_itr != result.size() - 1) {
            fout << "\n";
        }
    }

    fout << "\n";

    fout.close();

    return 0;
}
