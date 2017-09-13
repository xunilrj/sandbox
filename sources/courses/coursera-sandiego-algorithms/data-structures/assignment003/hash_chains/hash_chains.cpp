#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <list>
#include <algorithm>
#include <tuple>

using std::string;
using std::vector;

template <int p, int a, int b>
int hashf(int m, int x)
{
    return ((a*x + b) % p) % m;
}

template <size_t p, size_t a>
int polinomial_hashf(int m, const std::string& x)
{
    // static const size_t multiplier = 263;
    // static const size_t prime = 1000000007;
    unsigned long long hash = 0;
    for (int i = static_cast<int> (x.size()) - 1; i >= 0; --i)
        hash = (hash * a + x[i]) % p;
    return hash % m;
}

template <typename TKey, typename TValue>
struct Hashmap
{

    bool get(TKey key, TValue& out) const
    {
        auto i = hash(key);
        auto& t = *table[i];
        auto it = std::find_if(t.begin(), t.end(), [&](const std::tuple<TKey,TValue>& x){return std::get<0>(x) == key;});

        if(it != t.end())
        {
            out = std::get<1>(*it);
            return true;
        }
        else
        {
            return false;
        }
    }

    void set(TKey key, TValue value)
    {
        auto i = hash(key);        
        auto& t = *table[i];
         auto it = std::find_if(t.begin(), t.end(), [&](const std::tuple<TKey,TValue>& x){return std::get<0>(x) == key;});

        if(it == t.end())
        {
            t.emplace_back(key,value);
        }
    }

    void remove(TKey key)
    {
        auto i = hash(key);
        auto& t = *table[i];
        auto it = std::find_if(t.begin(), t.end(), [&](const std::tuple<TKey,TValue>& x){return std::get<0>(x) == key;});

        if(it != t.end())
        {
            t.erase (it);
        }
    }

    int hash(TKey key) const
    {
        return polinomial_hashf<1000000007, 263>(m, key);
    }

    template <typename F>
    void check(int i, F f) const
    {
        auto& t = *table[i];

        std::for_each(t.rbegin(), t.rend(), [&](const std::tuple<TKey,TValue>& x)
        {
            f(std::get<1>(x));
        });
    }

    Hashmap(int size) : m(size)
    {
         table.reserve(m);
         for(int i = 0;i < m;++i)
         {
             table[i] = new std::list< std::tuple<TKey,TValue> >();
         }
    }

    int m;
    std::vector< std::list< std::tuple<TKey,TValue> >* > table;
};


struct Query {
    string type, s;
    size_t ind;
};

class QueryProcessor {
    int bucket_count;
    // store all strings in one vector
    // vector<string> elems;
    Hashmap<std::string,std::string> elems;

    size_t hash_func(const string& s) const {
        
    }

public:
    explicit QueryProcessor(int bucket_count): bucket_count(bucket_count), elems(bucket_count)
    {

    }

    Query readQuery(std::istream& in) const {
        Query query;
        in >> query.type;
        if (query.type != "check")
            in >> query.s;
        else
            in >> query.ind;
        return query;
    }

    void writeSearchResult(std::ostream& out, bool was_found) const {
        out << (was_found ? "yes\n" : "no\n");
    }

    void processQuery(std::ostream& out, const Query& query) {
        if (query.type == "check") {
            elems.check(query.ind, [&](const std::string& x){
                // std::cout << x ;
                out << x << " ";
            });
            // // use reverse order, because we append strings to the end
            // for (int i = static_cast<int>(elems.size()) - 1; i >= 0; --i)
            //     if (hash_func(elems[i]) == query.ind)
            //         out << elems[i] << " ";
            out << "\n";
        } else {
            // vector<string>::iterator it = std::find(elems.begin(), elems.end(), query.s);
            if (query.type == "find"){
                std::string resultstr;
                auto result = elems.get(query.s, resultstr);
                writeSearchResult(out, result);
            //     writeSearchResult(out, it != elems.end());
            }
            else if (query.type == "add") {
                elems.set(query.s,query.s);
            //     if (it == elems.end())
            //         elems.push_back(query.s);
            }
            else if (query.type == "del") {
                elems.remove(query.s);
            //     if (it != elems.end())
            //         elems.erase(it);
            }
        }
    }

    void processQueries(std::istream& in, std::ostream& out) {
        int n;
        in >> n;
        for (int i = 0; i < n; ++i)
            processQuery(out, readQuery(in));
    }
};

void run(std::istream &in, std::ostream& out)
{
    int bucket_count;
    in >> bucket_count;
    QueryProcessor proc(bucket_count);
    proc.processQueries(in, out);
}


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& instr, const std::string& expectedoutstr)
{
    auto instream = std::stringstream{instr};
    auto actualOutStream = std::stringstream{};
    auto expectedOutStream = std::stringstream{expectedoutstr};

    run(instream, actualOutStream);
    actualOutStream.seekg(0);

    std::string actualLine, expectedLine;
    while(!expectedOutStream.eof())
    {
        std::getline(expectedOutStream, expectedLine);
        std::getline(actualOutStream, actualLine);

        REQUIRE(expectedLine == actualLine);
    }
}

TEST_CASE("","")
{
    test(R"(5
        12
        add world
        add HellO
        check 4
        find World
        find world
        del world
        check 4
        del HellO
        add luck
        add GooD
        check 2
        del good)",R"(HellO world 
no
yes
HellO 
GooD luck )");
}


#else

int main() {
    run(std::cin, std::cout);
    return 0;
}

#endif
