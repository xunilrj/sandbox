#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <list>
#include <tuple>
#include <algorithm>

using std::string;
using std::vector;

struct Query {
    string type, name;
    int number;
};

std::ostream& operator << (std::ostream& out, Query q)
{
    out << q.type << "," << q.number << "," << q.name;
}

template <int p, int a, int b>
int hashf(int m, int x)
{
    return ((a*x + b) % p) % m;
}

template <typename TKey, typename TValue>
struct Hashmap
{

    bool get(TKey key, TValue& out) const
    {
        // std::cout << "get" << std::endl;

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

        if(it != t.end())
        {
            t.erase(it);
        }

        t.emplace_back(key,value);
    }

    void remove(TKey key)
    {
        auto i = hash(key);
        auto& t = *table[i];

        // std::cout << "remove" << std::endl;

        auto it = std::find_if(t.begin(), t.end(), [&](const std::tuple<TKey,TValue>& x){return std::get<0>(x) == key;});
        if(it != t.end())
        {
            t.erase (it);
        }
    }

    int hash(TKey key) const
    {
        return hashf<10000019, 34, 2>(m, key);
    }

    Hashmap() : m(1000)
    {
         table.reserve(m);
         for(int i = 0;i < 1000;++i)
         {
             table[i] = new std::list< std::tuple<TKey,TValue> >();
         }
    }

    int m;
    std::vector< std::list< std::tuple<TKey,TValue> >* > table;
};

vector<Query> read_queries(std::istream& in) {
    int n;
    in >> n;
    vector<Query> queries(n);
    for (int i = 0; i < n; ++i) {
        in >> queries[i].type;
        if (queries[i].type == "add")
            in >> queries[i].number >> queries[i].name;
        else
            in >> queries[i].number;
    }
    return queries;
}

void write_responses(std::ostream& out, const vector<string>& result) {
    for (size_t i = 0; i < result.size(); ++i)
        out << result[i] << "\n";
}

vector<string> process_queries(const vector<Query>& queries) {
    Hashmap<int, std::string> table;

    vector<string> result;
    // Keep list of all existing (i.e. not deleted yet) contacts.
    vector<Query> contacts;
    for (size_t i = 0; i < queries.size(); ++i)
    {
        auto q = queries[i];
        // std::cout << q << std::endl;
        if (q.type == "add") {
            
            table.set(q.number, q.name);
            // bool was_founded = false;
            // // if we already have contact with such number,
            // // we should rewrite contact's name
            // for (size_t j = 0; j < contacts.size(); ++j)
            //     if (contacts[j].number == queries[i].number) {
            //         contacts[j].name = queries[i].name;
            //         was_founded = true;
            //         break;
            //     }
            // // otherwise, just add it
            // if (!was_founded)
            //     contacts.push_back(queries[i]);
        } else if (q.type == "del") {
            table.remove(q.number);
            // for (size_t j = 0; j < contacts.size(); ++j)
            //     if (contacts[j].number == queries[i].number) {
            //         contacts.erase(contacts.begin() + j);
            //         break;
            //     }            
        } else {
            string response = "not found";
            table.get(q.number, response);
            result.push_back(response);
            
            // for (size_t j = 0; j < contacts.size(); ++j)
            //     if (contacts[j].number == queries[i].number) {
            //         response = contacts[j].name;
            //         break;
            //     }
            // result.push_back(response);
        }
    }
    return result;
}

void run(std::istream& in, std::ostream& out)
{
    write_responses(out, process_queries(read_queries(in)));
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string strin, const std::string strExpectedOut)
{
    auto instream = std::stringstream{strin};
    auto expectedOutstream = std::stringstream{strExpectedOut};
    auto outstream = std::stringstream{};
    run(instream,outstream);

    std::cout << strExpectedOut << std::endl;
    std::cout << "-------------------" << std::endl;
    std::cout << outstream.str() << std::endl;
    std::cout << "-------------------" << std::endl;

    outstream.seekg(0);

    std::string expected, actual;
    while(!outstream.eof())
    {
        std::getline(expectedOutstream, expected);        
        std::getline(outstream, actual);
        
        REQUIRE(expected == actual);
    }
}

TEST_CASE("","")
{
    test(R"(12
    add 911 police
    add 76213 Mom
    add 17239 Bob
    find 76213
    find 910
    find 911
    del 910
    del 911
    find 911
    find 76213
    add 76213 daddy
    find 76213)",R"(Mom
not found
police
not found
Mom
daddy
)");
    test(R"(8
        find 3839442
        add 123456 me
        add 0 granny
        find 0
        find 123456
        del 0
        del 0
        find 0)", R"(not found
granny
me
not found
)");
}

#else

int main() {
    run(std::cin, std::cout);
    return 0;
}

#endif