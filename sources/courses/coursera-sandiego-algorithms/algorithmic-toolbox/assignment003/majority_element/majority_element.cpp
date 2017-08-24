#include <algorithm>
#include <iostream>
#include <vector>
#include <tuple>
#include <map>

using std::vector;



namespace majority
{
    namespace
    {
        std::ostream & tab(int size)
        {
            return (std::cout << std::string(size * 3, ' '));
        }

        struct major_result
        {
            int major;
            std::map<int,int> cardinality;

            static major_result merge(major_result l, major_result r)
            {
                auto result = major_result();
                for(auto i : l.cardinality)
                {
                    result.cardinality.emplace(i.first,i.second);
                }
                for(auto i : r.cardinality)
                {
                    result.cardinality[i.first] += i.second;
                }

                auto lmajorcard = l.cardinality[l.major] + r.cardinality[l.major];
                auto rmajorcard = l.cardinality[r.major] + r.cardinality[r.major];

                if(lmajorcard > rmajorcard) result.major = l.major;
                if(lmajorcard < rmajorcard) result.major = r.major;
                if(lmajorcard == rmajorcard) result.major = l.major;

                return result;
            }
        };

        template<typename S, typename T>
        std::ostream & operator<<(std::ostream & o, const std::map<S,T> m)
        {
            o << "[";

            for(auto i : m)
            {
                o  << "(" << i.first << ":" << i.second << "),";
            }

            o << "]";
            return o;
        }

        std::ostream & operator<<(std::ostream & o, const major_result & m)
        {
            o << "{" << m.major << ";" << m.cardinality << "}";
            return o;
        }

        template<typename K, typename V>
        major_result make_result(K key, V value)
        {
            auto m = std::map<K,V>();
            m.emplace(key,value);
            return {key, m};
        }

        major_result get_major(vector<int> &a, int left, int right, int depth) {
            // tab(depth) << left << " " << right << "[" << a[left] << "," << a[right] << "]";

            if (left == right)
            {
                // tab(depth) << "return (" << a[left] << ",1)" << std::endl; 
                return make_result(a[left],1);
            }

            auto pivot = left + ((right - left)/2);

            // tab(depth) << " -> [" << left << "," << pivot << "][" << pivot + 1 << "," << right << "]" << std::endl;
            
            auto ml = get_major(a, left, pivot, depth + 1);
            auto mr = get_major(a, pivot+1, right, depth + 1);

            // tab(depth) << "merging [" << left << "," << pivot << "] = " << ml
                // << ", [" << pivot+1 << "," << right << "] = " << mr << std::endl;

            if(ml.major != ml.major)
            {
                // tab(depth) << " diff" << std::endl;
                return major_result();
            }
            else
            {
                auto result = major_result::merge(ml, mr);
                // tab(depth) << "merge result: " << result << std::endl;
                return result;
            }    
        }
    }

    int get_majority_element(vector<int> &a, int left, int right) { 
        auto result = get_major(a,left, right, 0);
        auto cardinality = result.cardinality[result.major];
        auto majority_needed = ((right - left + 1) / 2) + 1;
        // std::cout << "Result: maj_need:" << majority_needed << " - card: " << cardinality << std::endl;  
        return (cardinality >= majority_needed)?(1):(0);
    }
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

int get_majority_element_(vector<int> a) {
  return majority::get_majority_element(a,0,a.size()-1);
}

TEST_CASE("get_majority_element_ must work", "[get_majority_element_]")
{
    REQUIRE(get_majority_element_({2, 2}) == 1);
    REQUIRE(get_majority_element_({2, 2, 3}) == 1);
    REQUIRE(get_majority_element_({2, 3, 3}) == 1);
    REQUIRE(get_majority_element_({2, 3, 9, 2, 2}) == 1);
    REQUIRE(get_majority_element_({1,2,3,4}) == 0);
    REQUIRE(get_majority_element_({512766168, 717383758, 5, 126144732,5, 573799007, 5, 5, 5, 405079772}) == 0);
}

TEST_CASE("get_majority_element_ corner cases", "[get_majority_element_]")
{
    auto votes = std::vector<int>(100000);
    REQUIRE(get_majority_element_(votes) == 1);

    REQUIRE(get_majority_element_({1000000000}) == 1);
}

#else

int main() {
  int n;
  std::cin >> n;
  vector<int> a(n);
  for (size_t i = 0; i < a.size(); ++i) {
    std::cin >> a[i];
  }
  std::cout << majority::get_majority_element(a, 0, a.size()-1) << '\n';
}

#endif