#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>



template <size_t p, size_t a>
unsigned long long polinomial_hash_family(const std::string& x, int beginv = 0, int endv = 0)
{
    if(endv == 0)
    {
        endv = x.size();
    }
    // static const size_t multiplier = 263;
    // static const size_t prime = 1000000007;
    unsigned long long hash = 0;
    for (int i = (endv - 1); i >= beginv; --i)
        hash = (hash * a + x[i]) % p;
    return hash;
}

unsigned long long phash(const std::string& str, int begin = 0, int end = 0)
{
    return polinomial_hash_family<1000000007, 263>(str, begin, end);
}

//https://stackoverflow.com/questions/20412405/rolling-hash-in-rabin-karp
unsigned long long shiftleft_phash(const std::string& str, unsigned long long currenthash, int newbegin, int newend, unsigned long long ypower = 0)
{
    unsigned long long p = 1000000007;
    unsigned long long m = 263;
    if(ypower == 0)
    {
        ypower = 1;
        for(int i = 1; i <= (newend - newbegin); ++i)
        {
            ypower = (ypower * m) % p;
        }
    }
    
    unsigned long long shifted = (currenthash * m) % p;
    unsigned long long remove = (str[newend] * ypower) % p;
    unsigned long long add  = str[newbegin];    
    return (shifted - remove + add) % p;
}

class RabinKarp
{
public:
    RabinKarp(bool uh) : useHash(uh)
    {
    }

    std::vector<int> Run(const std::string& text, const std::string& pattern) const
    {
        std::cout << "Text " << text << " Pattern " << pattern << std::endl;

        auto result = std::vector<int>();
        auto patternHash = phash(pattern);

        auto hashes = computeHashTable(text, pattern);

        auto last = (text.size() - pattern.size()) + 1;
        for(int i = 0; i < last;++i)
        {
            auto ihash = hashes[i];

            std::cout << "index " << i << " " << patternHash << " " << ihash;
            
            if((useHash)&&(ihash != patternHash))
            {
                std::cout << std::endl;
                 continue;
            }

            std::cout << " hashequal";

            if(AreEqual(text, pattern, i))
            {
                std::cout << " and equal" << std::endl;
                result.push_back(i);
            }
            else
            {
                std::cout << " but different " << std::endl;
            }
        }

        return result;
    }
private:
    bool useHash;

    std::vector<unsigned long long> computeHashTable(const std::string& text, const std::string& pattern) const
    {
        auto psize = pattern.size();
        bool firsthash = true;
        unsigned long long hash = 0;

        auto resultsize = text.size() - pattern.size() + 1;
        auto result = std::vector<unsigned long long>(resultsize);

        // unsigned long long ypower = 1;
        // for(int i = 0; i < psize; ++i)
        // {
        //     ypower = (ypower * 263) % 1000000007;
        // }
        auto ypower = 0;

        for(int i = resultsize - 1; i >= 0; --i)
        {
            if(firsthash)
            {
                hash = phash(text, i, i + psize);
                firsthash = false;
            }
            else
            {
                hash = shiftleft_phash(text, hash, i, i + psize, ypower);
            }

            result[i] = hash;
        }

        return result;
    }

    //Can be elegantly implemented as reduce(logicalAnd, zip(text,pattern, testEquality))
    bool AreEqual(const std::string& text, const std::string& pattern, int i) const
    {
        bool result = true;

        for(int j = i, jj = 0; jj < pattern.size(); ++j,++jj)
        {
            result = result && (text[j] == pattern[jj]);
        }

        return result;
    }
};


using std::string;
typedef unsigned long long ull;

struct Data {
    string pattern, text;
};

Data read_input(std::istream& in) {
    Data data;
    in >> data.pattern >> data.text;
    return data;
}

void print_occurrences(std::ostream& out, const std::vector<int>& output) {
    for (size_t i = 0; i < output.size(); ++i)
        out << output[i] << " ";
    out << "\n";
}

std::vector<int> get_occurrences(const Data& input, bool useHash) {
    // const string& s = input.pattern, t = input.text;
    // std::vector<int> ans;
    // for (size_t i = 0; i + s.size() <= t.size(); ++i)
    //     if (t.substr(i, s.size()) == s)
    //         ans.push_back(i);
    // return ans;
    const string& s = input.pattern, t = input.text;
    auto sk = RabinKarp(useHash);
    return sk.Run(t, s);
}


void run(std::istream& in, std::ostream& out, bool useHash)
{
    print_occurrences(out, get_occurrences(read_input(in), useHash));
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& inStr, const std::string& expectedOutStr)
{
    auto instream = std::stringstream{inStr};
    auto expectedOutStream = std::stringstream{expectedOutStr};

    auto actualOutStream = std::stringstream{};

    run(instream, actualOutStream, true);
    actualOutStream.seekg(0);

    std::string actual, expected;
    while(!expectedOutStream.eof())
    {
        actualOutStream >> actual;
        expectedOutStream >> expected;

        REQUIRE(expected == actual);
    }
}

TEST_CASE("","")
{
    // auto powers = std::vector<unsigned long long>(53);
    // unsigned long long ypower = 1;
    // powers[0] = 1;
    // for(int i = 1; i <= 52; ++i)
    // {
    //     ypower = (ypower * 263) % 1000000007;
    //     powers[i] = ypower;
    // }

    // // auto h1 = ('e'*powers[0] + 's'*powers[1] + 't'*powers[2] + 't'*powers[3]) % 1000000007;
    // auto h1 = phash("NoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXu");
    // auto h2 = phash("lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUX");
    // auto h3 = shiftleft_phash("lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXu", h1, 0, 52);
    
    // // std::cout << "mh1 " << mh1 << " h1 " << h1 << " " << (mh1 == h1) << std::endl;

    // auto s = (263*h1) % 1000000007;
    // auto diff = ('u'*powers[52]) % 1000000007;
    // auto mh2 = (s + 'l'*powers[0] - diff) % 1000000007;

    // std::cout << "h1 " << h1 << " and h2 " << h2 << " and mh2 " << mh2 << " and h3 " << h3 << std::endl;
    // auto dmh2 = ('t'*powers[0] + 'e'*powers[1] + 's'*powers[2] + 't'*powers[3]) % 1000000007;
    // auto dh2 = ;

    // auto oldchar = 't'*powers[3] % 1000000007;
    // auto shmh2 = ((h1 - oldchar) * 263 ) % 1000000007 + 't';

    //  std::cout << "dh2 " << dh2 << " shmh2 " << shmh2 << std::endl;

    
    // REQUIRE(h3 == h2);
    // test("a a", "0");
    // test("aba abacaba", "0 4");
    // test("Test testTesttesT", "4");
    // test("aaaaa baaaaaaa", "1 2 3");
    test("lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUX lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXua", "0");
    // test("lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUX ZtonpqnFzlpvUKZrBbRlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXxtHmTxoLuMbRYsvSpxhtrlvABBlFYmndFzHypOmJyFxjHEPlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXbDiEAvtPlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXRRNoBCUMQVOlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXRLKlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXAYPDKWtVpShhclNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXOJlUlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXglmlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXuaOibGlVrwghvNTgLfltIbEdBlgjelFjQkBeFrdEV",
    //     "19 118 178 241 296 361 417 472 ");
}

TEST_CASE("random","")
{
    
}

#else

int main() {
    run(std::cin, std::cout, true);
    return 0;
}

#endif
