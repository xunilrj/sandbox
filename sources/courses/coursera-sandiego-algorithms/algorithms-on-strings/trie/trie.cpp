#include <string>
#include <iostream>
#include <vector>
#include <map>

using std::map;
using std::vector;
using std::string;

typedef map<char, int> edges;
typedef vector<edges> trie;

trie build_trie(vector<string> & patterns) {
	trie t;
	t.push_back(edges());

	for (auto str : patterns)
	{
		int index = 0;
		for (auto c : str)
		{
			auto ci = t[index].find(c);
			if (ci == t[index].end()) {
				t.push_back(edges());
				index = t[index][c] = t.size() - 1;
			}
			else {
				index = ci->second;
			}
		}
	}
	return t;
}

void run(std::istream& in, std::ostream& out)
{
	size_t n;
	in >> n;
	vector<string> patterns;
	for (size_t i = 0; i < n; i++) {
		string s;
		in >> s;
		patterns.push_back(s);
	}

	trie t = build_trie(patterns);
	for (size_t i = 0; i < t.size(); ++i) {
		for (const auto & j : t[i]) {
			out << i << "->" << j.second << ":" << j.first << "\n";
		}
	}
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../catch.hpp"

void test(const std::string& instr, const std::string& expectedOut)
{
	auto in = std::stringstream{ instr };
	auto actualOut = std::stringstream();

	run(in, actualOut);

	REQUIRE(expectedOut == actualOut.str());
}

TEST_CASE("", "")
{
	test("1 ATA", R"(0->1:A
1->2:T
2->3:A
)");
	test("3 AT AG AC", R"(0->1:A
1->4:C
1->3:G
1->2:T
)");
	test("3 ATAGA ATC GAT", R"(0->1:A
1->2:T
2->3:A
3->4:G
4->5:A
2->6:C
0->7:G
7->8:A
8->9:T
)");
}

#else

int main() {
	run(std::cin, std::cout);
	return 0;
}

#endif