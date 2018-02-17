#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <deque>

int const Letters = 4;
int const NA = -1;

struct Node
{
	int next[Letters];
	int startPosition;

	Node() :startPosition(NA)
	{
		std::fill(next, next + Letters, NA);
	}

	bool isLeaf() const
	{
		return (next[0] == NA && next[1] == NA && next[2] == NA && next[3] == NA);
	}
};

int letterToIndex(char letter)
{
	switch (letter)
	{
	case 'A': return 0; break;
	case 'C': return 1; break;
	case 'G': return 2; break;
	case 'T': return 3; break;
	default: assert(false); return -1;
	}
}

void build_suffix_trie(std::vector<Node>& t, const std::string& pattern) {
	auto sstart = 0;
	auto send = pattern.size();
	while (sstart < send)
	{
		auto index = 0;
		for (auto i = sstart; i < send; ++i)
		{
			auto c = pattern[i];
			auto cint = letterToIndex(c);
			auto ci = t[index].next[cint];
			if (ci == NA) {
				t.push_back(Node());
				index = t[index].next[cint] = t.size() - 1;
			}
			else {
				index = ci;
			}
		}
		t[index].startPosition = sstart;
		sstart++;
	}
}

std::vector <int> solve(const std::string& text, int n, const std::vector <std::string>& patterns)
{
	std::vector<Node> root;
	root.emplace_back();
	build_suffix_trie(root, text);

	std::vector<int> result;

	for (auto& str : patterns)
	{
		auto node_index = 0;
		for (auto i = 0; i < str.size(); ++i)
		{
			auto cint = letterToIndex(str[i]);
			auto ci = root[node_index].next[cint];
			if (ci == NA) {
				node_index = 0;
				break;
			}
			node_index = ci;
		}

		if (node_index == 0) continue;

		auto q = std::deque<int>();
		q.push_back(node_index);
		while (q.size() > 0)
		{
			node_index = q.back();
			q.pop_back();
			if (root[node_index].startPosition != NA)
			{
				result.push_back(root[node_index].startPosition);
			}
			if (root[node_index].next[0] != NA) q.push_back(root[node_index].next[0]);
			if (root[node_index].next[1] != NA) q.push_back(root[node_index].next[1]);
			if (root[node_index].next[2] != NA) q.push_back(root[node_index].next[2]);
			if (root[node_index].next[3] != NA) q.push_back(root[node_index].next[3]);
		}
	}

	std::sort(result.begin(), result.end());
	return result;
}

void run(std::istream& in, std::ostream& out)
{
	std::string t;
	in >> t;

	int n;
	in >> n;

	std::vector<std::string> patterns(n);
	for (int i = 0; i < n; i++)
	{
		in >> patterns[i];
	}

	std::vector <int> ans;
	ans = solve(t, n, patterns);

	for (int i = 0; i < (int)ans.size(); i++)
	{
		out << ans[i];
		if (i + 1 < (int)ans.size())
		{
			out << " ";
		}
		else
		{
			out << std::endl;
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
	test("AAA 1 AA", R"(0 1
)");
	test("AA 1 T", R"()");
	test("A 1 AA", R"()");
}

#else

int main(void)
{
	run(std::cin, std::cout);
	return 0;
}

#endif