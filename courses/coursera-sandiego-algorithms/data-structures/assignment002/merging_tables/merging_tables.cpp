#include <cstdio>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <memory>

using std::endl;
using std::max;
using std::vector;

struct DisjointSetsElement {
	int size, parent, rank;
	
	DisjointSetsElement(int size = 0, int parent = -1, int rank = 0):
	    size(size), parent(parent), rank(rank) {}
};

std::ostream& operator << (std::ostream& out, const DisjointSetsElement& x)
{
	return out << "{" << x.size << "," << x.parent << "," << x.rank << "}";
}

template <template <typename,typename> typename C, typename T, typename A = std::allocator<T>>
std::ostream& operator << (std::ostream & out, const C<T,A>& v)
{
	out << "[";
	for(auto x : v)
	{
		out << x << ",";		
	}
	return out << "]";
}

struct DisjointSets {
	int size;
	int max_table_size;
	vector <DisjointSetsElement> sets;

	DisjointSets(int size): size(size), max_table_size(0), sets(size) {
		for (int i = 0; i < size; i++)
			sets[i].parent = i;
	}

	int getParent(int table) {
		auto originalTable = table;
		auto x = sets[table];
		while(x.parent != table)
		{
			table = x.parent;
			x = sets[table];
		}

		sets[originalTable].parent = table;
		return table;
	}

	void merge(int destination, int source) {
		// std::cout << sets << std::endl;
		// std::cout << "merge " << destination << " " << source << std::endl;

		int realDestination = getParent(destination);
		int realSource = getParent(source);
		if (realDestination != realSource) {
			auto& d = sets[realDestination];
			auto& s = sets[realSource];
			if(d.rank >= s.rank)
			{
				d.size += s.size;
				s.parent = realDestination;
				if(d.rank == s.rank) ++d.rank;

				if(d.size > max_table_size) max_table_size = d.size;
			}
			else
			{
				s.size += d.size;
				d.parent = realSource;

				if(s.size > max_table_size) max_table_size = s.size;
			}		
			
			// merge two components
			// use union by rank heuristic
			// update max_table_size
		}		
	}
};

void run (std::istream& in, std::ostream &out)
{
	int n, m;
	in >> n >> m;

	DisjointSets tables(n);
	for (auto &table : tables.sets) {
		in >> table.size;
		tables.max_table_size = max(tables.max_table_size, table.size);
	}

	for (int i = 0; i < m; i++) {
		int destination, source;
		in >> destination >> source;
                --destination;
                --source;
		
		
		tables.merge(destination, source);
	        out << tables.max_table_size << endl;
	}
}


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string &inputString, const std::string& expectedOutputString)
{
	auto expectedOut = std::stringstream{expectedOutputString};

	auto in = std::stringstream{inputString};	
	auto out = std::stringstream{};
	run(in, out);
	out.seekg(0);

	int result, expected;
	while(!out.eof())
	{
		out >> result;
		expectedOut >> expected;

		REQUIRE(expected == result);
	}
}

TEST_CASE("","")
{
	test(R"(5 5
	1 1 1 1 1
	3 5
	2 4
	1 4
	5 4
	5 3)", "2 2 3 5 5");
	test(R"(6 4
		10 0 5 0 3 3
		6 6
		6 5
		5 4
		4 3)", "10 10 10 11");
}

#else

int main() {
	run(std::cin, std::cout);
	return 0;
}

#endif