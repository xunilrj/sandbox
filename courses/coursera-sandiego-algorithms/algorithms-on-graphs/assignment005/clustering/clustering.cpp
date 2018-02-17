#include <algorithm>
#include <iostream>
#include <iomanip>
#include <cassert>
#include <vector>
#include <cmath>
#include <deque>

using std::vector;
using std::pair;


class UF {
	int *id, cnt, *sz;
public:
	// Create an empty union find data structure with N isolated sets.
	UF(int N) {
		cnt = N;
		id = new int[N];
		sz = new int[N];
		for (int i = 0; i < N; i++) {
			id[i] = i;
			sz[i] = 1;
		}
	}
	~UF() {
		delete[] id;
		delete[] sz;
	}
	// Return the id of component corresponding to object p.
	int find(int p) {
		int root = p;
		while (root != id[root])
			root = id[root];
		while (p != root) {
			int newp = id[p];
			id[p] = root;
			p = newp;
		}
		return root;
	}
	// Replace sets containing x and y with their union.
	void merge(int x, int y) {
		int i = find(x);
		int j = find(y);
		if (i == j) return;

		// make smaller root point to larger one
		if (sz[i] < sz[j]) {
			id[i] = j;
			sz[j] += sz[i];
		}
		else {
			id[j] = i;
			sz[i] += sz[j];
		}
		cnt--;
	}
	// Are objects x and y in the same set?
	bool connected(int x, int y) {
		return find(x) == find(y);
	}
	// Return the number of disjoint sets.
	int count() {
		return cnt;
	}
};

struct EdgeCost { int u, v; long double cost; };

double clustering(vector<int> x, vector<int> y, int k) {
  auto cost = [&](const int& l, const int&r) {
		long double dx = x[l] - x[r];
		long double dy = y[l] - y[r];
		return std::sqrt(dx*dx + dy*dy);
	};
	auto at = [&](int xx, int yy) {
		return yy*x.size() + xx;
	};
	auto visited = std::vector<int>(x.size() * x.size(), 0);
	auto E = std::deque<EdgeCost>();

	for (int i = 0; i < x.size(); ++i)
	{
		for (int j = 0; j < x.size(); ++j)
		{
			if (i == j) continue;
			if ((visited[at(i, j)] == 1) || (visited[at(j, i)] == 1)) continue;
			visited[at(i, j)] = visited[at(j, i)] = 1;
			E.push_back({ i, j, cost(i, j)});
		}
	}

	std::sort(std::begin(E), std::end(E), [](const EdgeCost& l, const EdgeCost& r) {return l.cost < r.cost; });
	auto sets = UF(x.size());
   
	EdgeCost last;
	while (sets.count() > k)
	{
		last = E.front();
    E.pop_front();
		if (sets.connected(last.u, last.v)) continue;
		sets.merge(last.u, last.v);
	}

	while (E.size() > 0)
	{
		last = E.front();
    E.pop_front();
		if (!sets.connected(last.u, last.v)) break;
	}

	return last.cost;
} 

void run(std::istream& in, std::ostream& out)
{
  size_t n;
  int k;
  in >> n;
  vector<int> x(n), y(n);
  for (size_t i = 0; i < n; i++) {
    in >> x[i] >> y[i];
  }
  in >> k;
  out << std::setprecision(10) << clustering(x, y, k) << std::endl;
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string &instr, const std::string& expectedOut)
{
	auto in = std::stringstream{ instr };
	auto actualOut = std::stringstream();

	run(in, actualOut);

	REQUIRE(expectedOut == actualOut.str());
}

TEST_CASE("","")
{
  test(R"(12
    7 6
    4 3
    5 1
    1 7
    2 7
    5 7
    3 3
    7 8
    2 8
    4 4
    6 7
    2 6
    3)",R"(2.828427125
)");
}

#else

int main() {
 run(std::cin, std::cout);
 return 0;
}

#endif
