#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <tuple>
#include <algorithm>
#include <numeric>
#include <set>
#include <deque>
#include <queue>

using std::vector;

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

double minimum_distance_kruskal(const vector<int>& x, const vector<int>& y)
{
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
	double tco = 0.0;
	 
	for (auto e : E) {
		if (sets.count() == 1) break;
		if (sets.connected(e.u, e.v)) continue;
		tco += e.cost;
		sets.merge(e.u, e.v);
	}

	return (double)tco;
}

const int INF = std::numeric_limits<int>::max();
const auto INFL = std::numeric_limits<long double>::max();
double minimum_distance(const vector<int>& x, const vector<int>& y) {
	std::vector<long double> dist(x.size(), INFL);
	std::vector<int> prev(x.size(), INF);
	std::vector<int> visited(x.size(), 0);

	dist[0] = 0;

	auto cost = [&](const int& l, const int&r) {
		long double dx = x[l] - x[r];
		long double dy = y[l] - y[r];
		return std::sqrt(dx*dx + dy*dy);
	};

	auto cmp = [&](const int& l, const int& r) {
		return dist[l] > dist[r];
	};
	auto H = std::priority_queue<int, std::deque<int>, decltype(cmp)>(cmp);
	H.push(0);

	long double tco = 0.0;
	auto inctco = [&](const int& v) {
		if (prev[v] == INF) return;
		tco += dist[v];
	};

	while (H.size() > 0)
	{
		auto u = H.top();
		H.pop();

		if (visited[u] == 1) continue;
		visited[u] = 1;

		inctco(u);

		for (int vi = 0; vi < x.size(); ++vi)
		{
			if (visited[vi] == 1) continue;
			auto v = vi;
			auto wuv = cost(u, v);
			if (dist[v] > wuv) {
				dist[v] = wuv;
				prev[v] = u;
				H.push(v);
			}
		}
	}

	return tco;
}

void run(std::istream& in, std::ostream& out)
{
	size_t n;
	in >> n;
	vector<int> x(n), y(n);
	for (size_t i = 0; i < n; i++) {
		in >> x[i] >> y[i];
	}
	out << std::setprecision(10) << minimum_distance_kruskal(x, y) << std::endl;
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

TEST_CASE("", "")
{
	test(R"(4
  0 0
  0 1
  1 0
  1 1)", R"(3
)");
	test(R"(5
  0 0
  0 2
  1 1
  3 0
  3 2)", R"(7.064495102
)");
}

#else

int main() {
	run(std::cin, std::cout);
	return 0;
}

#endif