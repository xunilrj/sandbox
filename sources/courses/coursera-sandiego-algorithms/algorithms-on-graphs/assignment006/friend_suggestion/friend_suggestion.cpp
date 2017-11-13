#include <cstdio>
#include <cassert>
#include <iostream>
#include <vector>
#include <queue>
#include <limits>
#include <utility>
#include <functional>

using namespace std;

// External vector of size 2 - for forward and backward search.
// Internal 2-dimensional vector is vector of adjacency lists for each node.
typedef vector<vector<vector<int>>> Adj;

// Distances can grow out of int type
typedef long long Len;

// Vector of two priority queues - for forward and backward searches.
// Each priority queue stores the closest unprocessed node in its head.
typedef vector<priority_queue<pair<Len, int>, vector<pair<Len, int>>, std::greater< std::pair<Len, int> > > > Queue;

const Len LEN_INFINITY = numeric_limits<Len>::max() / 4;

class Bidijkstra {
	// Number of nodes
	int n_;
	// Graph adj_[0] and cost_[0] correspond to the initial graph,
	// adj_[1] and cost_[1] correspond to the reversed graph.
	// Graphs are stored as vectors of adjacency lists corresponding
	// to nodes.
	// Adjacency list itself is stored in adj_, and the corresponding
	// edge costs are stored in cost_.
	Adj adj_;
	Adj cost_;
	// distance_[0] stores distances for the forward search,
	// and distance_[1] stores distances for the backward search.
	vector<vector<Len>> distance_;
	// Stores all the nodes visited either by forward or backward search.
	vector<int> workset_;
	// Stores a flag for each node which is True iff the node was visited
	// either by forward or backward search.
	vector<int> visited_;

public:
	Bidijkstra(int n, Adj adj, Adj cost)
		: n_(n), adj_(adj), cost_(cost), distance_(2, vector<Len>(n, LEN_INFINITY)), visited_(n, -1)
	{
		workset_.reserve(n);
	}

	// Initialize the data structures before new query,
	// clear the changes made by the previous query.
	void clear() {
		for (int i = 0; i < workset_.size(); ++i) {
			int v = workset_[i];
			distance_[0][v] = distance_[1][v] = LEN_INFINITY;
			visited_[v] = -1;
		}
		workset_.clear();
	}

	// Processes visit of either forward or backward search 
	// (determined by value of side), to node v trying to
	// relax the current distance by dist.
	bool visit(Queue& queue, int side, int u, Len dist) {
		if (visited_[u] == side) return false;
		if (visited_[u] >= 0 && visited_[u] != side) return true;

		workset_.push_back(u);
		visited_[u] = side;

		auto& uedges = adj_[side][u];
		auto& costs = cost_[side][u];
		auto& d = distance_[side];
		auto& q = queue[side];
		for (int vi = 0; vi < uedges.size(); ++vi)
		{
			auto v = uedges[vi];
			auto wuv = costs[vi];
			if (d[v] > d[u] + wuv)
			{
				workset_.push_back(v);
				d[v] = d[u] + wuv;
				q.emplace(d[v], v);
			}
		}

		return false;
	}

	// Returns the distance from s to t in the graph.
	Len query(int s, int t) {
		if (s == t) return 0;

		clear();
		Queue q(2);

		distance_[0][s] = 0;
		distance_[1][t] = 0;

		auto& q0 = q[0];
		auto& q1 = q[1];

		q0.emplace(0, s);
		q1.emplace(0, t);

		while (q0.size() > 0 || q1.size() > 0)
		{
			if (q0.size() > 0)
			{
				auto u = q0.top();
				q0.pop();
				auto found = visit(q, 0, u.second, u.first);
				if (found) return (u.second + distance_[1][u.first]);
			}
			if (q1.size() > 0)
			{
				auto u = q1.top();
				q1.pop();
				auto found = visit(q, 1, u.second, u.first);
				if (found) return (u.first + distance_[0][u.second]);
			}
		}

		return -1;
	}
};

void run(std::istream& in, std::ostream& out)
{
	int n, m;
	in >> n >> m;
	// scanf("%d%d", &n, &m);
	Adj adj(2, vector<vector<int>>(n));
	Adj cost(2, vector<vector<int>>(n));
	for (int i = 0; i < m; ++i) {
		int u, v, c;
		// scanf("%d%d%d", &u, &v, &c);
		in >> u >> v >> c;
		adj[0][u - 1].push_back(v - 1);
		cost[0][u - 1].push_back(c);
		adj[1][v - 1].push_back(u - 1);
		cost[1][v - 1].push_back(c);
	}

	Bidijkstra bidij(n, adj, cost);

	int t;
	in >> t;
	// scanf("%d", &t);
	for (int i = 0; i < t; ++i) {
		int u, v;
		in >> u >> v;
		// scanf("%d%d", &u, &v);
		out << bidij.query(u - 1, v - 1) << std::endl;
		// printf("%lld\n", bidij.query(u-1, v-1));
	}
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& strin, const std::string& expectedOut)
{
	auto in = std::stringstream{ strin };
	auto actualOut = std::stringstream();

	run(in, actualOut);

	REQUIRE(expectedOut == actualOut.str());
}

TEST_CASE("", "")
{
	test(R"(2 1
        1 2 1
        4
        1 1
        2 2
        1 2
        2 1)", R"(0
0
1
-1
)");
	test(R"(4 4
1 2 1
4 1 2
2 3 2
1 3 5
1
1 3)", R"(3
)");
	test(R"(5 20
1 2 667
1 3 677
1 4 700
1 5 622
2 1 118
2 3 325
2 4 784
2 5 11
3 1 585
3 2 956
3 4 551
3 5 559
4 1 503
4 2 722
4 3 331
4 5 366
5 1 880
5 2 883
5 3 461
5 4 228
10
1 1
1 2
1 3
1 4
1 5
2 1
2 2
2 3
2 4
2 5)", R"(0
667
677
700
622
118
0
325
239
11
)");
}

#else

int main() {
	run(std::cin, std::cout);
	return 0;
}

#endif