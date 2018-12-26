// searches.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <tuple>
#include <queue>
#include <set>
#include <optional>
#include <deque>
#include "json.hpp"

struct StateSpace;

struct Action
{
	int Target;
};

struct State
{
	int ID;
	std::vector<Action> actions;
};

struct Node
{
	StateSpace* problem;
	Node* parent;
	State* state;
	int action;
};

struct Path
{
	const Node* node;

	std::deque<const Node*> getPath()
	{
		std::deque<const Node*> path;
		auto q = node;
		while (q != nullptr)
		{
			path.push_front(q);
			q = q->parent;
		}
		return path;
	}
};

struct Result
{
	bool ok;
	Path solution;
};

struct StateSpace
{
	std::tuple<bool, Node*> getInitial()
	{
		if (initials.empty()) return { false, {} };
		return { true, new Node{this, nullptr, &states[initials[0]], 0} };
	}

	template<typename F>
	std::optional<Result> forEach(Node* node, F&& f)
	{
		for (auto&& x : node->state->actions)
		{
			std::optional<Result> r = f(new Node{ this, node, &states[x.Target], 0 });
			if (r) return r;
		}
		return {};
	}

	bool isGoal(State* s)
	{
		auto r = std::find_if(std::begin(goal), std::end(goal), [&](auto item) {
			return s->ID == states[item].ID;
		});
		return r != std::end(goal);
	}

	std::vector<State> states;
	std::vector<int> initials;
	std::vector<int> goal;
};





Result BreadthFirstSearch(StateSpace& space)
{
	auto[has, node] = space.getInitial();
	if (!has) return {};
	if (space.isGoal(node->state)) return { true, { { node } } };

	auto cost = 0;
	auto frontier = std::queue<Node*>{ };
	frontier.push(node);
	auto explored = std::set<int>{};

	while (true)
	{
		if (frontier.empty()) {
			return {};
		}
		node = frontier.front();
		frontier.pop();
		explored.insert(node->state->ID);

		auto r = space.forEach(node, [&](auto* newNode) -> std::optional<Result> {
			auto alreadyExplored = explored.find(newNode->state->ID) != std::end(explored);
			if (!alreadyExplored) {
				if (space.isGoal(newNode->state)) return { { true, { { newNode } } } };
				frontier.push(newNode);
			}
			return {};
		});
		if (r) return r.value();
	}

	return { };
}

bool TestSearch(std::istream& in)
{
	auto space = StateSpace{};

	nlohmann::json j;
	in >> j;
	for (auto&& state : j["states"])
	{
		auto A = State{ state["ID"] };
		for (auto&& action : state["actions"])
		{
			A.actions.push_back({ action });
		}
		space.states.push_back(A);

		if (state.count("initial") && state["initial"])
			space.initials.push_back(space.states.size() - 1);
		if (state.count("goal") && state["goal"])
			space.goal.push_back(space.states.size() - 1);
	}
	
	for (auto&&x : space.states)
	{
		for (auto&& action : x.actions)
		{
			auto r = std::find_if(std::begin(space.states), std::end(space.states), [&](auto&& item) {
				return item.ID == action.Target;
			});
			action.Target = r - std::begin(space.states);
		}
	}

	if (space.initials.size() == 0 && space.states.size() > 0)
		space.initials.push_back(0);
	if (space.goal.size() == 0 && space.states.size() > 0)
		space.goal.push_back(0);

	auto r = BreadthFirstSearch(space);
	std::stringstream ss;
	ss << (r.ok ? "true" : "false");

	for(auto&& x : r.solution.getPath())
	{
		ss << " " << x->state->ID;
	}
	std::string outexpected = j["expected"];

	auto ok = ss.str() == outexpected;

	std::cout << (ok ? "true" : "false");

	return ok;
}

bool TestSearch(const std::string& in)
{
	auto sin = std::stringstream{ in };
	return TestSearch(sin);
}

int main()
{
	/*return TestSearch(R"({
	"states": [
		{"ID": 1, "initial": true, "actions": [2]},
		{"ID": 2, "actions": [3]},
		{"ID": 3, "actions": [1, 4]},
		{"ID": 4, "goal": true}
	],
	"expected": "true 1 2 3 4"
})");*/
	return TestSearch(std::cin);
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
