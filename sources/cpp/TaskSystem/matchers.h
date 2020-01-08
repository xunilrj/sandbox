#include <string>
#include <sstream>

#include "TaskSystem.h"
#include "../common/catch.hpp"

class FinishedMatcher : public Catch::MatcherBase<std::tuple<array<100>&, array<100>&>> {
public:
	FinishedMatcher(std::initializer_list<uint32_t> l) : list(l) {}

	bool match(const std::tuple<array<100>&, array<100>&>& r) const override {
		auto [finished, _] = r;
		auto sizeEqual = finished.size() == list.size();
		
		auto allElementsEqual = true;
		uint32_t i = 0;
		for (auto&& l : list)
		{
			if (!(finished[i] == l))
			{
				allElementsEqual = false;
				break;
			}
			++i;
		}

		return sizeEqual && allElementsEqual;
	}

	// Produces a string describing what this matcher does. It should
	// include any provided data (the begin/ end in this case) and
	// be written as if it were stating a fact (in the output it will be
	// preceded by the value under test).
	virtual std::string describe() const override {
		std::ostringstream ss;
		ss << "must finished {";
		bool first = true;
		for (auto&& x : list)
		{
			if (first) {
				ss << x;
				first = false;
			}
			else {
				ss << ", " << x;
			}
		}
		ss << "}";
		return ss.str();
	}
private:
	std::initializer_list<uint32_t> list;
};

class StartedMatcher : public Catch::MatcherBase<std::tuple<array<100>&, array<100>&>> {
public:
	StartedMatcher(std::initializer_list<uint32_t> l) : list(l) {}

	bool match(const std::tuple<array<100>&, array<100>&>& r) const override {
		auto [_, started] = r;
		auto sizeEqual = started.size() == list.size();

		auto allElementsEqual = true;
		uint32_t i = 0;
		for (auto&& l : list)
		{
			if (!(started[i] == l))
			{
				allElementsEqual = false;
				break;
			}
			++i;
		}

		return sizeEqual && allElementsEqual;
	}

	// Produces a string describing what this matcher does. It should
	// include any provided data (the begin/ end in this case) and
	// be written as if it were stating a fact (in the output it will be
	// preceded by the value under test).
	virtual std::string describe() const override {
		std::ostringstream ss;
		ss << "must started {";
		bool first = true;
		for (auto&& x : list)
		{
			if (first) {
				ss << x;
				first = false;
			}
			else {
				ss << ", " << x;
			}
		}
		ss << "}";
		return ss.str();
	}
private:
	std::initializer_list<uint32_t> list;
};

FinishedMatcher finished(std::initializer_list<uint32_t> list) { return { list }; }
StartedMatcher started(std::initializer_list<uint32_t> list) { return { list }; }