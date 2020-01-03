
#include "TaskSystem.h"

#define CATCH_CONFIG_MAIN 
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

TEST_CASE("TaskSystem.Must be possible to create simple 'then' task", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_duration(16, true);
	auto t2 = tsys.make_task();
	tsys.then(t1, t2);

	REQUIRE_THAT(tsys.run( 0), started({ 1 }) && finished({    }));
	REQUIRE_THAT(tsys.run(16), started({ 2 }) && finished({1, 2}));
	REQUIRE_THAT(tsys.run(16), started({   }) && finished({    }));
}

TEST_CASE("TaskSystem.Must take into account the negative duration of previous when startinh 'then' task", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_duration(14, true);
	auto t2 = tsys.make_duration(16); 
	tsys.then(t1, t2);

	REQUIRE_THAT(tsys.run( 0), started({ 1 }) && finished({   }));
	REQUIRE_THAT(tsys.run(16), started({ 2 }) && finished({ 1 }));
	REQUIRE_THAT(tsys.run(14), started({   }) && finished({ 2 }));
	REQUIRE_THAT(tsys.run(14), started({   }) && finished({   }));
}

TEST_CASE("TaskSystem.Must be possible to create a sequence of tasks (with duration)", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_duration(16, true);
	auto t2 = tsys.make_duration(16); 
	auto t3 = tsys.make_duration(16); 
	auto t4 = tsys.make_duration(16); 

	tsys.sequence({ t1, t2, t3, t4 });

	REQUIRE_THAT(tsys.run( 0), started({ 1 }) && finished({      }));
	REQUIRE_THAT(tsys.run(16), started({ 2 }) && finished({ 1    }));
	REQUIRE_THAT(tsys.run(16), started({ 3 }) && finished({ 2    }));
	REQUIRE_THAT(tsys.run(16), started({ 4 }) && finished({ 3    }));
	REQUIRE_THAT(tsys.run(16), started({ 5 }) && finished({ 4, 5 }));
	REQUIRE_THAT(tsys.run(16), started({   }) && finished({      }));
}

TEST_CASE("TaskSystem.Must be possible to create a sequence of tasks (immediately finish)", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_task(true);
	auto t2 = tsys.make_task(); 
	auto t3 = tsys.make_task(); 
	auto t4 = tsys.make_task(); 

	tsys.sequence({ t1, t2, t3, t4 });

	REQUIRE_THAT(tsys.run( 0), started({ 1, 2, 3, 4, 5 }) && finished({ 1, 2, 3, 4, 5 }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
	//auto r = tsys.run(0); assert_finished(r, {1, 2, 3, 4, 5});
	//r = tsys.run(16);assert_finished(r, {});
}

TEST_CASE("TaskSystem.Must be possible to postpone when a task id done (enabled = true)", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_task();
	auto t2 = tsys.make_task(); 
	tsys.then(t1, t2);

	// nothing happens because t1 is disabled
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));

	tsys.enable(t1);

	REQUIRE_THAT(tsys.run(16), started({ 1, 2 }) && finished({ 1, 2 }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
}

TEST_CASE("TaskSystem.Must be possible to depend on a lower ID task", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_task();
	auto t2 = tsys.make_task(); 
	tsys.then(t2, t1);

	// nothing happens because t1 is disabled
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));

	tsys.enable(t2);

	REQUIRE_THAT(tsys.run(16), started({ 2, 1 }) && finished({ 2, 1 }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
}

TEST_CASE("TaskSystem.Must be possible to chain complex dependencies", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1_1 = tsys.make_duration(16, true);							// 1
	auto t1_2 = tsys.make_duration(16);									// 2
	auto t1_3 = tsys.make_duration(16);									// 3
	auto t1_last = tsys.sequence({ t1_1, t1_2, t1_3 });					// 4

	auto t2_1 = tsys.make_duration(16, true);							// 5
	auto t2_2 = tsys.make_duration(16);									// 6
	auto t2_3 = tsys.make_duration(16);									// 7
	auto t2_4 = tsys.make_duration(16);									// 8
	auto t2_last = tsys.sequence({ t2_1, t2_2, t2_3, t2_4 });			// 9

	auto t3_1 = tsys.make_duration(16, true);							// 10
	auto t3_2 = tsys.make_duration(16);									// 11
	auto t3_3 = tsys.make_duration(16);									// 12
	auto t3_4 = tsys.make_duration(16);									// 13
	auto t3_5 = tsys.make_duration(16);									// 14
	auto t3_last = tsys.sequence({ t3_1, t3_2, t3_3, t3_4, t3_5 });		// 15

	tsys.sequence({ t1_last, t2_last, t3_last });						// 16

	REQUIRE_THAT(tsys.run( 0), started({  1,  5, 10 }) && finished({                }));
	REQUIRE_THAT(tsys.run(16), started({  2,  6, 11 }) && finished({  1,  5, 10     }));
	REQUIRE_THAT(tsys.run(16), started({  3,  7, 12 }) && finished({  2,  6, 11     }));
	REQUIRE_THAT(tsys.run(16), started({  4,  8, 13 }) && finished({  3,  4,  7, 12 }));
	REQUIRE_THAT(tsys.run(16), started({  9, 14     }) && finished({  8,  9, 13     }));
	REQUIRE_THAT(tsys.run(16), started({ 15, 16     }) && finished({ 14, 15, 16     }));
	REQUIRE_THAT(tsys.run(16), started({ }) && finished({ }));
}

TEST_CASE("TaskSystem.When All", "[TaskSystem]")
{
	TaskSystem<100, 100> tsys;
	auto t1 = tsys.make_duration(16, true);
	auto t2 = tsys.make_duration(32, true);
	auto t3 = tsys.make_duration(48, true);

	auto& twhenall = tsys.when_all(t1, t2, t3);

	auto t5 = tsys.make_duration(16, true);
	tsys.then(twhenall, t5);

	REQUIRE_THAT(tsys.run(16), started({ 1, 2, 3 }) && finished({ 1    }));
	REQUIRE_THAT(tsys.run(16), started({         }) && finished({ 2    }));
	REQUIRE_THAT(tsys.run(16), started({ 4, 5    }) && finished({ 3, 4 }));
	REQUIRE_THAT(tsys.run(16), started({         }) && finished({ 5    }));
	REQUIRE_THAT(tsys.run(16), started({         }) && finished({      }));
}

TEST_CASE("TaskSystem.Must reuse tasks", "[TaskSystem]")
{
	TaskSystem<2, 100> tsys;
	auto t1 = tsys.make_duration(16, true);
	auto t2 = tsys.make_duration(32, true);
	auto tfailed = tsys.make_duration(32, true);

	REQUIRE_FALSE(tfailed.ok);

	tsys.run(16);

	auto t3 = tsys.make_duration(32, true);

	REQUIRE(t3.ok);
}