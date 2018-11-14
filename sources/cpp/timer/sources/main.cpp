#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include "timer.h"

using Timer1 = LinkedListTimer<NewAllocator>;
using Timer2 = PriorityQueueTimer<NewAllocator>;

//////////////////////////////////////////////////// Binary Heap

TEST_CASE("Must insert first node in a empty bh", "[BinaryHeap]")
{
	auto heap = BinaryHeap<int, NewAllocator>();
	heap.emplace(1, 1);
	auto r = heap.pop();
	REQUIRE(r == 1);
}

TEST_CASE("Must insert smaller than bigger and remove them", "[BinaryHeap]")
{
	auto heap = BinaryHeap<int, NewAllocator>();

	heap.emplace(1, 1);
	heap.emplace(2, 2);

	auto r = heap.pop();
	REQUIRE(r == 1);

	r = heap.pop();
	REQUIRE(r == 2);
}

//////////////////////////////////////////////////// TIMER TESTS

#define TIMERTEST template <typename TTimer> void 

TIMERTEST EmptyTimer(TTimer& timer)
{
	auto& experied = timer.tick(1);
	REQUIRE(experied.size() == 0);
}

TIMERTEST AddTick(TTimer& timer)
{
	timer.add(1);
	auto& expired = timer.tick(1);

	REQUIRE(expired.size() == 1);
	REQUIRE(expired[0] == 0);
}

TIMERTEST AddTwoItemsTickTwice(TTimer& timer)
{
	timer.add(1);
	timer.add(2);
	auto& experied = timer.tick(1);

	REQUIRE(experied.size() == 1);
	REQUIRE(experied[0] == 0);

	auto& experied2 = timer.tick(2);

	REQUIRE(experied.size() == 1);
	REQUIRE(experied[0] == 1);
}

TIMERTEST AddRemoveTick(TTimer& timer)
{
	auto id = timer.add(1);
	auto r = timer.remove(id);
	
	REQUIRE(r);

	auto& experied = timer.tick(1);

	REQUIRE(experied.size() == 0);
}

//////////////////////////////////////////////////// LINKED LIST TIMER TESTS

TEST_CASE("Timer must tick even if empty", "[LINKEDLISTTIMER]") {
	auto timer = Timer1();
	EmptyTimer(timer);
}

TEST_CASE("Timer must expire when now is extacly equal required timer", "[LINKEDLISTTIMER]") {
	auto timer = Timer1();
	AddTick(timer);
}

TEST_CASE("Timer must expire two timers", "[LINKEDLISTTIMER]") {
	auto timer = Timer1();
	AddTwoItemsTickTwice(timer);
}

TEST_CASE("Timer must remove", "[LINKEDLISTTIMER]") {
	auto timer = Timer1();
	AddRemoveTick(timer);
}

//////////////////////////////////////////////////// PRIORITY TIMER TESTS

TEST_CASE("PRIORITYTIMER must tick even if empty", "[PRIORITYTIMER]") {
	auto timer = Timer2();
	EmptyTimer(timer);
}

TEST_CASE("PRIORITYTIMER must expire when now is extacly equal required timer", "[PRIORITYTIMER]") {
	auto timer = Timer2();
	AddTick(timer);
}

TEST_CASE("PRIORITYTIMER must expire two timers", "[PRIORITYTIMER]") {
	auto timer = Timer2();
	AddTwoItemsTickTwice(timer);
}

TEST_CASE("PRIORITYTIMER must remove", "[PRIORITYTIMER]") {
	auto timer = Timer2();
	AddRemoveTick(timer);
}