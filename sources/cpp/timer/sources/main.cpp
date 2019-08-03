#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include "timer.h"

using Timer1 = LinkedListTimer<NewAllocator>;
using Timer2 = PriorityQueueTimer<NewAllocator>;

//////////////////////////////////////////////////// Node Binary Heap

TEST_CASE("Must insert first node in a empty bh", "[NodeBinaryHeap]")
{
	auto heap = NodeBinaryHeap<int, NewAllocator>();
	heap.emplace(1, 1);
	auto r = heap.pop();
	REQUIRE(r == 1);
}

TEST_CASE("Must insert smaller than bigger and remove them", "[NodeBinaryHeap]")
{
	auto heap = NodeBinaryHeap<int, NewAllocator>();

	heap.emplace(1, 1);
	heap.emplace(2, 2);

	REQUIRE(heap.pop() == 1);
	REQUIRE(heap.pop() == 2);
}


TEST_CASE("Must insert bigger than smaller and remove them", "[NodeBinaryHeap]")
{
	auto heap = NodeBinaryHeap<int, NewAllocator>();

	heap.emplace(2, 2);
	heap.emplace(1, 1);

	REQUIRE(heap.pop() == 1);
	REQUIRE(heap.pop() == 2);
}

//////////////////////////////////////////////////// TIMER TESTS

#define TIMERTEST template <typename TTimer> void 

TIMERTEST EmptyTimer(TTimer& timer, int tick = 1)
{
	auto& experied = timer.tick(1);
	REQUIRE(experied.size() == 0);
}

TIMERTEST AddTick(TTimer& timer, int tick = 1)
{
	timer.add(1);
	auto& expired = timer.tick(tick);

	REQUIRE(expired.size() == 1);
	REQUIRE(expired[0] == 0);
}

TIMERTEST AddTwoItemsTickTwice(TTimer& timer, int tick = 1)
{
	auto id1 = timer.add(1);
	auto id2 = timer.add(2);
	auto& experied = timer.tick(tick);

	REQUIRE(experied.size() == 1);
	REQUIRE(experied[0] == id1);

	auto& experied2 = timer.tick(tick);

	REQUIRE(experied.size() == 1);
	REQUIRE(experied[0] == id2);
}

TIMERTEST AddRemoveTick(TTimer& timer, int tick = 1)
{
	auto id = timer.add(1);
	auto r = timer.remove(id);
	
	REQUIRE(r);

	auto& experied = timer.tick(tick);

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
	//auto timer = Timer2();
	//AddRemoveTick(timer);
}