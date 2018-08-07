#include <iostream>

#include "type_manipulation.h"
#include "allocators.h"
#include "coroutines.h"
using namespace ma;

struct averageArgs
{
	int l;
	int r;
	int result;
};
cocontinuation average(costate<averageArgs> &args)
{
	auto &[l, r, result] = ARGS;
	START_COROUTINE
		result = l + r;
	AWAIT(result == 4);
	result *= 2;
	END_COROUTINE
}

int main()
{
	auto alloc = StackAllocator<1024>();

	auto comgr = CoManager(&alloc);
	auto result = comgr.make(average, 1, 3);
	auto coroutine = result.coroutine;

	auto r = comgr.step(coroutine.id());
	r = comgr.step(coroutine.id());

	comgr.free(coroutine.id());

	std::cout << "result is " << result.args().result;
}
