#include "stdafx.h"
#include "CppUnitTest.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

template<typename GETINITIALSTATE,
	typename INITFRONTIER,
	typename ADDTOFRONTIER,
	typename ISEMPTY,
	typename SUCCESS,
	typename FAILURE,
	typename POPFROMFRONTIER,
	typename ISGOAL,
	typename EXPANDFRONTIER>
	auto treeSearch() -> decltype(SUCCESS())
{
	auto initialState = GETINITIALSTATE();
	auto frontier = INITFRONTIER();
	ADDTOFRONTIER(frontier, initialState);

	while (true)
	{
		if (ISEMPTY(frontier)) {
			return FAILURE();
		}
		auto current = POPFROMFRONTIER(frontier);
		auto isGoal = ISGOAL(current);
		if (isGoal) {
			return SUCCESS(current);
		}

		auto newFrontier = EXPANDFRONTIER(current);
		ADDTOFRONTIER(frontier, newFrontier);
	}
}

namespace iaalgos
{
	TEST_CLASS(UnitTest1)
	{
	public:

		TEST_METHOD(TestMethod1)
		{
			// TODO: Your test code here
		}

	};
}