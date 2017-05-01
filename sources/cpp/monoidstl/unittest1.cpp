#include "stdafx.h"
#include "CppUnitTest.h"

#include <algorithm>
#include <iostream>
#include <functional>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace monoidstd
{
	template <typename TArg1>
	class GeneralFunctor
	{
	public:
		std::function<void(TArg1)> function;
		GeneralFunctor(std::function<void(TArg1)> f) : function(f)
		{
		}

		template<typename T1>
		void operator () (T1* begin, T1* end)
		{
			std::for_each(begin, end, function);
		}

		template <typename F2>
		GeneralFunctor<TArg1> AndThen(GeneralFunctor<TArg1> f)
		{
			auto f1 = this->function;
			auto f2 = f.function;
			return GeneralFunctor<TArg1>([f1,f2](int x) {
				f1(x);
				f2(x);
			});
		}
	};

	template <typename TArg1>
	GeneralFunctor<TArg1> for_each(std::function<void(TArg1)> f)
	{
		return GeneralFunctor<TArg1>(f);
	}
}

namespace monoidstl
{
	TEST_CLASS(UnitTest1)
	{
	public:

		TEST_METHOD(TestMethod1)
		{
			auto numbers = { 1,2,3,4,5 };
			std::for_each(std::begin(numbers), std::end(numbers), [](int x) {
				std::cout << x << std::endl;
			});
			
			auto result = monoidstd::for_each<int>([](int x) {
				std::cout << x << std::endl;
			}).AndThen(monoidstd::for_each([](int x) {
				std::cout << x << std::endl;
			}));
			result(std::begin(numbers), std::end(numbers));
		}

	};
}