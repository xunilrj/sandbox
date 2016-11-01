#include <iterator>
#include "gtest/gtest.h"
#include "cpplinq/linq.hpp"
#include "boost/iterator/counting_iterator.hpp"
#include <complex>

//If we list all the natural numbers below 10 that are 
//multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//Find the sum of all the multiples of 3 or 5 below 1000.
TEST(EulerTests, Problem0001) {
 auto n = cpplinq::from(
			boost::iterators::counting_iterator<int>(0),
			boost::iterators::counting_iterator<int>(1000))
		.where([](auto x) { 
			return (x % 3 == 0) || (x % 5 == 0);
		})
		.sum();
 
 EXPECT_EQ(233168, n);
}
