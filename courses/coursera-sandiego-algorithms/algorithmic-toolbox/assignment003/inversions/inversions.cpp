#include <iostream>
#include <vector>

using std::vector;

std::ostream & tab(int size)
{
	return (std::cout << std::string(size * 3, ' '));
}

void printVector(const std::vector<int> &a)
{
  std::cout << "[";
  for(auto i : a)
  {
    std::cout << i << ",";
  }
  std::cout << "]" << std::endl;
}

long long get_number_of_inversions(vector<int> &a, vector<int> &b, size_t left, size_t right, int depth) {
	// tab(depth) << "[" << left << "," << right << "]";
	long long number_of_inversions = 0;
	if (right <= left)
	{
		// std::cout << "{" << a[left] << "," << a[right] << "}" << std::endl;
		return number_of_inversions;
	}
	else
	{
		// std::cout << std::endl;
	}

	size_t ave = left + (right - left) / 2;
	
	number_of_inversions += get_number_of_inversions(a, b, left, ave, depth + 1);
	number_of_inversions += get_number_of_inversions(a, b, ave+1, right, depth + 1);

	// tab(depth) << "merging [" << left << "," << ave << "] & [" << ave+1 << "," << right << "]" << std::endl; 
	// tab(depth);
	// printVector(b);
	int target = left, il = left + 0, ir = ave + 1;
	while(il <= ave && ir <= right)
	{
		if(a[il] <= a[ir])
		{
			// tab(depth) << "l[" << a[il] << "," << a[ir] << "]";
			b[target] = a[il];
			// printVector(b);
			il++;
		}
		else
		{
			auto invscore = ir - target;
			number_of_inversions += invscore;
			// tab(depth) << "r[" << a[il] << "," << a[ir] << "]{+" << invscore << "}";
			b[target] = a[ir];
			// printVector(b);
			ir++;
		}
		target++;
	}
	while(il <= ave)
	{
		// tab(depth) << "l[" << a[il] << "]";
		b[target] = a[il];
		il++;
		target++;
		// printVector(b);
	}
	while(ir <= right)
	{
		// tab(depth) << "r[" << a[ir] << "]";
		b[target] = a[ir];
		ir++;
		target++;
		// printVector(b);
	}

	// tab(depth);
	// printVector(b);

	std::copy(b.begin() + left, b.begin() + right + 1, a.begin() + left );

	//write your code here
	return number_of_inversions;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

long long get_number_of_inversions_(vector<int> a)
{
	// std::cout << "---------------------" << std::endl;
	auto b = std::vector<int>(a.size());
	auto result = get_number_of_inversions(a, b, 0, a.size() - 1, 0);
	// printVector(b);
	return result;
}

TEST_CASE("get_number_of_inversions must work", "[get_number_of_inversions]")
{
	REQUIRE(get_number_of_inversions_({2,3,9,2,9}) == 2);

	//2,1 -> 1,2
	REQUIRE(get_number_of_inversions_({2,1}) == 1);
	//3,2,1 -> 3,1,2 -> 1,3,2 -> 1,2,3
	REQUIRE(get_number_of_inversions_({3,2,1}) == 3);
	//4,3,2,1 -> 4,3,1,2 -> 4,1,3,2 -> 1,4,3,2 -> 1,4,2,3 -> 1,2,4,3 -> 1,2,3,4
	REQUIRE(get_number_of_inversions_({4,3,2,1}) == 6);
	//5,4,3,2,1 -> 5,4,3,1,2 -> 5,4,1,3,2 -> 5,1,4,3,2 -> 1,5,4,3,2 -> 1,5,4,2,3
	//1,5,2,4,3 -> 1,2,5,4,3 -> 1,2,5,3,4 -> 1,2,3,5,4 -> 1,2,3,4,5
	REQUIRE(get_number_of_inversions_({5,4,3,2,1}) == 10);
}

TEST_CASE("get_number_of_inversions corner case", "[get_number_of_inversions]")
{
	auto numbers = std::vector<int>(100000);
	REQUIRE(get_number_of_inversions_(numbers) == 0);

	numbers = std::vector<int>(100000);
	std::iota(numbers.begin(), numbers.end(), 1);
	REQUIRE(get_number_of_inversions_(numbers) == 0);
}

#else
int main() {
  int n;
  std::cin >> n;
  vector<int> a(n);
  for (size_t i = 0; i < a.size(); i++) {
    std::cin >> a[i];
  }
  vector<int> b(a.size());
  std::cout << get_number_of_inversions(a, b, 0, a.size() - 1, 0) << '\n';
}

#endif