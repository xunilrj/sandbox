#include <random>
#include <algorithm>
#include <numeric>
#include <iterator>
#include <iostream>
#include <chrono>
#include <string>

//T must
//have t.size()
//have random access by []
template <typename T>
struct  RandomIterator : public std::iterator<std::forward_iterator_tag, typename T::value_type>
{
    RandomIterator() : Data(nullptr)
	{
	}

	template <typename G>
	RandomIterator(const T &source, G& g) : Data(&source)
	{
		Order = std::vector<int>(source.size());
		std::iota(begin(Order), end(Order), 0);
		std::shuffle(begin(Order), end(Order), g);
		OrderIterator = begin(Order);
		OrderIteratorEnd = end(Order);
	}

	const typename T::value_type& operator* () const noexcept
	{
		return (*Data)[*OrderIterator];
	}

	const typename T::value_type* operator->() const noexcept
	{
		return &((*Data)[*OrderIterator]);
	}

	RandomIterator<T>& operator++() noexcept
	{
		++OrderIterator;
		return *this;
	}

	RandomIterator<T> operator++(int) noexcept
	{
		RandomIterator<T> tmp(*this);
		operator++();
		return tmp;
	}

	int operator== (const RandomIterator<T>& other) const noexcept
	{
		if (Data == nullptr && other.Data == nullptr)
		{
			return 1;
		}
		else if ((OrderIterator == OrderIteratorEnd) && (other.Data == nullptr))
		{
			return 1;
		}
		return 0;
	}

	int operator!= (const RandomIterator<T>& other) const noexcept
	{
		return !(*this == other);
	}
private:
	const T *Data;
	std::vector<int> Order;
	std::vector<int>::iterator OrderIterator;
	std::vector<int>::iterator OrderIteratorEnd;
};

template <typename T, typename G>
RandomIterator<T> random_begin(const T& v, G& g) noexcept
{
	return RandomIterator<T>(v, g);
}

template <typename T>
RandomIterator<T> random_end(const T& v) noexcept
{
	return RandomIterator<T>();
}

class Timer
{
public:
	Timer() : beg_(clock_::now())
	{
	}

	~Timer()
	{
		auto t = elapsed();
		std::cout << t << std::endl;
	}

	void reset() { beg_ = clock_::now(); }
	double elapsed() const {
		return std::chrono::duration_cast<second_>
			(clock_::now() - beg_).count();
	}

private:
	typedef std::chrono::high_resolution_clock clock_;
	typedef std::chrono::duration<double, std::ratio<1> > second_;
	std::chrono::time_point<clock_> beg_;
};

void run(int size)
{
	std::vector<int> arrTest(size);
	std::iota(begin(arrTest), end(arrTest), 0);
	
	auto seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
	std::mt19937 g((unsigned long)seed);

	{
		Timer t;

		for (int i = 0; i < 10; i++) {
			auto start = random_begin(arrTest, g);
			auto end = random_end(arrTest);

			std::for_each(start, end, [](auto x) {
				//std::cout << " " << x;
			});			
		}

		std::cout << size << ":";
	}
}

int main()
{
	for (int i = 0; i < 10; i++)
	{
		run((int)std::pow(10,i));
	}
	
	getchar();

	return 0;
}