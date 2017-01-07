#include <algorithm>
#include <string>
#include <random>
#include <chrono>
#include <iostream>

namespace Karatsuba
{
	namespace detail
	{
		void padTo(std::string &str, const size_t num, const char paddingChar = ' ')
		{
			if (num > str.size())
				str.insert(0, num - str.size(), paddingChar);
		}
	}

	char * sum_char(int lsize, char * l, int rsize, char * r)
	{
		auto li = lsize - 1;
		auto ri = rsize - 1;

		auto maxlength = std::max(lsize, rsize);
		auto result = new char[maxlength + 2];

		result[maxlength + 1] = 0;

		int carriage = 0;
		for (int i = maxlength; i > 0; i--)
		{
			int lc = 0;
			if (li >= 0)
			{
				lc = (int)l[li] - 48;
				li--;
			}

			int rc = 0;
			if (ri >= 0)
			{
				rc = (int)r[ri] - 48;
				ri--;
			}

			auto re = lc + rc + carriage;

			if (re >= 10)
			{
				re = re - 10;
				carriage = 1;
			}
			else
			{
				carriage = 0;
			}

			result[i] = (char)(re + 48);
		}

		if (carriage == 0)
		{
			return (result + 1);
		}
		else
		{
			result[0] = '1';
			return result;
		}
	}

	std::string sum(std::string l, std::string r)
	{
		int li = l.length() - 1;
		int ri = r.length() - 1;

		auto maxlength = std::max(l.length(), r.length());
		auto result = std::string(maxlength + 1, '0');

		int carriage = 0;
		for (int i = maxlength; i > 0; i--)
		{
			int lc = 0;
			if (li >= 0)
			{
				lc = (int)l[li] - 48;
				li--;
			}

			int rc = 0;
			if (ri >= 0)
			{
				rc = (int)r[ri] - 48;
				ri--;
			}

			auto re = lc + rc + carriage;

			if (re >= 10)
			{
				re = re - 10;
				carriage = 1;
			}
			else
			{
				carriage = 0;
			}

			result[i] = (char)(re + 48);
		}

		if (carriage == 0)
		{
			return result.substr(1, result.length() - 1);
		}
		else
		{
			result[0] = '1';
			return result;
		}
	}

	std::string multiply(std::string l, std::string r)
	{
		if (l.length() == 1)
		{
			return std::to_string(std::atoi(l.c_str()) * std::atoi(r.c_str()));
		}
		else
		{
			int ll = l.length();

			if (l.length() > r.length())
			{
				detail::padTo(r, l.length(), '0');
			}
			else if (l.length() < r.length())
			{
				detail::padTo(l, r.length(), '0');
			}

			if (l.length() % 2 != 0)
			{
				detail::padTo(l, l.length() + (l.length() % 2), '0');
				detail::padTo(r, r.length() + (r.length() % 2), '0');
			}

			auto a = l.substr(0, l.length() / 2);
			auto b = l.substr(l.length() / 2, l.length() / 2);

			auto c = r.substr(0, l.length() / 2);
			auto d = r.substr(l.length() / 2, l.length() / 2);

			auto ac = multiply(a, c);
			auto ad = multiply(a, d);
			auto bc = multiply(b, c);
			auto bd = multiply(b, d);

			auto adbc = sum(ad, bc);

			auto p1 = ac + std::string(ll, '0');
			auto p2 = adbc + std::string(ll / 2, '0');
			//auto p1 = (BigInteger.Pow(10, ll) * BigInteger.Parse(ac)).ToString();
			//auto p2 = (BigInteger.Pow(10, ll / 2) * BigInteger.Parse(adbc)).ToString();

			auto result = sum(sum(bd, p2), p1);

			//var result = ((BigInteger.Parse(bd) + BigInteger.Parse(p2)) + BigInteger.Parse(p1)).ToString();

			//Console.WriteLine($"{l} * {r} = {result} = {p1} + {p2} + {bd}");

			return result;
		}
	}
}

template<typename TimeT = std::chrono::milliseconds>
struct measure
{
	template<typename F, typename ...Args>
	static typename TimeT::rep execution(F&& func, Args&&... args)
	{
		auto start = std::chrono::steady_clock::now();

		std::forward<decltype(func)>(func)(std::forward<Args>(args)...);

		auto duration = std::chrono::duration_cast<TimeT>
			(std::chrono::steady_clock::now() - start);

		return duration.count();
	}
};

int main(int argc, char **argv)
{
	/*auto result = Karatsuba::sum_char(1, "1", 1, "2");
	auto result2 = Karatsuba::sum("1", "2");
	result2 = Karatsuba::sum("6", "6");

	auto time = measure<std::chrono::milliseconds>::execution([&result2]()
	{
		std::random_device random;
		for (auto i = 0; i < 100000; i++)
		{
			auto l = std::to_string(random());
			auto r = std::to_string(random());

			result2 = Karatsuba::sum(l, r);
		}
	});

	std::cout << time;*/

	auto result = Karatsuba::multiply("10", "200");
	std::cout << result;
}