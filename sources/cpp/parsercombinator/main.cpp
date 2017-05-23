constexpr int factorial(int n)
{
	return n <= 1 ? 1 : (n * factorial(n - 1));
}

struct SIMPLESUCCESS {
	bool operator () () { return true; }
};
struct SIMPLEERROR {
	bool operator () () { return false; }
};

template<typename L, typename R>
class _andThen
{
public:
	_andThen(L &l, R &r) : ll(l), rr(r)
	{
	}

	L &ll;
	R &rr;

	template<typename I>
	auto operator () (I& i)->decltype(rr(i))
	{
		auto lresult = ll(i);
		if (lresult) {
			return rr(i);
		}
		else {
			return SIMPLEERROR{}();
		}
	}
};

template<typename L, typename R>
_andThen<L, R> andThen(L &ll, R & rr)
{
	return _andThen<L, R>{ll, rr};
}



class pchar
{
public:
	pchar(char c) : Expected(c)
	{
	}

	template<typename I, typename SUCCESS = SIMPLESUCCESS, typename ERROR = SIMPLEERROR>
	auto operator () (I& i) -> decltype(SUCCESS{}())
	{
		if (*i == Expected) {
			++i;
			return SUCCESS{}();
		}
		else {
			return ERROR{}();
		}
	}
private:
	char Expected;
};

#include <iterator>
#include <iostream>

int main(int argc, char**argv)
{
	auto i = std::begin("<html></html>");
	auto panglel = pchar{ '<' };
	auto ph = pchar{ 'h' };
	auto panglelh = andThen(panglel, ph);
	auto r = panglel(i);

	auto result = panglelh(i);

	std::cout << result << std::endl;

	return 0;
}