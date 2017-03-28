
#include <string>
#include <iterator>
#include <functional>
#include <cassert>
#include <memory>

template <typename T>
struct function_traits
	: public function_traits<decltype(&T::operator())>
{};

template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType(ClassType::*)(Args...) const>
{
	enum { arity = sizeof...(Args) };
	typedef ReturnType result_type;
	template <size_t i>
	struct arg
	{
		typedef typename std::tuple_element<i, std::tuple<Args...>>::type type;
	};
};

template<typename Derived, typename Base, typename Del>
std::unique_ptr<Derived, Del> down_cast(std::unique_ptr<Base, Del>&& p)
{
	auto d = static_cast<Derived *>(p.release());
	return std::unique_ptr<Derived, Del>(d, std::move(p.get_deleter()));
}

template <typename TValue>
struct ParserResult
{
	bool Success;
	TValue Value;

	ParserResult(TValue value) : Value(value), Success(true)
	{
	}

	ParserResult() : Success(false)
	{
	}
};

template <typename TValue>
struct SuccessParserResult : ParserResult<TValue>
{
	SuccessParserResult(TValue value) : ParserResult(value)
	{
	}
};

template <typename TValue>
struct FailureParserResult : ParserResult<TValue>
{
	FailureParserResult()
	{
	}
};

template <typename TValue>
std::unique_ptr<ParserResult<TValue>> success(TValue value)
{
	return std::make_unique<SuccessParserResult<TValue>>(value);
}

template <typename TValue>
std::unique_ptr<ParserResult<TValue>> failure()
{
	return std::make_unique<FailureParserResult<TValue>>();
}

template<typename TObject, typename TValue>
class Parser
{
public:
	typedef typename TObject::iterator TIterator;

	Parser(std::function<std::unique_ptr<ParserResult<TValue>>(TIterator&)> pf) : ParserFunction(pf)
	{
	}

	std::unique_ptr<ParserResult<TValue>> Parse(TIterator& iterator)
	{
		auto r = ParserFunction(iterator);
		++iterator;
		return r;
	}
private:
	std::function<std::unique_ptr<ParserResult<TValue>>(TIterator&)> ParserFunction;
};


namespace builders
{
	template<typename T>
	struct parsers
	{
		Parser<typename T, char>* pchar(char c)
		{
			return new Parser<typename T, char>([c](T::iterator& iterator) {
				auto currentchar = *iterator;
				if (currentchar == c) return success(currentchar);
				else return failure<char>();
			});
		}

		template<typename TValueL, typename TValueR>
		static Parser<typename T, std::tuple<TValueL, TValueR>>* andThen(Parser<typename T, TValueL>* l, Parser<typename T, TValueR>* r)
		{
			return new Parser<typename T, std::tuple<TValueL, TValueR>>([l, r](typename T::iterator obj) {
				auto resultl = l->Parse(obj);
				if (resultl->Success) {
					auto resultr = r->Parse(obj);
					if (resultr->Success) {
						return success(std::make_tuple(resultl->Value, resultr->Value));
					}
				}
				return failure<std::tuple<TValueL, TValueR>>();
			});
		}

		template<typename F,
			typename TValue = typename function_traits<F>::result_type,
			typename TValueL = typename function_traits<F>::arg<0>::type>
			static Parser<typename T, TValue>* map(Parser<typename T, TValueL> * l, F f)
		{
			return _map<TValue, TValueL>(l, f);
		}

		template<typename F,
			typename TValue = typename function_traits<F>::result_type,
			typename TValueL = typename function_traits<F>::arg<0>::type,
			typename TValueR = typename function_traits<F>::arg<1>::type>
			static Parser<typename T, TValue>* map(Parser<typename T, std::tuple<TValueL, TValueR>> * l, F f)
		{
			return _map<TValue, TValueL, TValueR>(l, f);
		}

		template<typename TValue>
		Parser<typename T, TValue>* ret(TValue value)
		{
			return new Parser<typename T, TValue([&value](typename T obj) {
				return success(value);
			});
		}

		template<typename F,
			typename TValue = typename function_traits<F>::result_type,
			typename TValueL = typename function_traits<F>::arg<0>::type>
			std::function<Parser<typename T, TValue>*(Parser<typename T, TValueL>*)> lift1(F f)
		{
			return [this, f](Parser<typename T, TValueL>* l) {
				return this->map(l, f);
			};
		}

		template<typename F,
			typename TValue = typename function_traits<F>::result_type,
			typename TValueL = typename function_traits<F>::arg<0>::type,
			typename TValueR = typename function_traits<F>::arg<1>::type>
			std::function<Parser<typename T, TValue>*(Parser<typename T, TValueL>*, Parser<typename T, TValueR>*)> lift2(F f)
		{
			return [this, f](Parser<typename T, TValueL>* l, Parser<typename T, TValueR>* r) {
				auto p = andThen(l, r);
				return this->map(p, f);
			};
		}

	private:
		template<typename TValue, typename TValueL>
		static Parser<typename T, TValue>* _map(Parser<typename T, TValueL> * l, std::function<TValue(TValueL)> f)
		{
			return new Parser<typename T, TValue>([l, f](typename T::iterator obj) {
				auto r = l->Parse(obj);
				if (r->Success) {
					auto value = f(r->Value);
					return success(value);
				}
				return failure<TValue>();
			});
		}

		template<typename TValue, typename TValueL, typename TValueR>
		static Parser<typename T, TValue>* _map(Parser<typename T, std::tuple<TValueL, TValueR>> * l, std::function<TValue(TValueL, TValueR)> f)
		{
			return new Parser<typename T, TValue>([l, f](typename T::iterator obj) {
				auto r = l->Parse(obj);
				if (r->Success) {
					auto valuel = std::get<0>(r->Value);
					auto valuer = std::get<1>(r->Value);
					auto value = f(valuel, valuer);
					return success(value);
				}
				return failure<TValue>();
			});
		}
	};
}

template<typename T, typename TValueL, typename TValueR>
Parser<typename T, std::tuple<TValueL, TValueR>>* operator & (Parser<typename T, TValueL>& l, Parser<typename T, TValueR>& r)
{
	return builders::parsers<T>::andThen(&l, &r);
}

template<typename T, typename F,
	typename TValue = typename function_traits<F>::result_type,
	typename TValueL = typename function_traits<F>::arg<0>::type>
	Parser<typename T, TValue>* operator >> (Parser<typename T, TValueL>& l, F f)
{
	return builders::parsers<T>::map(&l, f);
}

void main()
{
	auto text = std::string("11");
	auto i = text.begin();

	auto stringparser = builders::parsers<std::string>();
	auto atoi = [](char c) {
		return (c - '0') % 48;
	};
	auto sum = [](int l, int r) {return l + r; };

	//PARSER COMPOSITION WITH OPERATORS
	auto * p1 = stringparser.pchar('1');
	auto * p11 = *(*(*p1 >> atoi) & *(*p1 >> atoi)) >> sum;

	auto result = p11->Parse(i);
	assert(result->Value == 2);

	//PARSER COMPOSITION WITH LIFTEDFUNCTIONS
	auto pAtoi = stringparser.lift1(atoi);
	auto pSum = stringparser.lift2(sum);
	p11 = pSum(pAtoi(p1), pAtoi(p1));

	i = text.begin();
	result = p11->Parse(i);
	assert(result->Value == 2);
}