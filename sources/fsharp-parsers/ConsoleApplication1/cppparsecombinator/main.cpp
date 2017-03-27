

namespace parsers
{
	Parser<char, char> pchar(char c)
	{
		return Parser<char, char>();
	}
}

template <typename TValue>
struct ParserResult
{
	TValue Value;

	TypeNamePlaceholder(TValue value) : Value(value)
	{
	}
};

template<typename TStream, typename TValue>
class Parser
{
public:
	ParserResult<TValue> Parse(TStream stream)
	{
		return ParserResult(stream);
	}
};


void main()
{
	auto p1 = parsers::pchar('1');

	auto result = p1.Parse('1');
}