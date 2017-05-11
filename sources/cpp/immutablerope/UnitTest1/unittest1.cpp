#include "stdafx.h"
#include "CppUnitTest.h"
#include <functional>
#include <iostream>
#include <vector>
#include <algorithm>
#include <memory>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

class Rope
{
public:
	template<size_t N>
	Rope(const char(&buffer)[N]) : Left(nullptr), Right(nullptr)
	{
		auto str = std::string(buffer, N);
		F = [str]() {return str; };
	}

	Rope(const char * buffer) : Left(nullptr), Right(nullptr)
	{
		auto str = std::string(buffer);
		F = [str]() {return str; };
	}

	Rope(std::function<std::string()> f) : F(f), Left(nullptr), Right(nullptr)
	{
	}

	Rope(Rope * l, Rope * r) : Left(l), Right(r)
	{

	}

	template<size_t N>
	Rope andThen(const char(&buffer)[N])
	{
		return Rope(new Rope(*this), new Rope(buffer));

	}
	Rope andThen(const char * buffer)
	{
		return Rope(new Rope(*this), new Rope(buffer));
	}

	Rope andThen(std::function<std::string()> f)
	{
		return Rope(new Rope(*this), new Rope(f));
	}

	std::string eval() const
	{
		auto temp = std::string();
		eval_recursive(temp);
		return temp;
	}
private:
	const Rope* Left;
	const Rope* Right;

	std::function<std::string()> F;

	void eval_recursive(std::string &str) const
	{
		if (Left == nullptr) {
			eval_insert(str);
		}
		else {
			Left->eval_recursive(str);
			Right->eval_recursive(str);
		}
	}

	void eval_insert(std::string &str) const
	{
		auto value = F();

		std::stringstream ss;
		ss << "Cost: " << value.size() - 1 << std::endl;
		Logger::WriteMessage(ss.str().c_str());

		str = str.append(value.c_str());
	}
};

Rope operator"" _asrope(const char* temp, size_t)
{
	return Rope(temp);
}

Rope operator & (Rope &l, const char *r)
{
	return l.andThen(r);
}

Rope operator & (Rope &l, std::function<std::string()> r)
{
	return l.andThen(r);
}

Rope operator & (Rope &l, std::string& r)
{
	return l.andThen([&r]() {return r; });
}

Rope operator & (Rope &l, int &r)
{
	return l.andThen([&r]() {return std::to_string(r); });
}

namespace UnitTest1
{
	TEST_CLASS(UnitTest1)
	{
	public:
		TEST_METHOD(MustReturnStringContent)
		{
			auto html = "SOMESTRING"_asrope;
			auto result = html.eval();

			Assert::AreEqual("SOMESTRING", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}

		TEST_METHOD(MustReturnConcatenatedStringContent)
		{
			auto html = "SOMESTRING"_asrope & "2";
			auto result = html.eval();

			Assert::AreEqual("SOMESTRING2", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}

		TEST_METHOD(MustReturnConcatenated2StringContent)
		{
			auto html = "SOMESTRING"_asrope & "2" & "3";
			auto result = html.eval();

			Assert::AreEqual("SOMESTRING23", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}

		TEST_METHOD(MustReturnConcatenated3StringContent)
		{
			std::string username{ "username1" };
			auto html = "SOMESTRING "_asrope & [&username]() {return username; };

			auto result = html.eval();
			Assert::AreEqual("SOMESTRING username1", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());

			username.append("!");
			result = html.eval();
			Assert::AreEqual("SOMESTRING username1!", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}

		TEST_METHOD(MustReturnConcatenated4StringContent)
		{
			std::string username{ "username1" };
			auto html = "SOMESTRING "_asrope & username;

			auto result = html.eval();
			Assert::AreEqual("SOMESTRING username1", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());

			username.append("!");
			result = html.eval();
			Assert::AreEqual("SOMESTRING username1!", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}

		TEST_METHOD(MustReturnConcatenated5StringContent)
		{
			auto integer = 5;
			auto html = "SOMESTRING "_asrope & integer;

			auto result = html.eval();
			Assert::AreEqual("SOMESTRING 5", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());

			integer = 7;
			result = html.eval();
			Assert::AreEqual("SOMESTRING 7", result.c_str(), L"Simple String Node eval must return string content", LINE_INFO());
		}
	};
}