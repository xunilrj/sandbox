#include "stdafx.h"
#include "CppUnitTest.h"
#include <functional>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

class Rope
{
public:
	Rope(const char * buffer) : Buffer(buffer)
	{
	}

	Rope(const Rope *node) : Node(node)
	{
	}

	Rope append(std::function<std::string()> get) const
	{
		return Rope(this);
	}

	Rope append(const char * buffer) const
	{
		return Rope(Buffer);
	}

	const char * eval() const
	{
		if (Node == nullptr) {
			return Buffer;
		}
		else {
			return Node->eval();
		}
	}
private:
	const Rope* Node;
	const char * Buffer;
};

//Rope operator "" _r(const char16_t * temp, size_t)
//{
//	return Rope();
//}

namespace UnitTest1
{
	TEST_CLASS(UnitTest1)
	{
	public:

		TEST_METHOD(TestMethod1)
		{
			auto html = Rope("<html><body><span>Hello ").append([]() {
				return "";
			}).append("</span></body></html>");

			auto result = html.eval();

			Assert::AreEqual("", result, L"message", LINE_INFO());
		}
	};
}