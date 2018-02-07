#include <Windows.h>

#pragma comment(lib,"ntdll.lib")
#pragma comment(lib,"kernel32.lib")

//memory

enum class MemoryBlockType
{
	Pointer,
	OwnerItem,
	OwnerArray
};
class MemoryBlock
{
public:
	template<typename T>
	Memory(MemoryBlockType type, size_t size, const T * at) : Type(type), Size(size), Value((void*)at)
	{
	}

	~Memory()
	{
		switch (Type)
		{
		case MemoryBlockType::OwnerItem:
		{
			delete Pointer;
			break;
		}
		case MemoryBlockType::OwnerItem:
		{
			delete[] Pointer;
			break;
		}
		default:
			break;
		}
	}

	template <typename T>
	T* as() const
	{
		return (T*)Pointer;
	}
private:
	MemoryBlockType Type;
	size_t Size;
	void * Pointer;
};

template <typename T>
MemoryBlock make_memory_block(MemoryBlockType type, size_t size, const T * at)
{
	return { type, size,at };
}

void memcopy(void * source, size_t size, void * destination)
{
	char * csource = (char*)source;
	char * cdestination = (char*)destination;
	for (size_t i = 0; i < size; ++i) {
		cdestination[i] = csource[i];
	}
}

class Allocator
{
public:
	Allocator(HANDLE handle) : Handle(handle)
	{
	}

	MemoryBlock allocate(size_t size)
	{
		return {MemoryBlockType:: size, HeapAlloc(Handle, 0, size) };
	}

	void deallocate(void * pointer)
	{
		HeapFree(Handle, 0, pointer);
	}
private:
	HANDLE Handle;
};

Allocator * Global_Allocator;

Allocator get_std_allocator()
{
	return { GetProcessHeap() };
}

void * operator new[](unsigned int size)
{
	return Global_Allocator->allocate(size);
}

void operator delete[](void * pointer)
{
	Global_Allocator->deallocate(pointer);
}

//computation
template<typename T>
class error_code
{
public:
	error_code(T code) :Value(code) {}
private:
	T Value;
};

class error_tag {};
error_tag error;

class ok_tag {};
ok_tag ok;


template<typename TValue, typename TError = error_code<int> >
struct value_data
{
	value_data() {}
	value_data(char type, TValue value) : Type(type), Value(value) {}
	value_data(char type, TError error) : Type(type), Error(error) {}
	char Type;
	union
	{
		TValue Value;
		TError Error;
	};
};

template<typename TValue, typename TError = error_code<int> >
class value
{
public:
	value(ok_tag, TValue value) : Data(0, value) {}
	value(error_tag, TError error) : Data(1, error) {}
	template <typename T2> value(const value<T2, TError>& v)
	{
		Data.Type = v.Data.Type;
		switch (Data.Type)
		{
			case 0: {Data.Value = Data.Value; break; }
			case 1: {Data.Error = Data.Error; break; }
		}
	}
	void operator = (TValue value)
	{
		Data.Type = 0;
		Data.Value = value;
	}

private:
	value_data<TValue,TError> Data;	
	template <typename,typename> friend class value;
};

//string
size_t get_newline_size() { return 2; }
const char * get_newline_c_str() { return "\r\n"; }


class String
{
public:
	String(const MemoryBlock&& block) : Block(block)
	{
	}

	String(const char * l, size_t lsize, const char * r, size_t rsize)
		: Block(Global_Allocator->)
	{
		Text = new const char[lsize + rsize + 1];
		memcopy((void*)l, lsize, (void*)Text);
		memcopy((void*)r, rsize, (void*)(Text + lsize));
		((char*)Text)[lsize + rsize] = 0;
		Size = lsize + rsize;
	}

	~String()
	{
	}

	const char * c_str() const
	{
		return Block.as<const char*>();
	}
	size_t size() const
	{
		return Size;
	}
private:
	StringType Type;
	MemoryBlock Block;
};

#pragma warning(disable:4455) 
String operator "" str(const char * t, size_t size)
{
	return { make_memory_block(size,t) };
}
String operator "" line(const char * t, size_t size)
{
	return { t,size, get_newline_c_str(), get_newline_size() };
}
#pragma warning(default:4505) 


//math
value<char> operator "" aschar(unsigned long long value)
{
	if (value > 256) return { error, 0 };
	return { ok, (char)value};
}

template<size_t SIZE>
class Integer
{
};

template<>
class Integer<1>
{
public:
	Integer() :Value(0) {}
	Integer(char value) : Value(value) {}

	template<typename T>
	Integer& operator = (const T& value) noexcept
	{
		Value = value;
		return *this;
	}
private:
	char Value;
};

using int1 = value<Integer<1>, error_code<int>>;

//IO
class Stream
{
public:
	Stream(HANDLE handle) : Handle(handle)
	{
	}

	void Write(char ch)
	{
		WriteConsole(Handle, &ch, 1, nullptr, nullptr);
	}

	void Write(int i)
	{
		const char * strint = "TODO";
		WriteConsole(Handle, strint, 4, nullptr, nullptr);
	}

	void Write(const String& text)
	{
		WriteConsole(Handle, text.c_str(), text.size(), nullptr, nullptr);
	}
private:
	HANDLE Handle;
};

Stream get_std_output()
{
	return Stream{ GetStdHandle(STD_OUTPUT_HANDLE) };
}

Stream& operator << (Stream& out, char ch)
{
	out.Write(ch);
	return out;
}

Stream& operator << (Stream& out, const String& text)
{
	out.Write( text );
	return out;
}

//value io extensions
template <typename TValue, typename TError>
Stream& operator << (Stream& out, const value<TValue, TError>& v)
{
	value_data<TValue, TError>& data = *(value_data<TValue, TError>*)(&v);
	switch (data.Type)
	{
	default:
		break;
	}
	out << data.Type;
	return out << data.Value;
}

//math io extensions
template <int SIZE>
Stream& operator << (Stream& out, const Integer<SIZE>& i)
{
	char& data = *(char*)(&i);
	return out << (int)data;
}

class asd
{
public:
	void await_ready()
	{
	}

	void await_resume()
	{
	}

	void await_suspend()
	{
	}
};

int main(int argc, char** argv)
{
	auto allocator = get_std_allocator();
	Global_Allocator = &allocator;

	auto out = get_std_output();
	//TODO: out << newline without copying this string.
	out << "Hello"str << "World"line;

	int1 counter = 257aschar;
	out << counter;

	co_await asd{};

	ExitProcess(0);
	return 0;
}