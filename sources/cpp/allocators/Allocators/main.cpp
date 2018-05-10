#include <functional>

#define CATCH_CONFIG_MAIN 
#include "catch.hpp"

class Failure
{
public:
	static Failure Nothing;
	static Failure Generic;
	Failure()
	{
	}

	Failure(int error) : ErrorCode(error)
	{
	}

	Failure(const Failure& f)
	{
		this->ErrorCode = f.ErrorCode;
	}

	bool operator == (const Failure& other) const noexcept
	{
		return this->ErrorCode == other.ErrorCode;
	}

	bool operator != (const Failure& other) const noexcept
	{
		return this->ErrorCode != other.ErrorCode;
	}
private:
	int ErrorCode;
};
Failure Failure::Nothing = Failure(0);
Failure Failure::Generic = Failure(1);

template <typename... T>
class Fallible {};

template <typename T>
class Fallible<T>
{
public:
	Fallible(const T& result) : Failure(Failure::Nothing), Item(result)
	{
	}

	Fallible(const Failure& f) : Failure(f)
	{
	}

	Fallible(const Fallible<T>& other)
	{
		this->Failure = other.Failure;
		this->Item = other.Item;
	}

	bool isFailed() const noexcept
	{
		return this->Failure != Failure::Nothing;
	}

	template <typename T2>
	T2 map(std::function<T2(const T&)> ok, std::function<T2(const Failure&)> failure) const noexcept
	{
		if (this->Failure == Failure::Nothing) failure(this->Failure);
		else ok(this->Item);
	}

	Fallible<T> mapFailed(std::function<Fallible<T>(const Failure&)> failure) const noexcept
	{
		if (this->Failure == Failure::Nothing) return failure(this->Failure);
		else return *this;
	}
private:
	Failure Failure;
	T Item;
};

template <>
class Fallible<>
{
public:
	static Fallible<> Ok;
	Fallible() : Failure(Failure::Nothing)
	{
	}

	Fallible(const Failure& f) : Failure(f)
	{
	}

	Fallible(const Fallible<>& other)
	{
		this->Failure = other.Failure;
	}

	template <typename T2>
	T2 map(std::function<T2()> ok, std::function<T2(const Failure&)> failure) const noexcept
	{
		if (this->Failure == Failure::Nothing) failure(this->Failure);
		else ok();
	}

	template <typename T2>
	Fallible<> mapFailure(std::function<Fallible<>(const Failure&)> failure) const noexcept
	{
		if (this->Failure == Failure::Nothing) failure(this->Failure);
		else return this;
	}
private:
	Failure Failure;
};
Fallible<> Fallible<>::Ok = Fallible<>();

using FallibleVoid = Fallible<>;
using MemoryBlockSize = size_t;

class MemoryBlock
{
public:
	MemoryBlock() :Size(0), Address(nullptr)
	{
	}

	//how to deny size = 0 and address valid
	MemoryBlock(MemoryBlockSize size, void * address) : Address(address), Size(size)
	{
	}

	void mapValid(std::function<void(const MemoryBlockSize, void *)> f) const noexcept
	{
		if (this->Address) f(this->Size, this->Address);
	}
private:
	MemoryBlockSize Size;
	void * Address;
};

class NullAllocator
{
public:
	bool owns(const MemoryBlock& block) const noexcept
	{
		return false;
	}

	Fallible<MemoryBlock> allocate(MemoryBlockSize size) const noexcept
	{
		return Failure::Generic;
	}

	FallibleVoid deallocate(const MemoryBlock& block) const noexcept
	{
		return Failure::Generic;
	}
};

template <typename Primary, typename Fallback>
class FallbackAllocator :
	private Primary,
	private Fallback
{
public:
	bool owns(const MemoryBlock& block) const noexcept
	{
		return Primary::owns(block) || Fallback::owns(block);
	}

	Fallible<MemoryBlock> allocate(MemoryBlockSize size) noexcept
	{
		return Primary::allocate(size).mapFailed([&](const auto& e) -> Fallible<MemoryBlock> {
			return Fallback::allocate(size);
		});
	}

	FallibleVoid deallocate(const MemoryBlock& block) noexcept
	{
		return Primary::deallocate(block).mapFailed([](auto& e) {
			return Fallback::deallocate(block);
		});
	}
};

#include <cstdlib>
class MallocAllocator
{
public:
	bool owns(const MemoryBlock& block) const noexcept
	{
		//No easy way to detect that I own this block
		return true;
	}

	Fallible<MemoryBlock> allocate(MemoryBlockSize size) noexcept
	{
		auto mem = malloc(size);
		if (!mem) return Failure::Generic;
		return MemoryBlock{ size, mem };
	}

	FallibleVoid deallocate(const MemoryBlock& block) noexcept
	{
		block.mapValid([](auto size, auto address) {
			free(address);
		});
		return FallibleVoid::Ok;
	}
};

template<typename T> void assertIsFailed(const Fallible<T>& f) { REQUIRE(f.isFailed()); }
template<typename T> void assertIsSuccess(const Fallible<T>& f) { REQUIRE(!f.isFailed()); }

TEST_CASE("FallbackAllocator.must.fail", "[FallbackAllocator]")
{
	FallbackAllocator<MallocAllocator, NullAllocator> a;
	assertIsFailed(a.allocate(0));
	assertIsSuccess(a.allocate(1));
}