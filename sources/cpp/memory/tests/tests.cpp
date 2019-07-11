#include "../source/allocators.h"
using namespace ma;

#include "catch.hpp"

template <typename T>
void allocateSizeNOK(T& alloc, size_t size)
{
	auto blk = alloc.allocate(size);
	REQUIRE(blk == Block::Null);
}

template <typename T>
auto allocateSizeMustOK(T& alloc, size_t size)
{
	auto blk = alloc.allocate(size);
	REQUIRE(blk != Block::Null);
	REQUIRE(blk.Size >= size);

	return blk;
}

template <typename T>
void ownsOK(T& alloc, const Block& blk)
{
	REQUIRE(alloc.owns(blk));
}

template <typename T>
void ownsNOK(T& alloc, const Block& blk)
{
	REQUIRE(!alloc.owns(blk));
}

template <typename T>
void deallocateNOK(T& alloc, const Block& blk)
{
	REQUIRE(!alloc.deallocate(blk));
}

template <typename T>
void deallocateOK(T& alloc, const Block& blk)
{
	auto r = alloc.deallocate(blk);
	REQUIRE(r);
}

TEST_CASE("Null Allocator Tests", "[NullAllocator]")
{
	auto allocator = NullAllocator();
	allocateSizeNOK(allocator, 0);
	allocateSizeNOK(allocator, 1);

	ownsNOK(allocator, Block::Null);

	deallocateNOK(allocator, Block::Null);
}

TEST_CASE("Stack Allocator Tests", "[StackAllocator]") 
{
	auto allocator = StackAllocator<2>();

	REQUIRE(allocator.isEmpty());

	allocateSizeNOK(allocator, 0);
	auto blk1 = allocateSizeMustOK(allocator, 1);
	auto blk2 = allocateSizeMustOK(allocator, 1);

	REQUIRE(allocator.isFull());

	allocateSizeNOK(allocator, 1);

	ownsNOK(allocator, Block::Null);
	ownsOK(allocator, blk1);
	ownsOK(allocator, blk2);

	deallocateNOK(allocator, blk1);
	deallocateOK(allocator, blk2);
	deallocateOK(allocator, blk1);

	REQUIRE(allocator.isEmpty());
}

TEST_CASE("FreeList Allocator Tests", "[FreeListAllocator]") 
{
	auto stack = StackAllocator<45>{};
	auto allocator = stack << freeListAllocator<4, 8, 2>();

	allocateSizeNOK(allocator, 0);

	// Allocate does not increase the list
	//												Apparent Alloc	Real Alloc
	auto blk1 = allocateSizeMustOK(allocator, 1); //32				43
	REQUIRE(allocator.qtd() == 0);
	
	auto blk2 = allocateSizeMustOK(allocator, 4); //31				35
	REQUIRE(allocator.qtd() == 0);
	
	auto blk3 = allocateSizeMustOK(allocator, 8); //27				27
	REQUIRE(allocator.qtd() == 0);
	
	auto blk4 = allocateSizeMustOK(allocator, 9); //19				18
	REQUIRE(allocator.qtd() == 0);

	auto blk5 = allocateSizeMustOK(allocator, 1); //11				17
	REQUIRE(allocator.qtd() == 0);

	auto blk6 = allocateSizeMustOK(allocator, 1); //10				16
	REQUIRE(allocator.qtd() == 0);

	ownsNOK(allocator, Block::Null);
	ownsOK(allocator, blk1);
	ownsOK(allocator, blk2);
	ownsOK(allocator, blk3);
	ownsOK(allocator, blk4);
	ownsOK(allocator, blk5);
	ownsOK(allocator, blk6);

	auto blk7 = allocateSizeMustOK(allocator, 4); //9				8
	auto blk8 = allocateSizeMustOK(allocator, 4); //1				1
	auto blk9 = allocateSizeMustOK(allocator, 1); //0				1

	//Deallocation does increase the list
	//but we do not care for this block (size 1)
	deallocateOK(allocator, blk9);
	REQUIRE(allocator.qtd() == 0);

	//we care about this block (8 bytes)
	deallocateOK(allocator, blk3);
	REQUIRE(allocator.qtd() == 1);

	//we also care about this block (4 bytes) new root
	deallocateOK(allocator, blk2);
	REQUIRE(allocator.qtd() == 2);

	//we care about this block but we are already full
	//ignore it
	deallocateOK(allocator, blk8);
	REQUIRE(allocator.qtd() == 2);

	//now the allocator will use its internal list 
	//instead of calling the parent allocator.
	//the Root node will point to the allocation of 4 bytes 
	//but we will ask for 8 bytes.
	//The allocator should have actually asked for 8 ealier
	blk2 = allocateSizeMustOK(allocator, 8);
	REQUIRE(allocator.qtd() == 1);
}

TEST_CASE("SegregatorAllocator Tests", "[SegregatorAllocator]")

{
	auto allocator = SegregatorAllocator<5, 
		StackAllocator<10>,
		StackAllocator<20>>();

	allocateSizeNOK(allocator, 0);

	auto blk1 = allocateSizeMustOK(allocator, 1);
	REQUIRE(allocator.smaller().qtd() == 1);
	REQUIRE(allocator.larger().qtd() == 0);

	auto blk2 = allocateSizeMustOK(allocator, 5);
	REQUIRE(allocator.smaller().qtd() == 1);
	REQUIRE(allocator.larger().qtd() == 1);

	ownsNOK(allocator, Block::Null);
	ownsOK(allocator, blk1);
	ownsOK(allocator, blk2);

	deallocateOK(allocator, blk1);
	REQUIRE(allocator.smaller().qtd() == 0);
	REQUIRE(allocator.larger().qtd() == 1);

	deallocateOK(allocator, blk2);
	REQUIRE(allocator.smaller().qtd() == 0);
	REQUIRE(allocator.larger().qtd() == 0);
}


TEST_CASE("BitmapAllocator Tests", "[BitmapAllocator]")
{
	auto allocator = BitmapAllocator<
		StackAllocator<64 * 1024>, 1024>();

	allocateSizeNOK(allocator, 0);
	
	REQUIRE(allocator.flags() == 0);

	int f = 2;
	for (int i = 0; i < 32; ++i)
	{
		auto blk1 = allocateSizeMustOK(allocator, 1);
		REQUIRE(allocator.flags() == (f-1));
		f *= 2;
	}

	REQUIRE(allocator.isFull());
	REQUIRE(allocator.deallocateAll());

	auto blk1 = allocateSizeMustOK(allocator, 1);
	auto blk2 = allocateSizeMustOK(allocator, 1);
	auto blk3 = allocateSizeMustOK(allocator, 1);
	
	ownsNOK(allocator, Block::Null);
	ownsOK(allocator, blk1);
	ownsOK(allocator, blk2);
	ownsOK(allocator, blk3);

	deallocateOK(allocator, blk2);
	REQUIRE(allocator.flags() == (4|1));

	deallocateOK(allocator, blk3);
	REQUIRE(allocator.flags() == 1);

	deallocateOK(allocator, blk1);
	REQUIRE(allocator.flags() == 0);
	REQUIRE(allocator.isEmpty());
}


TEST_CASE("StaticBufferAllocator Tests", "[StaticBufferAllocator]")
{
	size_t size = 1024;
	auto buffer = new char[size]; //we gonna leak here!
	StaticBufferAllocator::Buffer = { buffer, size };
	auto allocator = StaticBufferAllocator();

	allocateSizeNOK(allocator, 0);
	auto blk1 = allocateSizeMustOK(allocator, 1);
	REQUIRE(blk1.Size == size);
	REQUIRE(allocator.isFull());

	allocateSizeNOK(allocator, 1);
	ownsOK(allocator, blk1);

	deallocateOK(allocator, blk1);
	REQUIRE(allocator.isEmpty());
}

TEST_CASE("RingBufferAllocator Tests", "[RingBufferAllocator]")
{
	auto stack = StackAllocator<1024 + 24>{};
	auto allocator = stack << ringBufferAllocator<1024 + 24>();

	auto b1 = allocateSizeMustOK(allocator, 10);
	auto b2 = allocateSizeMustOK(allocator, 1014);
	allocateSizeNOK(allocator, 10);

	ownsOK(allocator, b1);
	ownsOK(allocator, b2);

	deallocateOK(allocator, b1);
	deallocateOK(allocator, b2);
}

#include "../source/memory.h"
Memory mem;

struct Test
{
	int a;
	float b;
};

TEST_CASE("WindowsTests", "[HeapTest]")
{
	auto h1 = mem.GetProcessHeap();

	auto reserved = mem.reserve(1);
	auto mem1 = reserved.commit();
	auto freeList =  mem1.allocator() << freeListAllocator<1, 100, 1000>();
	auto t = new (freeList) Test{ 5, 7.3f };

	REQUIRE(t->a == 5);
	REQUIRE(t->b == 7.3f);
	
	auto block2 = freeList.allocate(50);	
}