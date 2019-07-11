#pragma once

#include <Windows.h>
#include "allocators.h"

class Heap
{
	HANDLE Handle;
	bool Destroy;
public:
	Heap(HANDLE h, bool destroy) : Handle{ h }, Destroy{ destroy }
	{
	}

	~Heap()
	{
		if (Destroy)
		{
			auto r = HeapDestroy(Handle); //TODO test
		}
	}
};

class Committed;

class ComittedAllocator
{
	bool IsFull;
	Committed* Memory;
public:
	ComittedAllocator(Committed* mem) : Memory{ mem }, IsFull(false)
	{
	}

	bool isFull() const { return IsFull; }
	bool isEmpty() const { return !IsFull; }

	Block allocate(size_t s);
	bool deallocate(Block blk);
	bool owns(Block blk) const;
};

class Committed
{
	uint8_t * Start;
	size_t Size;
	ComittedAllocator Allocator;

	friend class ComittedAllocator;
public:
	Committed(uint8_t* start, size_t size) : 
		Start{ start }, 
		Size{ size }, 
		Allocator { this }
	{
	}

	ComittedAllocator& allocator() { return Allocator; }
};

Block ComittedAllocator::allocate(size_t s)
{
	if (s == 0) return Block::Null;
	else if (!IsFull)
	{
		IsFull = true;
		return { Memory->Start, Memory->Size };
	}
	else return Block::Null;
}

bool ComittedAllocator::deallocate(Block blk)
{
	if (blk == Block::Null) return false;
	if (blk.Pointer == Memory->Start && blk.Size == Memory->Size)
	{
		IsFull = false;
		return true;
	}
	else return false;
}

bool ComittedAllocator::owns(Block blk) const
{
	if (blk == Block::Null) return false;
	if (blk.Pointer == Memory->Start && blk.Size == Memory->Size)
	{
		return true;
	}
	else return false;
}

class Reserved
{
	uint8_t * Start;
	uint8_t * Current;
	bool Destroy;
public:
	Reserved(uint8_t * start, bool destroy) : 
		Start{ start }, 
		Current{ start },
		Destroy{ destroy }
	{
	}

	~Reserved()
	{
		if(Destroy)
			VirtualFree(Start, 0, MEM_RELEASE);
	}


	Committed commit()
	{
		SYSTEM_INFO sSysInfo;
		GetSystemInfo(&sSysInfo);
		auto pageSize = sSysInfo.dwPageSize;

		auto start = (uint8_t*) VirtualAlloc(
			Current,
			pageSize,
			MEM_COMMIT,
			PAGE_READWRITE);
		Current += pageSize;
		return { start, pageSize };
	}
};

class Memory
{
public:
	Heap GetProcessHeap() const { return { ::GetProcessHeap(), false }; }

	Reserved reserve(unsigned int pages)
	{
		SYSTEM_INFO sSysInfo;
		GetSystemInfo(&sSysInfo);
		auto pageSize = sSysInfo.dwPageSize;

		auto start = (uint8_t*) VirtualAlloc(
			NULL,
			pages*pageSize,
			MEM_RESERVE,
			PAGE_NOACCESS);

		return { start, true };
	}
};