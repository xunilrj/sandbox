#ifndef ALLOCATORS_H
#define ALLOCATORS_H

#include <Windows.h>

//using size_t = unsigned long long;
using uint8_t = unsigned char;
using uint32_t = unsigned int;
using uint64_t = unsigned long long;

template <typename T>
void* operator new(size_t size, T& allocator)
{
	auto block = allocator.allocate(size);
	return block.Pointer;
}

namespace ma
{
	// https://embeddedgurus.com/state-space/2014/09/fast-deterministic-and-portable-counting-leading-zeros/
	static inline uint32_t CLZ1(uint32_t x) {
		static uint8_t const clz_lkup[] = {
			32U, 31U, 30U, 30U, 29U, 29U, 29U, 29U,
			28U, 28U, 28U, 28U, 28U, 28U, 28U, 28U,
			27U, 27U, 27U, 27U, 27U, 27U, 27U, 27U,
			27U, 27U, 27U, 27U, 27U, 27U, 27U, 27U,
			26U, 26U, 26U, 26U, 26U, 26U, 26U, 26U,
			26U, 26U, 26U, 26U, 26U, 26U, 26U, 26U,
			26U, 26U, 26U, 26U, 26U, 26U, 26U, 26U,
			26U, 26U, 26U, 26U, 26U, 26U, 26U, 26U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			25U, 25U, 25U, 25U, 25U, 25U, 25U, 25U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U,
			24U, 24U, 24U, 24U, 24U, 24U, 24U, 24U
		};
		uint32_t n;
		if (x >= (1U << 16)) {
			if (x >= (1U << 24)) {
				n = 24U;
			}
			else {
				n = 16U;
			}
		}
		else {
			if (x >= (1U << 8)) {
				n = 8U;
			}
			else {
				n = 0U;
			}
		}
		return (uint32_t)clz_lkup[x >> n] - n;
	}

#define CLZ(x) CLZ1(x)

	struct Block
	{
		void *Pointer;
		size_t Size;

		Block(void* ptr, size_t size) :Pointer(ptr), Size(size)
		{
		}

		Block() : Pointer(nullptr), Size(0)
		{
		}

		template <typename T, typename... TArgs>
		T& emplace(TArgs... args)
		{
			return *(new (Pointer) T{ args... });
		}

		uint8_t* end() const { return (uint8_t*)Pointer + Size; }
		template<typename T> bool owns(T* ptr) const { return (uint8_t*)ptr < ((uint8_t*)Pointer + Size); }
		template<typename T> int position(T* ptr) const
		{
			auto p = (uint8_t*)ptr;
			auto end = (uint8_t*)Pointer + Size;

			if (p >= Pointer && p < end) return 0;
			if (p >= end) return 1;
			if (p <= Pointer) return -1;
			return 0; //impossible
		}

		static const Block Null;
	};
	const Block Block::Null{ nullptr, 0 };

	bool operator == (const Block& l, const Block& r) { return l.Pointer == r.Pointer; }
	bool operator != (const Block& l, const Block& r) { return l.Pointer != r.Pointer; }

	////////////////////////////////////////////////////////////////////////////////////// NULL ALLOCATOR

	class NullAllocator
	{
	public:
		static constexpr unsigned short alignment = 0;
		static constexpr size_t goodSize(size_t s) { return s; }
		Block allocate(size_t size) { return Block::Null; }
		Block allocateAll() { return Block::Null; }
		bool expand(Block&, size_t delta) { return false; }
		bool owns(Block) { return false; }
		bool deallocate(Block) { return false; }
		bool deallocateAll() { return false; };

		Block alignedAllocate(size_t, unsigned short alignment)
		{
			return Block::Null;
		}

		Block alignedReallocate(size_t, unsigned short alignment)
		{
			return Block::Null;
		}
	};
	
	////////////////////////////////////////////////////////////////////////////////////// FALLBACK ALLOCATOR

	template <typename Primary, typename Fallback>
	class FallbackAllocator : private Primary,
		private Fallback
	{
	public:
		bool owns(Block b)
		{
			return Primary::owns(b) || Fallback::owns(b);
		}

		Block allocate(size_t size)
		{
			auto block = Primary::allocate(size);
			if (block == Block::Null)
			{
				block = Fallback::allocate(size);
			}
			return block;
		}

		void deallocate(Block block)
		{
			if (Primary::owns(block))
				Primary::deallocate(block);
			else
				Fallback::deallocate(block);
		}
	};

	////////////////////////////////////////////////////////////////////////////////////// STACK ALLOCATOR

	template <size_t SIZE>
	class StackAllocator
	{
	public:
		StackAllocator() : Qtd(0)
		{
			Top = Buffer;
		}

		bool owns(Block block)
		{
			return block.Pointer >= Buffer && block.Pointer < Top;
		}

		Block allocate(size_t size)
		{
			if (size == 0) return Block::Null;
			if ((Top + size) > (Buffer + SIZE)) return Block::Null;

			++Qtd;
			auto p = Top;
			Top += size;
			return Block{ p, size };
		}

		bool deallocate(Block block)
		{
			auto ptr = (char*)block.Pointer;
			if ((ptr + block.Size) == Top)
			{
				--Qtd;
				Top -= block.Size;
				return true;
			}
			else return false;
		}

		bool isFull() const { return Top != Buffer && Top >= (Buffer + SIZE); }
		bool isEmpty() const { return Top == Buffer; }
		size_t qtd() const { return Qtd; }
	private:
		size_t Qtd;
		char Buffer[SIZE];
		char *Top;
	};

	////////////////////////////////////////////////////////////////////////////////////// FREE LIST ALLOCATOR

	template <class TFallback, size_t MIN, size_t MAX, size_t QTDMAX>
	class FreeListAllocator
	{
		struct Node
		{
			Node *Next;
		};

		TFallback& Fallback;
		Node *Root;
		size_t Qtd;

		inline bool isMySize(size_t size) {
			return size >= MIN && size <= MAX;;
		}
	public:
		FreeListAllocator(TFallback& fallback) : Fallback{ fallback }
		{

		}

		inline size_t qtd() const { return Qtd; }
		Block allocate(size_t size)
		{
			if (size == 0) return Block::Null;
			if (isMySize(size))
			{
				if (Root != nullptr)
				{
					Block b = { Root, size };
					Root = Root->Next;
					--Qtd;
					return b;
				}
				else return Fallback.allocate(MAX);
			}
			else
			{
				return Fallback.allocate(size);
			}
		}

		bool owns(Block b)
		{
			return isMySize(b.Size) || Fallback.owns(b);
		}

		bool deallocate(Block b)
		{
			if (Qtd >= QTDMAX) return Fallback.deallocate(b);
			if (!isMySize(b.Size)) return Fallback.deallocate(b);

			auto p = (Node*)b.Pointer;
			p->Next = Root;
			Root = p;
			++Qtd;
			return true;
		}
	};

	template <size_t MIN, size_t MAX, size_t QTDMAX>
	struct FreeListAllocatorBuilder
	{
		template<typename T>
		FreeListAllocator<T, MIN, MAX, QTDMAX> make(T& fallback) const { return { fallback }; }
	};
	template <class T, size_t MIN, size_t MAX, size_t QTDMAX>
	auto operator << (T& a, const FreeListAllocatorBuilder<MIN,MAX,QTDMAX> &b) { return b.make(a); }
	template <size_t MIN, size_t MAX, size_t QTDMAX>	
	auto freeListAllocator() { return FreeListAllocatorBuilder<MIN,MAX,QTDMAX>{}; }

	////////////////////////////////////////////////////////////////////////////////////// AFFIX ALLOCATOR

	//TODO
	template<typename A,
		typename Prefix,
		typename Suffix = void>
		class AffixAllocator
	{

	};

	////////////////////////////////////////////////////////////////////////////////////// STATS ALLOCATOR
	//TODO
	template<typename A,
		unsigned long flags>
		class StatsAllocator
	{

	};


	////////////////////////////////////////////////////////////////////////////////////// BITMAP ALLOCATOR
	template <typename T, size_t SIZE>
	class BitmapAllocator
	{
		// 0 - means the n-chunk is free
		//indices are in reverse order
		//                                         ... 9 8 7 6 5 4 3 2 1 0
		// 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
		// so chunk 0 is free
		// but chunk 2 is not
		uint32_t Flags;
		Block Blk;
		T Parent;
		//https://graphics.stanford.edu/~seander/bithacks.html
		//https://skalkoto.blogspot.com/2008/01/bit-operations-find-first-zero-bit.html
		inline int findFree(int v)
		{
			int r1 = ~v;
			int r2 = (~r1) + 1;
			return CLZ(r1 & r2) + 1;
		}
	public:
		int flags() const { return Flags; }
		bool isEmpty() const { return Flags == 0; }
		bool isFull() const { return Flags == (~0); }
		bool deallocateAll()
		{
			Flags = 0;
			return true;
		}
		Block allocate(size_t size)
		{
			//First Request allocate
			if (Blk == Block::Null)
			{
				Blk = Parent.allocate(SIZE);
			}
			if (Blk == Block::Null) return Block::Null;

			// Organize the block
			if (size == 0) return Block::Null;
			if (isFull()) return Block::Null;

			int ifree = 0;
			if (!isEmpty())
			{
				ifree = findFree(Flags);
			}

			Flags = Flags | (1 << (32 - ifree));
			return Block{
				(char*)Blk.Pointer + ((SIZE / 32) * ifree),
				SIZE / 32
			};
		}
		bool deallocate(Block blk)
		{
			if (blk == Block::Null) return false;

			auto ptr = (char*)blk.Pointer;
			auto offset = ptr - (char*)Blk.Pointer;
			auto ichunk = offset / 32;

			if (ichunk >= 0 && ichunk < 32)
			{
				ichunk = 32 - ichunk;
				Flags = Flags & ~(1 << ichunk);
				return true;
			}
			else
			{
				return false;
			}
		}

		bool owns(Block blk) const
		{
			if (blk == Block::Null) return false;

			auto ptr = (char*)blk.Pointer;
			auto offset = ptr - (char*)Blk.Pointer;
			auto ichunk = offset / 32;

			if (ichunk >= 0 && ichunk < 32) return true;
			else return false;
		}
	};

	////////////////////////////////////////////////////////////////////////////////////// CASCADING ALLOCATOR
	//TODO
	template <typename Creator>
	class CascadingAllocator
	{

	};

	////////////////////////////////////////////////////////////////////////////////////// SEGREGATOR ALLOCATOR
	template <size_t THRESHOLD,
		typename TSmall,
		typename TLarge>
	class SegregatorAllocator
	{
		TSmall Small;
		TLarge Large;
	public:
		const TSmall& smaller() const { return Small; }
		const TLarge& larger() const { return Large; }
		Block allocate(size_t s)
		{
			if (s == 0) return Block::Null;
			if (s < THRESHOLD) return Small.allocate(s);
			else return Large.allocate(s);
		}

		bool owns(Block b)
		{
			if (b.Size < THRESHOLD) return Small.owns(b);
			else return Large.owns(b);
		}

		bool deallocate(Block b)
		{
			if (b.Size < THRESHOLD) return Small.deallocate(b);
			else return Large.deallocate(b);
		}
	};

	////////////////////////////////////////////////////////////////////////////////////// STATIC BUFFER ALLOCATOR
	class StaticBufferAllocator
	{
		bool IsFull;
	public:
		static Block Buffer;

		StaticBufferAllocator() : IsFull(false)
		{
		}

		bool isFull() const { return IsFull; }
		bool isEmpty() const { return !IsFull; }

		Block allocate(size_t s)
		{
			if (s == 0) return Block::Null;
			else if (!IsFull)
			{
				IsFull = true;
				return Buffer;
			}
			else return Block::Null;
		}

		bool deallocate(Block blk)
		{
			if (blk == Block::Null) return false;
			if (blk.Pointer == Buffer.Pointer && blk.Size == Buffer.Size)
			{
				IsFull = false;
				return true;
			}
			else return false;
		}

		bool owns(Block blk) const
		{
			if (blk == Block::Null) return false;
			if (blk.Pointer == Buffer.Pointer && blk.Size == Buffer.Size)
			{
				return true;
			}
			else return false;
		}
	};
	Block StaticBufferAllocator::Buffer{ nullptr, 0 };
	
	////////////////////////////////////////////////////////////////////////////////////// RING BUFFER ALLOCATOR
	template <typename TAllocator, size_t SIZE, size_t QTD>
	class RingBufferSlicedAllocator
	{
		struct info
		{
			uint64_t Free;
		};

		TAllocator& Allocator;
		
		Block MetadataBlock;
		info* Metadata;
		Block Data;
		
		uint64_t Current;
		uint64_t Allocated;
	public:
		RingBufferSlicedAllocator(TAllocator& allocator) :
			Allocator{ allocator },
			Current{ 0 },
			Allocated{ 0 }
		{
			Data = Allocator.allocate(SIZE * QTD);
			MetadataBlock = Allocator.allocate(sizeof(info) * QTD);
			Metadata = (info*)MetadataBlock.Pointer;
			memset(Metadata, 0, MetadataBlock.Size);
		}

		bool owns(Block blk) const
		{
			if (blk == Block::Null) return false;
			return Data.owns(blk.Pointer);
		}

		bool isFull()
		{
			auto a = InterlockedCompareExchange(&Allocated, 0, 0);
			return a >= QTD;
		}

		Block allocate(size_t s)
		{
			if (s == 0) return Block::Null;

			//todo do we need a fence here?
			auto c = InterlockedCompareExchange(&Current, 0, 0 );
			auto tries = QTD * 2;
			while(tries > 0)
			{
				auto r = InterlockedCompareExchange(&Metadata[c].Free, 1, 0);
				if (r == 0)
				{
					InterlockedIncrement(&Allocated);
					auto at = (uint8_t*)Data.Pointer + (c * SIZE);
					return { at, SIZE };
				}
				else
				{
					auto oldc = InterlockedCompareExchange(&Current, 0, 0);
					auto newc = oldc + 1;
					if (newc >= QTD) newc = 0;
					c = InterlockedCompareExchange(&Current, newc, oldc);
				}
				--tries;
			}
			return Block::Null;
		}

		bool deallocate(Block blk)
		{
			if (blk == Block::Null) return false;
			auto delta = (uint8_t*)blk.Pointer - (uint8_t*)Data.Pointer;
			auto pos = delta / SIZE;

			auto r = InterlockedCompareExchange(&Metadata[pos].Free, 0, 1);
			if (r == 1)
			{
				InterlockedDecrement(&Allocated);
				return true;
			}
			else return false;
		}
	};

	template <size_t SIZE, size_t QTD>
	struct RingBufferSlicedAllocatorBuilder
	{
		template<typename T>
		RingBufferSlicedAllocator<T, SIZE, QTD> make(T& fallback) const { return { fallback }; }
	};
	template <class T, size_t SIZE, size_t QTD>
	auto operator << (T& a, const RingBufferSlicedAllocatorBuilder<SIZE, QTD> &b) { return b.make(a); }
	template <size_t SIZE, size_t QTD>
	auto ringBufferSlicedAllocator() { return RingBufferSlicedAllocatorBuilder<SIZE, QTD>{}; }
}

#endif