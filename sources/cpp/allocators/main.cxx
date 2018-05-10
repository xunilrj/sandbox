#include <cstddef>

template <typename T>
struct Result
{
    bool Success;
    T Result;
}

struct Block
{
    void* Pointer;
    size_t Size;

    static const Block Null;
};
const Block Block::Null{};

class NullAllocator
{
public:
    bool owns(Block){
        return false;
    }

    Block allocate(size_t size)
    {
        return Block::Null;
    }

    void deallocate(Block)
    {

    }
};

#include <cstdlib>

class MallocAllocator
{
public:
    bool owns(Block)
    {
        //No easy way to detect that I own this block
        return true;
    }

    Block allocate(size_t size)
    {
        auto mem = malloc(size);
        return {mem, size};
    }

    void deallocate(Block block)
    {
        free(block.Pointer);
    }
};

template<typename Primary, typename Fallback>
class FallbackAllocator : private Primary, private Fallback
{
public:
    bool owns(Block b)
    {
        return Primary::owns(b) || Fallback::owns(b);
    }

    Block allocate(size_t size)
    {
        auto block = Primary::allocate(size);
        if(block.Pointer == false){
            block = Fallback::allocate(size);
        }
        return block;
    }

    void deallocate(Block block)
    {
        if(Primary::owns(block)) Primary::deallocate(block);
        else Fallback::deallocate(block);
    }
};

template<size_t BufferSize>
class StackAllocator
{
public:
    bool owns(Block block)
    {
        return block.Pointer > Buffer && block.Pointer < Top;
    }

    Block allocate(size_t size)
    {
        auto p = Top;
        Top += size;
        return Block{p,size};
    }

    void deallocate(Block block)
    {
        if(block.Pointer == Top)
        {
            Top -= block.Size;
        }
    }
private:
    char Buffer[BufferSize];
    char * Top;
};

#include <gtest/gtest.h>
#include <string>

TEST(FallbackAllocator, MustAllocateAndDeallocate) {
    auto allocator = FallbackAllocator<StackAllocator<16384>,MallocAllocator>{};
    auto block = allocator.allocate(1);
    allocator.deallocate(block);
}