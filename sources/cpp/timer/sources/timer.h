#include <utility>
#include <limits>

#define ui32 unsigned int
#define ui64 unsigned long long
#define LAMBDA [&](auto& x) -> auto

template <typename T, typename TAlloc>
class LinkedList : protected TAlloc
{
public:
	LinkedList() : Head(nullptr)
	{
	}

	template<typename... TArgs>
	void emplace_front(TArgs... args)
	{
		this->Head = TAlloc::template alloc<Node>(
			this->Head,
			std::forward<TArgs>(args)...);
	}

	template <typename F1>
	bool removeIf(F1 f)
	{
		auto last = this->Head;
		auto current = this->Head;
		while (current != nullptr)
		{
			if (auto r = f(*current); r)
			{
				del(this->Head, last, current);
				return true;
			}
			last = current;
			current = current->next;
		}

		return false;
	}

	template <typename F1, typename F2>
	bool removeIf(F1 f, F2 before)
	{
		auto last = this->Head;
		auto current = this->Head;
		while (current != nullptr)
		{
			if (auto r = f(*current); r)
			{
				before(*current);
				del(this->Head, last, current);
				return true;
			}
			last = current;
			current = current->next;
		}

		return false;
	}
private:
	struct Node : public T
	{
		Node* next;

		Node() : next(nullptr)
		{
		}

		template<typename... TArgs>
		Node(Node* next, TArgs&&... args) :
			next(next),
			T{ std::forward<TArgs>(args)... }
		{

		}
	};

	Node* Head;

	void del(Node*& head, Node*& last, Node *& n)
	{
		auto next = n->next;
		auto isHead = n == last;
		TAlloc::del(n);

		if (isHead)
		{
			head = nullptr;
			n = nullptr;
		}
		else
		{
			last->next = next;
			n = next;
		}
	}
};

//http://theoryofprogramming.com/2015/02/01/binary-heaps-and-heapsort-algorithm/
// emplace - O(log2(n))
// pop - O(log2(n))
// remove - O(n)
// TAOCP - page 334
//		2.3.2. Binary Tree Representation of Trees 
template <typename T, typename TAlloc>
class BinaryHeap
{
	struct Node
	{
		Node* parent;
		Node* left;
		Node* right;
		ui32 id;
		ui64 p;
		T data;
	};

	ui32 Id;
	ui32 Size;
	Node* Root;

	unsigned leading_zero_naive3(ui32 x)
	{
// #ifdef _MSC_VER
// 		unsigned long index;
// 		auto isNonzero = _BitScanReverse(&index, (unsigned long)x);
// 		if (isNonzero)
// 			return (sizeof(x) * 8) - index - 1;
// 		else return sizeof(x) * 8;
// #else
		int n;
		if (x == 0) return 32;
		for (n = 0; ((x & 0x80000000) == 0); n++, x <<= 1);
		return n;
//#endif
	}

	template <typename... TArgs>
	Node* emplaceAtInsertionPoint(ui64 p, TArgs&&... args)
	{
		auto path = ++Size;
		auto pos = leading_zero_naive3(path) + 1;

		Node* parent = nullptr;
		Node* current = Root;

		while (current != nullptr && pos < 32)
		{
			auto dir = (path & (1 << ((sizeof(path) * 8) - pos))) > 0;
			parent = current;
			if (dir)
			{
				if (current->right == nullptr)
				{
					current->right = new Node{ parent, nullptr, nullptr, Id++, p, {std::forward<TArgs>(args)...} };
					return current->right;
				}
				current = current->right;
			}
			else
			{
				if (current->left == nullptr)
				{
					current->left = new Node{ parent, nullptr, nullptr, Id++, p, { std::forward<TArgs>(args)... } };
					return current->left;
				}
				current = current->left;
			}

			++pos;
		}

		Root = new Node{ nullptr, nullptr, nullptr, Id++, p, { std::forward<TArgs>(args)... } };
		return Root;
	}

	Node* detachAtRemotionPoint()
	{
		auto path = Size--;
		auto pos = leading_zero_naive3(path) + 1;
		Node* current = Root;

		while (current != nullptr && pos < 32)
		{
			auto dir = (path & (1 << ((sizeof(path) * 8) - pos))) > 0;
			if (dir == 1)
			{
				if (current->right == nullptr) break;
				current = current->right;
			}
			else if (dir == 0)
			{
				if (current->left == nullptr) break;
				current = current->left;
			}
			++pos;
		}

		//assert is not null
		//assert is leaf

		if (current->parent == nullptr)
		{
			Root = nullptr;
			return current;
		}

		if (current->parent->left == current)
			current->parent->left = nullptr;
		else
			current->parent->right = nullptr;
		current->parent = nullptr;

		return current;
	}

	/*
			pp
			|
			p
			/ \
			n   pr
			/ \
		nl  nr
	*/
	void LeftSwap(Node*& parent, Node*& n)
	{
		auto pparent = parent->parent;
		auto pright = parent->right;
		auto nleft = n->left;
		auto nright = n->right;

		n->parent = pparent;
		n->right = pright;
		n->left = parent;

		parent->parent = n;
		parent->left = nleft;
		parent->right = nright;
	}

	/*
			pp
			|
			p
			/ \
			pl  n
				/ \
			nl  nr
	*/
	void RightSwap(Node*& parent, Node*& n)
	{
		auto pparent = parent->parent;
		auto pleft = parent->left;
		auto nleft = n->left;
		auto nright = n->right;

		n->parent = pparent;
		n->right = parent;
		n->left = pleft;

		parent->parent = n;
		parent->left = nleft;
		parent->right = nright;
	}

	void Swap(Node*& parent, Node*& n)
	{
		if (parent == nullptr) return;
		if (n == nullptr) return;

		if (parent->left == n)
			LeftSwap(parent, n);
		else
			RightSwap(parent, n);
	}

	void PercolateUp(Node* n)
	{
		if (n == nullptr) return;
		while ((n->parent) && (n->parent->p > n->p))
			Swap(n->parent, n);
	}

	void PercolateDown(Node* n)
	{
		if (n == nullptr) return;
		while (
			((n->left) && (n->p > n->left->p)) ||
			((n->right) && (n->p > n->right->p))
			) {
			int swap;
			if (n->left && n->right)
			{
				if (n->left->p < n->right->p)
					swap = 0;
				else swap = 1;
			}
			else if (n->right == nullptr) swap = 0;
			else swap = 1;

			if (swap == 0)
				Swap(n, n->left);
			else if (swap == 1)
				Swap(n, n->right);
		}
	}

	void DetachLeaf(Node* n)
	{
		if (n->parent == nullptr) return;

		auto dir = n->parent->right == n ? 1 : 0;
		if (dir == 0)
			n->parent->left = nullptr;
		else
			n->parent->right = nullptr;
		n->parent = nullptr;
	}

	Node* remove(ui32 id, Node * n)
	{
		if (n == nullptr) return nullptr;
		if (n->id == id)
		{
			n->p = std::numeric_limits<ui32>::max();
			PercolateDown(n);
			DetachLeaf(n);
			return n;
		}
		auto l = remove(id, n->left);
		if (l != nullptr) return l;

		return remove(id, n->right);
	}
public:
	BinaryHeap() : Size(0), Root(nullptr)
	{
	}

	ui32 size() const
	{
		return Size;
	}

	template <typename... TArgs>
	ui32 emplace(ui64 p, TArgs&&... args)
	{
		auto node = emplaceAtInsertionPoint(p, std::forward<TArgs>(args)...);
		PercolateUp(node);
		return node->id;
	}

	T pop()
	{
		auto oldRoot = Root;
		auto data = Root->data;
		auto node = detachAtRemotionPoint();

		node->left = oldRoot->left;
		node->right = oldRoot->right;
		Root = node;
		PercolateDown(node);

		delete oldRoot;

		return data;
	}

	T peek()
	{
		return Root->data;
	}

	bool remove(ui32 id)
	{
		auto node = remove(id, Root);

		delete node;

		return node != nullptr;
	}
};

// Non Ordered linked-list
// implementation simplest
// add - o(1)
// remove - o(n)
// tick - o(n)
// TAOCP	- page 238 INFORMATION STRUCTURES 
//		2.2. LINEAR LISTS
//		2.2.1. Stacks, Queues, and Deques 
//		2.2.3. Linked Allocation 
template<typename TAlloc>
class LinkedListTimer : protected TAlloc
{
	struct TimerItem
	{
		ui32 id;
		ui64 when;
	};
	ui32 Id;
	LinkedList<TimerItem, TAlloc> List;
	std::vector<ui32> expired;
public:
	LinkedListTimer() : Id(0), List{}
	{
	}

	ui32 add(ui64 when)
	{
		auto currentId = Id++;
		List.emplace_front(currentId, when);
		return currentId;
	}

	bool remove(ui32 id)
	{
		return List.removeIf(LAMBDA{ return x.id == id; });
	}

	const std::vector<ui32>& tick(ui64 now)
	{
		this->expired.clear();
		List.removeIf(
			LAMBDA{ return x.when <= now; },
			LAMBDA{ this->expired.push_back(x.id); });
		return expired;
	}
};

template<typename TAlloc>
class PriorityQueueTimer : protected TAlloc
{
	struct TimerItem
	{
		ui32 id;
		ui64 when;
	};
	ui32 Id;
	BinaryHeap<TimerItem, TAlloc> Heap;
	std::vector<ui32> expired;
public:
	PriorityQueueTimer() : Id(0), Heap{}
	{
	}

	ui32 add(ui64 when)
	{
		auto currentId = Id++;
		Heap.emplace(when, currentId, when);
		return currentId;
	}

	bool remove(ui32 id)
	{
		return Heap.remove(id);
	}

	const std::vector<ui32>& tick(ui64 now)
	{
		this->expired.clear();

		while (true)
		{
			if (Heap.size() == 0) break;

			auto data = Heap.peek();
			if (data.when <= now)
			{
				expired.push_back(data.id);
				Heap.pop();
			}
			else break;
		}

		return expired;
	}
};

// Ordered linked-list
// implementation hard
// add - o(n)
// remove - o(n)
// tick - o(1)

class NewAllocator
{
protected:
	template <typename T, typename... TArgs>
	T* alloc(TArgs&&... args)
	{
		return ::new T(std::forward<TArgs>(args)...);
	}

	template <typename T>
	void del(T* item)
	{
		delete item;
	}
};