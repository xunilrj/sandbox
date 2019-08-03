// Heap
//https://cacm.acm.org/magazines/2010/7/95061-youre-doing-it-wrong/fulltext#F1
//http://theoryofprogramming.com/2015/02/01/binary-heaps-and-heapsort-algorithm/
// emplace - O(log2(n))
// pop - O(log2(n))
// remove - O(n)
// TAOCP - page 334
//		2.3.2. Binary Tree Representation of Trees 
template <typename T, typename TAlloc>
class NodeBinaryHeap : protected TAlloc
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
	Node* emplaceAtInsertionPoint(ui64 priotity, TArgs&&... args)
	{
		// While inserting itself we could keep a track of the heap size. So,
		// this takes constant time. While inserting, we must search for the
		// appropriate location. For this, we will convert the size into its
		// binary representation and begin the traversal from the root. How?
		// Keep reading the bits of the binary representation you just
		// generated, if you encounter ‘0’ go to the left child of the parent,
		// if it is a ‘1’ go to the right child of the parent. I know it
		// sounds really weird, but that’s the best thing we can do. 
		//http://theoryofprogramming.com/2015/02/01/binary-heaps-and-heapsort-algorithm/
		auto path = ++Size;
		auto pos = leading_zero_naive3(path) + 1;

		Node* parent = nullptr;
		Node* current = Root;

		while (current != nullptr && pos < 32)
		{
			parent = current;
			auto dir = (path & (1 << ((sizeof(path) * 8) - pos))) > 0;
			if (dir)
			{
				if (current->right == nullptr)
				{
					current->right = TAlloc::template alloc<Node>(
						parent,
						nullptr,
						nullptr, 
						Id++, 
						priotity, 
						T{std::forward<TArgs>(args)...});
					return current->right;
				}
				current = current->right;
			}
			else
			{
				if (current->left == nullptr)
				{
					current->left = TAlloc::template alloc<Node>(
						parent,
						nullptr,
						nullptr, 
						Id++, 
						priotity, 
						T{std::forward<TArgs>(args)...});
					return current->left;
				}
				current = current->left;
			}

			++pos;
		}

		Root = TAlloc::template alloc<Node>(
			nullptr,
			nullptr,
			nullptr, 
			Id++, 
			priotity, 
			T{std::forward<TArgs>(args)...});
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
		  pl    n
				/ \
			 nl    nr
	*/
	void RightSwap(Node*& parent, Node*& n)
	{
		Node* pparent = parent->parent;
		Node* pleft = parent->left;
		Node* nleft = n->left;
		Node* nright = n->right;

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
	NodeBinaryHeap() : Size{0}, Root{nullptr}, Id {0} {	}

	ui32 size() const {	return Size; }

	template <typename... TArgs>
	ui32 emplace(ui64 priority, TArgs&&... args)
	{
		auto node = emplaceAtInsertionPoint(priority, std::forward<TArgs>(args)...);
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

		TAlloc::del(oldRoot);

		return data;
	}

	T& peek() const { return Root->data; }

	bool remove(ui32 id)
	{
		auto node = remove(id, Root);
		TAlloc::del(node);
		return node != nullptr;
	}
};