#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <tuple>
#include <functional>
#include <sstream>
#include <algorithm>

std::ostream* debugStream(&std::cout);
std::ostream& DebugStream()
{
	return *debugStream;
}

bool runPrintTree = true;
std::ofstream nullStream;
void BeQuiet()
{
	runPrintTree = false;
	debugStream = &nullStream;
}

// template <typename T1, typename T2>
// std::ostream& operator << (std::ostream& out, std::tuple<T1,T2> t)
// {
// 	return out << std::get<0>(t) << " " << std::get<1>(t);
// }

// template<typename T>
// struct SplayTreeNode {
// 	using NODE = SplayTreeNode<T>;

// 	T key;
// 	T range_length;
// 	T pos_since_start;

// 	T original_start;	

// 	SplayTreeNode* left;
// 	SplayTreeNode* right;
// 	SplayTreeNode* parent;

// 	SplayTreeNode(T key, T length, T posSinceStart, T originalstart, NODE* left, NODE* right, NODE* parent) 
// 	: key(key), range_length(length), pos_since_start(posSinceStart), original_start(originalstart), left(left), right(right), parent(parent)
// 	{

// 	}
// };

// template<typename T>
// class SplayTree
// {
// 	SplayTreeNode<T>* root;
// 	using NODE = SplayTreeNode<T>;

// 	NODE* merge(NODE* left, NODE* right)
// 	{
// 		// DebugStream() << "    merging" << std::endl;
// 		// DebugStream() << "        before left----------------------" << std::endl;
// 		// printTree(left, 4);
// 		// DebugStream() << "        before right----------------------" << std::endl;
// 		// printTree(right, 4);

// 		if (left == nullptr) return right;
// 		if (right == nullptr) return left;

// 		NODE* min_right = right;

// 		while (min_right->left != nullptr)
// 		{
// 		  min_right = min_right->left;
// 		}

// 		splay(right, min_right);
// 		right->left = left;
// 		update(right);

// 		// DebugStream() << "        after right----------------------" << std::endl;
// 		// printTree(right, 4);
// 		return right;
// 	}

// 	void split(NODE* root, T key, NODE*& left, NODE*& right)
// 	{
// 		// DebugStream() << "    splitting " << key << std::endl;

// 		right = find(root, key);
// 		splay(root, right);

// 		if (right == nullptr)
// 		{
// 		//   DebugStream() << "    right is null " << std::endl;
// 		  left = root;
// 		  return;
// 		}

// 		left = right->left;
// 		right->left = nullptr;

// 		if (left != nullptr)
// 		{
// 		  left->parent = nullptr;
// 		}

// 		update(left);
// 		update(right);
// 	}

// 	void update(NODE* v)
// 	{
// 		// DebugStream() << "    updating ";
// 		if (v == nullptr) return;
// 		// DebugStream() << v->key << std::endl;

// 		if(v->left != nullptr) v->pos_since_start = v->left->pos_since_start + v->left->range_length;
// 		else v->pos_since_start = 0;
// 		// if(v->right != nullptr) v->right->key = v->key + 1;

// 		// if(v->right != nullptr)
// 		// {
// 		// 	v->right->key = v->key + 1;
// 		// }

// 		v->key = v->pos_since_start;

// 		if (v->left != nullptr)
// 		{
// 		  v->left->parent = v;
// 		}

// 		if (v->right != nullptr)
// 		{
// 		  v->right->parent = v;
// 		}
// 	}

// 	NODE* find(NODE*& root, T key) {
// 		DebugStream() << "    finding " << key << std::endl;
// 		// DebugStream() << "        before----------------------" << std::endl;
// 		// printTree(root, 4);
// 		// DebugStream() << "        ----------------------" << std::endl;

// 		if(root!= nullptr && root->key == key) return root;

// 		NODE* last = root;
// 		while (last != nullptr) {
// 			auto l = last->key;
// 			auto r = last->key + last->range_length;
// 			// DebugStream() << "current last [" << l << "-" << r << "]" << std::endl;

// 			if (key >= l && key < r)
// 			{
// 				// DebugStream() << "node found" << std::endl;
// 				break;
// 		  	}		  	
// 			else if (key < l)
// 			{
// 				last = last->left;
// 		  	} else {
// 				last = last->right;
// 		  	}
// 		}

// 		if( last != nullptr) DebugStream() << "    last is " << last->key <<std::endl;

// 		splay(root, last);

// 		// DebugStream() << "      after----------------------" << std::endl;
// 		// printTree(root, 4);

// 		//split the range
// 		// find 6
// 		//     last             last
// 		//     [5-2]            [6-1]
// 		//     [1-8]            []
// 		//    /    \           /     \
// 		//   l     r    =>  new       r
// 		//  [1-4] [7-2]     [5-1]    [7-2]
// 		//  [1-4] [7-2]     []       [7-2]
// 		//                 /
// 		//                l
// 		//                [1-4]
// 		//                [1-4]

// 		if(last != nullptr && last->key < key) {
// 			// DebugStream() << "    need new node" << std::endl;
// 			// DebugStream() << "    before--------------------------" << std::endl;
// 			// printTree(last);
// 			// DebugStream() << "    --------------------------" << std::endl;

// 			NODE *newNode = new NODE(last->key,
// 				key - last->key,
// 				last->original_start,
// 				0,
// 				nullptr, nullptr, nullptr);
// 			last->key  = newNode->key + newNode->range_length;
// 			last->range_length -= newNode->range_length;
// 			last->original_start = newNode->original_start + newNode->range_length;
// 			newNode->left = last->left;

// 			update(newNode);
// 			last->left = newNode;
// 			update(last);

// 			// DebugStream() << "    after--------------------------" << std::endl;
// 			// printTree(last);
// 			// DebugStream() << "    --------------------------" << std::endl;
// 		}			

// 		return last;
// 	}

// 	void splay(NODE*& root, NODE* v) {
// 		// DebugStream() << "    splaying ";  
// 		if (v == nullptr) return;

// 		// DebugStream() << v->key << " on top of ";
// 		if(root != nullptr)
// 		{
// 			// DebugStream() << root-> key << std::endl;
// 		}

// 		while (v->parent != nullptr) {
// 		  if (v->parent->parent == nullptr) {
// 			// DebugStream() << "    small_rotation" << std::endl;
// 			small_rotation(v);
// 			break;
// 		  }
// 		//   DebugStream() << "    big_rotation" << std::endl;
// 		  big_rotation(v);
// 		}

// 		// DebugStream() << "    changing root" << std::endl;
// 		root = v;
// 	}

// 	void small_rotation(NODE* v)
// 	{
// 		NODE* parent = v->parent;
// 		if (parent == nullptr) {
// 		  return;
// 		}

// 		NODE* grandparent = v->parent->parent;
// 		/*
// 		  p                   v
// 		 / \                 / \
// 		v  pr       =>     vl   p
// 	   / \                     / \
// 	  vl   m                  m  pr
// 		*/
// 		if (parent->left == v) {
// 		//   DebugStream() << "      small rotate right" << std::endl;
// 		//   DebugStream() << "      before----------------------" << std::endl;
// 		//   printTree(parent, 4);

// 		  NODE* m = v->right;

// 		  v->parent = parent->parent;
// 		  v->right = parent;
// 		  parent->parent = v;

// 		  parent->left = m;
// 		  if(m != nullptr) m->parent = parent;

// 		//   DebugStream() << "      after----------------------" << std::endl;
// 		//   printTree(v, 4);
// 		} 
// 		/*
// 		  p                   v
// 		 / \                 / \
// 	   pl   v       =>      p   vr
// 		   / \             / \
// 		  m  vr           pl  m
// 		*/
// 		else {
// 		  // DebugStream() << "      small rotate left" << std::endl;
// 		  NODE* m = v->left;
// 		  v->left = parent;
// 		  parent->right = m;
// 		}

// 		update(parent);
// 		update(v);

// 		v->parent = grandparent;

// 		if (grandparent != NULL) {
// 		  if (grandparent->left == parent) {
// 			grandparent->left = v;
// 		  } else {
// 			grandparent->right = v;
// 		  }
// 		}
// 	  }

// 	  void big_rotation(NODE* v) {
// 		if (v->parent->left == v && v->parent->parent->left == v->parent) {
// 		  // Zig-zig
// 		  small_rotation(v->parent);
// 		  small_rotation(v);
// 		} else if (v->parent->right == v && v->parent->parent->right == v->parent) {
// 		  // Zig-zig
// 		  small_rotation(v->parent);
// 		  small_rotation(v);
// 		} else {
// 		  // Zig-zag
// 		  small_rotation(v);
// 		  small_rotation(v);
// 		}  
// 	  }


// 	  void printTree(const NODE * current, int depth = 0)
// 	  {
// 		if(runPrintTree == false) return;
// 		if(current == nullptr) return;

// 		DebugStream() << std::string(depth * 2, ' ') << current->key << ":" << current->range_length;
// 		DebugStream() << "[" << current->original_start << "-" << current->range_length << "]";

// 		if(current->parent != nullptr) DebugStream() << " (" << current->parent->key << ")";

// 		auto l = current->left;
// 		auto r = current->right;
// 		if(l != nullptr) DebugStream() << " lok";
// 		if(l == nullptr) DebugStream() << " ln";
// 		if(r != nullptr) DebugStream() << " rok";
// 		if(r == nullptr) DebugStream() << " rn";

// 		DebugStream() << " index: " << current->pos_since_start << std::endl;

// 		if(l != nullptr)
// 		{
// 		  printTree(l, depth+1);
// 		}

// 		if(r != nullptr)
// 		{
// 		  printTree(r, depth+1);
// 		}
// 	  }

// 	  void internal_inorder(NODE* first, std::function<bool(T,T)> f)
// 	  {
// 		  std::function<bool(NODE*)> internal;
// 		  internal = [&](NODE* current){
// 			  if(current == nullptr) return false;
// 			  auto l = current->left;
// 			  if(l != nullptr)
// 			  {
// 				  auto cont = internal(l);
// 				  if(!cont) return false;
// 			  }

// 			  {
// 				  auto cont = f(current->original_start, current->range_length);
// 				  if(!cont) return false;
// 			  }

// 			  auto r = current->right;
// 			  if(r != nullptr)
// 			  {
// 				  auto cont = internal(r);
// 				  if(!cont) return false;
// 			  }

// 			  return true;
// 		  };
// 		  internal(first);
// 	  }
// public:
// 	SplayTree():root(nullptr)
// 	{
// 	}

// 	void insert(T x, T length) {
// 		// DebugStream() << "insert " << x << " " << length << std::endl;

// 		NODE* left = nullptr;
// 		NODE* right = nullptr;
// 		NODE* new_vertex = nullptr;  

// 		split(root, x, left, right);

// 		if (right == nullptr || right->key != x) {
// 		  new_vertex = new NODE(x, length, x, x, nullptr, nullptr, nullptr);
// 		}

// 		root = merge(merge(left, new_vertex), right);
// 	}

// 	void moveRangeTo(int i, int j, int k)
// 	{
// 		DebugStream() << "moveRangeTo " << i << " " << j << " " << k << std::endl;
// 		DebugStream() << "moveRangeTo before-----------------------------" <<std::endl;
// 		printTree(root, 4);
// 		NODE *n, *t, *left, *right = nullptr;

// 		split(root, i, left, n);
// 		split(n, j+1, n, right);
// 		t = merge(left, right);

// 		//update k		
// 		if(k > 0)
// 		{
// 			auto newk = k;
// 			// internal_inorder(t, [&](const size_t& start, const size_t& length){
// 			// 	DebugStream() << "newk " << newk << " " << start << " " << length << std::endl;
// 			// 	if(length >= newk)
// 			// 	{
// 			// 		newk = start + newk;
// 			// 		return false;
// 			// 	}
// 			// 	else newk -= length;
// 			// 	return true;
// 			// });
// 			// DebugStream() << "new k "<< newk << std::endl;
// 			split(t, newk, left, right);
// 		}
// 		//if(k < (t->agg_key + t->agg_range_length))
// 		   //split(t, k, left, right);
// 	    else {
// 	       right = t;
// 	       left = nullptr;
// 		}

// 		DebugStream() << "      left -------------------" << std::endl;
// 		printTree(left, 4);
// 		DebugStream() << "      n -------------------" << std::endl;
// 		printTree(n, 4);
// 		DebugStream() << "      right -------------------" << std::endl;
// 		printTree(right, 4);

// 		auto leftn = merge(left, n);

// 		DebugStream() << "      left + n -------------------" << std::endl;
// 		printTree(leftn, 4);

// 		root = merge(leftn, right);
// 		DebugStream() << "      leftn + right--------------------------------" <<std::endl;
// 		printTree(root, 4);	   
// 	}

// 	void inorder(std::function<bool(T,T)> f)
// 	{
// 		std::function<bool(NODE*)> internal;
// 		internal = [&](NODE* current){
// 			if(current == nullptr) return false;
// 			auto l = current->left;
// 			if(l != nullptr)
// 			{
// 				auto cont = internal(l);
// 				if(!cont) return false;
// 			}

// 			{
// 				auto cont = f(current->original_start, current->range_length);
// 				if(!cont) return false;
// 			}

// 			auto r = current->right;
// 			if(r != nullptr)
// 			{
// 				auto cont = internal(r);
// 				if(!cont) return false;
// 			}

// 			return true;
// 		};
// 		internal(root);
// 	}
// };




struct range
{
	int start, end;

	bool split_at(int index, range& l, range& r) const
	{
		if (index != start) {
			l = { start, index };
			r = { index, end };
			return true;
		}
		else {
			l = { start,end };
			return false;
		}
	}

	bool is_inside(int index) const
	{
		return index >= start && index < end;
	}

	range merge(range other) const
	{
		return { std::min(start,other.start), std::max(end,other.end) };
	}
};

struct tree_node
{
	range this_node;
	range children;

	range original;

	tree_node * parent;
	tree_node * left;
	tree_node * right;

	tree_node(int start, int end) : this_node({ start,end }), children({ start,end }), original({ start,end }),
		left(nullptr), right(nullptr), parent(nullptr)
	{
	}

	tree_node* pop_left()
	{
		if (this->left == nullptr) return nullptr;

		auto left = this->left;
		left->parent = nullptr;
		this->left = nullptr;

		this->children.start = this->this_node.start;

		return left;
	}

	tree_node* pop_right()
	{
		if (this->right == nullptr) return nullptr;

		auto right = this->right;
		right->parent = nullptr;
		this->right = nullptr;

		this->children.end = this->this_node.end;

		return right;
	}

	tree_node* replace_left(tree_node * newLeft)
	{
		auto oldLeft = pop_left();

		this->left = newLeft;
		if (newLeft != nullptr)
		{
			newLeft->parent = this;
			this->children.start = newLeft->children.start;
		}

		return oldLeft;
	}

	tree_node* replace_right(tree_node * newRight)
	{
		auto oldRight = pop_right();

		this->right = newRight;
		if (newRight != nullptr)
		{
			newRight->parent = this;
			this->children.end = newRight->children.end;
		}

		return oldRight;
	}

	//before                          nll is bigger than left              nll is smaller than left  
	//         this                        this					                this
	//       /    \         =>          /      \				 =>          /      \
	//     left  right             newLeft     right			        newLeft     right
	//     / \                     /    \						        /    \
	//   ll  lr                  nll    nlr						      nll    nlr
	//                          /   \							     /   \
	//   newleft              left  ...							   left  ...
	//    /   \               /  \								   /  \
	// nll     nlr          ll    lr							 ll    lr
	void push_left(tree_node * newLeft)
	{
		if (newLeft == nullptr) return;

		auto left = this->pop_left();
		newLeft->push_left(left);
		this->replace_left(newLeft);
	}

	//before
	//    this                        this
	//   /    \         =>          /      \
	// left  right                left    newRight
	//                                       \
	// newRight                              right
	void push_right(tree_node * newRight)
	{
		if (newRight == nullptr) return;

		auto right = this->pop_right();
		newRight->push_right(right);
		this->replace_right(newRight);
	}

	//When split this node is the right
	//and the beginning is the left
	bool split_at(int index)
	{
		range l, r;

		auto split = this_node.split_at(index, l, r);
		if (split)
		{
			auto originalLength = this->this_node.end - this->this_node.start;
			auto newLength = r.end - r.start;
			auto slide = originalLength - newLength;

			this->this_node = r;
			this->original = range{ original.start + slide, original.end };

			auto newNode = new tree_node{ l.start,l.end };
			newNode->original = range{ original.start - slide, original.end - newLength };
			this->push_left(newNode);
			return true;
		}
		else
		{
			return false;
		}
	}

	int get_direction(int index) const
	{
		auto inside = this_node.is_inside(index);
		if (inside) return 0;
		else if (index < this_node.start) return -1;
		else return 1;
	}

	tree_node* find(int index) {
		tree_node* last = this;
		bool cont = true;
		while (cont && last != nullptr) {
			auto dir = last->get_direction(index);
			switch (dir)
			{
			case -1:
				if (last->left == nullptr) return last;
				last = last->left;
				break;
			case 0:
				last->split_at(index);
				return last;
			case 1:
				if (last->right == nullptr) return last;
				last = last->right;
				break;
			}
		}

		return last;
	}

	//ROTATIONS
	//small rotation right
	/*
		  D                  B
		 / \               /  \
  (this)B   E       =>    A    D
	  /  \                    / \
	 A    C                  C   E
		*/
	void small_rotation_right()
	{
		if (this->parent == nullptr) return;
		auto B = this;
		auto C = this->pop_right();
		auto D = this->parent;
		B = D->pop_left();
		D->replace_left(C);
		B->replace_right(D);
	}

	//small rotation left
	/*
		  D                  B
		 / \               /  \
		B   E       <=    A    D(this)
	  /  \                    / \
	 A    C                  C   E
		*/
	void small_rotation_left()
	{
		auto D = this;
		auto C = this->pop_left();
		auto B = this->parent;
		D = B->pop_right();
		B->replace_right(C);
		D->replace_left(B);
	}

	void small_rotation()
	{
		tree_node* parent = this->parent;
		if (parent == nullptr) return;

		if (parent->left == this)
		{
			small_rotation_right();
		}
		else
		{
			small_rotation_left();
		}
	}

	void big_rotation()
	{
		if (this->parent == nullptr) return;
		if (this->parent->parent == nullptr) return;

		if (this->parent->left == this && this->parent->parent->left == this->parent)
		{
			// Zig-zig
			this->parent->small_rotation();
			this->small_rotation();
		}
		else if (this->parent->right == this && this->parent->parent->right == this->parent)
		{
			// Zig-zig
			this->parent->small_rotation();
			this->small_rotation();
		}
		else
		{
			// Zig-zag
			this->small_rotation();
			this->small_rotation();
		}
	}

	void splay()
	{
		while (this->parent != nullptr)
		{
			if (this->parent->parent == nullptr)
			{
				small_rotation();
				break;
			}
			else
			{
				big_rotation();
				break;
			}
		}
	}
};

class tree
{
public:
	tree_node * root;

	tree(tree_node* root) : root(root)
	{
	}

	tree_node* find_min()
	{
		if (root == nullptr) return nullptr;

		tree_node* min = root;
		while (min->left != nullptr)
		{
			min = min->left;
		}

		return min;
	}

	tree() : root(nullptr)
	{
	}

	void insert_range(int start, int end)
	{
		if (root == nullptr)
		{
			root = new tree_node{ start,end };
		}
		else
		{
			// NODE* left = nullptr;
			// NODE* right = nullptr;
			// NODE* new_vertex = nullptr;  

			// split(root, x, left, right);

			// if (right == nullptr || right->key != x) {
			//   new_vertex = new NODE(x, length, x, x, nullptr, nullptr, nullptr);
			// }

			// root = merge(merge(left, new_vertex), right);
		}
	}

	void move(int from, int to, int insert_after)
	{
		//this = start -> from
		auto from_to_tree = this->split_and_return_right(from);
		auto to_end_tree = from_to_tree->split_and_return_right(to + 1);

		//this = start -> from + to -> end
		this->merge(to_end_tree);

		//this = start -> insert_after (without from -> to)
		//insert_after == 0 actually means before the first
		if (insert_after > 0)
		{
			insert_after++;
		}

		auto without_from_to_insert_point_end_tree = this->split_and_return_right(insert_after);

		//this -> start -> to (reordered)
		this->merge(from_to_tree);
		this->merge(without_from_to_insert_point_end_tree);
	}

	tree_node* find(int index)
	{
		if (root == nullptr) return nullptr;

		auto last = root->find(index);

		if (last == nullptr) return nullptr;

		last->splay();

		if (last->this_node.is_inside(index) == false) return nullptr;

		return last;
	}

	tree* split_and_return_right(int index)
	{
		tree_node* right = find(index);
		if (right == nullptr) return new tree{};

		root = right->pop_left();

		return new tree{ right };
	}

	void merge(tree* newRight)
	{
		if (newRight == nullptr) return;
		auto min_right = newRight->find_min();

		if (min_right == nullptr) return;

		min_right->splay();
		min_right->push_left(root);

		root = min_right;
		newRight->root = nullptr;
	}

	void inorder(std::function<bool(int, int)> f)
	{
		std::function<bool(tree_node*)> internal;
		internal = [&](tree_node* current) {
			if (current == nullptr) return false;
			auto l = current->left;
			if (l != nullptr)
			{
				auto cont = internal(l);
				if (!cont) return false;
			}

			{
				auto cont = f(current->original.start, current->original.end);
				if (!cont) return false;
			}

			auto r = current->right;
			if (r != nullptr)
			{
				auto cont = internal(r);
				if (!cont) return false;
			}

			return true;
		};
		internal(root);
	}
};

class Rope {
	tree Tree;
	std::string original;
public:
	Rope(const std::string &s) : original(s)
	{
		Tree.insert_range(0, s.size());
	}

	void process(int i, int j, int k) {
		Tree.move(i, j, k);
	}

	std::string result() {
		std::stringstream ss{ "" };
		Tree.inorder([&](const int& start, const int& end) {
			ss << original.substr(start, end - start);
			return true;
		});
		return ss.str();
	}
};

void run(std::istream& in, std::ostream& out)
{
	std::string s;
	in >> s;

	Rope rope(s);

	int actions;
	in >> actions;
	for (int action_index = 0; action_index < actions; ++action_index) {
		int i, j, k;
		in >> i >> j >> k;
		rope.process(i, j, k);
	}
	out << rope.result() << std::endl;
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& inString, const std::string& expectedOutString)
{
	std::stringstream in{ inString };
	std::stringstream expectedOut{ expectedOutString };
	std::stringstream out;

	run(in, out);
	out.seekg(0);

	std::string actual, expected;
	while (std::getline(expectedOut, expected))
	{
		std::getline(out, actual);
		REQUIRE(expected == actual);
	}
}

TEST_CASE("rope must suffle strings", "rope tests")
{
	//test("hlelowrold 2 1 1 2 6 6 7", "helloworld");
	test("abcdef 2 0 1 1 4 5 0", "efcabd");
}

TEST_CASE("range split when index in inside range", "range tests")
{
	range l, r;
	auto x = range{ 0, 10 };
	auto result = x.split_at(5, l, r);
	REQUIRE(result == true);
	REQUIRE(l.start == 0);
	REQUIRE(l.end == 5);
	REQUIRE(r.start == 5);
	REQUIRE(r.end == 10);
}

TEST_CASE("split when index in the first", "range tests")
{
	range l, r;
	auto x = range{ 0, 10 };
	auto result = x.split_at(0, l, r);
	REQUIRE(result == false);
	REQUIRE(l.start == 0);
	REQUIRE(l.end == 10);
}

TEST_CASE("is inside", "range tests")
{
	auto x = range{ 0, 10 };
	REQUIRE(x.is_inside(0) == true);
	REQUIRE(x.is_inside(9) == true);
	REQUIRE(x.is_inside(10) == false);
	REQUIRE(x.is_inside(-1) == false);
	REQUIRE(x.is_inside(11) == false);
}

TEST_CASE("merge with overlap on left", "range tests")
{
	auto x1 = range{ 0, 10 };
	auto x2 = range{ -5, 5 };
	auto result = x1.merge(x2);

	REQUIRE(result.start == -5);
	REQUIRE(result.end == 10);
}

TEST_CASE("merge with overlap on right", "range tests")
{
	auto x1 = range{ 0, 10 };
	auto x2 = range{ 5, 15 };
	auto result = x1.merge(x2);

	REQUIRE(result.start == 0);
	REQUIRE(result.end == 15);
}

//TREE NODE TESTS
TEST_CASE("tree_node creation must keep ranges updated", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.left == nullptr);
	REQUIRE(x1.right == nullptr);
}

TEST_CASE("tree_node split_at", "tree_node tests")
{
	auto x1 = tree_node{ 0, 10 };
	x1.original = { 10, 20 };
	x1.split_at(5);
	tree_node& l = *x1.left;

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 0);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.original.start == 15);
	REQUIRE(x1.original.end == 20);
	REQUIRE(x1.right == nullptr);

	REQUIRE(l.this_node.start == 0);
	REQUIRE(l.this_node.end == 5);
	REQUIRE(l.children.start == 0);
	REQUIRE(l.children.end == 5);
	REQUIRE(l.original.start == 10);
	REQUIRE(l.original.end == 15);
	REQUIRE(l.left == nullptr);
	REQUIRE(l.right == nullptr);
}

//TREE TESTS
TEST_CASE("tree split_at", "tree_node tests")
{
	auto x1 = tree{};
	x1.insert_range(0, 10);

	{
		auto& r = *x1.split_and_return_right(1);
		x1.merge(&r);

		REQUIRE(x1.root->children.start == 0);
		REQUIRE(x1.root->children.end == 10);

		REQUIRE(x1.root->original.start == 1);
		REQUIRE(x1.root->original.end == 10);

		REQUIRE(x1.root->left->original.start == 0);
		REQUIRE(x1.root->left->original.end == 1);
	}

	{
		auto& r = *x1.split_and_return_right(6);
		x1.merge(&r);
	}

	REQUIRE(x1.root->children.start == 0);
	REQUIRE(x1.root->children.end == 10);
}

//LEFT OPERATIONS
TEST_CASE("tree_node pop_left must keep ranges updated", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 0, 5 };
	x1.replace_left(&x2);
	auto left = *x1.pop_left();

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.left == nullptr);
	REQUIRE(x1.right == nullptr);

	REQUIRE(left.this_node.start == 0);
	REQUIRE(left.this_node.end == 5);
	REQUIRE(left.children.start == 0);
	REQUIRE(left.children.end == 5);
	REQUIRE(left.parent == nullptr);
	REQUIRE(left.left == nullptr);
	REQUIRE(left.right == nullptr);
}

TEST_CASE("tree_node replace_left must update children aggregate", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 0, 5 };
	x1.replace_left(&x2);

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 0);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.left == &x2);
	REQUIRE(x1.right == nullptr);

	REQUIRE(x2.this_node.start == 0);
	REQUIRE(x2.this_node.end == 5);
	REQUIRE(x2.children.start == 0);
	REQUIRE(x2.children.end == 5);
	REQUIRE(x2.parent == &x1);
	REQUIRE(x2.left == nullptr);
	REQUIRE(x2.right == nullptr);
}

TEST_CASE("push_left must point to the new node", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 0, 5 };

	x1.replace_left(&x2);

	x1.this_node.start = 7;
	auto x3 = tree_node{ 5, 7 };
	x1.push_left(&x3);

	REQUIRE(x1.children.start == 0);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.left == &x3);
	REQUIRE(x1.left->left == &x2);

	REQUIRE(x3.children.start == 0);
	REQUIRE(x3.children.end == 7);
	REQUIRE(x3.parent == &x1);

	REQUIRE(x2.children.start == 0);
	REQUIRE(x2.children.end == 5);
	REQUIRE(x2.parent == &x3);
}

TEST_CASE("push_left must work with nullptr left", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 10);

	auto x3 = tree_node{ 0, 5 };
	x1.push_left(&x3);
	REQUIRE(x1.left == &x3);
	REQUIRE(x1.left->left == nullptr);
	REQUIRE(x1.children.start == 0);
	REQUIRE(x1.children.end == 10);

	REQUIRE(x3.parent == &x1);
	REQUIRE(x3.children.start == 0);
	REQUIRE(x3.children.end == 5);
}

//RIGHT OPERATIONS

TEST_CASE("tree_node pop_right must keep ranges updated", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 10, 15 };
	x1.replace_right(&x2);
	auto left = *x1.pop_right();

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 10);
	REQUIRE(x1.left == nullptr);
	REQUIRE(x1.right == nullptr);

	REQUIRE(left.this_node.start == 10);
	REQUIRE(left.this_node.end == 15);
	REQUIRE(left.children.start == 10);
	REQUIRE(left.children.end == 15);
	REQUIRE(left.parent == nullptr);
	REQUIRE(left.left == nullptr);
	REQUIRE(left.right == nullptr);
}

TEST_CASE("tree_node replace_right must update children aggregate", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 10, 15 };
	x1.replace_right(&x2);

	REQUIRE(x1.this_node.start == 5);
	REQUIRE(x1.this_node.end == 10);
	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 15);
	REQUIRE(x1.left == nullptr);
	REQUIRE(x1.right == &x2);

	REQUIRE(x2.this_node.start == 10);
	REQUIRE(x2.this_node.end == 15);
	REQUIRE(x2.children.start == 10);
	REQUIRE(x2.children.end == 15);
	REQUIRE(x2.parent == &x1);
	REQUIRE(x2.left == nullptr);
	REQUIRE(x2.right == nullptr);
}

TEST_CASE("push_right must point to the new node", "tree_node tests")
{
	auto x1 = tree_node{ 5, 10 };
	auto x2 = tree_node{ 10, 15 };

	x1.replace_right(&x2);

	x1.this_node.end = 7;
	auto x3 = tree_node{ 7, 10 };
	x1.push_right(&x3);

	REQUIRE(x1.children.start == 5);
	REQUIRE(x1.children.end == 15);
	REQUIRE(x1.right == &x3);
	REQUIRE(x1.right->right == &x2);

	REQUIRE(x3.children.start == 7);
	REQUIRE(x3.children.end == 15);
	REQUIRE(x3.parent == &x1);
	REQUIRE(x3.right == &x2);

	REQUIRE(x2.children.start == 10);
	REQUIRE(x2.children.end == 15);
	REQUIRE(x2.parent == &x3);
	REQUIRE(x2.left == nullptr);
	REQUIRE(x2.right == nullptr);
}

//SPLIT OPERATIONS
TEST_CASE("tree_node split when index in inside range", "tree_node tests")
{
	auto x = tree_node{ 0, 10 };
	auto result = x.split_at(5);

	REQUIRE(result == true);
	REQUIRE(x.this_node.start == 5);
	REQUIRE(x.this_node.end == 10);
	REQUIRE(x.children.start == 0);
	REQUIRE(x.children.end == 10);

	REQUIRE(x.left->this_node.start == 0);
	REQUIRE(x.left->this_node.end == 5);
	REQUIRE(x.left->children.start == 0);
	REQUIRE(x.left->children.end == 5);
}

TEST_CASE("tree_node split when index in left border", "tree_node tests")
{
	auto x = tree_node{ 0, 10 };
	auto result = x.split_at(0);

	REQUIRE(result == false);
	REQUIRE(x.this_node.start == 0);
	REQUIRE(x.this_node.end == 10);
	REQUIRE(x.children.start == 0);
	REQUIRE(x.children.end == 10);

	REQUIRE(x.left == nullptr);
}

//DIRECTION OPERATIONS
TEST_CASE("tree_node directions", "tree_node tests")
{
	auto root = tree_node{ 5, 10 };
	auto l = tree_node{ 0, 5 };
	auto r = tree_node{ 10, 15 };
	root.left = &l;
	root.right = &r;

	REQUIRE(root.get_direction(4) == -1);
	REQUIRE(root.get_direction(5) == 0);
	REQUIRE(root.get_direction(6) == 0);
	REQUIRE(root.get_direction(9) == 0);
	REQUIRE(root.get_direction(10) == 1);
	REQUIRE(root.get_direction(11) == 1);
}

//FIND OPERATIONS
TEST_CASE("tree_node find", "tree_node tests")
{
	auto root = tree_node{ 5, 10 };
	auto l = tree_node{ 0, 5 };
	auto r = tree_node{ 10, 15 };
	root.left = &l;
	root.right = &r;

	REQUIRE(root.find(-1) == &l); //last
	REQUIRE(root.find(0) == &l);
	REQUIRE(root.find(4) == &l);
	REQUIRE(root.find(5) == &root);
	REQUIRE(root.find(6) == &root);
	REQUIRE(root.find(9) == &root);
	REQUIRE(root.find(10) == &r);
	REQUIRE(root.find(11) == &r);
	REQUIRE(root.find(14) == &r);
	REQUIRE(root.find(15) == &r); //last
}

//SMALL ROTATION
//small rotation right
	/*
		  D                  B
		 / \               /  \
  (this)B   E       =>    A    D
	  /  \                    / \
	 A    C                  C   E
		*/
TEST_CASE("tree_node small rotation right", "tree_node tests")
{
	auto A = tree_node{ 0, 2 };
	auto B = tree_node{ 2, 4 };
	auto C = tree_node{ 4, 6 };
	auto D = tree_node{ 6, 8 };
	auto E = tree_node{ 8, 10 };

	D.replace_left(&B);
	D.replace_right(&E);
	B.replace_left(&A);
	B.replace_right(&C);

	B.small_rotation_right();

	//TEST POINTERS
	REQUIRE(A.parent == &B);
	REQUIRE(A.left == nullptr);
	REQUIRE(A.right == nullptr);

	REQUIRE(B.parent == nullptr);
	REQUIRE(B.left == &A);
	REQUIRE(B.right == &D);

	REQUIRE(C.parent == &D);
	REQUIRE(C.left == nullptr);
	REQUIRE(C.right == nullptr);

	REQUIRE(D.parent == &B);
	REQUIRE(D.left == &C);
	REQUIRE(D.right == &E);

	REQUIRE(E.parent == &D);
	REQUIRE(E.left == nullptr);
	REQUIRE(E.right == nullptr);

	//Test Ranges
	REQUIRE(A.this_node.start == 0);
	REQUIRE(A.this_node.end == 2);
	REQUIRE(A.children.start == 0);
	REQUIRE(A.children.end == 2);

	REQUIRE(B.this_node.start == 2);
	REQUIRE(B.this_node.end == 4);
	REQUIRE(B.children.start == 0);
	REQUIRE(B.children.end == 10);

	REQUIRE(C.this_node.start == 4);
	REQUIRE(C.this_node.end == 6);
	REQUIRE(C.children.start == 4);
	REQUIRE(C.children.end == 6);

	REQUIRE(D.this_node.start == 6);
	REQUIRE(D.this_node.end == 8);
	REQUIRE(D.children.start == 4);
	REQUIRE(D.children.end == 10);

	REQUIRE(E.this_node.start == 8);
	REQUIRE(E.this_node.end == 10);
	REQUIRE(E.children.start == 8);
	REQUIRE(E.children.end == 10);
}

//small rotation left
	/*
		  D                  B
		 / \               /  \
		B   E       <=    A    D(this)
	  /  \                    / \
	 A    C                  C   E
		*/
TEST_CASE("tree_node small rotation left", "tree_node tests")
{
	auto A = tree_node{ 0, 2 };
	auto B = tree_node{ 2, 4 };
	auto C = tree_node{ 4, 6 };
	auto D = tree_node{ 6, 8 };
	auto E = tree_node{ 8, 10 };

	D.replace_left(&C);
	D.replace_right(&E);
	B.replace_left(&A);
	B.replace_right(&D);

	D.small_rotation_left();

	REQUIRE(A.parent == &B);
	REQUIRE(A.left == nullptr);
	REQUIRE(A.right == nullptr);

	REQUIRE(B.parent == &D);
	REQUIRE(B.left == &A);
	REQUIRE(B.right == &C);

	REQUIRE(C.parent == &B);
	REQUIRE(C.left == nullptr);
	REQUIRE(C.right == nullptr);

	REQUIRE(D.parent == nullptr);
	REQUIRE(D.left == &B);
	REQUIRE(D.right == &E);

	REQUIRE(E.parent == &D);
	REQUIRE(E.left == nullptr);
	REQUIRE(E.right == nullptr);

	//Test Ranges
	REQUIRE(A.this_node.start == 0);
	REQUIRE(A.this_node.end == 2);
	REQUIRE(A.children.start == 0);
	REQUIRE(A.children.end == 2);

	REQUIRE(B.this_node.start == 2);
	REQUIRE(B.this_node.end == 4);
	REQUIRE(B.children.start == 0);
	REQUIRE(B.children.end == 6);

	REQUIRE(C.this_node.start == 4);
	REQUIRE(C.this_node.end == 6);
	REQUIRE(C.children.start == 4);
	REQUIRE(C.children.end == 6);

	REQUIRE(D.this_node.start == 6);
	REQUIRE(D.this_node.end == 8);
	REQUIRE(D.children.start == 0);
	REQUIRE(D.children.end == 10);

	REQUIRE(E.this_node.start == 8);
	REQUIRE(E.this_node.end == 10);
	REQUIRE(E.children.start == 8);
	REQUIRE(E.children.end == 10);
}

//BIG ROTATION
//Zig-Zig
	/*
		   [   H   ]                               [   B  ]
		  /         \                            /         \
		 D           L       <=                 A          [   D    ]
	   /  \        /   \                                  C      H
	  B     F     J     N                                    F        L
	 / \   / \   / \   / \                                 / \      /   \
	A   C E   G I   K M   O                              E   G    J       N
																 /  \    / \
																I    K  M   O
		*/
TEST_CASE("tree_node big rotation left", "tree_node tests")
{
	auto A = tree_node{ 0, 2 };
	auto B = tree_node{ 2, 4 };
	auto C = tree_node{ 4, 6 };
	auto D = tree_node{ 6, 8 };
	auto E = tree_node{ 8, 10 };
	auto F = tree_node{ 10, 12 };
	auto G = tree_node{ 12, 14 };
	auto H = tree_node{ 14, 16 };
	auto I = tree_node{ 16, 18 };
	auto J = tree_node{ 18, 20 };
	auto K = tree_node{ 20, 22 };
	auto L = tree_node{ 22, 24 };
	auto M = tree_node{ 24, 26 };
	auto N = tree_node{ 26, 28 };
	auto O = tree_node{ 28, 30 };

	B.replace_left(&A); B.replace_right(&C);
	F.replace_left(&E); F.replace_right(&G);
	J.replace_left(&I); J.replace_right(&K);
	N.replace_left(&M); N.replace_right(&O);

	D.replace_left(&B); D.replace_right(&F);
	L.replace_left(&J); L.replace_right(&N);

	H.replace_left(&D); H.replace_right(&L);

	B.big_rotation();

	//POINTERS
	REQUIRE(B.parent == nullptr);
	REQUIRE(B.left == &A);
	REQUIRE(B.right == &D);

	REQUIRE(A.parent == &B);
	REQUIRE(A.left == nullptr);
	REQUIRE(A.right == nullptr);

	REQUIRE(D.parent == &B);
	REQUIRE(D.left == &C);
	REQUIRE(D.right == &H);

	REQUIRE(C.parent == &D);
	REQUIRE(C.left == nullptr);
	REQUIRE(C.right == nullptr);

	REQUIRE(H.parent == &D);
	REQUIRE(H.left == &F);
	REQUIRE(H.right == &L);

	REQUIRE(F.parent == &H);
	REQUIRE(F.left == &E);
	REQUIRE(F.right == &G);

	REQUIRE(L.parent == &H);
	REQUIRE(L.left == &J);
	REQUIRE(L.right == &N);

	REQUIRE(E.parent == &F);
	REQUIRE(E.left == nullptr);
	REQUIRE(E.right == nullptr);

	REQUIRE(G.parent == &F);
	REQUIRE(G.left == nullptr);
	REQUIRE(G.right == nullptr);

	REQUIRE(J.parent == &L);
	REQUIRE(J.left == &I);
	REQUIRE(J.right == &K);

	REQUIRE(N.parent == &L);
	REQUIRE(N.left == &M);
	REQUIRE(N.right == &O);

	REQUIRE(I.parent == &J);
	REQUIRE(I.left == nullptr);
	REQUIRE(I.right == nullptr);

	REQUIRE(K.parent == &J);
	REQUIRE(K.left == nullptr);
	REQUIRE(K.right == nullptr);

	REQUIRE(M.parent == &N);
	REQUIRE(M.left == nullptr);
	REQUIRE(M.right == nullptr);

	REQUIRE(O.parent == &N);
	REQUIRE(O.left == nullptr);
	REQUIRE(O.right == nullptr);

	//RANGES
	REQUIRE(B.this_node.start == 2);
	REQUIRE(B.this_node.end == 4);
	REQUIRE(B.children.start == 0);
	REQUIRE(B.children.end == 30);

	REQUIRE(A.this_node.start == 0);
	REQUIRE(A.this_node.end == 2);
	REQUIRE(A.children.start == 0);
	REQUIRE(A.children.end == 2);

	REQUIRE(D.this_node.start == 6);
	REQUIRE(D.this_node.end == 8);
	REQUIRE(D.children.start == 4);
	REQUIRE(D.children.end == 30);

	REQUIRE(H.this_node.start == 14);
	REQUIRE(H.this_node.end == 16);
	REQUIRE(H.children.start == 8);
	REQUIRE(H.children.end == 30);
}

//#define BACKWARD_HAS_BFD 1
//#include "../../../backward.hpp"
//namespace backward {backward::SignalHandling sh;}

#else

int main() {
	BeQuiet();
	run(std::cin, std::cout);
}

#endif
