#include <iostream>
#include <fstream>
#include <functional>

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

// Splay tree implementation

// Vertex of a splay tree
struct Vertex {
  int key;
  // Sum of all the keys in the subtree - remember to update
  // it after each operation that changes the tree.
  long long sum;
  Vertex* left;
  Vertex* right;
  Vertex* parent;

  Vertex(int key, long long sum, Vertex* left, Vertex* right, Vertex* parent) 
  : key(key), sum(sum), left(left), right(right), parent(parent) {}
};

void printTree(const Vertex * current, int depth = 0)
{
  return;

  // if(runPrintTree == false) return;
  // if(current == nullptr) return;

  // DebugStream() << std::string(depth * 2, ' ') << current->key << ":" << current-> sum;

  // if(current->parent != nullptr) DebugStream() << " (" << current->parent->key << ")";
  // DebugStream() << std::endl;

  // auto l = current->left;
  // if(l != nullptr)
  // {
  //   printTree(l, depth+1);
  // }

  // auto r = current->right;
  // if(r != nullptr)
  // {
  //   printTree(r, depth+1);
  // }
}

void update(Vertex* v) {
  // DebugStream() << "    updating ";
  if (v == NULL) return;
  // DebugStream() << v->key << std::endl;

  v->sum = v->key + (v->left != NULL ? v->left->sum : 0ll) + (v->right != NULL ? v->right->sum : 0ll);
  if (v->left != NULL) {
    v->left->parent = v;
  }
  if (v->right != NULL) {
    v->right->parent = v;
  }
}

void small_rotation(Vertex* v) {
  Vertex* parent = v->parent;
  if (parent == NULL) {
    return;
  }
  Vertex* grandparent = v->parent->parent;
  /*
    p                   v
   / \                 / \
  v  pr       =>     vl   p
 / \                     / \
vl   m                  m  pr
  */
  if (parent->left == v) {
    // DebugStream() << "      small rotate right" << std::endl;
    // DebugStream() << "      before----------------------" << std::endl;
    // printTree(parent, 4);

    Vertex* m = v->right;

    v->parent = parent->parent;
    v->right = parent;
    parent->parent = v;

    parent->left = m;
    if(m != nullptr) m->parent = parent;

    // DebugStream() << "      after----------------------" << std::endl;
    // printTree(v, 4);
  } 
  /*
    p                   v
   / \                 / \
 pl   v       =>      p   vr
     / \             / \
    m  vr           pl  m
  */
  else {
    // DebugStream() << "      small rotate left" << std::endl;
    Vertex* m = v->left;
    v->left = parent;
    parent->right = m;
  }
  update(parent);
  update(v);
  v->parent = grandparent;
  if (grandparent != NULL) {
    if (grandparent->left == parent) {
      grandparent->left = v;
    } else {
      grandparent->right = v;
    }
  }
}

void big_rotation(Vertex* v) {
  if (v->parent->left == v && v->parent->parent->left == v->parent) {
    // Zig-zig
    small_rotation(v->parent);
    small_rotation(v);
  } else if (v->parent->right == v && v->parent->parent->right == v->parent) {
    // Zig-zig
    small_rotation(v->parent);
    small_rotation(v);
  } else {
    // Zig-zag
    small_rotation(v);
    small_rotation(v);
  }  
}

// Makes splay of the given vertex and makes
// it the new root.
void splay(Vertex*& root, Vertex* v) {
  // DebugStream() << "    splaying ";  
  if (v == NULL) return;

  // DebugStream() << v->key << " on top of ";
  // if(root != nullptr)
  // {
    // DebugStream() << root-> key << std::endl;
  // }

  while (v->parent != NULL) {
    if (v->parent->parent == NULL) {
      // DebugStream() << "    small_rotation" << std::endl;
      small_rotation(v);
      break;
    }
    // DebugStream() << "    big_rotation" << std::endl;
    big_rotation(v);
  }

  // DebugStream() << "    changing root" << std::endl;
  root = v;
}

// Searches for the given key in the tree with the given root
// and calls splay for the deepest visited node after that.
// If found, returns a pointer to the node with the given key.
// Otherwise, returns a pointer to the node with the smallest
// bigger key (next value in the order).
// If the key is bigger than all keys in the tree, 
// returns NULL.
Vertex* find(Vertex*& root, int key) {
  // DebugStream() << "    finding " << key << std::endl;
  // DebugStream() << "        before----------------------" << std::endl;
  // printTree(root, 4);

  if( root!= nullptr && root->key == key) return root;

  Vertex* v = root;
  Vertex* last = root;
  Vertex* next = NULL;
  while (v != NULL) {
    if (v->key >= key && (next == NULL || v->key < next->key)) {
      next = v;
    }
    last = v;
    if (v->key == key) {
      break;      
    }
    if (v->key < key) {
      v = v->right;
    } else {
      v = v->left;
    }
  }
  // if( last != nullptr) DebugStream() << "    last is " << last->key <<std::endl;
  splay(root, last);
  // DebugStream() << "      after----------------------" << std::endl;
  // printTree(root, 4);

  return next;
}

void split(Vertex* root, int key, Vertex*& left, Vertex*& right) {
  // DebugStream() << "    splitting " << key << std::endl;
  right = find(root, key);
  splay(root, right);
  if (right == NULL) {
    // DebugStream() << "    right is null " << std::endl;
    left = root;
    return;
  }
  left = right->left;
  right->left = NULL;
  if (left != NULL) {
    left->parent = NULL;
  }
  update(left);
  update(right);
}

Vertex* merge(Vertex* left, Vertex* right) {
  // DebugStream() << "    merging" << std::endl;
  // DebugStream() << "        before left----------------------" << std::endl;
  // printTree(left, 4);
  // DebugStream() << "        before right----------------------" << std::endl;
  // printTree(right, 4);

  if (left == NULL) return right;
  if (right == NULL) return left;
  
  Vertex* min_right = right;
  while (min_right->left != NULL) {
    min_right = min_right->left;
  }
  splay(right, min_right);
  right->left = left;
  update(right);

  // DebugStream() << "        after right----------------------" << std::endl;
  // printTree(right, 4);
  return right;
}

// Code that uses splay tree to solve the problem

Vertex* root = NULL;

void insert(int x) {
  // DebugStream() << "insert " << x << std::endl;

  Vertex* left = NULL;
  Vertex* right = NULL;
  Vertex* new_vertex = NULL;  
  split(root, x, left, right);
  if (right == NULL || right->key != x) {
    new_vertex = new Vertex(x, x, NULL, NULL, NULL);
  }
  root = merge(merge(left, new_vertex), right);
}

//STDelete(N)
//  Splay(Next(N))
//  Splay(N)
//  Delete(N)
void erase(int x) {    
  // DebugStream() << "erase " << x;            
  // Implement erase yourself
  
  if(root == nullptr) return;

  auto node = find(root, x);

  if(node == nullptr)
  {
    // DebugStream() << std::endl;
    return;
  } 

  if(node->key == x)
  {
    auto next = node->parent;

      splay(root, next);
      splay(root, node);      
      root = merge(node->left, node->right);

      if(root != nullptr) root->parent = nullptr;
  }

  // DebugStream() << " ok" << std::endl;
}

bool find(int x) {  
  // DebugStream() << "find " << x; 
  
  if(root == nullptr)
  {
    // DebugStream() << " not found" << std::endl;
    return false;
  }
  
  auto node = find(root, x);

  if(node == nullptr)
  {
    // DebugStream() << " not found" << std::endl;
    return false;
  }

  if(node->key == x)
  {
    // DebugStream() << " found" << std::endl;
    return true;
  }
  else
  {
    // DebugStream() << " not found" << std::endl;
    return false;
  }
}

long long sum(int from, int to) {
  // DebugStream() << "sum " << from << " " << to << std::endl;;

  Vertex* left = NULL;
  Vertex* middle = NULL;
  Vertex* right = NULL;
  split(root, from, left, middle);
  split(middle, to + 1, middle, right);

  // DebugStream() << "middle ---------------------" <<std::endl;
  // printTree(middle);
  // DebugStream() << "---------------------" <<std::endl;

  long long ans = 0;
  if(middle != nullptr)
  {
    ans = middle->sum;
  }

  root = merge(merge(left, middle), right);
  // DebugStream() << " result " << ans << std::endl;
  return ans;  
}

const int MODULO = 1000000001;

void run(std::istream& in, std::ostream& out)
{
  int n;
  in >> n;
  // scanf("%d", &n);
  int last_sum_result = 0;
  for (int i = 0; i < n; i++) {
    // DebugStream() << "tree --------------------------------" << std::endl;
    // printTree(root);
    // DebugStream() << "--------------------------------" << std::endl;
    // char buffer[10];
    // scanf("%s", buffer);
    std::string buffer;
    in >> buffer;
    char type = buffer[0];
    switch (type) {
      case '+' : {
        int x;
        // scanf("%d", &x);
        in >> x;
        insert((x + last_sum_result) % MODULO);
      } break;
      case '-' : {
        int x;
        // scanf("%d", &x);
        in >> x;
        erase((x + last_sum_result) % MODULO);
      } break;            
      case '?' : {
        int x;
        // scanf("%d", &x);
        in >> x;
        auto f = find((x + last_sum_result) % MODULO);
        // printf( ? "Found\n" : "Not found\n");
        out << (f ? "Found" : "Not found") << std::endl;
      } break;
      case 's' : {
        int l, r;
        // scanf("%d %d", &l, &r);
        in >> l >> r;
        long long res = sum((l + last_sum_result) % MODULO, (r + last_sum_result) % MODULO);
        
        // printf("%lld\n", res);
        out << res << std::endl;
        last_sum_result = int(res % MODULO);
      }
    }
  }

  // DebugStream() << "tree --------------------------------" << std::endl;
  // printTree(root);
  // DebugStream() << "--------------------------------" << std::endl;
}

#ifdef UNITTESTS
#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

//#define BENCHPRESS_CONFIG_MAIN
//#include "../../../benchpress.hpp"

void test(std::string fileName)
{
  BeQuiet();
  
      root = nullptr;
      std::ifstream fin{fileName};
      std::ifstream fexpectedOut{fileName + ".a"};    
      std::stringstream out;
      run(fin, out);
       out.seekg(0);
       std::string actual, expected;
       while (std::getline(fexpectedOut, expected))
       {
         std::getline(out, actual);
         REQUIRE(expected == actual);    
      }
}

TEST_CASE("","")
{
  //test("./tests/01");
  // test("./tests/04");
  // test("./tests/05");
  // test("./tests/20");
  // test("./tests/36");
  test("./tests/83");
}

// BENCHMARK("file", [&](benchpress::context* ctx) {
//   for (size_t i = 0; i < ctx->num_iterations(); ++i) {    
//     root = nullptr;
//     std::string fileName{"./tests/01"};
//     std::ifstream fin{fileName};
//     std::ifstream fexpectedOut{fileName + ".a"};    
//     std::stringstream out;
//     run(fin, out);    
//   }
// })    

#else

int main(){
  BeQuiet();
  run(std::cin, std::cout);
  return 0;
}

#endif
