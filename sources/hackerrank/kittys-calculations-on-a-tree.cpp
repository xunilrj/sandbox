// #include <cmath>
// #include <cstdio>
// #include <iterator>
// #include <vector>
// #include <iostream>
// #include <algorithm>
// #include <unordered_map>
// #include <tuple>
// #include <string> 
// using namespace std;

// //https://stackoverflow.com/questions/20834838/using-tuple-in-unordered-map
// //https://stackoverflow.com/questions/919612/mapping-two-integers-to-one-in-a-unique-and-deterministic-way
// using cantor_pairing_key_t = std::tuple<int, int>;
// struct cantor_pairing_hash 
//     : public std::unary_function<cantor_pairing_key_t, long long>
// {
//     long long operator()(const cantor_pairing_key_t& k) const
//     {
//         long long a = std::get<0>(k);
//         long long b = std::get<1>(k);
//         return (a + b) * (a + b + 1) / 2 + a;
//     }
// };

// struct treeNode
// {
//     long value;
//     int children;
//     std::vector<int> path;
//     treeNode* parent;

//     treeNode(long v, treeNode* p) : value{v}, parent{p}, children{0}
//     {
//         if(parent == nullptr)
//             path = {};
//         else
//         {
//             //avoid extra allocation
//             path = std::vector<int>(parent->path.size() + 1);
//             path = parent->path;
//             path.push_back(parent->children);
//             ++parent->children;
//         }
//     }
// };

// long long cachehit = 0;
// long long cachetry = 0;

// struct tree
// {
//     treeNode* root;
//     std::unordered_map<long, treeNode*> nodes;
//     std::unordered_map<std::tuple<long,long>, long, cantor_pairing_hash> distCache;

//     tree(size_t n) : root {nullptr}, nodes (n)
//     {

//     }

//     treeNode* find(long v)
//     {
//         auto r = nodes.find(v);
//         if (r != nodes.end())
//             return r->second;
//         else
//             return nullptr;            
//     }

//     treeNode* setRoot(treeNode* node)
//     {
//         root = node;
//         nodes[node->value] = node;
//         return node;
//     }

//     treeNode* addNode(treeNode* node)
//     {
//         nodes[node->value] = node;
//         return node;
//     }

//     long distPaths(const std::vector<int>& a, const std::vector<int>& b)
//     {
//         auto asize = a.size();
//         auto bsize = b.size();
//         int i = 0;
//         while((i < asize) && (i < bsize) && (a[i] == b[i]))
//             ++i;

//         auto distance = (asize - i) + (bsize - i);
//         return distance;
//     }

//     long dist(treeNode* a, treeNode* b)
//     {
//         if(a == b) return 0;

//         ++cachetry;

//         auto bigger = std::max(a->value, b->value);
//         auto smaller = std::min(a->value, b->value);
//         auto key = std::make_tuple(bigger,smaller);

//         auto it = distCache.find(key);
//         if(it != distCache.end())
//         {
//             ++cachehit;
//             return it->second; 
//         }

//         auto dist = distPaths(a->path, b->path);

//         distCache[key] = dist;
        
//         return dist;
//     }
// };

// #include <chrono>

// class Fps
// {
// protected:
//     unsigned int m_fps;
//     unsigned int m_fpscount;
//     std::chrono::duration<double, std::milli> delta;
//     std::chrono::system_clock::time_point lastFrame;

// public:
//     // Constructor
//     Fps() : m_fps(0), m_fpscount(0)
//     {
//         delta = 0ms;
//         lastFrame = std::chrono::system_clock::now();
//     }

//     void inc()
//     {
//         m_fpscount++;
//     }

//     // Update
//     bool update()
//     {
//         // increase the counter by one
//         m_fpscount++;

//         std::chrono::system_clock::time_point now = std::chrono::system_clock::now();        
//         std::chrono::duration<double, std::milli> delta2 = now - lastFrame;
//         delta += delta2;
//         if (delta.count() >= 1000)
//         {
//             m_fps = m_fpscount;
//             m_fpscount = 0;
//             delta -= 1000ms;
//             lastFrame = now;
//             return true;
//         }
//         else
//         {
//             lastFrame = now;
//             return false;
//         }
//     }

//     // Get fps
//     unsigned int get() const
//     {
//         return m_fps;
//     }
// };



// int main() {
//     size_t n, q;
//     std::cin >> n >> q;

//     tree t {3*n};
//     for(int i = 0;i < n - 1; ++i)
//     {
//         long a,b;
//         std::cin >> a >> b ;

//         auto* nodea = t.find(a);
//         auto* nodeb = t.find(b);
//         if(nodea == nullptr && nodeb == nullptr)
//         {
//             nodea = t.setRoot(new treeNode{a, nullptr});
//             t.addNode(new treeNode{b, nodea});
//         }
//         else if(nodea == nullptr && nodeb != nullptr)
//             t.addNode(new treeNode{a, nodeb});
//         else if(nodeb == nullptr && nodea != nullptr)
//             t.addNode(new treeNode{b, nodea});
//     }

//     Fps fps;
//     std::vector<treeNode*> nodes (1000);
//     for(long i = 0;i < q; ++i)
//     {
//         size_t set_size;
//         std::cin >> set_size;

//         if(set_size <= 1)
//         {
//             long noden;
//             std::cin >> noden;
//             std::cout << 0 << std::endl;
//         }
//         else {
//             nodes.clear();
//             for(int inode = 0;inode < set_size; ++inode)
//             {
//                 long noden;
//                 std::cin >> noden;
//                 nodes.push_back(t.find(noden));
//             }



//             long accum = 0;
//             long modC = 1000000000 + 7;
//             long long fpsi = 0;
//             for(long pairstart = 0;pairstart < nodes.size(); ++pairstart)
//             {
//                 auto nodestart = nodes[pairstart];
//                 for(long pairend = pairstart + 1;pairend < nodes.size(); ++pairend)
//                 {
//                     // ++fpsi;
//                     // fps.inc();

//                     // if((fpsi % 1000 == 0) && fps.update())
//                     //     std::cout << "fps: " << fps.get() << std::endl;

//                     auto nodeend = nodes[pairend];
//                     auto dist = t.dist(nodestart, nodeend);
                    
//                     // //https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/modular-multiplication
//                     // //https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/modular-addition-and-subtraction
//                     long accum_term = (nodestart->value * nodeend->value * dist);
//                     accum += accum_term;
//                     // accum %= modC;
//                 }
//             }
//             std::cout << accum % modC << std::endl;
//         }

//         std::cout << "cache try" << cachetry << std::endl;
//         std::cout << "cache hit" << cachehit << std::endl;
//     }

//     return 0;
// }

#define _CRT_SECURE_NO_WARNINGS


#include <iostream>
#include <cstdio>
#include <vector>
#include <cstring>
#include <utility>
#include <algorithm>
using namespace std;

using pii = pair<int64_t,int64_t>;
const int MAX_N = 2e5 + 6;
const int MAX_P = 19;
const int64_t mod = 1e9 + 7;

vector<int> childrens[MAX_N];
int distance_to_node[MAX_P][MAX_N];
bool visited[MAX_N];

struct centroid_node {
    int parent;
    int depth;
    pii val_v_av;  //first --> val, second --> minus
    pii val_v;

    int64_t VALUE;
} centroids[MAX_N];

pii operator+(const pii& p1, const pii& p2) {
    return make_pair(p1.first + p2.first, p1.second + p2.second);
}

pii operator-(const pii& p1, const pii& p2) {
    return make_pair(p1.first - p2.first, p1.second - p2.second);
}

pii operator+=(pii& p1, const pii& p2) {
    p1 = p1 + p2;
    return p1;
}

pii operator-=(pii& p1, const pii& p2) {
    p1 = p1 - p2;
    return p1;
}

void Pure(pii& p)
{
    p.first = (p.first % mod + mod) % mod;
    p.second = (p.second % mod + mod) % mod;
}

void dotNode(int p, const std::string& color)
{
    auto& cp = centroids[p];
    std::cout << p << "[label=\"" << p 
        << ",value=" << cp.val_v.first 
        << ",minus=" << cp.val_v.second
        << ",value=" << cp.val_v_av.first
        << ",minus=" << cp.val_v_av.second
        << ",NODEVALUE=" << cp.VALUE
        << "\", color = \"" << color << "\"]" << std::endl;
}


struct centroid_decomposition
{
    vector<int> vertices;
    int qtd_descendents[MAX_N];

    int get_centroid_of(int id)
    {
        auto& descendents = vertices;
        
        descendents.clear();
        find_descendents(id);
        int qtd_my_descendents = descendents.size();
        int centroid_threshold = qtd_my_descendents / 2;
        
        int centroid = -1;
        for (int i : descendents)
        {
            if (max(0, qtd_my_descendents - qtd_descendents[i]) <= centroid_threshold)
            {
                centroid = i;
            }
            visited[i] = false;
        }
        return centroid;
    }

    void run(int id, int parent_in_centroid_tree, int depth_in_centroid_tree)
    {
        int id_in_centroid_tree = get_centroid_of(id);

        update_distances(id_in_centroid_tree, id_in_centroid_tree, depth_in_centroid_tree, 0);

        centroids[id_in_centroid_tree] =
        { 
            parent_in_centroid_tree,
            depth_in_centroid_tree,
            {0,0},
            {0,0}
        };
        visited[id_in_centroid_tree] = true;

        for (int i : childrens[id_in_centroid_tree])
        {
            if (!visited[i])
                run(i, id_in_centroid_tree, depth_in_centroid_tree + 1);
        }
    }

    void broadcast_value(int64_t x)
    {
        int64_t p = x;

        while (p != -1)
        {
            auto& cp = centroids[p];

            dotNode(p, "red");
            std::cout << "label = \"broadcast_value " 
                << x 
                << " x: " << x
                << ", depth: " << cp.depth
                << ", dist: " << distance_to_node[cp.depth][x]
                << "\"" 
                << std::endl;

            cp.val_v += {x, 0};
            cp.val_v_av += {x * distance_to_node[cp.depth][x], 0};

            cp.VALUE += x;
            dotNode(p, "green");

            if (cp.parent != -1)
            {
                int par = cp.parent;
                auto& cpar = centroids[par];

                std::cout << "label = \"broadcast_value "
                    << x
                    << " (parent) x: " << x
                    << ", depth: " << cpar.depth
                    << ", dist: " << distance_to_node[cpar.depth][x]
                    << "\""
                    << std::endl;
                cp.val_v -= {0, x};
                cp.val_v_av -= {0, x * distance_to_node[cpar.depth][x]};
            }

            //Pure(cp.val_v);
            //Pure(cp.val_v_av);

            dotNode(p, "black");

            p = cp.parent;
        }
    }

    void remove_value(int64_t x)
    {
        int64_t p = x;

        while (p != -1)
        {
            dotNode(p, "red");

            auto& cp = centroids[p];

            cp.val_v -= {x, 0};
            cp.val_v_av -= {x* distance_to_node[cp.depth][x], 0};

            cp.VALUE -= x;

            if (centroids[p].parent != -1)
            {
                int par = cp.parent;
                auto& cpar = centroids[par];

                cp.val_v += {0, x};
                cp.val_v_av += {0, x* distance_to_node[cpar.depth][x]};
            }

            //Pure(centroids[p].val_v);
            //Pure(centroids[p].val_v_av);
            p = centroids[p].parent;

            dotNode(p, "black");
        }
    }

    void dotQuery(int64_t x, int64_t p, int64_t ret, int64_t v, int64_t v_av, const std::string& op)
    {
        auto& cp = centroids[p];
        std::cout << "label = \"query " << x <<
            " ret=" << ret <<
            " v=" << v <<
            " v_av=" << v_av <<
            "distance to " << x << "=" << distance_to_node[cp.depth][x] <<
            op <<
            "\"" << std::endl;
    }

    int64_t query(int64_t x)
    {
        int64_t ret = 0;
        int64_t v = 0;
        int64_t v_av = 0;
        int p = x;

        while (p != -1)
        {
            auto& cp = centroids[p];

            v += cp.val_v.first;
            v_av += cp.val_v_av.first;
            ret += x * v_av;
            //ret %= mod;
            ret += x * distance_to_node[cp.depth][x] * v;
            //ret %= mod;
            v = cp.val_v.second;
            v_av = cp.val_v_av.second;

            p = cp.parent;
        }

        return ret;
    }
private:
    void find_descendents(int id)
    {
        vertices.push_back(id);
        visited[id] = true;
        qtd_descendents[id] = 1;

        for (int i : childrens[id])
        {
            if (!visited[i])
            {
                find_descendents(i);
                qtd_descendents[id] += qtd_descendents[i];
            }
        }
    }

    void update_distances(int id, int parent, int cen_depth, int dist)
    {
        distance_to_node[cen_depth][id] = dist;
        for (int i : childrens[id])
        {
            if (!visited[i] && i != parent)
            {
                update_distances(i, id, cen_depth, dist + 1);
            }
        }
    }
};


int64_t pow(int64_t a,int64_t n,int64_t mod)
{
    if (n==0) return 1;
    else if (n==1) return a;
    int64_t ret=pow(a,n/2,mod);
    ret*=ret;
    ret%=mod;
    if (n&1) {
        ret*=a;
        ret%=mod;
    }
    return ret;
}

void generateDOT(int n)
{
    while (n >= 0)
    {
        for (int i : childrens[n])
        {
            std::cout << n << "->" << i << ";" << std::endl;
        }
        --n;
    }
}

void generateCentroidDOT(int n)
{
    while(n >= 0)
    {
        auto& c = centroids[n];
        if(c.parent != -1)
            std::cout << n << "->" << c.parent << "[color=\"brown\"];" << std::endl;
        else
            dotNode(n, "brown");
        --n;
    }
}

int main () {
    int n, q;
    std::cin >> n >> q;

    int maxnode = 0;
    for (int i = 1;i <= n-1; ++i)
    {
        int a,b;
        std::cin >> a >> b;
        
        childrens[a].push_back(b);
        childrens[b].push_back(a);

        maxnode = std::max(a, maxnode);
        maxnode = std::max(b, maxnode);
    }

    generateDOT(maxnode);

    auto cd = new centroid_decomposition{};
    cd->run(1, -1, 0);

    generateCentroidDOT(maxnode);

    while (q--)
    {
        int k;
        std::cin >> k;

        vector<int> set_nodes;
        while (k--)
        {
            int x;
            std::cin >> x;
            set_nodes.push_back(x);
        }

        for (int i : set_nodes)
            dotNode(i, "green");

        for (int i : set_nodes)
        {
            std::cout << "label = \"broadcast_value " << i << "\"" << std::endl;
            cd->broadcast_value(i);
        }

        int64_t value = 0;
        for (int i : set_nodes)
        {
            std::cout << "label = \"query " << i << "\"" << std::endl;
            value += cd->query(i);
            value %= mod;
        }

        for (int i : set_nodes)
        {
            std::cout << "label = \"remove_value " << i << "\"" << std::endl;
            cd->remove_value(i);
        }

        //std::cout << (value * pow(2, mod - 2, mod) + mod) % mod << std::endl;
    }
}
