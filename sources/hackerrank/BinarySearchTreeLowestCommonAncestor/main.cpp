

/*The tree node has data, left child and right child 
class Node {
    int data;
    Node* left;
    Node* right;
};

*/

    struct lca_euler_tour
    {
        std::vector<Node*> nodes;
        std::vector<size_t> tour;

        void run(Node* current)
        {
            //TODO: also build segment tree
            if(current == nullptr) return;

            auto i = nodes.size();
            nodes.push_back(current);

            tour.push_back(i);
            if(current->left != nullptr)
            {
                run(current->left);
                tour.push_back(i);
            }
            if(current->right != nullptr)
            {
                run(current->right);
                tour.push_back(i);
            }

            
        }

        template <typename T>
        struct minagg
        {
            T min;

            minagg() : min{std::numeric_limits<T>::max()}
            {
            }

            minagg<T>& operator << (const T& candidate)
            {
                if(candidate < min)
                    min = candidate;
                return *this;
            }

            operator T() const { return min; }
        };

        Node* find(int v1, int v2)
        {
            int a = std::min(v1,v2);
            int b = std::max(v1,v2);

            //for(auto i : tour)
            //{
            //  std::cout << nodes[i]->data << " ";
            //}

            auto it = tour.begin();
            while(it != tour.end() && nodes[*it]->data != a)
                ++it;

            if(a == b)
                return nodes[*it];

            minagg<size_t> m;
            m << *it;
            while(it != tour.end() && nodes[*it]->data != b)
            {
                m << *it;
                ++it;
            }
            if(it != tour.end())
                m << *it;

            return nodes[m];
        }
    };
  
    Node *lca(Node *root, int v1,int v2)
    {        
        auto tour = lca_euler_tour{};
        tour.run(root);
        return tour.find(v1,v2);    		
    }

