
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode(int x) : val(x), left(NULL), right(NULL) {}
 * };
 */
class Solution {
public:
    TreeNode* lowestCommonAncestor(TreeNode* root, TreeNode* p, TreeNode* q) {
        //auto tour = lca_euler_tour<TreeNode>{};
        //tour.run(root);
        //return tour.find(p,q);
        int depthMin = -1;
        TreeNode* lca = nullptr;
        return run(root, p, q, 0, depthMin, lca);
    }
    
    TreeNode* run(TreeNode* current, TreeNode* p, TreeNode* q, int depth, int& depthMin, TreeNode*& lca)
    {
        if(current->val == p->val || current->val == q->val)
        {
            if(depthMin >= 0) 
            {
                //std::cout << "found at " << current->val << " " << depthMin << " " << lca->val << std::endl;
                return lca;
            }
            else
            {
                //std::cout << "starting at " << current->val << std::endl;
                depthMin = std::numeric_limits<int>::max();
            }
        }
        
        if(depth < depthMin)
        {
            depthMin = depth;
            lca = current;
        }
        
        if(current->left != nullptr)
        {
            auto r = run(current->left, p, q, depth + 1, depthMin, lca);
            if(r!=nullptr) return r;
        }
        
        if(depth < depthMin)
        {
            depthMin = depth;
            lca = current;
        }
        
        if(current->right != nullptr)
        {
            auto r =  run(current->right, p, q, depth + 1, depthMin, lca);
            if(r!=nullptr) return r;
        }
        
        if(depth < depthMin)
        {
            depthMin = depth;
            lca = current;
        }
        
        return nullptr;
    }
};