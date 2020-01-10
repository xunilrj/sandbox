#include <unordered_map>
#include <list>

class LRUCache {
public:
    LRUCache(int capacity) : max{capacity} {
        
    }
    
    int get(int key) {
        auto it = data.find(key);
        if(it == data.end()) {
            return -1;
        } else {
            return move_to_end(it);            
        }        
    }
    
    void put(int key, int value) {        
        insert(key, value);                  
    }
private:    
    struct kv
    {
        int key;
        int value;
        
        kv(int k, int v) : key{k}, value{v}
        {            
        }
    };
    
    int max;
    std::list<kv> lru;
    using iterator = decltype(lru)::iterator;
    std::unordered_map<int, iterator> data;
    using map_iterator = decltype(data)::iterator;

    
    
    void remove_if_full()
    {
        if(data.size() >= max) {
            data.erase(lru.front().key);            
            lru.pop_front();            
        }
    }
    
    void insert(int key, int value)
    {
        auto it = lru.emplace(lru.end(), key, value);
                    
        auto dit = data.find(key);
        if(dit != data.end()) {
            auto kvit = dit->second;
            lru.erase(kvit);
            dit->second = it;
        } else {        
            remove_if_full();        
            data.emplace_hint(dit, key, it);
        }
    }
    
    int move_to_end(map_iterator it)
    {
        auto newit = lru.emplace(lru.end(), it->second->key, it->second->value);
        lru.erase(it->second);
        it->second = newit;
        return it->second->value;
    }    
    
    
};