#include <map>
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
    };
    using iterator = std::list<kv>::iterator;
    using map_iterator = std::map<int, iterator>::iterator;
    
    
    void remove_if_full()
    {
        if(data.size() >= max) {
            auto i = lru.front();
            lru.pop_front();
            data.erase(i.key);            
        }
    }
    
    void insert(int key, int value)
    {
        auto dit = data.find(key);
        if(dit != data.end()) {
            //touch
            auto kvit = dit->second;
            lru.erase(kvit);
            auto lruit = lru.emplace(lru.end(), kv{key, value});                 lruit->value = value;   
            dit->second = lruit;            
        } else {        
            remove_if_full();        
            data[key] = lru.emplace(lru.end(), kv{key, value});
        }
    }
    
    int move_to_end(map_iterator it)
    {
        auto kvit = it->second;
        auto key = kvit->key;
        auto value = kvit->value;
        
        lru.erase(kvit);
        auto lruit = lru.emplace(lru.end(), kv{key, value});
        it->second = lruit;
        
        return value;
    }    
    
    int max;
    std::map<int, iterator> data;
    std::list<kv> lru;
};