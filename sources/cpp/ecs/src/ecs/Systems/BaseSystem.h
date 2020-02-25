struct BaseSystem
{
    virtual void update() = 0;
	void accept(EntityRef e) { entities.emplace(e); }
protected:
    std::set<EntityRef> entities;
	ComponentManager* components = nullptr;
};