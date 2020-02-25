class SystemManager
{
public:	
	SystemManager(ComponentManager& components) 
		: components{components}
	{
	}

    template <typename T>
    T& add()
    {
		auto t = new T{};
		t->set(components);

		auto& system = systems.emplace_back(t);

		ComponentMap map = T::TYPE::get_map();
		records.push_back({ map, *system });

		return *(T*)system.get();
    }

    void update()
    {
        for(auto& s : systems)
            s->update();
    }

	void accept(EntityRef ref, ComponentMap& components)
	{
		for(auto s : records)
        {
			auto is_accepted = (s.map & components) == s.map;
			if(is_accepted)
				s.system.accept(ref);
        }
	}
private:
	struct SystemRecord
	{
		ComponentMap map;
		BaseSystem& system;
	};
	ComponentManager& components;
	std::vector<SystemRecord> records;
    std::vector<std::unique_ptr<BaseSystem>> systems;
};