class Scene
{
public:
	Scene() 
		: systems {components}
	{

	}

	EntityBuilder build_entity() { 
		auto ref = entities.new_entity();
		components.new_entity(ref);
		return { ref, entities, components, systems };
	};

	AnimBuilder build_anim()
	{
		return {};
	}

	template<typename T, typename U>
	Task<100>& start(AnimBuilder& anim, EntityRef ref, T U::* m)
	{
		auto dst = components.get_ref(ref, m);
		copy_function f = &ComponentManager::copy_impl<T>;
		return anim.start(tsys, anims, dst, f);
	}

	template<typename T, typename U>
	Task<100>& start_after(AnimBuilder& anim, EntityRef ref, T U::* m, Task<100>* t)
	{
		auto dst = components.get_ref(ref, m);
		copy_function f = &ComponentManager::copy_impl<T>;
		return anim.start(tsys, anims, dst, f, t);
	}

    template <typename T> T& get(EntityRef ref) { return std::get<0>(components.values<T>(ref)); }

	void update()
	{ 
		auto& time = components.value<DeltaTimeComponent>();

		systems.update();

		auto [finished, started] = tsys.run((uint64_t)(time.dt * 1000.0));
		for(int i = 0; i < started.size(); ++i)		
		{
			auto v = started[i];

			anims.start(v);

			use_remove(thens, v, [&](auto& call_id) {
				call(call_id);
			});
		}
		anims.update(time.dt);

		components.copy_all();
	}

	//components
	template <typename T>
	Scene& enable() { components.enable<T>(); return *this; }

	template <typename T>
	Scene& set(const T& v)
	{
		components.set(v);
		return *this;
	}
	
	// systems
	template <typename T>
	Scene& add() { systems.add<T>(); return *this; }

	template <typename T>
	T& add_system() { return systems.add<T>(); }

	// dispatcher
	void add(uint32_t id, void(*f)(Scene&))
	{
		fs.emplace(id, f);
	}
	void call(uint32_t id, Task<100>* t = nullptr)
	{
		if (t == nullptr)
		{
			fs[id](*this);
		}
		else
		{
			auto t1 = tsys.make_task();
			tsys.then(*t, t1);

			thens.emplace(t1.task->id, id);
		}
	}
	std::map<uint32_t, void(*)(Scene&)> fs;
	std::map<uint32_t, uint32_t> thens;

    EntityManager entities;
    ComponentManager components;
    SystemManager systems;
	TaskSystem<100, 100> tsys;
	AnimManager anims;
};