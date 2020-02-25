class EntityBuilder
{
public:
	EntityBuilder(EntityRef ref, 
		EntityManager& entities,
		ComponentManager& components,
		SystemManager& systems) 
		:	ref(ref),
			entities{ entities },
			components{ components },
			systems{ systems },
			built{ false }
	{
	}

	~EntityBuilder()
	{
		build();
	}

	template <typename T>
	EntityBuilder& set(const T& data)
	{
		components.enable<T>(ref);
		components.set<T>(ref, data);
		return *this;

	}

	EntityRef build()
	{
		if (!built)
		{
			auto& map = components.get_map(ref);
			systems.accept(ref, map);
			built = true;
		}
		return ref;
	}
private:
	bool built;
	EntityRef ref;
	EntityManager& entities;
	ComponentManager& components;
	SystemManager& systems;
};