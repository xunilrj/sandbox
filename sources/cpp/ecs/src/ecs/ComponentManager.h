class ComponentManager
{
public:
	ComponentManager() : data{ 64 }, resources{ 64 }
	{
	}

    template <typename T>
    enable_if_component<T> enable()
    {
		auto storage = new store_of<T>();
		data[T::ID] = { (BaseStore*)storage };
    }

	template <typename T>
	enable_if_resource<T> enable()
	{
		auto storage = new store_of<T>();
		resources[T::RID] = { (BaseStore*)storage };
	}

	void new_entity(EntityRef ref)
	{
		for (auto&& d : data)
		{
			if(d.addr)
				d.addr->new_entity(ref);
		}
	}

	template <typename T>
    void enable(EntityRef ref)
    {
		auto r = records.find(ref);
		if (r != records.end()) {
			r->second.components.set(T::ID);
		}
		else {
			auto r = records.try_emplace(ref, ComponentRecord{ ref, {} });
			r.first->second.components.set(T::ID);
		}
    }

	ComponentMap& get_map(EntityRef ref)
	{
		return records[ref].components;
	}

    template <typename... TArgs>
    std::tuple<std::add_lvalue_reference_t<TArgs>...> values(EntityRef ref)
    {
		return {value<TArgs>(ref)...};
	}

	template <typename T> T& value(EntityRef ref)
	{
		auto& record = data[T::ID];
		auto storage = (store_of<T>*)record.addr;
		return storage->get(ref);
	}


	template <typename T> T& value()
	{
		auto& record = resources[T::RID];
		auto storage = (store_of<T>*)record.addr;
		return storage->get();
	}

	template <typename T> enable_if_component<T,store_of<T>&> store() { return *((store_of<T>*) data[T::ID].addr); }
	template <typename T> enable_if_resource<T,store_of<T>&> store() { return *((store_of<T>*) resources[T::RID].addr); }

	template <typename T>
    T& set(EntityRef ref, const T& v)
    {
		auto& record = data[T::ID];
		auto store = (store_of<T>*)record.addr;
		return store->set(ref, v);
    }

	template <typename T>
	void set(const T& v)
	{
		auto& record = resources[T::RID];
		auto store = (SingletonStore<T>*)record.addr;
		store->set(v);
	}

	template<typename TFrom, typename TTo, typename T>
	void build_copier(EntityRef f, T TFrom::* from_member, EntityRef t, T TTo::* to_member)
	{
		auto offf = member_offset(from_member);
		auto& fc = value<TFrom>(f);
		void* src = (void*)(((uint8_t*)(void*)&fc) + offf);

		auto offt = member_offset(to_member);
		auto& vc = value<TTo>(t);
		void* dst = (void*)(((uint8_t*)(void*)&vc) + offt);

		copies.push_back({ src, dst, &ComponentManager::copy_impl<T> });
	}

	template <typename TComponent, typename T>
	void* get_ref(EntityRef ref,T TComponent::* m)
	{
		auto offt = member_offset(m);
		auto& vc = value<TComponent>(ref);
		return (void*)(((uint8_t*)(void*)&vc) + offt);
	}

	template<typename T> static void copy_impl(void* src, void* dst) { *(T*)dst = *(T*)src; }
	
	void copy_all()
	{
		for (auto&& r : copies) { copy(r); }
	}
private:
	void copy(const CopyRecord& r)
	{
		r.f(r.src, r.dst);
	}

	template<class T, typename U>
	std::ptrdiff_t member_offset(U T::* member)
	{
		return reinterpret_cast<std::ptrdiff_t>(
			&(reinterpret_cast<T const volatile*>(NULL)->*member)
			);
	}

	struct ComponentRecord
	{
		EntityRef entity;
		ComponentMap components;
	};

	struct DataRecord
	{
		BaseStore* addr;
	};

	std::map<EntityRef, ComponentRecord> records;
	std::vector<DataRecord> data;
	std::vector<DataRecord> resources;
	std::vector<CopyRecord> copies;
};