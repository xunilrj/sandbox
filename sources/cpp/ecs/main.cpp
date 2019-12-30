
#include <memory>
#include <vector>
#include <bitset>
#include <map>
#include <initializer_list>
#include <set>
//#include <experimental/generator>
#include <tuple>
#include <type_traits>
#include <array>
#include <functional>
#include <utility>
//namespace std { template <typename T> using generator = std::experimental::generator<T>; }

class Scene;

// TYPE TRAITS
template <typename T> using store_of = typename T::TSTORE;
template <typename T> using element_of = typename T::TELEMENT;


struct Entity { uint64_t id; };
using EntityRef = uint64_t;

using ComponentRef = uint64_t;
using ComponentMap = std::bitset<64>;

template <typename... TArgs>
struct ComponentList
{
public:
	static ComponentMap get_map()
	{
		ComponentMap map;
		
		auto ids = { TArgs::ID ... };
		for (auto&& x : ids)
		{
			map.set(x);
		}

		return map;
	}
};

// STORES

class BaseStore
{
public:
	virtual void new_entity(EntityRef ref) = 0;
};

template <typename T>
class DenseStorage : BaseStore
{
public:
	using TELEMENT = T;

	void new_entity(EntityRef ref) override
	{
		data.push_back({});
	}

	T& get(EntityRef ref)
	{
		return data[(size_t)ref];
	}

	T& set(EntityRef ref, const T& value)
	{
		T& v = data[(size_t)ref];
		v = value;
		return v;
	}

	auto iter()	{ return data.begin(); }
private:
	std::vector<T> data;
};

template <typename T>
struct SingletonIterator
{
	T& data;

	void operator += (ptrdiff_t diff) {}
	T& operator* ()
	{
		return data;
	}
};

template <typename T>
class SingletonStore : BaseStore
{
public:
	using TELEMENT = T;

	void new_entity(EntityRef ref) override
	{
	}

	T& get(EntityRef ref)
	{
		return data;
	}

	T& set(EntityRef ref, const T& value)
	{
		data = value;
		return data;
	}

	T& set(const T& value)
	{
		data = value;
		return data;
	}

	auto iter() { return SingletonIterator<T>{ data }; }
private:
	T data;
};

// STORES JOINS

struct join
{
	template <typename TEntities, typename TF, typename... TArgs>
	static void run(const TEntities& entities, const TF& f, TArgs&... stores)
	{
		call(entities, f, stores.iter()...);
	}
private:
	template <typename T>
	static void inc(T& t, ptrdiff_t diff)
	{
		t += diff;
	}

	template <typename TEntities, typename TF, typename... TArgs>
	static void call(const TEntities& entities, const TF& f, TArgs... iters)
	{
		EntityRef last = 0;

		for (auto&& ref : entities)
		{
			auto diff = ptrdiff_t(ref - last);
			last = ref;

			(inc(iters, diff), ...);
			f((*iters)...);
		}
	}
};

// ENTITY MANAGER

class EntityManager
{
public:
	EntityManager() : last_id{0}
	{
	}

    EntityRef new_entity() 
    {
		entities.emplace_back();
		auto& e = entities.back();
		e.id = last_id++;
		return { e.id };
    }
private:
	uint64_t last_id;
	std::vector<Entity> entities;
};

// Components

#define COMPONENTID(x, store) static const uint32_t ID = x; using TSTORE = store;

class ComponentManager
{
public:
	ComponentManager() : data {64}
	{
	}

    template <typename T>
    void enable()
    {
		auto storage = new store_of<T>();
		data[T::ID] = { (BaseStore*)storage };
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

	template <typename T>
    store_of<T>& store()
    {
		return *((store_of<T>*) data[T::ID].addr);
	}

	template <typename T>
    T& set(EntityRef ref, const T& v)
    {
		auto& record = data[T::ID];
		auto store = (store_of<T>*)record.addr;
		return store->set(ref, v);
    }

	template <typename T>
	void singleton(const T& v)
	{
		auto& record = data[T::ID];
		auto store = (SingletonStore<T>*)record.addr;
		store->set(v);
	}
private:
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
};

struct BaseSystem
{
    virtual void update() = 0;
	void accept(EntityRef e) { entities.emplace(e); }
protected:
    std::set<EntityRef> entities;
	ComponentManager* components = nullptr;
};

template <typename... TArgs>
struct System : BaseSystem
{
public:
	using TYPE = ComponentList<TArgs...>;
	void set(ComponentManager& components)
	{
		BaseSystem::components = &components;
		(BaseSystem::components->enable<TArgs>(),...);
	}
protected:
	/*auto get() -> std::generator<decltype(components->values<TArgs...>({})) >
	{
		for (auto const& ref : entities)
        {
			co_yield components->values<TArgs...>(ref);
        }
	}*/
	
	template <typename F>
	void foreach(const F& f)
	{
		join::run(entities, f, ((store_of<TArgs>&)components->store<TArgs>())...);
	}
};

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
			auto is_accepted = (s.map & components) == components;
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

    template <typename T> T& get(EntityRef ref) { return std::get<0>(components.values<T>(ref)); }

    void update() { systems.update(); }

	//components
	template <typename T>
	Scene& enable() { components.enable<T>(); return *this; }

	template <typename T>
	Scene& set(const T& v)
	{
		components.singleton(v);
		return *this;
	}
	
	// systems
	template <typename T>
	Scene& add() { systems.add<T>(); return *this; }

    EntityManager entities;
    ComponentManager components;
    SystemManager systems;
};

struct DeltaTimeComponent
{
	COMPONENTID(0, SingletonStore<DeltaTimeComponent>);
	float dt;
};

struct PositionComponent
{
	COMPONENTID(1, DenseStorage<PositionComponent>);
	float x, y, z;
};

struct RigidBodyComponent
{
	COMPONENTID(2, DenseStorage<RigidBodyComponent>);
	float vx, vy, vz;
	float ax, ay, az;
};

struct EulerIntegratorSystem : System<DeltaTimeComponent, PositionComponent, RigidBodyComponent>
{
    virtual void update() override
    {
		foreach([](DeltaTimeComponent& time, PositionComponent& pos, RigidBodyComponent& rb) {
			pos.x += rb.vx * time.dt;
			pos.y += rb.vy;
			pos.z += rb.vz;

			rb.vx += rb.ax;
			rb.vy += rb.ay;
			rb.vz += rb.az;
		});
    }
};

int main()
{
    auto scene = Scene{};
	scene.add<EulerIntegratorSystem>();

	auto e1 = scene.build_entity()
		.set(PositionComponent{ 0, 0, 0 })
		.set(RigidBodyComponent{ 1, 0, 0, 0, 0, 0})
		.build();

	scene.set<DeltaTimeComponent>({ 0.016 });
    scene.update();

	auto& pos = scene.get<PositionComponent>(e1);
    return 0;
}