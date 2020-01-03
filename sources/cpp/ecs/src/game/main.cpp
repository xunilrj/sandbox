
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
#include <cstddef>
#include <stack>
#include <algorithm>
//namespace std { template <typename T> using generator = std::experimental::generator<T>; }
#define NOMINMAX
#include <Windows.h>

template <typename T>
struct deltaTime
{
	T delta;
	T desired;

	T freq;
	__int64 last;

	deltaTime(T desired, bool s)
		: desired{ desired },
		delta{ 0 },
		last{ 0 },
		freq{ 0 }
	{
		if (s) start();
	}

	void start()
	{
		LARGE_INTEGER c;
		QueryPerformanceCounter(&c);
		last = c.QuadPart;

		LARGE_INTEGER li;
		QueryPerformanceFrequency(&li);
		freq = T(li.QuadPart) / (T)1000.0;

		delta = 0;
	}

	T step()
	{
		LARGE_INTEGER c;
		QueryPerformanceCounter(&c);

		delta += T(c.QuadPart - last) / freq;
		last = c.QuadPart;

		T r = 0.0;
		if (delta > desired) {
			r = delta;
			delta = 0;
		}
		return r;
	}
};

// TASK SYSTEM

// Utils
#define CAT2(x, y) x ## y
#define CAT(x, y) CAT2(x,y)
#define RESULT(type, name, usingName) \
struct CAT(result,__LINE__) \
{ \
bool ok; \
type name; \
CAT(result,__LINE__)(type name) : ok {true}, name {name} {} \
CAT(result,__LINE__)(bool r) : ok {r} {} \
CAT(result,__LINE__)(bool r, type name) : ok {r}, name {name} {} \
operator type () const { return name; }\
operator type* () const { return &name; }\
operator type& () const { return name; }\
};\
using usingName = CAT(result,__LINE__);

#define RESULTP(type, name, capitalName, usingName) \
struct CAT(result,__LINE__) \
{ \
bool ok; \
type* name; \
CAT(result,__LINE__)(type* name) : ok {true}, name {name} {} \
CAT(result,__LINE__)(type& name) : ok {true}, name {&name} {} \
CAT(result,__LINE__)(bool r) : ok {r} {} \
CAT(result,__LINE__)(bool r, type* name) : ok {r}, name {name} {} \
CAT(result,__LINE__)(bool r, type& name) : ok {r}, name {&name} {} \
type& get ## capitalName() const { return *name; }\
operator type* () const { return name; }\
operator type& () const { return *name; }\
};\
using usingName = CAT(result,__LINE__);

#define autor(r, exp) auto r = exp; if(!r.ok) return {false};

#define NONCOPYABLE(TypeName) \
	TypeName(const TypeName&) = delete; \
	void operator=(const TypeName&) = delete

#define NONMOVEABLE(TypeName) \
	TypeName(TypeName&&) = delete; \
	TypeName& operator=(TypeName&&) = delete;

// Task System
// Task System
enum class TaskStatus
{
	ENABLED = 1,

	NOTIFY_START = 2,
	NOTIFY_END = 4,

	STARTED = 8,
};
DEFINE_ENUM_FLAG_OPERATORS(TaskStatus)

template <unsigned int TQTD>
struct Task
{
	Task() { }

	uint32_t id;
	TaskStatus status;

	uint8_t rendevouz;

	uint32_t then_size;
	uint32_t then[TQTD];

	int64_t duration;

	bool is_enabled() { return (status & TaskStatus::ENABLED) == TaskStatus::ENABLED; }
	bool is_started() { return (status & TaskStatus::STARTED) == TaskStatus::STARTED; }
	bool notify_start() { return (status & TaskStatus::NOTIFY_START) == TaskStatus::NOTIFY_START; }
	bool notify_end() { return (status & TaskStatus::NOTIFY_END) == TaskStatus::NOTIFY_END; }

	void enable(bool e) { if (e) enable(); else disable(); }
	void enable() { status |= TaskStatus::ENABLED; }
	void disable() { status &= ~TaskStatus::ENABLED; }
	void start() { status |= TaskStatus::STARTED; }
private:
	NONCOPYABLE(Task);
	NONMOVEABLE(Task);
};

// array

template <uint32_t MAX>
class array
{
	uint32_t count;
	uint32_t data[MAX];
public:
	array() : count{ 0 } { }

	uint32_t size() const { return count; }
	uint32_t operator[] (uint32_t i) const { return data[i]; }

	void clear() { count = 0; }
	bool push_back(uint32_t item)
	{
		auto i = count++;
		if (i > MAX) return false;

		data[i] = item;
		return true;
	}
};

template <unsigned int TASKQTD, unsigned int THENQTD>
class TaskSystem
{
	using TASK = Task<THENQTD>;
	using RUNRESULT = array<TASKQTD>;

	RESULTP(TASK, task, Task, TRESULT);

	uint64_t Now;
	uint32_t Current;
	array<TASKQTD> finished;
	array<TASKQTD> started;
	TASK Tasks[TASKQTD];

	void step(uint32_t currenti, uint64_t delta, uint32_t i)
	{
		auto& current = Tasks[i];
		if (current.id == 0 || !current.is_enabled()) return;

		if (!current.is_started())
		{
			current.start();
			if (current.notify_start()) started.push_back(current.id);
		}

		if (current.duration > 0)
			current.duration -= delta;

		if (current.duration <= 0)
		{
			kill_task(current);
			for (uint32_t i = 0; i < current.then_size; ++i)
			{
				auto then_id = current.then[i];
				auto& current_then = Tasks[then_id];

				--current_then.rendevouz;
				if (current_then.rendevouz == 0)
				{
					current_then.enable();

					if (current_then.duration > 0)
						current_then.duration += current.duration + delta;

					if (then_id < currenti)
						step(currenti, delta, then_id);
				}
			}
		}
	}

	TRESULT getFreeTask()
	{
		auto tries = TASKQTD * 2;
		while (tries > 0)
		{
			auto current = Current++;
			if (current >= TASKQTD)
				Current = current = 0;
			auto& t = Tasks[current];
			if (t.id == 0)
			{
				memset(&t, 0, sizeof(TASK));
				t.id = current + 1;
				return t;
			}

			--tries;
		}
		return { false };
	}

	void kill_task(TASK& t)
	{
		if (t.notify_end()) finished.push_back(t.id);

		Current = t.id - 1;
		t.id = 0;
		t.disable();
	}
public:
	TaskSystem() : Now{ 0 }, Current{ 0 }, finished{}, started{}
	{
		memset(&Tasks, 0, sizeof(Tasks));
	}

	TRESULT make_task(bool enabled = false)
	{
		autor(r, getFreeTask());

		auto& t = r.getTask();
		t.duration = 0;
		t.status = TaskStatus::NOTIFY_START | TaskStatus::NOTIFY_END;
		t.enable(enabled);

		return t;
	}

	TRESULT make_duration(unsigned int duration, bool enabled = false)
	{
		autor(r, getFreeTask());

		auto& t = r.getTask();
		t.duration = duration;
		t.status = TaskStatus::NOTIFY_START | TaskStatus::NOTIFY_END;
		t.enable(enabled);

		return t;
	}

	TASK& then(TASK& a, TASK& b) { return then(a.id, b.id); }
	TASK& then(unsigned int idA, unsigned int idB)
	{
		auto& t1 = Tasks[idA - 1];
		auto i = t1.then_size++;
		t1.then[i] = idB - 1;

		auto& t2 = Tasks[idB - 1];
		t2.disable();
		++t2.rendevouz;

		return t2;
	}
	template <typename... TArgs>
	TASK& when_all(TArgs... args)
	{
		auto t = make_task();
		(then(args, t), ...);

		return t;
	}

	TASK& sequence(std::initializer_list<uint32_t> list)
	{
		auto& t = make_task();

		uint32_t last;
		bool first = true;
		for (auto&& current : list)
		{
			if (!first)
				then(last, current);
			first = false;
			last = current;
		}

		then(last, t.id);
		return t;
	}

	TRESULT sequence(std::initializer_list<TASK*> list)
	{
		autor(r, make_task());
		auto& t = r.getTask();

		TASK* last = nullptr;
		bool first = true;
		for (auto current : list)
		{
			if (!first)
				then(last->id, current->id);
			first = false;
			last = current;
		}

		then(last->id, t.id);
		return t;
	}

	std::tuple<RUNRESULT&, RUNRESULT&> run(uint64_t delta)
	{
		Now += delta;

		finished.clear();
		started.clear();

		for (int i = 0; i < TASKQTD; ++i)
			step(i, delta, i);

		return { finished, started };
	}

	void enable(TASK& t) { enable(t.id); }
	void enable(uint32_t id)
	{
		auto& current = Tasks[id - 1];
		current.enable();
	}

	const TASK& operator [] (uint32_t id) const { return Tasks[id]; }
};

// ECS

class Scene;

// TYPE TRAITS
template <typename T> using store_of = typename T::TSTORE;
template <typename T> using element_of = typename T::TELEMENT;


struct Entity { uint64_t id; };
using EntityRef = uint64_t;

using ComponentRef = uint64_t;
using ComponentMap = std::bitset<64>;
template <typename T, typename R = void> using enable_if_component = std::enable_if_t<T::ID >= 0, R>;
template <typename T, typename R = void> using enable_if_resource = std::enable_if_t<T::RID >= 0, R>;

template <typename... TArgs>
struct ComponentList
{
public:
	static ComponentMap get_map()
	{
		ComponentMap map;
		
		auto ids = { id<TArgs>()... };
		for (auto&& x : ids)
		{
			if(x >= 0) map.set(x);
		}

		return map;
	}
private:
	template <typename T> static enable_if_component<T,int> id() { return T::ID; }
	template <typename T> static enable_if_resource<T,int> id() { return -1; }
};

// STORES

class BaseStore
{
public:
	virtual void new_entity(EntityRef ref) = 0;
};

template <typename T>
class DenseStore : BaseStore
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

	T& get()
	{
		return data;
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
#define RESOURCE(x, t) static const uint32_t RID = x; using TSTORE = SingletonStore<t>;

using copy_function = void(*)(void*,void*);
struct CopyRecord
{
	void* src;
	void* dst;
	copy_function f;
};

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
		join::run(entities, f, store<TArgs>()...);
	}
private:
	template <typename T> store_of<T>& store() { return components->store<T>(); }
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

template <typename T>
struct AnimRecord
{
	bool enabled;
	T* data;
	void* dst;
	copy_function f;
	Task<100>* when_finished;
};

struct InterpolationLerp
{
	static const uint32_t ID = 0;

	float start;
	float end;
	float duration;

	float t;
	float value;
};


template <typename TContainer, typename TKey, typename F>
void use_remove(TContainer& container, const TKey& key, const F& f)
{
	auto it = container.find(key);
	if (it == container.end()) return;

	// use
	f(it->second);
	// remove
	container.erase(it);
}

template <typename TContainer, typename TKey, typename TValue>
TValue move_remove(TContainer& container, const TKey& key)
{
	auto it = container.find(key);
	if (it == container.end()) return;

	// move
	TValue v = std::move(it->second);
	//remove
	container.erase(it);

	return v;
}

class AnimManager
{
public:
	void schedule(uint32_t id, uint32_t type, void* data, bool enabled, Task<100>* t,
		void* dst, copy_function f)
	{
		if (type == InterpolationLerp::ID)
		{
			auto d = new AnimRecord<InterpolationLerp>{ enabled, (InterpolationLerp*)data, dst, f, t };
			waiting.insert({ id, {type, (void*)d} });
		}
	}

	void start(uint32_t id)
	{
		use_remove(waiting, id, [&](auto&& record) {
			if (record.type == InterpolationLerp::ID) {
				lerps.push_back(*(AnimRecord<InterpolationLerp>*)(record.data));
			}
			delete record.data;
		});
	}

	void update(float dt)
	{
		run_lerps(dt);
	}

	static InterpolationLerp lerp(float duration, float start, float end)
	{
		return { start, end, duration, 0, start };
	}
private:
	template<typename T> static void copy_impl(void* src, void* dst) { *(T*)dst = *(T*)src; }

	template<class T, typename U>
	std::ptrdiff_t member_offset(U T::* member)
	{
		return reinterpret_cast<std::ptrdiff_t>(
			&(reinterpret_cast<T const volatile*>(NULL)->*member)
			);
	}

	void run_lerps(float dt)
	{
		for (auto&& r : lerps)
		{
			auto& l = *r.data;

			if (!r.enabled) continue;
			if (!r.when_finished->is_enabled())
			{
				if (r.when_finished->duration <= 0) {
					l.t = l.duration;
					l.value = l.end;
					r.f(&l.value, r.dst);
					r.enabled = false;
				}
			}
			else {
				l.t = l.duration - (r.when_finished->duration / 1000.0f);
				l.value = l.start + (l.t * ((l.end - l.start) / l.duration));
				r.f(&l.value, r.dst);
			}
		}
		auto it = std::remove_if(lerps.begin(), lerps.end(), [](auto&& l) {
			return !l.enabled;
		});
		lerps.erase(it, lerps.end());
	}
	std::vector<AnimRecord<InterpolationLerp>> lerps;

	struct BuildRecord
	{
		uint32_t type;
		void* data;
	};
	std::map<uint32_t, BuildRecord> waiting;
};

class AnimBuilder
{
public:
	AnimBuilder()
	{
		stack.push(&root);
	}

	template <typename T>
	AnimBuilder& then(const T& data, bool push = true)
	{
		auto* current = stack.top();
		auto* d = new T{};
		*d = data;

		current->children.push_back({ T::ID, data.duration, d, {}});
		if (push) stack.push(&current->children.back());

		return *this;
	}

	AnimBuilder& pop() { stack.pop(); return *this; }

	Task<100>& start(TaskSystem<100, 100>& tsys, AnimManager& anims, 
		void* dst, copy_function f,
		Task<100>* after = nullptr)
	{
		if (after == nullptr)
		{
			auto t = tsys.make_task(true);
			after = t.task;
		}
		return start_impl(tsys, anims, root, *after, dst, f);
	}
private:
	struct AnimNode
	{
		uint32_t type;
		float duration;
		void* data;
		std::vector<AnimNode> children;
	};

	Task<100>& start_impl(TaskSystem<100, 100>& tsys, AnimManager& anims, 
		AnimNode& cur, Task<100>& tcur,
		void* dst, copy_function f)
	{
		auto tall = tsys.make_task();
		tsys.then(tcur, tall);
		for (auto& child : cur.children)
		{
			auto tchild = tsys.make_duration((uint64_t)(child.duration * 1000.0f));
			tsys.then(tcur, tchild);
			tsys.then(tchild, tall);
			anims.schedule(tchild.task->id, child.type, child.data, true, tchild.task, dst, f);

			auto& tgrand_child = start_impl(tsys, anims, child, tchild, dst, f);
			tsys.then(tgrand_child, tall);
		}
		return tall;
	}
	
	AnimNode root;
	std::stack<AnimNode*> stack;
};

// OOB Resources

struct DeltaTimeComponent
{
	RESOURCE(0, DeltaTimeComponent);
	float dt;
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



// EXAMPLE 1

struct PositionComponent
{
	COMPONENTID(1, DenseStore<PositionComponent>);
	float x, y, z;
};

struct RigidBodyComponent
{
	COMPONENTID(2, DenseStore<RigidBodyComponent>);
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

#include <iostream>
#include <iomanip>

bool input_enabled = true;
void enable_input(Scene& s) { input_enabled = true; }
void disable_input(Scene& s) { input_enabled = false; }

int main()
{
    auto scene = Scene{};
	scene.add(0, disable_input);
	scene.add(1, enable_input);

	//TODO InputSystem
	//TODO System Order (Serial, Parallel)
	//TODO Rendering
	//TODO Animation
	scene
		.add<EulerIntegratorSystem>();
	//auto anim = scene.add<AnimationSystem>(); //TODO

	//TODO entity serialization
	auto e1 = scene.build_entity()
		.set(PositionComponent{ 0, 0, 0 })
		.set(RigidBodyComponent{ 1, 0, 0, 0, 0, 0})
		.build();

	auto e2 = scene.build_entity()
		.set(PositionComponent{ 0, 0, 0 })
		.build();

	auto anim1 = scene.build_anim()
		.then(AnimManager::lerp(1, 0, 1))
		.then(AnimManager::lerp(2, 1, 0.5));
	auto anim2 = scene.build_anim()
		.then(AnimManager::lerp(1, 0, 1));
	
	
	/*scene.components.build_copier(
		e2, &InterpolationComponent::last,
		e2, &PositionComponent::x
	);*/

	auto hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//deltaTime return uint64_t milliseconds
	deltaTime<double> dt{ 16, true };
	while (true)
	{
		auto elapsed = dt.step();
		if (elapsed > 0) {
			
			auto& int2 = scene.get<PositionComponent>(e2);

			if (input_enabled && (GetKeyState('A') & 0x8000))
			{
				scene.call(0);
				auto& anim_i1 = scene.start(anim1, e2, &PositionComponent::x);
				auto& anim_i2 = scene.start_after(anim2, e2, &PositionComponent::y, &anim_i1);
				scene.call(1, &anim_i2);
			}

			scene.set<DeltaTimeComponent>({ (float)(elapsed / 1000.0) });
			scene.update();

			std::cout.precision(4);
			std::cout << std::setw(4) << int2.x << " " << std::setw(4) << int2.y << std::endl;
		}
	}
    return 0;
}