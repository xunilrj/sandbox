#pragma once

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

#include <TaskSystem/TaskSystem.h>

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

#include "./Components/ComponentList.h"

// STORES

#include "./Stores/BaseStore.h"
#include "./Stores/DenseStore.h"
#include "./Stores/SingletonStore.h"

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

#include "EntityManager.h"

// Components
#include "./Components/Components.h"

using copy_function = void(*)(void*,void*);
struct CopyRecord
{
	void* src;
	void* dst;
	copy_function f;
};

#include "ComponentManager.h"

#include "./Systems/BaseSystem.h"
#include "./Systems/System.h"
#include "./Systems/SystemManager.h"

#include "./Entities/EntityBuilder.h"


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

#include "Scene.h"