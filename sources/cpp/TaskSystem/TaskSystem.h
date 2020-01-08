#pragma once

#include <iostream>
#include <initializer_list>
#include <Windows.h>

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