// ConsoleApplication2.cpp : Defines the entry point for the console application.
//

#include <iostream>
#include <future>
#include <chrono>
#include <experimental\coroutine>
#include <ppltasks.h>
using namespace std::chrono_literals;

template <typename T>
class Property
{
public:
	Property(T& value) : Value(value)
	{
	}

	operator T& () noexcept	{return Value;}
	operator T& () const noexcept	{return Value;}
private:
	T & Value;
};

//class async_manual_reset_event
//{
//public:
//
//	async_manual_reset_event(bool initiallySet = false) noexcept;
//
//	// No copying/moving
//	async_manual_reset_event(const async_manual_reset_event&) = delete;
//	async_manual_reset_event(async_manual_reset_event&&) = delete;
//	async_manual_reset_event& operator=(const async_manual_reset_event&) = delete;
//	async_manual_reset_event& operator=(async_manual_reset_event&&) = delete;
//
//	bool is_set() const noexcept;
//
//	struct awaiter;
//	awaiter operator co_await() const noexcept;
//
//	void set() noexcept;
//	void reset() noexcept;
//
//private:
//
//	friend struct awaiter;
//
//	// - 'this' => set state
//	// - otherwise => not set, head of linked list of awaiter*.
//	mutable std::atomic<void*> m_state;
//
//};
//
//struct async_manual_reset_event::awaiter
//{
//	awaiter(const async_manual_reset_event& event) noexcept
//		: m_event(event)
//	{}
//
//	bool await_ready() const noexcept;
//	bool await_suspend(std::experimental::coroutine_handle<> awaitingCoroutine) noexcept;
//	void await_resume() noexcept {}
//
//private:
//	friend class async_manual_reset_event;
//
//	const async_manual_reset_event& m_event;
//	std::experimental::coroutine_handle<> m_awaitingCoroutine;
//	awaiter* m_next;
//};
//
//bool async_manual_reset_event::awaiter::await_ready() const noexcept
//{
//	return m_event.is_set();
//}
//
//bool async_manual_reset_event::awaiter::await_suspend(
//	std::experimental::coroutine_handle<> awaitingCoroutine) noexcept
//{
//	// Special m_state value that indicates the event is in the 'set' state.
//	const void* const setState = &m_event;
//
//	// Remember the handle of the awaiting coroutine.
//	m_awaitingCoroutine = awaitingCoroutine;
//
//	// Try to atomically push this awaiter onto the front of the list.
//	void* oldValue = m_event.m_state.load(std::memory_order_acquire);
//	do
//	{
//		// Resume immediately if already in 'set' state.
//		if (oldValue == setState) return false;
//
//		// Update linked list to point at current head.
//		m_next = static_cast<awaiter*>(oldValue);
//
//		// Finally, try to swap the old list head, inserting this awaiter
//		// as the new list head.
//	} while (!m_event.m_state.compare_exchange_weak(
//		oldValue,
//		this,
//		std::memory_order_release,
//		std::memory_order_acquire));
//
//	// Successfully enqueued. Remain suspended.
//	return true;
//}
//
//async_manual_reset_event::async_manual_reset_event(
//	bool initiallySet) noexcept
//	: m_state(initiallySet ? this : nullptr)
//{}
//
//bool async_manual_reset_event::is_set() const noexcept
//{
//	return m_state.load(std::memory_order_acquire) == this;
//}
//
//void async_manual_reset_event::reset() noexcept
//{
//	void* oldValue = this;
//	m_state.compare_exchange_strong(oldValue, nullptr, std::memory_order_acquire);
//}
//
//void async_manual_reset_event::set() noexcept
//{
//	// Needs to be 'release' so that subsequent 'co_await' has
//	// visibility of our prior writes.
//	// Needs to be 'acquire' so that we have visibility of prior
//	// writes by awaiting coroutines.
//	void* oldValue = m_state.exchange(this, std::memory_order_acq_rel);
//	if (oldValue != this)
//	{
//		// Wasn't already in 'set' state.
//		// Treat old value as head of a linked-list of waiters
//		// which we have now acquired and need to resume.
//		auto* waiters = static_cast<awaiter*>(oldValue);
//		while (waiters != nullptr)
//		{
//			// Read m_next before resuming the coroutine as resuming
//			// the coroutine will likely destroy the awaiter object.
//			auto* next = waiters->m_next;
//			waiters->m_awaitingCoroutine.resume();
//			waiters = next;
//		}
//	}
//}
//
//async_manual_reset_event::awaiter
//async_manual_reset_event::operator co_await() const noexcept
//{
//	return awaiter{ *this };
//}

//same as std::experimental::suspend_never
class async_immediate
{
public:
	struct awaiter
	{
		bool await_ready() const noexcept { return true; }
		bool await_suspend(std::experimental::coroutine_handle<> awaitingCoroutine) noexcept { return true; }
		void await_resume() const noexcept { }
	};
	awaiter operator co_await () const noexcept
	{
		return awaiter{};
	}
};

//same as std::experimental::suspend_never
class async_never
{
public:
	struct awaiter
	{
		bool await_ready() const noexcept { return false; }
		bool await_suspend(std::experimental::coroutine_handle<> awaitingCoroutine) noexcept { return true; }
		void await_resume() const noexcept { }
	};
	awaiter operator co_await () const noexcept
	{
		return awaiter{};
	}
};

class async_manual_reset_event_awaiter;

class async_manual_reset_event_setter
{
public:
	async_manual_reset_event_setter(std::atomic<bool>* flag, async_manual_reset_event_awaiter* awaiter) : Flag(flag), Awaiter(awaiter)
	{
	}

	void set() noexcept;
private:
	std::atomic<bool>* Flag;
	async_manual_reset_event_awaiter* Awaiter;
};

class async_manual_reset_event_awaiter
{
public:
	async_manual_reset_event_awaiter(std::atomic<bool>* flag) : Flag(flag)
	{
	}

	struct awaiter;
	awaiter operator co_await() noexcept;
private:
	friend struct awaiter;
	friend class async_manual_reset_event_setter;


	void resume()
	{
	}

	std::atomic<bool>* Flag;
};

void async_manual_reset_event_setter::set() noexcept
{
	Flag->store(true);
	Awaiter->resume();
}

struct async_manual_reset_event_awaiter::awaiter
{
	awaiter(async_manual_reset_event_awaiter& awaiter) : Awaiter(awaiter)
	{
	}

	bool await_ready() const noexcept
	{
		return Awaiter.Flag->load();
	}

	bool await_suspend(std::experimental::coroutine_handle<> awaitingCoroutine) noexcept
	{
		return true;
	}

	void await_resume()
	{
	}
private:
	void resume()
	{

	}

	async_manual_reset_event_awaiter & Awaiter;
};

inline async_manual_reset_event_awaiter::awaiter async_manual_reset_event_awaiter::operator co_await() noexcept
{
	return { *this };
}

class async_manual_reset_event
{
public:
	async_manual_reset_event() :
		Flag(false),
		awaiter(&Flag),
		setter(&Flag, &awaiter),
		Setter(setter),
		Awaiter(awaiter)
	{
	}

	void set() noexcept
	{
		Flag.store(true);

	}

	Property<async_manual_reset_event_setter> Setter;
	Property<async_manual_reset_event_awaiter> Awaiter;
private:
	std::atomic<bool> Flag;
	async_manual_reset_event_setter setter;
	async_manual_reset_event_awaiter awaiter;
};

int value;
//async_manual_reset_event event;

// A single call to produce a value
void producer(async_manual_reset_event_setter& signal)
{
	std::this_thread::sleep_for(2s);
	//value = some_long_running_computation();
	value = 42;

	// Publish the value by setting the event.
	signal.set();
}

// Supports multiple concurrent consumers
std::future<void> consumer(async_manual_reset_event_awaiter& event)
{
	int x = 10;
	// Wait until the event is signalled by call to event.set()
	// in the producer() function.
	co_await event;

	// Now it's safe to consume 'value'
	// This is guaranteed to 'happen after' assignment to 'value'
	std::cout << value + x << std::endl;
}

int main()
{
	async_manual_reset_event master;
	std::thread t1([&]() {
		producer(master.Setter);
	});

	std::thread t2([&]() {
		consumer(master.Awaiter).get();
	});

	//t1.join();
	t2.join();
	return 0;
}


