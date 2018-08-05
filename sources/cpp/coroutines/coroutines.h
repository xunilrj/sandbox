#ifndef _MA_COROUTINES_H
#define _MA_COROUTINES_H

#include "type_manipulation.h"
#include "allocators.h"

namespace ma
{
template <typename T>
struct identity
{
	typedef T type;
};

template <class T>
inline T &&forward(typename remove_reference<T>::type &t) noexcept
{
	return static_cast<T &&>(t);
}

template <class T>
inline T &&forward(typename remove_reference<T>::type &&t) noexcept
{
	return static_cast<T &&>(t);
}

class cocontinuation
{
  public:
	cocontinuation(bool c) : result(c) {}
	bool result;
};

template <typename T>
class costate
{
  public:
	costate(const T &args) : _state(0), state(args)
	{
	}

	template <typename... TArgs>
	costate(TArgs &&... args) : _state(0), state({forward<TArgs>(args)...})
	{
	}

	int _state;
	T state;
	bool isFinished(int i)
	{
		return true;
	}
};

template <typename T, typename... TArgs>
costate<T> &make_costate(void *ptr, TArgs &&... args)
{
	return *(new (ptr) costate<T>(forward<TArgs>(args)...));
}

#define START_COROUTINE  \
	switch (args._state) \
	{                    \
	case 0:
#define END_COROUTINE \
	}                 \
	args._state = -1; \
	return {false};
#define YIELD               \
	args._state = __LINE__; \
	return {true};          \
	case __LINE__:
#define ARGS args.state
#define AWAIT_IMPL(N, x)    \
	args._state = __LINE__; \
	return {true};          \
	case __LINE__:          \
		if (!(x))           \
			return { true }
#define AWAIT(x) AWAIT_IMPL(__LINE__, x)

typedef cocontinuation (*FUNCPTR)(void *);

struct coroutine
{
	FUNCPTR function;
	ma::Block state;

	cocontinuation step()
	{
		return function(state.Pointer);
	}
};

template <typename T>
struct comakeresult
{
	int index;
	coroutine coroutine;

	T &args() const
	{
		auto &cos = *((costate<T> *)coroutine.state.Pointer);
		return cos.state;
	}
	bool isOk() const { return index >= 0; }
};

template <typename TBuffer, unsigned int SIZE = 1024>
class CoManager
{
	int pos;
	bool CoroutinesFree[SIZE];
	coroutine Coroutines[SIZE];
	TBuffer *Buffer;

  public:
	CoManager() : pos(0), Buffer(nullptr)
	{
		for (int i = 0; i < SIZE; ++i)
			CoroutinesFree[i] = true;
	}
	CoManager(TBuffer *buffer) : pos(0), Buffer(buffer)
	{
		for (int i = 0; i < SIZE; ++i)
			CoroutinesFree[i] = true;
	}

	void setBuffer(TBuffer *buffer) { Buffer = buffer; }

	template <typename F>
	using TFArg0 = unqualified_first_argument_of<F>;
	template <typename F>
	using TFArgs0Template = first_template_parameter_of<TFArg0<F>>;

	template <typename F, typename... TArgs>
	comakeresult<TFArgs0Template<F>>
	make(F f, TArgs &&... args)
	{
		auto sizeargs = sizeof(TFArg0<F>);
		auto blk = Buffer->allocate(sizeargs);

		auto &costate = make_costate<
			TFArgs0Template<F>,
			TArgs...>(blk.Pointer, forward<TArgs>(args)...);

		//find free slot
		int stopAt = (pos - 1) % SIZE;
		while (!CoroutinesFree[pos] && pos != stopAt)
		{
			pos = (pos + 1) % SIZE;
		}
		if (!CoroutinesFree[pos])
			return {-1, coroutine{nullptr, Block::Null}};

		auto oldpos = pos;
		auto &coroutine = Coroutines[oldpos];
		coroutine.function = (FUNCPTR)f;
		coroutine.state = blk;

		CoroutinesFree[oldpos] = false;

		++pos;
		return {oldpos, coroutine};
	}

	void free(int id)
	{
		if (id < 0)
			return;
		if (id >= SIZE)
			return;

		auto &coroutine = Coroutines[id];
		Buffer->deallocate(coroutine.state);

		coroutine.function = nullptr;
		coroutine.state = ma::Block::Null;

		CoroutinesFree[id] = true;
		pos = id;
	}

	cocontinuation step(int id)
	{
		if (id < 0)
			return {false};
		if (id >= 1024)
			return {false};

		auto &coroutine = Coroutines[id];
		return coroutine.step();
	}
};
} // namespace ma

#endif
