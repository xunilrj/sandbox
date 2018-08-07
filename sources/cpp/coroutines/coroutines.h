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
	costate(int id, const T &args) : _id(id), _state(0), state(args)
	{
	}

	template <typename... TArgs>
	costate(int id, TArgs &&... args) : _id(id), _state(0), state({forward<TArgs>(args)...})
	{
	}

	int _id;
	int _state;
	T state;

	inline int id() const { return _id; }
	bool isFinished(int i)
	{
		return _state == -1;
	}
};

template <typename T, typename... TArgs>
costate<T> &make_costate(int id, void *ptr, TArgs &&... args)
{
	return *(new (ptr) costate<T>(id, forward<TArgs>(args)...));
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

	int id() const { return *((int *)state.Pointer); }
	cocontinuation step()
	{
		return function(state.Pointer);
	}
};

template <typename T>
struct comakeresult
{
	coroutine coroutine;

	int id() const { return coroutine.id(); }
	T &args() const
	{
		auto &cos = *((costate<T> *)coroutine.state.Pointer);
		return cos.state;
	}
	bool isOk() const { return coroutine.id() >= 0; }
};

void *memset(void *ptr, int value, size_t num)
{
	auto ptr2 = (char *)ptr;
	while (num > 0)
	{
		(*ptr2) = value;
		++ptr2;
		num--;
	}
	return ptr;
}

template <typename TBuffer, unsigned int SIZE = 1024>
class CoManager
{
	int pos;
	bool CoroutinesBusy[SIZE];
	coroutine Coroutines[SIZE];
	TBuffer *Buffer;

  public:
	CoManager() : pos(0), Buffer(nullptr), CoroutinesBusy{0}
	{
	}
	CoManager(TBuffer *buffer) : pos(0), Buffer(buffer), CoroutinesBusy{0}
	{
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
		//find free slot - bitmap would be faster?
		int stopAt = (pos - 1) % SIZE;
		while (CoroutinesBusy[pos] && pos != stopAt)
			pos = (pos + 1) % SIZE;
		if (CoroutinesBusy[pos])
			return {coroutine{nullptr, Block::Null}};

		auto sizeargs = sizeof(TFArg0<F>);
		auto blk = Buffer->allocate(sizeargs);

		auto &costate = make_costate<
			TFArgs0Template<F>,
			TArgs...>(pos, blk.Pointer, forward<TArgs>(args)...);

		auto oldpos = pos;
		auto &coroutine = Coroutines[oldpos];
		coroutine.function = (FUNCPTR)f;
		coroutine.state = blk;

		CoroutinesBusy[oldpos] = true;

		++pos;
		return {coroutine};
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

		CoroutinesBusy[id] = false;
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
