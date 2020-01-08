#pragma once

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

	deltaTime(bool s)
		: delta{ 0 },
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

	T elapsed()
	{
		LARGE_INTEGER c;
		QueryPerformanceCounter(&c);

		return T(c.QuadPart - last) / freq;
	}
};