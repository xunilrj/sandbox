#include <chrono>

template <typename T>
struct deltaTime
{
	using clock_type = std::chrono::high_resolution_clock;
	using instant = clock_type::time_point;

	T delta;
	T desired;

	instant last;

	deltaTime(T desired, bool start_now)
		: desired{ desired },
		delta{ 0 }
	{
		if (start_now) start();
	}

	deltaTime(bool start_now)
		: desired{ 0 },
		delta{ 0 }
	{
		if (start_now) start();
	}

	void start()
	{
		last = clock_type::now();
		delta = 0;
	}

	T elapsed()
	{
		auto now = clock_type::now();
		auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now-last).count();
		return T(1e-3) * elapsed;
	}

	T step()
	{
		auto now = clock_type::now();
		auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now-last).count();
		delta += T(1e-3) * elapsed;
		last = now;

		T r = 0.0;
		if (delta > desired) {
			r = delta;
			delta = 0;
		}
		return r;
	}
};
