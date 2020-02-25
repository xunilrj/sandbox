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