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