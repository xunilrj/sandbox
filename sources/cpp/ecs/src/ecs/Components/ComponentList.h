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