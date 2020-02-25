template <typename... TArgs>
struct System : BaseSystem
{
public:
	using TYPE = ComponentList<TArgs...>;
	void set(ComponentManager& components)
	{
		BaseSystem::components = &components;
		(BaseSystem::components->enable<TArgs>(),...);
	}
protected:
	/*auto get() -> std::generator<decltype(components->values<TArgs...>({})) >
	{
		for (auto const& ref : entities)
        {
			co_yield components->values<TArgs...>(ref);
        }
	}*/
	
	template <typename F>
	void foreach(const F& f)
	{
		join::run(entities, f, store<TArgs>()...);
	}
private:
	template <typename T> store_of<T>& store() { return components->store<T>(); }
};