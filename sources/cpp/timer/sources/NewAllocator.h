class NewAllocator
{
protected:
	template <typename T, typename... TArgs>
	T* alloc(TArgs&&... args)
	{
		return ::new T{std::forward<TArgs>(args)...};
	}

	template <typename T>
	void del(T* item)
	{
		delete item;
	}
};