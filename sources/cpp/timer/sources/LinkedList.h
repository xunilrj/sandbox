
template <typename T, typename TAlloc>
class LinkedList : protected TAlloc
{
public:
	LinkedList() : Head{nullptr} {}

	template<typename... TArgs>
	void emplace_front(TArgs... args)
	{
		this->Head = TAlloc::template alloc<Node>(
			this->Head,
			std::forward<TArgs>(args)...);
	}

	template <typename F1>
	ui32 removeIf(F1 f) { return removeIf(f, LAMBDA{});	}

	template <typename F1, typename F2>
	ui32 removeIf(F1 f, F2 onBeforeDelete)
	{
		ui32 deleted = 0;
		auto previous = this->Head;
		auto current = this->Head;
		while (current != nullptr)
		{
			auto& value = current->value;
			if (auto r = f(value); r) {
				onBeforeDelete(value);
				deleteCurrent(previous, current);
				++deleted;
			}
			else 
				previous = current;
			current = previous->next;
		}

		return deleted;
	}
private:
	struct Node
	{
		Node* next;
		T value;

		template<typename... TArgs>
		Node(Node* next, TArgs&&... args) :
			next{next},
			value{std::forward<TArgs>(args)...}
		{

		}
	};

	Node* Head;

	void deleteCurrent(Node*& previous, Node *& current)
	{
		auto next = current->next;		
		auto isHead = Head == current;
		TAlloc::del(current);
		current = nullptr;

		if (isHead)
			Head = nullptr;
		else
			previous->next = next;
	}
};