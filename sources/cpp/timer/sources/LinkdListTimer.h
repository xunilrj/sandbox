// Non Ordered linked-list
// implementation simplest
// add - o(1)
// remove - o(n)
// tick - o(n)
// TAOCP	- page 238 INFORMATION STRUCTURES 
//		2.2. LINEAR LISTS
//		2.2.1. Stacks, Queues, and Deques 
//		2.2.3. Linked Allocation 
template<typename TAlloc>
class LinkedListTimer
{
	struct TimerItem
	{
		ui32 id;
		ui64 when;
	};
	ui32 Id;
	LinkedList<TimerItem, TAlloc> List;
	std::vector<ui32> expired;
public:
	LinkedListTimer() : Id{0}, List{}
	{
	}

	ui32 add(ui64 when)
	{
		auto currentId = Id++;
		List.emplace_front(currentId, when);
		return currentId;
	}

	bool remove(ui32 id)
	{
		auto qtd = List.removeIf(LAMBDA{ return x.id == id; });
		return qtd > 0;
	}

	const std::vector<ui32>& tick(ui64 now)
	{
		this->expired.clear();
		List.removeIf(
			LAMBDA{ return x.when <= now; },
			LAMBDA{ this->expired.push_back(x.id); });
		return expired;
	}
};