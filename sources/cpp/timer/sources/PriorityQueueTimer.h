template<typename TAlloc>
class PriorityQueueTimer
{
	struct TimerItem
	{
		ui32 id;
		ui64 when;
	};
	ui32 Id;
	NodeBinaryHeap<TimerItem, TAlloc> Heap;
	std::vector<ui32> expired;
public:
	PriorityQueueTimer() : Id{0}, Heap{} { }

	ui32 add(ui64 when)
	{
		auto currentId = Id++;
		Heap.emplace(when, currentId, when);
		return currentId;
	}

	bool remove(ui32 id) { return Heap.remove(id); }

	const std::vector<ui32>& tick(ui64 now)
	{
		this->expired.clear();

		while (true)
		{
			if (Heap.size() == 0) break;

			auto data = Heap.peek();
			if (data.when <= now)
			{
				expired.push_back(data.id);
				Heap.pop();
			}
			else break;
		}

		return expired;
	}
};