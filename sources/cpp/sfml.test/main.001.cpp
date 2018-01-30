#include <vector>
#include <array>
#include <algorithm>
#include <functional>
#include <iostream>
#include "plf_colony.h"
#include <SFML/Graphics.hpp>

template <typename T>
std::vector<T>& operator << (std::vector<T>& vector, const T& item)
{
	vector.push_back(item);
	return vector;
}

template <typename T>
class Stream
{
public:
	~Stream()
	{
		if (Type == 2) delete Function;
	}

	Stream & operator = (std::vector<T>& vector)
	{
		Type = 3;
		Vector = &vector;
		return *this;
	}

	Stream& operator = (std::function<void(const T&)>& f)
	{
		Type = 1;
		Function = &f;
		return *this;
	}

	Stream& operator = (std::function<void(const T&)>&& f)
	{
		Type = 2;
		Function = new std::function<void(const T&)>(f);
		return *this;
	}

	Stream& operator << (const T& item)
	{
		switch (Type)
		{
		case 1:
		case 2: {
			(*Function)(item);
			break;
		}
		case 3: {
			(*Vector) << item;
			break;
		}
		default:
			break;
		}
		return *this;
	}
private:
	char Type;
	union {
		std::vector<T>* Vector;
		std::function<void(const T&)>* Function;
		void* Value;
	};
};


template <typename T>
class EventHandler
{
protected:
	void Handle(const T& before, const T& after) const
	{
		std::cout << "Before: " << before << " after: " << after << std::endl;
	}
};
template <typename T> class DoNothingBeforePolicy : protected EventHandler<T> {};
template <typename T> class DoNothingAfterPolicy : protected EventHandler<T> {};

template <typename T> class NotifyAfterPolicy
{
public:
	virtual ~NotifyAfterPolicy()
	{
	}

	template <typename TContainer>
	void operator >> (TContainer& container)
	{
		Out = container;
	}
	void operator >> (std::function<void(const T&)>& f)
	{
		Out = f;
	}
	void operator >> (std::function<void(const T&)>&& f)
	{
		Out = std::move(f);
	}
protected:
	void Handle(const T& before, const T& after)
	{
		std::cout << "Propagating Before: " << before << " after: " << after << std::endl;
		Out << after;
	}
private:
	Stream<T> Out;
};

class Object 
{
};

struct ObjectP
{
	Object * Value;
};

template <typename T,
	typename TBeforeChange = DoNothingBeforePolicy<T>,
	typename TAfterChange = DoNothingAfterPolicy<T>>
	class Cell : public TBeforeChange, public TAfterChange, public Object
{
	using TComplete = Cell<T, TBeforeChange, TAfterChange>;
public:
	Cell() : Value()
	{
	}
	Cell(const T& value) : Value(value)
	{
	}

	Cell(TComplete&& other) {
		std::swap(this->Value, other.Value);
	}

	~Cell()
	{

	}

	TComplete& operator = (const T& value)
	{
		TBeforeChange::Handle(Value, value);
		auto oldValue = Value;
		Value = value;
		TAfterChange::Handle(oldValue, Value);

		return *this;
	}

	//Not copyable
	Cell(const TComplete&) = delete;
	TComplete& operator=(const TComplete&) = delete;
private:
	T Value;
};

template <typename T> using Cell_After = Cell<T, DoNothingBeforePolicy<T>, NotifyAfterPolicy<T>>;

class CellContainer
{
	template <typename T> using TReactiveCell = Cell_After<T>;
public:
	template <typename T>
	TReactiveCell<T>& new_reactive_cell()
	{
		auto& objp = *Cells.emplace();
		objp.Value = new TReactiveCell<T>();
		return *(TReactiveCell<T>*)objp.Value;
	}
private:
	//TODO: think a better alternative to different objects in this container
	plf::colony<ObjectP> Cells;
};

int main()
{
	sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
	sf::CircleShape shape(100.f);
	shape.setFillColor(sf::Color::Green);


	auto container = CellContainer();

	auto& radius = container.new_reactive_cell<float>();
	radius >> [&](auto value) {
		shape.setRadius(value);
	};
	auto& fromStart = container.new_reactive_cell<float>();
	fromStart >> [&](auto value) {
		radius = (std::sin(value) + 1.0f) * 50.0f;
	};
	auto& elapsed = container.new_reactive_cell<float>();

	sf::Clock start;
	while (window.isOpen())
	{
		sf::Event event;
		sf::Clock clock;

		while (window.pollEvent(event))
		{
			if (event.type == sf::Event::Closed)
			{
				window.close();
			}
		}

		sf::Time elapsedTimeFromStart = start.getElapsedTime();
		sf::Time elapsedTimeFromLastFrame = clock.restart();
		
		fromStart = elapsedTimeFromStart.asSeconds();
		elapsed = elapsedTimeFromLastFrame.asSeconds();

		//kernel.update(fromStart, elapsed);

		window.clear();
		window.draw(shape);
		window.display();
	}

	return 0;
}