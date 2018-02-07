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
	std::shared_ptr<Object> Value;
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

	template <typename TOperand>
	TComplete& operator += (TOperand value)
	{
		*this = Value + value;
		return *this;
	}

	template<typename TCast>
	operator TCast() const
	{
		return (TCast)Value;
	}

	//Not copyable
	Cell(const TComplete&) = delete;
	TComplete& operator=(const TComplete&) = delete;
private:
	T Value;
};

template <typename T> using Cell_After = Cell<T, DoNothingBeforePolicy<T>, NotifyAfterPolicy<T>>;
template <typename T> using TReactiveCell = Cell_After<T>;
using cellf = TReactiveCell<float>;
using cellb = TReactiveCell<bool>;

class CellContainer
{
public:
	template <typename T>
	TReactiveCell<T>& new_reactive_cell()
	{
		auto& objp = *Cells.emplace();
		objp.Value = std::make_shared<TReactiveCell<T>>();
		return *(TReactiveCell<T>*)objp.Value.get();
	}
private:
	//TODO: think a better alternative to different objects in this container
	plf::colony<ObjectP> Cells;
};

class IO
{
public:
	virtual void process()
	{
	}
};

class Clock
{
public:
	Clock() :
		Container(),
		Tick(Container.new_reactive_cell<unsigned long long>()),
		FromStart(Container.new_reactive_cell<float>())
	{
	}

	void Init()
	{
		Start.restart();
	}

	void Update()
	{
		//Tick += 1;
		FromStart = Start.getElapsedTime().asSeconds();
	}
public:
	TReactiveCell<unsigned long long>& Tick;
	TReactiveCell<float>& FromStart;
private:
	sf::Clock Start;
	CellContainer Container;
};

class Model
{
public:
	Model() :
		Container(),
		Radius(Container.new_reactive_cell<float>())
	{
	}

	void Init(Clock& clock)
	{
		clock.FromStart >> [&](auto value) {
			Radius = (std::sin(value) + 1.0f) * 50.0f;
		};
	}
public:
	cellf & Radius;
private:
	CellContainer Container;
};

class Renderer
{
public:
	virtual void Render()
	{

	}
};

class SFML : public Renderer
{
public:
	SFML(sf::RenderWindow& window) :
		Window(window),
		IO(Window)
	{
	}
	void Init(const Model& model)
	{
		Shape = sf::CircleShape(100.f);
		Shape.setFillColor(sf::Color::Green);

		model.Radius >> [&](auto value) {
			Shape.setRadius(value);
		};
	}

	virtual void Render()
	{
		Window.clear();
		Window.draw(Shape);
		Window.display();
	}
private:
	CellContainer Container;
	sf::RenderWindow& Window;
	sf::CircleShape Shape;

	class SFMLIO : public IO
	{
	public:
		SFMLIO(sf::RenderWindow& window) : 
			Window(window),
			Container(),
			WindowOpened(Container.new_reactive_cell<bool>())
		{
			//TODO: cell neeed defaut value
			WindowOpened = true;
			WindowOpened >> [&](auto value) {
				Window.close();
			};
		}

		virtual void process()
		{
			sf::Event event;
			while (Window.pollEvent(event))
			{
				if (event.type == sf::Event::Closed)
				{
					WindowOpened = false;
				}
			}
		}
	private:
		sf::RenderWindow& Window;
		CellContainer Container;
	public:
		cellb & WindowOpened;
	};
public:
	SFMLIO IO;
};

class GameLoop
{
public:
	void Run(IO& io, Clock& clock, Renderer& renderer, cellb& runWhile)
	{
		while (runWhile)
		{
			io.process();
			clock.Update();
			renderer.Render();
		}
	}
};

#include <Windows.h>
 int __stdcall WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	sf::RenderWindow* window = new sf::RenderWindow(sf::VideoMode(200, 200), "SFML works!");

	Clock clock;

	Model model;
	SFML sfml(*window);

	sfml.Init(model);
	model.Init(clock);

	GameLoop loop;
	loop.Run(sfml.IO, clock, sfml, sfml.IO.WindowOpened);

	return 0;
}
