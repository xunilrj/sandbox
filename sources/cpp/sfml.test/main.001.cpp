#include <vector>
#include <array>
#include <algorithm>
#include <functional>
#include <SFML/Graphics.hpp>

template<class T>
class UpdatableValue
{
public:
	UpdatableValue()
	{
	}

	void bind(const std::function<void(const T&)>& f)
	{
		Listeners.push_back(f);
	}

	void operator = (const T& value)
	{
		Value = value;
		std::for_each(std::begin(Listeners), std::end(Listeners), [&](auto& f) {
			f(Value);
		});
	}
private:
	T Value;
	std::vector < std::function<void(const T&)> > Listeners;
};

template<class T>
UpdatableValue<T> make_updatableValue(const std::function<void(const T&)>& f)
{
	auto v = UpdatableValue<T>();
	v.bind(f);
	return v;
}

class Updatable
{
public:
	std::function<void(const sf::Time&, const sf::Time&)> Update;
};

class BoardInitializer
{
public:
	void Init(std::array<int, 8 * 8> cells) const
	{
		int classicCells[8 * 8] = {
			1, 2, 3, 4, 5, 3, 2, 1,
			4, 4, 4, 4, 4, 4, 4, 4,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			4, 4, 4, 4, 4, 4, 4, 4,
			1, 2, 3, 4, 5, 3, 2, 1
		};
		std::copy(std::begin(classicCells), std::end(classicCells), std::begin(classicCells));
	}
};

class Board
{
public:
	Board() : Cells() {
	}

	void Init(const BoardInitializer& init) {
		init.Init(Cells);
	}
private:
	std::array<int, 8 * 8> Cells;
};

class Kernel
{
public:
	Kernel()
	{
	}

	Updatable& make_updatable()
	{
		Updatables.emplace_back();
		return Updatables[Updatables.size() - 1];
	}

	void update(const sf::Time& fromStart, const sf::Time& elapsed)
	{
		std::for_each(std::begin(Updatables), std::end(Updatables), [&](auto& item) {
			item.Update(fromStart, elapsed);
		});
	}
private:
	std::vector<Updatable> Updatables;
};

int main()
{
	sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
	sf::CircleShape shape(100.f);
	shape.setFillColor(sf::Color::Green);

	auto value = make_updatableValue<float>([&](auto v) {
		shape.setRadius(v);
	});
	value = 50.0f;

	auto kernel = Kernel{};
	auto& radius = kernel.make_updatable();
	radius.Update = [&](const sf::Time& start, const sf::Time& elapsed) {
		value = (std::sin(start.asSeconds()) + 1) * 50.0f;
	};

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

		sf::Time fromStart = start.getElapsedTime();
		sf::Time elapsed = clock.restart();
		kernel.update(fromStart, elapsed);

		window.clear();
		window.draw(shape);
		window.display();
	}

	return 0;
}