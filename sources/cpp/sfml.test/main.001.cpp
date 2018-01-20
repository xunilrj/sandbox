#include <vector>
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
UpdatableValue<T> make_updatableValue(const T& value, const std::function<void(const T&)>& f)
{
	auto v = UpdatableValue<T>();
	v.bind(f);
	v = value;
	return v;
}

class Updatable
{
public:
	std::function<void(const sf::Time&, const sf::Time&)> Update;
};

int main()
{
	sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
	sf::CircleShape shape(100.f);
	shape.setFillColor(sf::Color::Green);

	auto value = make_updatableValue<float>(50.0f, [&](auto v) {
		shape.setRadius(v);
	});

	auto updatables = std::vector<Updatable>();
	updatables.emplace_back();
	auto& radius = updatables[updatables.size() - 1];
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
		std::for_each(std::begin(updatables), std::end(updatables), [&](auto& item) {
			item.Update(fromStart, elapsed);
		});

		window.clear();
		window.draw(shape);
		window.display();
	}

	return 0;
}