#include <vector>
#include <iostream>

struct Point
{
	float x, y;
	Point(float x = 0, float y = 0)
		:x(x), y(y)
	{
	};
};

struct AABB
{
	Point centre;
	Point halfSize;

	AABB(Point centre = Point(), Point halfSize = Point())
		: centre(centre),
		halfSize(halfSize)
	{
	};

	bool contains(Point p) const
	{
		auto a = p.x < centre.x + halfSize.x;
		auto b = p.x > centre.x - halfSize.x;
		auto c = p.y < centre.y + halfSize.y;
		auto d = p.y > centre.y - halfSize.y;
		return a && b && c && d;
	}

	bool intersects(AABB other) const
	{
		auto a = centre.x + halfSize.x > other.centre.x - other.halfSize.x;
		auto b = centre.x - halfSize.x < other.centre.x + other.halfSize.x;
		auto c = centre.y + halfSize.y > other.centre.y - other.halfSize.y;
		auto d = centre.y - halfSize.y < other.centre.y + other.halfSize.y;
		return (a || b) && (c || d);
	}
};

template <typename T>
struct Data
{
	Point pos;
	T* load;

	Data(Point pos = Point(), T* data = nullptr) 
		: pos(pos), load(data)
	{
	};
};

template <class T>
class Quadtree
{
private:
	static constexpr int CAPACITY = 4;

	//4 children
	Quadtree* nw;
	Quadtree* ne;
	Quadtree* sw;
	Quadtree* se;

	AABB boundary;

	std::vector< Data<T> > objects;
public:
	Quadtree<T>();
	Quadtree<T>(AABB boundary);
	~Quadtree();

	bool insert(Data<T> d);
	bool insert(Point pos, T& data);
	void subdivide();
	std::vector< Data<T> > queryRange(AABB range) const;
};

template <class T>
Quadtree<T>::Quadtree()
	: nw(nullptr),
	ne(nullptr),
	sw(nullptr),
	se(nullptr)
{
	boundary = AABB();
	objects = std::vector< Data<T> >();
}

template <class T>
Quadtree<T>::Quadtree(AABB boundary)
	: nw(nullptr),
	ne(nullptr),
	sw(nullptr),
	se(nullptr)
{
	objects = std::vector< Data<T> >();
	this->boundary = boundary;
}

template <class T>
Quadtree<T>::~Quadtree()
{
	if (nw) delete nw;
	if (sw) delete sw;
	if (ne) delete ne;
	if (se) delete se;
}

template <class T>
void Quadtree<T>::subdivide()
{
	Point qSize = Point(boundary.halfSize.x, boundary.halfSize.y);
	Point qCentre = Point(boundary.centre.x - qSize.x, boundary.centre.y - qSize.y);
	nw = new Quadtree(AABB(qCentre, qSize));

	qCentre = Point(boundary.centre.x + qSize.x, boundary.centre.y - qSize.y);
	ne = new Quadtree(AABB(qCentre, qSize));

	qCentre = Point(boundary.centre.x - qSize.x, boundary.centre.y + qSize.y);
	sw = new Quadtree(AABB(qCentre, qSize));

	qCentre = Point(boundary.centre.x + qSize.x, boundary.centre.y + qSize.y);
	se = new Quadtree(AABB(qCentre, qSize));
}

template <class T>
bool Quadtree<T>::insert(Data<T> d)
{
	if (!boundary.contains(d.pos))
	{
		return false;
	}

	if (objects.size() < CAPACITY)
	{
		objects.push_back(d);
		return true;
	}

	if (nw == nullptr)
	{
		subdivide();
	}

	if (nw->insert(d))
	{
		return true;
	}
	if (ne->insert(d))
	{
		return true;
	}
	if (sw->insert(d))
	{
		return true;
	}
	if (se->insert(d))
	{
		return true;
	}

	return false;
}

template <class T>
bool Quadtree<T>::insert(Point pos, T& data)
{
	Data<T> d = { pos, &data };
	return insert(d);
}

template <class T>
std::vector< Data<T> > Quadtree<T>::queryRange(AABB range) const
{
	std::vector< Data<T> > pInRange = std::vector< Data<T> >();

	if (!boundary.intersects(range))
	{
		return pInRange;
	}

	for (int i = 0; i < objects.size(); i++)
	{
		if (range.contains(objects.at(i).pos))
		{
			pInRange.push_back(objects.at(i));
		}
	}

	if (nw == nullptr)
	{
		return pInRange;
	}

	std::vector< Data<T> > temp = nw->queryRange(range);
	pInRange.insert(pInRange.end(), temp.begin(), temp.end());

	temp = ne->queryRange(range);
	pInRange.insert(pInRange.end(), temp.begin(), temp.end());

	temp = sw->queryRange(range);
	pInRange.insert(pInRange.end(), temp.begin(), temp.end());

	temp = se->queryRange(range);
	pInRange.insert(pInRange.end(), temp.begin(), temp.end());

	return pInRange;
}

int main(int argv, char** argc)
{
	int i = 0;

	Quadtree<int> tree = {};
	tree.insert({ 00,0 }, i);
	tree.insert({ 10,10 }, i);
	tree.insert({ 20,20 }, i);
	tree.insert({ 25,25 }, i);
	tree.insert({ 26,26 }, i);
	tree.insert({ 27.5,26.5 }, i);

	auto data = tree.queryRange({ { -1,-1 }, { 5, 5 } });
	return 0;
}