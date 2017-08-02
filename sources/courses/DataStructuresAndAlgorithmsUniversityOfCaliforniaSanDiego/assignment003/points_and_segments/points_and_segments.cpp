#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

using std::vector;

void printVector(const std::vector<int> &a)
{
  std::cout << "[";
  for(auto i : a)
  {
    std::cout << i << ",";
  }
  std::cout << "]" << std::endl;
}

struct Comp
{
		int segmenti;
		std::vector<int> & points;
		std::vector<int> & starts;
		std::vector<int> & ends;

		Comp(int i, std::vector<int> & po,std::vector<int> & s,std::vector<int> & e) : 
			segmenti(i),
			points(po),
			starts(s),
			ends(e)
		{
		}

    bool operator() ( const std::tuple<int>& s, const int& r )
    {
				auto pointr = points[r];
				auto rscore = (pointr < starts[segmenti]) ? (-1) : ((pointr > ends[segmenti])?(1):(0));
				// std::cout << "segment " << segmenti << "[" <<  starts[segmenti] << "," << ends[segmenti] << "]" << " p index " << r << " tuple<" << std::get<0>(s) << ">,int<" << pointr << "> == " << rscore << std::endl;
				return std::get<0>(s) < rscore;
    }

    bool operator() ( const int& l, const std::tuple<int>& s )
    {
        auto pointl = points[l];
				auto lscore = (pointl < starts[segmenti]) ? (-1) : ((pointl > ends[segmenti])?(1):(0));
				// std::cout << "segment " << segmenti << "[" <<  starts[segmenti] << "," << ends[segmenti] << "]" << " p index " << l << " int<" << pointl << ">,tuple<" << std::get<0>(s) << "> == " << lscore << std::endl;
				return lscore < std::get<0>(s);
    }
};

vector<int> fast_count_segments(vector<int> starts, vector<int> ends, vector<int> points) {
	auto pointsorder = std::vector<int>(points.size());
	std::iota(pointsorder.begin(), pointsorder.end(), 0);
	std::sort(pointsorder.begin(), pointsorder.end(), [&](const int & l, const int & r){
		return points[l] < points[r];
	});
	// printVector(pointsorder);

	// auto segmentsorder = std::vector<int>(starts.size());
	// std::iota(segmentsorder.begin(), segmentsorder.end(), 0);
	// std::sort(segmentsorder.begin(), segmentsorder.end(), [&](const int & l, const int & r){
	// 	return ends[l] < ends[r];
	// });

	vector<int> cnt(points.size());

	for (size_t j = 0; j < starts.size(); j++) {
		auto segmenti = j;//segmentsorder[j];	
		auto range = std::equal_range(pointsorder.begin(),pointsorder.end(),
			std::make_tuple(0), Comp(segmenti, points, starts, ends));
		// std::cout << "[" << pointsorder[range.first - pointsorder.begin()] << "," << pointsorder[range.second - pointsorder.begin()] << "]" << std::endl;
		// printVector(cnt);
		std::for_each(range.first, range.second, [&](const int& x){
			// std::cout << "increasing " << x << std::endl;
			++cnt[x];
		});
		// printVector(cnt);
	}
	
	// auto segmentjstart = 0;
	// for (size_t i = 0; i < points.size(); i++) {
	// 	auto pointi = pointsorder[i];
	// 	auto point = points[pointi];
	// 	for (size_t j = segmentjstart; j < starts.size(); j++) {
	// 		auto segmenti = segmentsorder[j];	
	// 		cnt[pointi] += starts[segmenti] <= point && point <= ends[segmenti];

	// 		if(point > ends[segmenti])
	// 		{
	// 			segmentjstart++;
	// 		}
	// 	}
	// }
	return cnt;
}

vector<int> naive_count_segments(vector<int> starts, vector<int> ends, vector<int> points) {
  vector<int> cnt(points.size());
  for (size_t i = 0; i < points.size(); i++) {
    for (size_t j = 0; j < starts.size(); j++) {
      cnt[i] += starts[j] <= points[i] && points[i] <= ends[j];
    }
  }
  return cnt;
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

bool areEqual(vector<int> starts, vector<int> ends, vector<int> points)
{
	auto naive = naive_count_segments(starts,ends,points);
	auto fast = fast_count_segments(starts,ends,points);
	// std::cout << "Comparing" << std::endl;
	// printVector(naive);
	// printVector(fast);
	// std::cout << "---------------" << std::endl;
	return naive == fast;
}

TEST_CASE("fast_count_segments must work", "[fast_count_segments]")
{
	REQUIRE(areEqual({0,5},{7,10},{1,6,11}));
	REQUIRE(areEqual({-10},{10},{-100,100,0}));
	REQUIRE(areEqual({0,-3,7},{5,2,10},{1,6}));
}

void printProg(int x, int total){
    int progress = ((double)x / total) * 100;
    std::cout << "[";
    for (int i=1;i<=100;i++){
        if (i<progress || progress==100)
            std::cout << "=";
        else if (i==progress)
            std::cout << ">";
        else
            std::cout << " ";
    }

    std::cout << "] " << x << "/" << total << "\r" << std::flush;
}

TEST_CASE("fast_count_segments random cases", "[fast_count_segments]")
{
	for(int i = 0; i <= 100000; ++i)
  {
    printProg(i, 100000);

    auto starts = std::vector<int>(rand() % 2000);
    std::generate(starts.begin(), starts.end(), [](){ return std::rand() % 1000;});

		auto ends = std::vector<int>(starts.size());
		std::transform(starts.begin(), starts.end(), ends.begin(),
                   [](const int x) -> int { x + std::rand() % 1000; });
	
		auto points = std::vector<int>(rand() % 10000);
		std::generate(points.begin(), points.end(), []() { return std::rand() % 1000;});
    
		REQUIRE(areEqual(starts,ends,points));
  }
}

#else

int main() {
  int n, m;
  std::cin >> n >> m;
  vector<int> starts(n), ends(n);
  for (size_t i = 0; i < starts.size(); i++) {
    std::cin >> starts[i] >> ends[i];
  }
  vector<int> points(m);
  for (size_t i = 0; i < points.size(); i++) {
    std::cin >> points[i];
  }
  //use fast_count_segments
  vector<int> cnt = naive_count_segments(starts, ends, points);
  for (size_t i = 0; i < cnt.size(); i++) {
    std::cout << cnt[i] << ' ';
  }
}

#endif