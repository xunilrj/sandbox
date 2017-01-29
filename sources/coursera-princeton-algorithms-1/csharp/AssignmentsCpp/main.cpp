#include <algorithm>
#include <string>
#include <random>
#include <chrono>
#include <iostream>

#include<stdio.h>  
#include<stdlib.h>  
#include<string.h>  
#include<ctype.h>  
#include<time.h>  

#define MAX 800  
#define NUM 200  
const char INFILE[] = "C:\\github\\xunilrj-sandbox\\sources\\coursera-princeton-algorithms-1\\csharp\Assignements\\_f370cd8b4d3482c940e4a57f489a200b_kargerMinCut.txt";

struct EDGE  
{
	short vertex;
	struct EDGE *next;
};

namespace Karatsuba
{
	namespace detail
	{
		void padTo(std::string &str, const size_t num, const char paddingChar = ' ')
		{
			if (num > str.size())
				str.insert(0, num - str.size(), paddingChar);
		}
	}

	char * sum_char(int lsize, char * l, int rsize, char * r)
	{
		auto li = lsize - 1;
		auto ri = rsize - 1;

		auto maxlength = std::max(lsize, rsize);
		auto result = new char[maxlength + 2];

		result[maxlength + 1] = 0;

		int carriage = 0;
		for (int i = maxlength; i > 0; i--)
		{
			int lc = 0;
			if (li >= 0)
			{
				lc = (int)l[li] - 48;
				li--;
			}

			int rc = 0;
			if (ri >= 0)
			{
				rc = (int)r[ri] - 48;
				ri--;
			}

			auto re = lc + rc + carriage;

			if (re >= 10)
			{
				re = re - 10;
				carriage = 1;
			}
			else
			{
				carriage = 0;
			}

			result[i] = (char)(re + 48);
		}

		if (carriage == 0)
		{
			return (result + 1);
		}
		else
		{
			result[0] = '1';
			return result;
		}
	}

	std::string sum(std::string l, std::string r)
	{
		int li = l.length() - 1;
		int ri = r.length() - 1;

		auto maxlength = std::max(l.length(), r.length());
		auto result = std::string(maxlength + 1, '0');

		int carriage = 0;
		for (int i = maxlength; i > 0; i--)
		{
			int lc = 0;
			if (li >= 0)
			{
				lc = (int)l[li] - 48;
				li--;
			}

			int rc = 0;
			if (ri >= 0)
			{
				rc = (int)r[ri] - 48;
				ri--;
			}

			auto re = lc + rc + carriage;

			if (re >= 10)
			{
				re = re - 10;
				carriage = 1;
			}
			else
			{
				carriage = 0;
			}

			result[i] = (char)(re + 48);
		}

		if (carriage == 0)
		{
			return result.substr(1, result.length() - 1);
		}
		else
		{
			result[0] = '1';
			return result;
		}
	}

	std::string multiply(std::string l, std::string r)
	{
		if (l.length() == 1)
		{
			return std::to_string(std::atoi(l.c_str()) * std::atoi(r.c_str()));
		}
		else
		{
			int ll = l.length();

			if (l.length() > r.length())
			{
				detail::padTo(r, l.length(), '0');
			}
			else if (l.length() < r.length())
			{
				detail::padTo(l, r.length(), '0');
			}

			if (l.length() % 2 != 0)
			{
				detail::padTo(l, l.length() + (l.length() % 2), '0');
				detail::padTo(r, r.length() + (r.length() % 2), '0');
			}

			auto a = l.substr(0, l.length() / 2);
			auto b = l.substr(l.length() / 2, l.length() / 2);

			auto c = r.substr(0, l.length() / 2);
			auto d = r.substr(l.length() / 2, l.length() / 2);

			auto ac = multiply(a, c);
			auto ad = multiply(a, d);
			auto bc = multiply(b, c);
			auto bd = multiply(b, d);

			auto adbc = sum(ad, bc);

			auto p1 = ac + std::string(ll, '0');
			auto p2 = adbc + std::string(ll / 2, '0');
			//auto p1 = (BigInteger.Pow(10, ll) * BigInteger.Parse(ac)).ToString();
			//auto p2 = (BigInteger.Pow(10, ll / 2) * BigInteger.Parse(adbc)).ToString();

			auto result = sum(sum(bd, p2), p1);

			//var result = ((BigInteger.Parse(bd) + BigInteger.Parse(p2)) + BigInteger.Parse(p1)).ToString();

			//Console.WriteLine($"{l} * {r} = {result} = {p1} + {p2} + {bd}");

			return result;
		}
	}
}

template<typename TimeT = std::chrono::milliseconds>
struct measure
{
	template<typename F, typename ...Args>
	static typename TimeT::rep execution(F&& func, Args&&... args)
	{
		auto start = std::chrono::steady_clock::now();

		std::forward<decltype(func)>(func)(std::forward<Args>(args)...);

		auto duration = std::chrono::duration_cast<TimeT>
			(std::chrono::steady_clock::now() - start);

		return duration.count();
	}
};


//  All the functions defined in this problem  
struct EDGE ** fetch_graph(struct EDGE **graph);

void print_graph(struct EDGE **graph);

short *random_pick(struct EDGE **graph, short *pair);

void merge_vertices(struct EDGE **graph, short *pair);

void delete_selfloop(struct EDGE **graph);

void contraction(struct EDGE **graph);

short solve_mincut(struct EDGE **graph);
//  Here starts the main function  
void main()
{
	srand(time(0));
	struct EDGE **graph, *node;
	short j, MinCut, newc;
	MinCut  = 10000;
	for (j = 0; j<1000; j++) {
		newc  = solve_mincut(fetch_graph(graph));
		MinCut  = (newc<MinCut) ? newc : MinCut;
	}
	printf("MinCut = %d\n", MinCut);
}

struct EDGE ** fetch_graph(struct EDGE **graph)
{
	struct EDGE *node;
	FILE *fp  = fopen(INFILE, "r");
	char line[MAX], *token;
	short j;

	graph  = (struct EDGE**)calloc(NUM, sizeof(struct EDGE*));
	for (j = 0; j<NUM; j++) {
		node  = (struct EDGE*)malloc(sizeof(struct EDGE));
		node->vertex  = j + 1;
		node->next  = NULL;
		graph[j] = node;
		//  here 'node' is a sentinel in linked list  
		fgets(line, MAX, fp);
		token  = strtok(line, "\t");
		while (token  = strtok(NULL, "\t")) {
			if (atoi(token)) {
				node  = (struct EDGE*)malloc(sizeof(struct EDGE));
				node->vertex  = atoi(token);
				node->next  = graph[j]->next;
				graph[j]->next  = node;
			}
		}
	}
	return graph;
}

void print_graph(struct EDGE **graph)
{
	short j;
	struct EDGE *node;
	for (j = 0; j<NUM; j++) {
		printf("%d\t", j + 1);
		for (node = graph[j]; node->next; node = node->next)
			printf("%d\t", node->next->vertex);
		printf("\n");
	}
}

short *random_pick(struct EDGE **graph, short *pair)
{
	short i, j, pick, m = 0; //  m is the double edge number  
	struct EDGE *node;
	pair  = (short *)calloc(2, sizeof(short));
	for (j = 0; j<NUM; j++)
		for (node  = graph[j]; node->next; node  = node->next)
		m++;
	pick  = rand() % m + 1;

	node  = (struct EDGE*)malloc(sizeof(struct EDGE));
	node->next  = NULL;
	for (i = 0, j = 0; j<pick; )
		if (node->next) {
		node  = node->next;
		j++;
	}
	else  
		node  = graph[i++];
	pair[0] = i;
	pair[1] = node->vertex;
	//  printf("merge %d and %d (replace all '%d' with '%d')\n",pair[0],pair[1],pair[0],pair[1]);  
	return pair;
}

void merge_vertices(struct EDGE **graph, short *pair)
{
	struct EDGE *node;
	short i, j, k;
	for (k = 0; k<NUM; k++)
		for (node  = graph[k]; node->next; node  = node->next)
		if (node->next->vertex  == pair[0])
		node->next->vertex  = pair[1];
	//  printf("after replacing\n");  
	//  print_graph(graph);  
	node  = graph[pair[1] - 1];
	while (node->next)
		node  = node->next;
	node->next  = graph[pair[0] - 1]->next;
	graph[pair[0] - 1]->next  = NULL;
	//  printf("after merging\n");  
	//  print_graph(graph);  
}

void delete_selfloop(struct EDGE **graph)
{
	struct EDGE *node;
	short k, ref;
	for (k = 0; k<NUM; k++)
		if (graph[k]->next) {
		ref  = graph[k]->vertex;
		for (node  = graph[k]; node->next; )
			if (node->next->vertex  == ref)
			node->next  = node->next->next;
		else  
			node  = node->next;
	}
}

void contraction(struct EDGE **graph)
{
	short *pair;
	pair  = random_pick(graph, pair);
	merge_vertices(graph, pair);
	delete_selfloop(graph);
}

short solve_mincut(struct EDGE **graph)
{
	short k, m = 0;
	struct EDGE *node;
	for (k  = NUM; k>2; k--)
		contraction(graph);
	for (k = 0; k<NUM; k++)
		for (node  = graph[k]; node->next; node  = node->next)
		m++;
	return m / 2;
}

int mmain(int argc, char **argv)
{
	/*auto result = Karatsuba::sum_char(1, "1", 1, "2");
	auto result2 = Karatsuba::sum("1", "2");
	result2 = Karatsuba::sum("6", "6");

	auto time = measure<std::chrono::milliseconds>::execution([&result2]()
	{
		std::random_device random;
		for (auto i = 0; i < 100000; i++)
		{
			auto l = std::to_string(random());
			auto r = std::to_string(random());

			result2 = Karatsuba::sum(l, r);
		}
	});

	std::cout << time;*/

	auto result = Karatsuba::multiply("10", "200");
	std::cout << result;
}