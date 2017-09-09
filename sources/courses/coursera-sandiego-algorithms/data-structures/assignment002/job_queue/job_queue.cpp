#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

using std::vector;

template <typename T1, typename T2>
std::ostream& operator << (std::ostream& out, std::tuple<T1,T2> t)
{
  return out << std::get<0>(t) << "," << std::get<1>(t);
}

template <typename T, typename F>
class heap{
public:
  heap(std::vector<T>& v, F f) : c(v), getkey(f)
  {
  }

  void insert(T item)
  {
    auto s = c.size();
    c.push_back(item);
    SiftUp(s);
  }

  T top() const 
  {
    if(c.size() > 0) return c[0];
    //ERROR
  }

  T pop()
  {
    auto result = c[0];
    c[0] = c[c.size() - 1];
    c.pop_back();
    SiftDown(0);
    return result;
  }

  void printTree(int i = 0, int depth = 0)
  {
      std::cout << std::string(depth*2, ' ') << getkey(c[i]) << std::endl;

    auto lindex = lchild_of(i);
    if(lindex < c.size())
    {
      printTree(lindex, depth + 1);
    }
    
    auto rindex = rchild_of(i);
    if(rindex < c.size())
    {
      printTree(rindex, depth + 1);
    }
  }
private:
  int parent_of(int i) const{
    return ((i+1)/2)-1;
  }

  int lchild_of(int i) const{
    return (2*(i+1)) - 1;
  }

  int rchild_of(int i) const {
    return (2*(i+1));
  }

  template <typename T1>
  bool compare(T1& l, T1& r) const {
    // std::cout << "compare " << std::endl;
    return l < r;
  }
  
  void SiftUp(int i)
  {
    while(true)
    {
      if(i < 0) return;

      // std::cout << "siftinUp " << i << std::endl;
      auto ivalue = getkey(c[i]);
      auto pindex = parent_of(i);

      if(pindex < 0) return;
      
      auto pvalue = getkey(c[pindex]);
      
      if(compare(ivalue,pvalue))
      {
        std::swap(c[i], c[pindex]);
        i = pindex;
      }
      else
      {
        break;
      }
    }
  }

  void SiftDown(int i){
    while(true){
      auto betterindex = i;
      auto bettervalue = getkey(c[i]);

      auto lindex = lchild_of(i);
      if(lindex < c.size())
      {
        auto lvalue = getkey(c[lindex]);
        if(compare(lvalue,bettervalue))
        {
          betterindex = lindex;
          bettervalue = lvalue;
        }  
      }
      
      auto rindex = rchild_of(i);
      if(rindex < c.size())
      {
        auto rvalue = getkey(c[rindex]);
        if(compare(rvalue,bettervalue))
        {
          betterindex = rindex;
          bettervalue = rvalue;
        }  
      }      
      
      if(betterindex != i)
      {
        std::swap(c[i], c[betterindex]);
        i = betterindex;
      }
      else
      {
        break;
      }
    }
  }

  std::vector<T> &c;
  F getkey;
};

template <typename T, typename F>
heap<T,F> make_heap(std::vector<T>& v, F f)
{
  return {v,f};
}

class JobQueue {
 private:
  int num_workers_;
  vector<int> jobs_;

  vector<int> assigned_workers_;
  vector<long long> start_times_;

  void WriteResponse(std::ostream &out) const {
    for (int i = 0; i < jobs_.size(); ++i) {
      out << assigned_workers_[i] << " " << start_times_[i] << "\n";
    }
  }

  void ReadData(std::istream &in) {
    int m;
    in >> num_workers_ >> m;
    jobs_.resize(m);
    for(int i = 0; i < m; ++i)
      in >> jobs_[i];
  }

  void AssignJobs() {
    // // TODO: replace this code with a faster algorithm.
    // assigned_workers_.resize(jobs_.size());
    // start_times_.resize(jobs_.size());
    // vector<long long> next_free_time(num_workers_, 0);
    // for (int i = 0; i < jobs_.size(); ++i) {
    //   int duration = jobs_[i];
    //   int next_worker = 0;
    //   for (int j = 0; j < num_workers_; ++j) {
    //     if (next_free_time[j] < next_free_time[next_worker])
    //       next_worker = j;
    //   }
    //   assigned_workers_[i] = next_worker;
    //   start_times_[i] = next_free_time[next_worker];
    //   next_free_time[next_worker] += duration;
    // }
    
    assigned_workers_.resize(jobs_.size());
    start_times_.resize(jobs_.size());
    
    struct thread_freeon{int thread; long long freeOn;};
    auto threadsv = std::vector<thread_freeon>();
    auto threads = make_heap(threadsv, [](const thread_freeon& x){return std::make_tuple(x.freeOn,x.thread);});

    // std::cout << "initializing..." << num_workers_ << std::endl;

    for(int i = 0;i < num_workers_;++i)
    {
      // std::cout << "init " << i << std::endl;
      threads.insert(thread_freeon{i,0});
    }

    // std::cout << "done." << std::endl;

    for (int i = 0; i < jobs_.size(); ++i) {
      auto currentjob_duration = jobs_[i];
      // std::cout << "job " << i << " has duration " << currentjob_duration << std::endl;
      // threads.printTree();
      
      auto nextworker = threads.pop();
      auto endsAt = nextworker.freeOn + currentjob_duration;
      // std::cout << "next worker " << nextworker.thread << " start at " << nextworker.freeOn << " ends at " << endsAt << std::endl;

      assigned_workers_[i] = nextworker.thread;
      start_times_[i] = nextworker.freeOn;
      
      auto nextitem = thread_freeon{nextworker.thread, endsAt};
      threads.insert(nextitem);
    }
  }

 public:
  void Solve(std::istream &in, std::ostream &out) {
    ReadData(in);
    AssignJobs();
    WriteResponse(out);
  }
};


#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void test(const std::string& instr, const std::string& expectedOut)
{
  auto instream = std::stringstream{instr};
  auto outstream = std::stringstream{};

  JobQueue job_queue;
  job_queue.Solve(instream, outstream);
  outstream.seekg(0);
  // std::cout << outstream.str();
  // outstream.seekg(0);

  auto expectedOutStream = std::stringstream{expectedOut};
  while (!expectedOutStream.eof()) {
    int e, a;
    expectedOutStream >> e;
    outstream >> a;

    REQUIRE(e == a);
  }  
}

TEST_CASE("","")
{
  test("2 5 1 2 3 4 5", "0 0 1 0 0 1 1 2 0 4");
  test("4 20 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1", "0 0 1 0 2 0 3 0 0 1 1 1 2 1 3 1 0 2 1 2 2 2 3 2 0 3 1 3 2 3 3 3 0 4 1 4 2 4 3 4");
}

#else

int main() {
  JobQueue job_queue;
  job_queue.Solve(std::cin, std::cout);
  return 0;
}

#endif