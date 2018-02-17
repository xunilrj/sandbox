#include <iostream>
#include <queue>
#include <vector>

struct Request
{
    Request(int arrival_time, int process_time):
        arrival_time(arrival_time),
        process_time(process_time)
    {}

    int arrival_time;
    int process_time;
};

struct Response
{
    Response(bool dropped, int start_time):
        dropped(dropped),
        start_time(start_time)
    {}

    bool dropped;
    int start_time;
};

//https://stackoverflow.com/a/29325258/5397116
//https://stackoverflow.com/questions/29289974/what-does-the-operator-do/29291621#29291621
template <class ADAPTER>
typename ADAPTER::container_type & get_container (ADAPTER &a)
{
    //std::queue<T>::c is the underlying container for the queue
    //&hack::c if the memory access to its container.    
    struct hack : ADAPTER {
        static typename ADAPTER::container_type & get (ADAPTER &a) {
            return a.*&hack::c;
        }
    };
    return hack::get(a);
}

std::ostream & operator << (std::ostream & out, std::queue<int>& q)
{
    out << "[";
    for(auto x : get_container(q))
    {
        out << x << ",";       
    }
    return out << "]";
}

class Buffer
{
public:
    Buffer(int size):
        size_(size),
        finish_time_()
    {}

    Response Process(const Request &request)
    {
        // std::cout << finish_time_ << std::endl;
        
        while(finish_time_.size() > 0)
        {
            auto lastFinished = finish_time_.front();
            if(lastFinished <= request.arrival_time)
            {
                finish_time_.pop();
            }
            else
            {
                break;
            }
        }

        auto lastFinished = 0;
        if(finish_time_.size() > 0)
        {
            lastFinished = finish_time_.back();
        }

        if(canAdd())
        {
            auto start = std::max(lastFinished, request.arrival_time);
            auto finish = start + request.process_time;
            finish_time_.emplace(finish);
            return {false,start};
        }
        else
        {
            // std::cout << "dropping package" << std::endl;
            return {true,-1};
        }
    }
private:
    bool canAdd() const{
        return finish_time_.size() < size_;
    }
    int size_;
    std::queue <int> finish_time_;
};

std::vector<Request> ReadRequests(std::istream& in)
{
    std::vector<Request> requests;
    int count;
    in >> count;
    for (int i = 0; i < count; ++i)
    {
        int arrival_time, process_time;
        in >> arrival_time >> process_time;
        requests.push_back(Request(arrival_time, process_time));
    }
    return requests;
}

std::vector<Response> ProcessRequests(const std::vector<Request> &requests, Buffer * buffer)
{
    std::vector<Response> responses;
    for (int i = 0; i < requests.size(); ++i)
    {
        responses.push_back(buffer->Process(requests[i]));
    }
    return responses;
}

std::vector<int> GetAnswer(const std::vector <Response> &responses)
{
    auto answer = std::vector<int>();
    for (int i = 0; i < responses.size(); ++i)
    {
        auto a = (responses[i].dropped ? -1 : responses[i].start_time);
        answer.push_back(a);
    }
    return answer;
}

Buffer * readBuffer(std::istream &in)
{
    int size;
    in >> size;
    return new Buffer(size);
}

std::vector<int> RunAll(std::istream &in)
{
    auto buffer = readBuffer(in);
    auto requests = ReadRequests(in);
    auto responses = ProcessRequests(requests, buffer);
    return GetAnswer(responses);
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

void TestAll(const std::string& str, std::vector<int> expected)
{
    auto in = std::stringstream{str, std::ios_base::in | std::ios_base::out};
    auto answer = RunAll(in);

    REQUIRE(expected.size() == answer.size());
    for(int i = 0; i < expected.size();++i)
    {
        REQUIRE(expected[i] == answer[i]);
    }
}

TEST_CASE("getHeight must work","getHeight")
{
    // TestAll("1 0", {});
    // TestAll("1 1 0 0", {0});
    // TestAll("1 1 1 0", {1});
    // TestAll("1 2 0 1 0 1", {0, -1});
    // TestAll("1 2 0 0 0 0", {0, 0});
    // TestAll("2 2 0 1 0 1", {0, 1});
    //--0----1----2----3----4----5----6----7----8
    //  x---------x {false, 0} [2]
    //       xxxxxx---------x {false, 2} [2,4]
    //            xxxxxxxxxxx---------x {false, 4} [4,6]
    //                 xxxxxxxxxxxxxxxx---------x {false, 6} [4,6,8]
    //                      xxxxxxxxxxxxxxxxxxxxx---------x {false, 8} [6,8,10]
    //                           x {true, -1}
    //
    TestAll("3 6 0 2 1 2 2 2 3 2 4 2 5 2", {0, 2, 4, 6, 8, -1});
}

#else

int main() {
    auto answers = RunAll(std::cin);
    for(auto x : answers)
    {
        std::cout << x << std::endl;
    }
    return 0;
}

#endif