# Distributed Counter

## Hello Counter

Let us start creating the most simple counter ever possible.

    class Counter{
    public:
        Counter():Value(0){
        }

        Counter& operator++(){
            ++Value;
            return *this;
        }
    private:
        int Value;
    };

## Distributed Counter

Now suppose we have two servers. Both serving a simple HTML page that somehow allows you to view and like (but not dislike) a video.

In the end we will have something like this.

    auto likesServer1 = Counter{};
    ++likesServer1;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter{};
    ++likesServer2;
    ++likesServer2;
    std::cout << likesServer2 << std::endl;

With the expected result of

    > ./counter.001.out
    1
    2

We have a problem. We have three likes, but they are separated. If a newer user request the page from the first server, he will see 1 like, and 2 likes if he request from the second server.

The first possible solution is, off course, to unify all counters on a single and remote server. Something along the lines of what Redis does. See https://redis.io/commands/incr

    redis> SET video1.likes "10"
    "OK"
    redis> INCR video1.likes
    (integer) 11
    redis> GET video1.likes
    "11"
    
Problem solved! Until you need to run a Redis cluster. Off course, redis support cluster. See https://redis.io/topics/cluster-tutorial. But, we have to admit that we lost that fascinating simplicity of the previous solution.

Another solution is to implement some sort of sharding on top of disconnected Redis servers. Something like.

    auto key = "video1";
    auto keyhash = hash(key);
    auto server = serverFromHash(keyhash);
    auto redis = connect(server);
    auto newValue = redis.incr(key);
    auto currentValue = redis.get(key);

This will off course have problems with cluster reconfiguration. You have now replication in case of server disruption, nor can you easily add or remove servers. That is why cluster is hard and have a lot of configuration.

But have we another possible solution?

## CRDT Counter

CRDT in this context stands for Conflict-free Replicated Data Type. A fancy way of describing data structures in append-only ways and living with eventual consistency.

This data structures became famous because of the paper Conflict-free Replicated Data Types by Marc Shapiro, Nuno Preguiça, Carlos Baquero, Marek Zawirski  (see https://hal.inria.fr/inria-00609399/document).

If you are a video person you can see Marc Shapiro and Nuno Preguiça presenting their finding at https://vimeo.com/144863140. If you a video person, please, starting becoming more a reading person. Use video only to aid your study. There is a reason why videos reference serious studies and serious studies does not reference videos.

The idea of the CRDT Counter is very simple. Let us let each server run its own local copy of the server. Looking to the data of two Counters, allow us to know which is "newer". And having two Counters allow a way to create a newer version, merging both Counters. With this simple approach we can create a distributed counter with eventual consistency if we have a way to broadcast updates to the Counter. For example:

### Separated Counters

As its first step, let us try to separate each server's counter to allow them to co-exist. For example:

    template<size_t N>
    class Counter{
        public:
            Counter(size_t index):Values(),Index(index)
            {
                assert(index < N);
            }

            Counter& operator++(){
                ++Values[Index];
                return *this;
            }
        private:
            size_t Index;
            std::array<int, N> Values;
    };

Now we keep each server on a specific position in the array. We increment the counter as:

    auto likesServer1 = Counter<2>{0};
    ++likesServer1;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter<2>{1};
    ++likesServer2;
    ++likesServer2;
    std::cout << likesServer2 << std::endl;

The result is

    > ./counter.002.out
    Running...
    [1,0]
    [0,2]

No surprises until now. Support, for as a simplification that we can easily broadcast the counter after each increment. With this we can easily create a merge function that just select the max value on each array cell. Why? Remember that we cannot decrease the counter. So, if I saw "zero" and I am seeing now "two", it means that "two" is newer. It is a easy and simple way to always keep the data updated.


    class Counter{
        ...
        Counter operator & (const Counter<N>& other) const
        {
            auto result = Counter<N>(this->Index);
            for(int i = 0;i < N; ++i){
                result.Values[i] = std::max(this->Values[i],other.Values[i]);
            }            
            return result;
        }
        ...
    }
    ...
    ...
    likesServer1 = likesServer1 & likesServer2;
    std::cout << likesServer1 << std::endl;
    ...

The result here is:

    > ./counter.003.out
    Running...
    [1,0]
    [0,2]
    [1,2]

So far so good, but I can not print the quantity os likes in the HTML page yet. I need a way to aggregate the quantity of likes of all servers. Fortunately I can do this very easily now, because I have the quantity of all servers.

For example:

    class Counter{
        ...
        int getCurrentValue() const
        {
            return std::accumulate(
                std::begin(Values), std::end(Values),
                0, [](auto& acc, auto& current){
                    return acc + current;
                });
        }
        ...
    }
    ...
    std::cout << likesServer1.getCurrentValue() << std::endl;
    ...

Not surprisingly you will see something like:

    > ./counter.004.out
    Running...
    [1,0]
    [0,2]
    [1,2]
    3

You just need to keep all servers connected and exchanging updates now!

### Decrements

OK. But what if the user wants to unlike the video. The hasty reader may come to the conclusion that we just need to decrement "our", the server quantity and thats it! Sorry! You cannot do that, because of our merge function. Remember? We relied on the fact that the value was increment only to merge objects using "max".

If you decrement the value and broadcast to another server, this decremented value will be totally ignored by the "max" on the "merge" function.

But we do have a solution to this. We need another, increment-only, integer. That will count decrements. Our Counter will be something like this:

    template<size_t N>
    class Counter{
        public:
            Counter(size_t index):Index(index),
                Increments(),
                Decrements()
            {
                assert(index < N);
            }

            Counter& operator++(){
                ++Increments[Index];
                return *this;
            }

            Counter& operator--(){
                ++Decrements[Index];
                return *this;
            }

            Counter operator & (const Counter<N>& other) const
            {
                auto result = Counter<N>(this->Index);
                for(int i = 0;i < N; ++i){
                    result.Increments[i] = std::max(
                        this->Increments[i],
                        other.Increments[i]);
                }   
                for(int i = 0;i < N; ++i){
                    result.Decrements[i] = std::max(
                        this->Decrements[i],
                        other.Decrements[i]);
                }          
                return result;
            }

            int getCurrentValue() const
            {
                auto value = std::accumulate(
                    std::begin(Increments), std::end(Increments),
                    0, [](auto& acc, auto& current){
                        return acc + current;
                    });
                return std::accumulate(
                    std::begin(Decrements), std::end(Decrements),
                    value, [](auto& acc, auto& current){
                        return acc - current;
                    });
            }
        private:
            size_t Index;
            std::array<int, N> Increments;
            std::array<int, N> Decrements;
    };

And with a code like:

    std::cout << "Running..." << std::endl;
    
    auto likesServer1 = Counter<2>{0};
    ++likesServer1;
    std::cout << "Server 1" << std::endl;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter<2>{1};
    ++likesServer2;
    ++likesServer2;
    --likesServer2;
    std::cout << "Server 2" << std::endl;
    std::cout << likesServer2 << std::endl;
    
    likesServer1 = likesServer1 & likesServer2;
    std::cout << "Merged Server 1" << std::endl;
    std::cout << likesServer1 << std::endl;

    std::cout << "Final Value" << std::endl;
    std::cout << likesServer1.getCurrentValue() << std::endl;

We will have a result like:

    > ./counter.005.out
    Running...
    Server 1
    Running...
    Server 1
    Increments: [1,0]
    Decrements: [0,0]

    Server 2
    Increments: [0,2]
    Decrements: [0,1]

    Merged Server 1
    Increments: [1,2]
    Decrements: [0,1]

    Final Value
    2


### Back To The Paper

If we now analyze what it is written in the paper we will probably easily understand what Marc Shapiro meant.

    4.1 Integer vectors and counters 
    Consider the state-oriented specification of a vector-of-integers object:

    (Nn,[0,...,0],≤n, [0,...,0],value,inc,maxn). 

    Vectors v,v0 ∈ Nn are (partially) ordered by 
    
    v ≤n v' ⇔ ∀j ∈ [0 ..n−1], v[i] ≤ v'[i].
    
    A query invocation value() returns a copy of the local payload. An update inc(i)
    increments the payload entry at index i, that is,

    s.inc(i)=[s0[0],...,s0[n−1]] 
        where s0[j] = s[j]+1 if i = j and s0[j] = s[j] otherwise. 
    
    Merging two vectors takes the per-index maximum, i.e., 

    s.maxn(s0) = [max(s[0],s0[0]),...,max(s[n−1],s0[n−1])].
    
    We omit the proof that it is a CRDT.
    
    If each process pi is restricted to increment ing its own index inc(i), this is the
    well-known vector clock [11]. An increment-only integer counter is very similar; the
    only difference being that query invocation value() of a vector in state v returns
    
    |v| = SUM v[j].
    
    We construct an integer counter that can be both incremented and decremented, by
    basically associating two increment-only counters I and D, where incrementing
    increments I and decrementing increments D, whereas value() returns |I| − |D|.
    The ordering method ≤ is defined as 
    
    (I,D) ≤ (I',D') = (I ≤n I') ∧ (D ≤n D')

# To Be Continued...