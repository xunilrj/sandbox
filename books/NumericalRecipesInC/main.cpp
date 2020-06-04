#include <vector>
#include <iostream>

struct data
{
    int e;
    int m;
};

data datamax(data &a, data &b)
{
    if (a.m > b.m)
        return a;
    else
        return b;
}

#define T(e, d) t[((d) * (start_energy + 1) + (e))]

int calculate_profit(int start_energy, const std::vector<int> &earnings, const std::vector<int> cost)
{
    // for (auto i = 0; i < earnings.size(); ++i)
    // {
    //     std::cout << "\033[" << 0 << ";" << (i * 9) + 10 << "H";
    //     std::cout << earnings[i] << "," << cost[i];
    // }

    auto t = std::vector<data>((earnings.size() + 1) * (start_energy + 1), {0, 0});

    for (auto day = 0; day <= earnings.size(); ++day)
    {
        for (auto e = 0; e <= start_energy; ++e)
        {
            std::cout << "\033[" << e + 2 << ";" << (day * 9) + 1 << "H";
            std::cout << e << "," << day << "=";
            if (day == 0)
            {
                T(e, day) = {e, 0};
                std::cout << T(e, day).e << "," << T(e, day).m << std::endl;
                continue;
            }

            auto last_best = T(e, day - 1);

            auto money = last_best.m;
            auto max_food_yesterday = std::min(start_energy, (day < 2) ? 0 : (money / cost[day - 2]));
            auto change = money - (max_food_yesterday * cost[day - 2]);

            auto energy = last_best.e;

            data try_improve{0,
                             energy * earnings[day - 1] +
                                 (change) + (max_food_yesterday * earnings[day - 1])};

            data keep_energy_and_work_today{0,
                                            e * earnings[day - 1]};

            //std::cout << "y" << yesterday << " f" << possible_food_yesterday << " e" << possible_earning_today << " ";

            T(e, day) = datamax(
                last_best,    // Do nothing
                try_improve); // buy most food yesterday to work today
            T(e, day) = datamax(
                T(e, day),                   // Do nothing
                keep_energy_and_work_today); // buy most food yesterday to work today
            std::cout << T(e, day).e << "," << T(e, day).m << std::endl;
        }
    }

    return T(start_energy, earnings.size()).m;
}

int main()
{
    std::cout << calculate_profit(5, {1, 2}, {1, 4}) << std::endl;
    std::cout << calculate_profit(4, {1, 5, 5}, {2, 1, 4}) << std::endl;
    std::cout << calculate_profit(5, {1, 8, 6, 7}, {1, 3, 4, 1}) << std::endl;
    std::cout << calculate_profit(5, {7, 2, 4}, {7, 3, 6}) << std::endl;
    return 0;
}