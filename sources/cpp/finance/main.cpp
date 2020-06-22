#include "Random1.h"
#include <iostream>
#include <cmath>
using namespace std;

template <typename T>
T CallPayoff(T thisSpot, T Strike) { return std::max(thisSpot - Strike, 0); }

template <typename T>
T PutPayoff(T thisSpot, T Strike) { return std::max(Strike - thisSpot, 0); }

template <typename T, typename FPayoff>
T calc_option_price(T Expiry, T Strike, T Spot, T Vol, T r, unsigned long NumberOfPaths, FPayoff payoff)
{
    T variance = Vol * Vol * Expiry;
    T rootVariance = sqrt(variance);
    T itoCorrection = -0.5 * variance;
    T movedSpot = Spot * exp(r * Expiry + itoCorrection);
    T thisSpot;
    T runningSum = 0;

    for (unsigned long i = 0; i < NumberOfPaths; i++)
    {
        T thisGaussian = GetOneGaussianByBoxMuller();
        thisSpot = movedSpot * exp(rootVariance * thisGaussian);
        runningSum += payoff(thisSpot, Strike);
    }
    T mean = runningSum / NumberOfPaths;
    mean *= exp(-r * Expiry);
    return mean;
}
int main()
{
    double Expiry;
    double Strike;
    double Spot;
    double Vol;
    double r;
    unsigned long NumberOfPaths;
    cout << "\nEnter expiry\n";
    cin >> Expiry;
    cout << "\nEnter strike\n";
    cin >> Strike;
    cout << "\nEnter spot\n";
    cin >> Spot;
    cout << "\nEnter vol\n";
    cin >> Vol;
    cout << "\nr\n";
    cin >> r;
    cout << "\nNumber of paths\n";
    cin >> NumberOfPaths;
    double result = calc_option_price(Expiry,
                                      Strike,
                                      Spot,
                                      Vol,
                                      r,
                                      NumberOfPaths,
                                      CallPayoff<double>);
    cout << "the price is " << result << "\n";
    double tmp;
    cin >> tmp;
    return 0;
}