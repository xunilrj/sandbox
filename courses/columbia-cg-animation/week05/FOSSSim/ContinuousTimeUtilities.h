#ifndef CONTINUOUS_TIME_UTILITIES_H
#define CONTINUOUS_TIME_UTILITIES_H

#include <vector>
#include <list>
#include <cassert>
#include <iostream>
#include "rpoly.h"

struct Interval
{
    Interval(double s, double e): m_s(s), m_e(e) {assert(m_s <= m_e);}
    double m_s, m_e;
    
    bool operator<(const Interval &other) const {return m_s < other.m_s;}
};

bool overlap(const Interval &a, const Interval &b);
Interval Iunion(const Interval &a, const Interval &b);
Interval Iintersect(const Interval &a, const Interval &b);

class Intervals
{
public:
    Intervals(const std::vector<Interval> &intervals);
    
    double findNextSatTime(double t);
    
    friend Intervals intersect(const Intervals &i1, const Intervals &i2);
    
    friend std::ostream& operator<<(std::ostream &os, const Intervals &inter);
    
private:
    std::list<Interval> m_intervals;
    
    void consolidateIntervals();
    
};

Intervals intersect(const Intervals &i1, const Intervals &i2);
std::ostream &operator<<(std::ostream &os, const Intervals &inter);

class Polynomial
{
public:
    Polynomial(const std::vector<double> &coeffs);
    
    double evaluate(double t) const ;
    
    const std::vector<double> &getCoeffs() const {return m_coeffs;}
    
private:
    std::vector<double> m_coeffs;
};

class PolynomialIntervalSolver
{
public:
    static double findFirstIntersectionTime(const std::vector<Polynomial> &polys);

    static void writePolynomials(std::ostream & os);
    static void readPolynomials(std::vector<Polynomial> & polynomials, std::istream & is);
    
    static std::vector<Polynomial> & getPolynomials() { return s_polynomials; }
    static void clearPolynomials() { s_polynomials.clear(); }
    
private:
    
    static Intervals findPolyIntervals(const Polynomial &poly);
    
    static RootFinder rf;
    
private:
    static std::vector<Polynomial> s_polynomials;
    
};


#endif
