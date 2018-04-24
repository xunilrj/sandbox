#include "ContinuousTimeCollisionHandler.h"
#include <iostream>
#include "ContinuousTimeUtilities.h"

// BEGIN STUDENT CODE //


// Given the start position (oldpos) and end position (scene.getX) of two
// particles, and assuming the particles moved in a straight line between the
// two positions, determines whether the two particles were overlapping and
// approaching at any point during that motion.
// If so, returns true, sets n to the the vector between the two particles
// at the time of collision, and sets t to the time (0 = start position, 
// 1 = end position) of collision.
// Inputs:
//   scene:  The scene data structure. The new positions and radii of the 
//           particles can be obtained from here.
//   qs:     The start-of-timestep positions.
//   qe:     The predicted end-of-timestep positions.
//   idx1:   The index of the first particle. (ie, the degrees of freedom
//           corresponding to this particle are entries 2*idx1 and 2*idx1+1 in
//           scene.getX()).
//   idx2:   The index of the second particle.
// Outputs:
//   n:    The vector between the two particles at the time of collision.
//   time: The time (scaled to [0,1]) when the two particles collide.
//   Returns true if the two particles overlap and are approaching at some point
//   during the motion.
bool ContinuousTimeCollisionHandler::detectParticleParticle(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int idx1, int idx2, Vector2s &n, double &time)
{
    VectorXs dx = qe-qs;
    
    VectorXs x1 = qs.segment<2>(2*idx1);
    VectorXs x2 = qs.segment<2>(2*idx2);
    
    VectorXs dx1 = dx.segment<2>(2*idx1);
    VectorXs dx2 = dx.segment<2>(2*idx2);
    
    double r1 = scene.getRadius(idx1);
    double r2 = scene.getRadius(idx2);

    std::vector<double> position_polynomial;
    std::vector<double> velocity_polynomial;
    
    VectorXs dx2dx1 = dx2-dx1;
    VectorXs x2x1 = x2-x1;
    scalar dx2dx1dotdx2dx1 = (dx2dx1).dot(dx2dx1);
    scalar dx2dx1dotx2x1 = (dx2dx1).dot(x2x1);
    scalar x2x1dotx2x1 = (x2x1).dot(x2x1);
    position_polynomial.push_back( -dx2dx1dotdx2dx1 );
    position_polynomial.push_back( -2*dx2dx1dotx2x1 );
    position_polynomial.push_back( (r1+r2)*(r1+r2) - x2x1dotx2x1 );
    velocity_polynomial.push_back( -dx2dx1dotdx2dx1 );
    velocity_polynomial.push_back( -dx2dx1dotx2x1 );
  
    std::vector<Polynomial> polynomials;
    polynomials.push_back(Polynomial(position_polynomial));
    polynomials.push_back(Polynomial(velocity_polynomial));
    
    time = PolynomialIntervalSolver::findFirstIntersectionTime(polynomials);
    
    if(time <= 1.0)
    {
        n = (x2+ time*dx2) - (x1+time*dx1);
        return true;
    }
    return false;
}


// Given start positions (oldpos) and end positions (scene.getX) of a
// particle and an edge, and assuming the particle and edge endpoints moved in 
// a straight line between the two positions, determines whether the two 
// objects were overlapping and approaching at any point during that motion.
// If so, returns true, sets n to the the vector between the particle and the
// edge at the time of collision, and sets t to the time (0 = start position, 
// 1 = end position) of collision.
// Inputs:
//   scene:  The scene data structure. 
//   qs:     The start-of-timestep positions.
//   qe:     The predicted end-of-timestep positions.
//   vidx:   The index of the particle.
//   eidx:   The index of the edge.
// Outputs:
//   n:    The shortest vector between the particle and edge at the time of 
//         collision.
//   time: The time (scaled to [0,1]) when the two objects collide.
//   Returns true if the particle and edge overlap and are approaching at
//   some point during the motion.
// Given start positions (oldpos) and end positions (scene.getX) of a
// particle and an edge, and assuming the particle and edge endpoints moved in 
// a straight line between the two positions, determines whether the two 
// objects were overlapping and approaching at any point during that motion.
bool ContinuousTimeCollisionHandler::detectParticleEdge(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int eidx, Vector2s &n, double &time)
{
    VectorXs dx = qe - qs;
    
    VectorXs x1 = qs.segment<2>(2*vidx);
    VectorXs x2 = qs.segment<2>(2*scene.getEdge(eidx).first);
    VectorXs x3 = qs.segment<2>(2*scene.getEdge(eidx).second);
    
    VectorXs dx1 = dx.segment<2>(2*vidx);
    VectorXs dx2 = dx.segment<2>(2*scene.getEdge(eidx).first);
    VectorXs dx3 = dx.segment<2>(2*scene.getEdge(eidx).second);

    double r1 = scene.getRadius(vidx);
    double r2 = scene.getEdgeRadii()[eidx];

    std::vector<double> position_polynomial;
    std::vector<double> alpha_greater_than_zero_polynomial;
    std::vector<double> alpha_less_than_one_polynomial;
    
    // Your implementation here should fill the polynomials with right coefficients
    VectorXs x1x2 = x1-x2;
    VectorXs x3x2 = x3-x2;
    VectorXs dx1dx2 = dx1-dx2;
    VectorXs dx3dx2 = dx3-dx2;
    scalar a = x1x2.dot(x3x2);
    scalar b = x1x2.dot(dx3dx2) + (dx1dx2).dot(x3x2);
    scalar c = dx1dx2.dot(dx3dx2);
    scalar d = x1x2.dot(x1x2);
    scalar e = dx1dx2.dot(x1x2);
    scalar f = dx1dx2.dot(dx1dx2);
    scalar g = x3x2.dot(x3x2);
    scalar h = dx3dx2.dot(x3x2);
    scalar i = dx3dx2.dot(dx3dx2);
    scalar rs = (r1+r2)*(r1+r2);
    
    position_polynomial.push_back( c*c - f*i );
    position_polynomial.push_back( 2*b*c - 2*e*i - 2*f*h );
    position_polynomial.push_back( 2*c*a + b*b - d*i - f*g -4*e*h + rs * i );
    position_polynomial.push_back( 2*a*b - 2*d*h - 2*e*g + 2*rs*h );
    position_polynomial.push_back( a*a - d*g + rs*g );
    
    alpha_greater_than_zero_polynomial.push_back( (dx1-dx2).dot(dx3-dx2) );
    alpha_greater_than_zero_polynomial.push_back( (dx1-dx2).dot(x3-x2) + (x1-x2).dot(dx3-dx2) );
    alpha_greater_than_zero_polynomial.push_back( (x1-x2).dot(x3-x2) );
    
    alpha_less_than_one_polynomial.push_back( (dx1-dx3).dot(dx2-dx3) );
    alpha_less_than_one_polynomial.push_back( (dx1-dx3).dot(x2-x3) + (x1-x3).dot(dx2-dx3) );
    alpha_less_than_one_polynomial.push_back( (x1-x3).dot(x2-x3) );
    
    // Here's the quintic velocity polynomial:
    std::vector<double> velcity_polynomial;
    {
        double a = (x3-x2).dot(x3-x2);
        double b = (x3-x2).dot(dx3-dx2);
        double c = (dx3-dx2).dot(dx3-dx2);
        double d = (dx2-dx1).dot(dx2-dx1);
        double e = (dx2-dx1).dot(x2-x1);
        double f = (x1-x2).dot(x3-x2);
        double g = (x1-x2).dot(dx3-dx2) + (dx1-dx2).dot(x3-x2);
        double h = (dx1-dx2).dot(dx3-dx2);
        double i = (dx3-dx2).dot(x2-x1) + (dx2-dx1).dot(x3-x2);
        double j = (dx3-dx2).dot(dx2-dx1);
        double k = a*f;
        double l = a*g+2*b*f;
        double m = a*h+2*b*g+c*f;
        double n = c*g+2*b*h;
        double o = c*h;
        double p = (dx3-dx2).dot(x3-x2);
        double q = (dx3-dx2).dot(dx3-dx2);
        
        velcity_polynomial.push_back( -h*h*q - c*c*d - 2*o*j );
        velcity_polynomial.push_back( -h*h*p - 2*g*h*q - 4*b*c*d - c*c*e - o*i - 2*n*j );
        velcity_polynomial.push_back( -2*g*h*p - 2*f*g*q - g*g*q - 2*a*c*d - 4*b*b*d - 4*b*c*e - n*i - 2*m*j );
        velcity_polynomial.push_back( -2*f*h*p - g*g*p - 2*f*g*q - 4*a*b*d - 2*a*c*e - 4*b*b*e - m*i - 2*l*j );
        velcity_polynomial.push_back( -2*f*g*p - f*f*q - a*a*d - 4*a*b*e - l*i - 2*k*j );
        velcity_polynomial.push_back( -f*f*p - a*a*e - k*i );
    }

    // Do not change the order of the polynomials here, or your program will fail the oracle
    std::vector<Polynomial> polynomials;
    polynomials.push_back(Polynomial(position_polynomial));
    polynomials.push_back(Polynomial(alpha_greater_than_zero_polynomial));
    polynomials.push_back(Polynomial(alpha_less_than_one_polynomial));
    polynomials.push_back(Polynomial(velcity_polynomial));
    
    time = PolynomialIntervalSolver::findFirstIntersectionTime(polynomials);
    
    if(time <= 1.0)
    {
        VectorXs ln = x1 + time*dx1 - x2 - time*dx2;
        VectorXs rn = x3+time*dx3-x2-time*dx2;
        scalar numerator = ln.dot(rn);
        VectorXs ld = x3+time*dx3-x2-time*dx2;
        VectorXs rd = x3+time*dx3-x2-time*dx2;
        scalar denominator = ld.dot(rd);
        scalar alpha = numerator/denominator;
        n = (x2+time*dx2) + alpha*(x3+time*dx3 -x2 - time*dx2) - x1 -time*dx1;
        return n.norm() != 0;
    }
    return false;
}

// Given start positions (oldpos) and end positions (scene.getX) of a
// particle and a half-plane, and assuming the particle endpoints moved in 
// a straight line between the two positions, determines whether the two 
// objects were overlapping and approaching at any point during that motion.
// If so, returns true, sets n to the the vector between the particle and the
// half-plane at the time of collision, and sets t to the time (0 = start 
// position, 1 = end position) of collision.
// Inputs:
//   scene:  The scene data structure. 
//   qs:     The start-of-timestep positions.
//   qe:     The predicted end-of-timestep positions.
//   vidx:   The index of the particle.
//   eidx:   The index of the half-plane. The vectors (px, py) and (nx, ny) can
//           be retrieved by calling scene.getHalfplane(pidx).
// Outputs:
//   n:    The shortest vector between the particle and half-plane at the time 
//         of collision.
//   time: The time (scaled to [0,1]) when the two objects collide.
//   Returns true if the particle and half-plane overlap and are approaching at
//   some point during the motion.
// Given start positions (oldpos) and end positions (scene.getX) of a
// particle and a half-plane, and assuming the particle endpoints moved in 
// a straight line between the two positions, determines whether the two 
// objects were overlapping and approaching at any point during that motion.
bool ContinuousTimeCollisionHandler::detectParticleHalfplane(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int pidx, Vector2s &n, double &time)
{
    VectorXs dx = qe - qs;
    
    VectorXs x1 = qs.segment<2>(2*vidx);
    VectorXs dx1 = dx.segment<2>(2*vidx);
    
    VectorXs xp = scene.getHalfplane(pidx).first;
    VectorXs np = scene.getHalfplane(pidx).second;
    
    double r = scene.getRadius(vidx);
 
    std::vector<double> position_polynomial;
    std::vector<double> velocity_polynomial;
    
    // Your implementation here should fill the polynomials with right coefficients
    scalar dx1dotnp = dx1.dot(np);
    VectorXs xpx1 = xp-x1;
    scalar xpx1dotnp = xpx1.dot(np);
    scalar npdotnp = np.dot(np);
    position_polynomial.push_back( -dx1dotnp * dx1dotnp );
    position_polynomial.push_back( 2*xpx1dotnp * dx1dotnp );
    position_polynomial.push_back( -xpx1dotnp * xpx1dotnp + r*r*npdotnp);
    velocity_polynomial.push_back(- dx1dotnp * dx1dotnp );
    velocity_polynomial.push_back(xpx1dotnp * dx1dotnp );
    
    // Do not change the order of the polynomials here, or your program will fail the oracle
    std::vector<Polynomial> polynomials;
    polynomials.push_back(Polynomial(position_polynomial));
    polynomials.push_back(Polynomial(velocity_polynomial));
    
    time = PolynomialIntervalSolver::findFirstIntersectionTime(polynomials);
    
    // Your implementation here should compute n, and examine time to decide the return value
    if(time <= 1.0)
    {
        n = (xpx1 - time*dx1).dot(np)/npdotnp * np;
        return true;
    }
    return false;
}


