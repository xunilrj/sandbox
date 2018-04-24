#include "SimpleCollisionHandler.h"
#include <iostream>
#include <set>

typedef std::vector<std::pair<int,int> > edges;
#define MASS(id)  scene.isFixed(id) ? std::numeric_limits<double>::infinity() : M[2*id]

// BEGIN STUDENT CODE //


// Detects whether two particles are overlapping (including the radii of each)
// and approaching.
// If the two particles overlap and are approaching, returns true and sets 
// the vector n to be the vector between the first and second particle.
// Inputs:
//   scene: The scene data structure. The positions and radii of the particles
//          can be obtained from here.
//   idx1:  The index of the first particle. (Ie, the degrees of freedom
//          corresponding to this particle are entries 2*idx1 and 2*idx1+1 in
//          scene.getX().
//   idx2:  The index of the second particle.
// Outputs:
//   n: The vector between the two particles.
//   Returns true if the two particles overlap and are approaching.
bool SimpleCollisionHandler::detectParticleParticle(TwoDScene &scene, int idx1, int idx2, Vector2s &n)
{
    int index1 = 2*idx1;
    int index2 = 2*idx2;
    
    VectorXs x1 = scene.getX().segment<2>(index1);
    VectorXs x2 = scene.getX().segment<2>(index2);
    
    scalar r1 = scene.getRadius(idx1);
    scalar r2 = scene.getRadius(idx2);
    
    n = x2-x1;
    scalar nlength = n.norm();
    
    bool intersection = nlength < (r1+r2);
    if(intersection){
        VectorXs v1 = scene.getV().segment<2>(index1);
        VectorXs v2 = scene.getV().segment<2>(index2);
        scalar relvel = (v1-v2).dot(n);
        return (relvel > 0);
    } else {    
        return false;
    }
}

// Detects whether a particle and an edge are overlapping (including the radii 
// of both) and are approaching.
// If the two objects overlap and are approaching, returns true and sets the 
// vector n to be the shortest vector between the particle and the edge.
// Inputs:
//   scene: The scene data structure.
//   vidx:  The index of the particle.
//   eidx:  The index of the edge. (Ie, the indices of particle with index e are
//          scene.getEdges()[e].first and scene.getEdges()[e].second.)
// Outputs:
//   n: The shortest vector between the particle and the edge.
//   Returns true if the two objects overlap and are approaching.
bool SimpleCollisionHandler::detectParticleEdge(TwoDScene &scene, int vidx, int eidx, Vector2s &n)
{
    const edges& EDGES = scene.getEdges();
    const VectorXs& X = scene.getX();
    const VectorXs& V = scene.getV();
    
    int index1 = 2*vidx;
    int index2 = 2*EDGES[eidx].first;
    int index3 = 2*EDGES[eidx].second;
    
    VectorXs x1 = X.segment<2>(index1);
    VectorXs x2 = X.segment<2>(index2);
    VectorXs x3 = X.segment<2>(index3);
    
    scalar r1 = scene.getRadius(vidx);
    scalar redge = scene.getEdgeRadii()[eidx];
    
    scalar numerator = (x1-x2).dot(x3-x2);
    scalar denominator = (x3-x2).dot(x3-x2);
    scalar alpha = numerator/denominator;
    alpha = clamp(alpha, 0.0, 1.0);
    
    VectorXs closest = x2 + alpha*(x3-x2);
    n = closest-x1;
    scalar nlength = n.norm();
    
    if(nlength < (r1+redge)) 
    {
        //they are approaching or moving apart
        VectorXs v1 = V.segment<2>(index1);
        VectorXs v2 = V.segment<2>(index2);
        VectorXs v3 = V.segment<2>(index3);
        double relvel = (v1 - v2 - alpha*(v3-v2)).dot(n);
        return relvel > 0;
    }
    return false;
}

// Detects whether a particle and a half-plane are overlapping (including the 
// radius of the particle) and are approaching.
// If the two objects overlap and are approaching, returns true and sets the 
// vector n to be the shortest vector between the particle and the half-plane.
// Inputs:
//   scene: The scene data structure.
//   vidx:  The index of the particle.
//   pidx:  The index of the halfplane. The vectors (px, py) and (nx, ny) can
//          be retrieved by calling scene.getHalfplane(pidx).
// Outputs:
//   n: The shortest vector between the particle and the half-plane.
//   Returns true if the two objects overlap and are approaching.
bool SimpleCollisionHandler::detectParticleHalfplane(TwoDScene &scene, int vidx, int pidx, Vector2s &n)
{
    VectorXs& V = scene.getV();
    VectorXs x1 = scene.getX().segment<2>(2*vidx);
    
    std::pair<VectorXs, VectorXs> halfPlane = scene.getHalfplane(pidx);
    VectorXs px = halfPlane.first;
    VectorXs pn = halfPlane.second;    
    pn.normalize();
    
    n = (px-x1).dot(pn)*pn;
    scalar nlength = n.norm();
    
    scalar r1 = scene.getRadius(vidx);
    
    if(nlength < r1)
    {
        double relvel = V.segment<2>(2*vidx).dot(n);
        return relvel > 0;
    }
    return false;
}


// Responds to a collision detected between two particles by applying an impulse
// to the velocities of each one.
// You can get the COR of the simulation by calling getCOR().
// Inputs:
//   scene: The scene data structure.
//   idx1:  The index of the first particle.
//   idx2:  The index of the second particle.
//   n:     The vector between the first and second particle.
// Outputs:
//   None.
void SimpleCollisionHandler::respondParticleParticle(TwoDScene &scene, int idx1, int idx2, const Vector2s &n)
{
    int index1 = 2*idx1, index2 = 2*idx2;

    const VectorXs &M = scene.getM();
    VectorXs &v = scene.getV();
    VectorXs v1 = v.segment<2>(index1), v2 = v.segment<2>(index2);
    
    VectorXs nhat = n;
    nhat.normalize();
    
    double cfactor = (1.0 + getCOR())/2.0;
    double numerator = 2 * cfactor * (v2-v1).dot(nhat);
    double m1 = MASS(idx1), m2 = MASS(idx2);

    if(!scene.isFixed(idx1))
    {
        double denom1 = 1+(m1/m2);
        v.segment<2>(index1) += numerator/denom1 * nhat;
    }
    if(!scene.isFixed(idx2))
    {
        double denom2 = (m2/m1) + 1;
        v.segment<2>(index2) -= numerator/denom2 * nhat;
    }
}

// Responds to a collision detected between a particle and an edge by applying
// an impulse to the velocities of each one.
// Inputs:
//   scene: The scene data structure.
//   vidx:  The index of the particle.
//   eidx:  The index of the edge.
//   n:     The shortest vector between the particle and the edge.
// Outputs:
//   None.
void SimpleCollisionHandler::respondParticleEdge(TwoDScene &scene, int vidx, int eidx, const Vector2s &n)
{
    const VectorXs& X = scene.getX();
    const VectorXs& M = scene.getM();
    VectorXs& V = scene.getV();
    const edges& EDGES = scene.getEdges();
    
    int eidx1 = EDGES[eidx].first;
    int eidx2 = EDGES[eidx].second;
    
    int index1 = 2*vidx;
    int index2 = 2*eidx1;
    int index3 = 2*eidx2;
    
    VectorXs x1 = X.segment<2>(index1);
    VectorXs x2 = X.segment<2>(index2);
    VectorXs x3 = X.segment<2>(index3);
    
    VectorXs v1 = V.segment<2>(index1);
    VectorXs v2 = V.segment<2>(index2);
    VectorXs v3 = V.segment<2>(index3);
    
    VectorXs nhat = n;
    nhat.normalize();
    
    VectorXs x1x2 = x1-x2;
    VectorXs x3x2 = x3-x2;    
    double alpha = (x1x2).dot(x3x2)/(x3x2).dot(x3x2);
    alpha = std::min(1.0, std::max(0.0, alpha) );
    double OneMAlpha = 1.0 - alpha;
    
    VectorXs v3v2 = v3 - v2;
    VectorXs vedge = v2 + alpha*(v3v2);
    double cfactor = (1.0 + getCOR())/2.0;
    
    double m1 = MASS(vidx);
    double m2 = MASS(eidx1);
    double m3 = MASS(eidx2);
    
    double numerator = 2*cfactor*(vedge-v1).dot(nhat);
    if(!scene.isFixed(vidx))
    {
        double denom1 = 1.0 + OneMAlpha*OneMAlpha*m1/m2 + alpha*alpha*m1/m3;
        V.segment<2>(index1) += numerator/denom1 * nhat;
    }
    if(!scene.isFixed(eidx1))
    {
        double denom2 = m2/m1 + OneMAlpha*OneMAlpha + alpha*alpha*m2/m3;
        V.segment<2>(index2) -= (OneMAlpha)*numerator/denom2 * nhat;
    }
    if(!scene.isFixed(eidx2))
    {
        double denom3 = m3/m1 + OneMAlpha*OneMAlpha*m3/m2 + alpha*alpha;
        V.segment<2>(index3) -= alpha * numerator/denom3 * nhat;
    }
}


// Responds to a collision detected between a particle and a half-plane by 
// applying an impulse to the velocity of the particle.
// Inputs:
//   scene: The scene data structure.
//   vidx:  The index of the particle.
//   pidx:  The index of the half-plane.
//   n:     The shortest vector between the particle and the half-plane.
// Outputs:
//   None.
void SimpleCollisionHandler::respondParticleHalfplane(TwoDScene &scene, int vidx, int pidx, const Vector2s &n)
{
    VectorXs nhat = n;
    nhat.normalize();
    
    double cfactor = (1.0+getCOR())/2.0;
    
    int index1 = 2*vidx;
    VectorXs& V = scene.getV();
    VectorXs v1 = V.segment<2>(index1);
    V.segment<2>(index1) -= 2*cfactor*v1.dot(nhat)*nhat;
}
