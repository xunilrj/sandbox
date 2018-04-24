#ifndef CONTINUOUS_TIME_COLLISION_HANDLER_H
#define CONTINUOUS_TIME_COLLISION_HANDLER_H

#include "CollisionHandler.h"
#include <vector>
#include <iostream>


class ContinuousTimeCollisionHandler : public CollisionHandler
{
public:
    ContinuousTimeCollisionHandler(double COR) : CollisionHandler(COR) {}
    
    virtual void handleCollisions(TwoDScene &scene, const VectorXs &oldpos, VectorXs &oldvel, scalar dt );
    
    virtual std::string getName() const;
    
public:
    
    bool detectParticleParticle     (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int idx1, int idx2, Vector2s &n, double &time);
    bool detectParticleEdge         (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int eidx, Vector2s &n, double &time);
    bool detectParticleHalfplane    (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int pidx, Vector2s &n, double &time);
    
    void respondParticleParticle    (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int idx1, int idx2, const Vector2s &n, double time, double dt, VectorXs &qm, VectorXs &qdotm);
    void respondParticleEdge        (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int eidx, const Vector2s &n, double time, double dt, VectorXs &qm, VectorXs &qdotm);
    void respondParticleHalfplane   (const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, int vidx, int pidx, const Vector2s &n, double time, double dt, VectorXs &qm, VectorXs &qdotm);
    
};

#endif
