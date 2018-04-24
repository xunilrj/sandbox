#include "CollisionHandler.h"

class SimpleCollisionHandler : public CollisionHandler
{
public:
    SimpleCollisionHandler(double COR) : CollisionHandler(COR) {}
    
    virtual void handleCollisions(TwoDScene &scene, const VectorXs &oldpos, VectorXs &oldvel, scalar dt);
    
    virtual std::string getName() const;
    
private:
    
    bool detectParticleParticle(TwoDScene &scene, int idx1, int idx2, Vector2s &n);
    bool detectParticleEdge(TwoDScene &scene, int vidx, int eidx, Vector2s &n);
    bool detectParticleHalfplane(TwoDScene &scene, int vidx, int pidxs, Vector2s &n);
    
    
    void respondParticleParticle(TwoDScene &scene, int idx1, int idx2, const Vector2s &n);
    void respondParticleEdge(TwoDScene &scene, int vidx, int eidx, const Vector2s &n);
    void respondParticleHalfplane(TwoDScene &scene, int vidx, int pidx, const Vector2s &n);
    
};
