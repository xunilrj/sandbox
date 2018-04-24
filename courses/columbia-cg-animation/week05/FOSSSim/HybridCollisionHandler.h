#ifndef HYBRID_COLLISION_HANDLER_H
#define HYBRID_COLLISION_HANDLER_H

#include "CollisionHandler.h"
#include <vector>
#include <set>
#include <list>
#include "ContinuousTimeCollisionHandler.h"

struct ImpactZone
{
    ImpactZone(std::set<int> vertices, bool halfplane) : m_verts(vertices), m_halfplane(halfplane) {}
    std::set<int> m_verts;
    bool m_halfplane;
    
    bool operator==(const ImpactZone &other) const
    {
        return m_verts == other.m_verts && m_halfplane == other.m_halfplane;
    }
    
    bool operator<(const ImpactZone &other) const
    {
        if(!m_halfplane && other.m_halfplane)
            return true;
        if(m_halfplane && !other.m_halfplane)
            return false;
        if(m_verts.size() == 0)
        {
            if(other.m_verts.size() > 0)
                return true;
            return false;
        }
        if(other.m_verts.size() == 0)
            return false;
        return *m_verts.begin() < *other.m_verts.begin();
    }
};

typedef std::vector<ImpactZone> ImpactZones;

class HybridCollisionHandler : public ContinuousTimeCollisionHandler
{
public:
    HybridCollisionHandler(int maxiters, double COR);
    ~HybridCollisionHandler();
    
    virtual void handleCollisions(TwoDScene &scene, const VectorXs &oldpos, VectorXs &oldvel, scalar dt );
    
    bool applyIterativeImpulses(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, const VectorXs &qdote, double dt, VectorXs &qefinal, VectorXs &qdotefinal);
    
    std::vector<CollisionInfo> detectCollisions(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe);
    
    void applyImpulses(const TwoDScene &scene, const std::vector<CollisionInfo> &collisions, const VectorXs &qs, const VectorXs &qe, const VectorXs &qdote, double dt, VectorXs &qm, VectorXs &qdotm);
    
    void applyGeometricCollisionHandling(const TwoDScene &scene, const VectorXs &qs, const VectorXs &qe, const VectorXs &qdote, double dt, VectorXs &qefinal, VectorXs &qdotefinal);
    
    void performFailsafe(const TwoDScene &scene, const VectorXs &oldpos, const ImpactZone &zone, double dt, VectorXs &qe, VectorXs &qdote);
    
    virtual std::string getName() const;
    
private:
    
    const int m_maxiters;
    
};

#endif
