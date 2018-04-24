#ifndef __COLLISION_HANDLER__
#define __COLLISION_HANDLER__

#include "TwoDScene.h"
#include "MathDefs.h"
#include <fstream>
#include <vector>

struct CollisionInfo
{
public:
    enum collisiontype { PP, PE, PH };
    
    CollisionInfo(collisiontype type, int idx1, int idx2, const Vector2s &n, double time) : m_type(type), m_idx1(idx1), m_idx2(idx2), m_n(n), m_time(time) {}
    
    std::string toString() const
    {
        std::stringstream ss;
        switch (m_type)
        {
            case PP:
                ss << "Particle " << m_idx1 << " - Particle " << m_idx2 << " with normal = " << m_n.transpose();
                break;
            case PE:
                ss << "Particle " << m_idx1 << " - Edge " << m_idx2 << " with normal = " << m_n.transpose();
                break;
            case PH:
                ss << "Particle " << m_idx1 << " - Halfplane " << m_idx2 << " with normal = " << m_n.transpose();
                break;
        }
        return ss.str();
    }
    
    bool operator == (const CollisionInfo & c) const { return m_type == c.m_type && m_idx1 == c.m_idx1 && m_idx2 == c.m_idx2; }

    collisiontype m_type;
    int m_idx1, m_idx2;
    Vector2s m_n;
    double m_time;
};

class CollisionHandler
{
public:
    CollisionHandler(double COR) : m_COR(COR) {}
    
    virtual ~CollisionHandler() {}
    
    virtual void handleCollisions( TwoDScene& scene, const VectorXs &oldpos, VectorXs &oldvel, scalar dt ) = 0;
    
    virtual std::string getName() const = 0;
    
    // Returns the coefficient of restitution set in the scene XML
    double getCOR() {return m_COR;}
    
    void clearImpulses() {m_impulses.clear();}
    
    const std::vector<CollisionInfo> &getImpulses() const {return m_impulses;}
    
    void serializeImpulses(std::ofstream &ofs);
    
    void loadImpulses(std::vector<CollisionInfo> &impulses, std::ifstream &ifs);
    
protected:
    void addParticleParticleImpulse(int idx1, int idx2, const Vector2s &n, double time);
    
    void addParticleEdgeImpulse(int vidx, int eidx, const Vector2s &n, double time);
    
    void addParticleHalfplaneImpulse(int vidx, int fidx, const Vector2s &n, double time);
    
private:
    
    double m_COR;
    
    std::vector<CollisionInfo> m_impulses;
    
};

#endif
