#ifndef HYBRID_COLLISION_COMPARISON
#define HYBRID_COLLISION_COMPARISON

#include "HybridCollisionHandler.h"
#include <vector>
#include <iostream>

struct ImpactZoneInfo
{
    ImpactZones Z;
    VectorXs q;
    VectorXs qdot;
};

class HybridCollisionComparison
{
public:
    HybridCollisionComparison();
    
    void reset();
    void load(std::ifstream &ifs);
    void serialize(std::ofstream &ofs);
    
    void serializeImpactZones(const ImpactZones &Z, const VectorXs &q, const VectorXs &qdot, int iter);
    void serializePostImpulses(const VectorXs &q, const VectorXs &qdot);
    
    bool comparePI(VectorXs &q, VectorXs &qdot);
    
    bool compareIZ(const ImpactZones &iz, const VectorXs &q, const VectorXs &qdot, int iter);
    bool doneWithImpactZones(int iter);
    
    bool failed() {return fail;}
    
private:
    VectorXs PIq;
    VectorXs PIqdot;
    std::vector<ImpactZoneInfo> IZiters;
    
    bool fail;
    
};

#endif
