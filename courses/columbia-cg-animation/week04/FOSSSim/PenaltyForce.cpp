#include "PenaltyForce.h"
#include "TwoDScene.h"

void PenaltyForce::addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E )
{
    // Feel free to implement if you feel like doing so.
}

// BEGIN STUDENT CODE


// Adds the gradient of the penalty potential (-1 * force) for a pair of 
// particles to the total.
// Read the positions of the particles from the input variable x. Radii can
// be obtained from the member variable m_scene, the penalty force stiffness 
// from member variable m_k, and penalty force thickness from member variable
// m_thickness.
// Inputs:
//   x:    The positions of the particles in the scene. 
//   idx1: The index of the first particle, i.e. the position of this particle
//         is ( x[2*idx1], x[2*idx1+1] ).
//   idx2: The index of the second particle.
// Outputs:
//   gradE: The total gradient of penalty force. *ADD* the particle-particle
//          gradient to this total gradient.
void PenaltyForce::addParticleParticleGradEToTotal(const VectorXs &x, int idx1, int idx2, VectorXs &gradE)
{
    int index1 = 2*idx1;
    int index2 = 2*idx2;
    
    VectorXs x1 = x.segment<2>(index1);
    VectorXs x2 = x.segment<2>(index2);
    
    double r1 = m_scene.getRadius(idx1);
    double r2 = m_scene.getRadius(idx2);
    
    VectorXs n = x2-x1;
    scalar nnorm = n.norm();
    
    VectorXs nhat = n;
    nhat.normalize();
    
    scalar size = r1 + r2 + m_thickness; 
    if(nnorm < size)
    {        
        gradE.segment<2>(index1) -= m_k * (nnorm - size) * nhat;
        gradE.segment<2>(index2) += m_k * (nnorm - size) * nhat;
    }    
}

// Adds the gradient of the penalty potential (-1 * force) for a particle-edge
// pair to the total.
// Read the positions of the particle and edge endpoints from the input
// variable x.
// Inputs:
//   x:    The positions of the particles in the scene.
//   vidx: The index of the particle.
//   eidx: The index of the edge, i.e. the indices of the particle making up the
//         endpoints of the edge are given by m_scene.getEdge(eidx).first and 
//         m_scene.getEdges(eidx).second.
// Outputs:
//   gradE: The total gradient of penalty force. *ADD* the particle-edge
//          gradient to this total gradient.
void PenaltyForce::addParticleEdgeGradEToTotal(const VectorXs &x, int vidx, int eidx, VectorXs &gradE)
{
    std::pair<int,int> edge = m_scene.getEdge(eidx);
    
    int index1 = 2*vidx;
    int index2 = 2*edge.first;
    int index3 = 2*edge.second;
    
    VectorXs x1 = x.segment<2>(index1);
    VectorXs x2 = x.segment<2>(index2);
    VectorXs x3 = x.segment<2>(index3);
    
    double r1 = m_scene.getRadius(vidx);
    double r2 = m_scene.getEdgeRadii()[eidx];
    
    VectorXs x3x2 = x3-x2;
    
    double alpha = (x1-x2).dot(x3x2)/(x3x2).dot(x3x2);
    alpha = clamp(alpha, 0.0, 1.0);
    scalar MAlpha = 1.0 - alpha;
    
    VectorXs n = x2 + alpha*(x3x2) - x1;
    scalar nnorm = n.norm();
    
    VectorXs nhat = n;
    nhat.normalize();
    
    scalar size = (r1+r2 + m_thickness);
    if(nnorm < size)
    {
        scalar nnormsize = nnorm - size;
        gradE.segment<2>(index1) -= m_k*nnormsize*nhat;        
        gradE.segment<2>(index2) += m_k*MAlpha*nnormsize*nhat;
        gradE.segment<2>(index3) += m_k*alpha*nnormsize*nhat;
    }    
}

// Adds the gradient of the penalty potential (-1 * force) for a particle-
// half-plane pair to the total.
// Read the positions of the particle from the input variable x.
// Inputs:
//   x:    The positions of the particles in the scene.
//   vidx: The index of the particle.
//   pidx: The index of the half-plane, i.e. the position and normal vectors
//         for the half-plane can be retrieved by calling
//         m_scene.getHalfplane(pidx).
// Outputs:
//   gradE: The total gradient of the penalty force. *ADD* the particle-
//          half-plane gradient to this total gradient.
void PenaltyForce::addParticleHalfplaneGradEToTotal(const VectorXs &x, int vidx, int pidx, VectorXs &gradE)
{
    int index1 = 2*vidx;
    
    VectorXs x1 = x.segment<2>(index1);
    std::pair<VectorXs, VectorXs> plane = m_scene.getHalfplane(pidx);
    VectorXs pdir = plane.first;
    VectorXs pn = plane.second;
    
    scalar pndotpn = pn.dot(pn);
    VectorXs n = ((pdir - x1).dot(pn)/pndotpn)*pn;
    scalar nnorm = n.norm();
    
    VectorXs nhat = n;
    nhat.normalize();
    
    double r = m_scene.getRadius(vidx);    
    scalar size = r + m_thickness;
    if(nnorm < size)
    {
        gradE.segment<2>(index1) -= m_k*(nnorm-size)*nhat.dot(pn)/pndotpn*pn;
    }
    
}
