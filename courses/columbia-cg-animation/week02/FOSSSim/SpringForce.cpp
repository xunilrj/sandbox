#include "SpringForce.h"

void SpringForce::addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E )
{
  assert( x.size() == v.size() );
  assert( x.size()%2 == 0 );
  assert( m_endpoints.first >= 0 );  assert( m_endpoints.first < x.size()/2 );
  assert( m_endpoints.second >= 0 ); assert( m_endpoints.second < x.size()/2 );

  // Add milestone 2 code here.
}

void SpringForce::addGradEToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, VectorXs& gradE )
{
  assert( x.size() == v.size() );
  assert( x.size() == gradE.size() );
  assert( x.size()%2 == 0 );
  assert( m_endpoints.first >= 0 );  assert( m_endpoints.first < x.size()/2 );
  assert( m_endpoints.second >= 0 ); assert( m_endpoints.second < x.size()/2 );

  //GET PARTICLES
  int size = 2;
  int xistart = m_endpoints.first*2;
  int xjstart = m_endpoints.second*2;  
  VectorXs xi = x.segment(xistart, size);
  VectorXs xj = x.segment(xjstart, size);
	
  //SPRING FORCE
  VectorXs nhat = xi-xj;  
  double lxixj = nhat.norm();
  nhat /= lxixj;	
  VectorXs dxiU = m_k*(lxixj-m_l0)*nhat;
  VectorXs dxjU = -dxiU;  
    
  //SPRING DAMPING FORCE 
  VectorXs vi = v.segment(xistart, size);
  VectorXs vj = v.segment(xjstart, size);
  double lvivj = (vi-vj).dot(nhat);
  dxiU += m_b * nhat * lvivj; //F-
  dxjU += -m_b * nhat * lvivj; //F+
	
  //CHANGE SYSTEM
  gradE[xistart+0] += dxiU[0];
  gradE[xistart+1] += dxiU[1];	
  gradE[xjstart+0] += dxjU[0];
  gradE[xjstart+1] += dxjU[1];
}
