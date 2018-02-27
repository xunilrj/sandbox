#include "GravitationalForce.h"

void GravitationalForce::addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size()%2 == 0 );
  assert( m_particles.first >= 0 );  assert( m_particles.first < x.size()/2 );
  assert( m_particles.second >= 0 ); assert( m_particles.second < x.size()/2 );

  // Add milestone 2 code here.
}

void GravitationalForce::addGradEToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, VectorXs& gradE )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size() == gradE.size() );
  assert( x.size()%2 == 0 );
  assert( m_particles.first >= 0 );  assert( m_particles.first < x.size()/2 );
  assert( m_particles.second >= 0 ); assert( m_particles.second < x.size()/2 );
	
  //GET PARTICLES
  int size = 2;
  int xistart = m_particles.first*2;
  int xjstart = m_particles.second*2;  
  VectorXs xi = x.segment(xistart, size);
  VectorXs xj = x.segment(xjstart, size);
	
  //CALCULATE FORCE
  VectorXs n = xi-xj;  
  double lxixj = n.norm();
  n /= lxixj;
  double forceLength = m_G*m[xistart]*m[xjstart]/(lxixj*lxixj);
  VectorXs dxiU = forceLength*n;
  VectorXs dxjU = -forceLength*n;
	
  //CHANGE SYSTEM
  gradE[xistart+0] += dxiU[0];
  gradE[xistart+1] += dxiU[1];	
  gradE[xjstart+0] += dxjU[0];
  gradE[xjstart+1] += dxjU[1];
}
