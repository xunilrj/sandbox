#include "GravitationalForce.h"



void GravitationalForce::addHessXToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size() == hessE.rows() );
  assert( x.size() == hessE.cols() );
  assert( x.size()%2 == 0 );
    
  int index_i = 2*m_particles.first;
  int index_j = 2*m_particles.second;
    
  //m_local
  scalar mi = m(index_i);
  scalar mj = m(index_j);
    
  //q_local
  Vector2s pi = x.segment<2>(index_i);
  Vector2s pj = x.segment<2>(index_j);  
  Vector2s nhat = pj-pi; 
  scalar l = nhat.norm(); 
  nhat /= l;

  //j_local
  Matrix2s K = Matrix2s::Identity() - 3.0*nhat*nhat.transpose();
  K *= -m_G*mi*mj/(l*l*l);
    
  Matrix4s J = Matrix4s::Zero();
  J.block<2,2>(0,0) = K;
  J.block<2,2>(2,0) = -K;
  J.block<2,2>(0,2) = -K;
  J.block<2,2>(2,2) = K;

  //j_global
  add(hessE, m_particles.first, m_particles.second,  -J);
}

void GravitationalForce::addHessVToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size() == hessE.rows() );
  assert( x.size() == hessE.cols() );
  assert( x.size()%2 == 0 );
  // Nothing to do.
}
