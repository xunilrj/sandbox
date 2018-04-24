#include "SpringForce.h"

void SpringForce::addHessXToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size() == hessE.rows() );
  assert( x.size() == hessE.cols() );
  assert( x.size()%2 == 0 );
  assert( m_endpoints.first >= 0 );  assert( m_endpoints.first < x.size()/2 );
  assert( m_endpoints.second >= 0 ); assert( m_endpoints.second < x.size()/2 );

  // Implement force Jacobian here!
  // Contribution from elastic component
  int index_i = 2*m_endpoints.first;
  int index_j = 2*m_endpoints.second;
  //m_local
  scalar mi = m(index_i);
  scalar mj = m(index_j);
  //q_local
  Vector2s pi = x.segment<2>(index_i);
  Vector2s pj = x.segment<2>(index_j);
  //v_local
  Vector2s vi = v.segment<2>(index_i);
  Vector2s vj = v.segment<2>(index_j);
    
  Vector2s nhat = pj-pi; 
  scalar l = nhat.norm(); 
  nhat /= l;

  //j_local
  Matrix2s nhatnhatt = nhat*nhat.transpose();
  scalar normalized_displacement = (l-m_l0)/l;
  Matrix2s K = nhatnhatt + normalized_displacement*(Matrix2s::Identity()-nhatnhatt);
  K *= -m_k;
    
  Matrix4s J = Matrix4s::Zero();
  J.block<2,2>(0,0) = K;
  J.block<2,2>(2,0) = -K;
  J.block<2,2>(0,2) = -K;
  J.block<2,2>(2,2) = K;

  // Contribution from damping
  Vector2s vivj = vi-vj;
  Vector2s vivjt = vivj.transpose();
  K = -(m_b/l)
      *(nhat.dot(vivj)*Matrix2s::Identity() + nhat*vivj.transpose())
      *(Matrix2s::Identity() - nhatnhatt);
    
  J.block<2,2>(0,0) += K;J.block<2,2>(2,0) += -K;
  J.block<2,2>(0,2) += -K;J.block<2,2>(2,2) += K;     
    
  //j_global
  add(hessE, m_endpoints.first, m_endpoints.second,  -J);
}

void SpringForce::addHessVToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE )
{
  assert( x.size() == v.size() );
  assert( x.size() == m.size() );
  assert( x.size() == hessE.rows() );
  assert( x.size() == hessE.cols() );
  assert( x.size()%2 == 0 );
  assert( m_endpoints.first >= 0 );  assert( m_endpoints.first < x.size()/2 );
  assert( m_endpoints.second >= 0 ); assert( m_endpoints.second < x.size()/2 );

  // Implement force Jacobian here!
  int index_i = 2*m_endpoints.first;
  int index_j = 2*m_endpoints.second;
  // Contribution from damping
  Vector2s pi = x.segment<2>(index_i);
  Vector2s pj = x.segment<2>(index_j);
  Vector2s nhat = pj-pi; 
  scalar l = nhat.norm(); 
  nhat /= l;
  Matrix2s B = m_b*nhat*nhat.transpose();

  Matrix4s J = Matrix4s::Zero();
  J.block<2,2>(0,0) = -B;J.block<2,2>(2,0) =  B;
  J.block<2,2>(0,2) =  B;J.block<2,2>(2,2) = -B;
    
  add(hessE, m_endpoints.first, m_endpoints.second,  -J);
}
