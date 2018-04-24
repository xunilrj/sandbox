#ifndef __GRAVITATIONAL_FORCE_H__
#define __GRAVITATIONAL_FORCE_H__

#include <Eigen/Core>
#include "Force.h"
#include <iostream>

class GravitationalForce : public Force
{
public:

  GravitationalForce( const std::pair<int,int>& particles, const scalar& G );

  virtual ~GravitationalForce();

  virtual void addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E );

  virtual void addGradEToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, VectorXs& gradE );

  virtual void addHessXToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );

  virtual void addHessVToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );

  virtual Force* createNewCopy();

void add1(MatrixXs& hessE, int Fi, int qi, const Matrix2s& f)
{
    hessE.block<2,2>(Fi, qi) += f;
}

void add(MatrixXs& hessE, int i, int j, const Matrix4s& J)
{
    int index_i = 2*i;
    int index_j = 2*j;
    add1(hessE, index_i, index_i, J.block<2,2>(0,0));add1(hessE, index_i, index_j, J.block<2,2>(2,0));
    add1(hessE, index_j, index_i, J.block<2,2>(0,2));add1(hessE, index_j, index_j, J.block<2,2>(2,2));
}
private:
  std::pair<int,int> m_particles;
  // Gravitational constant
  scalar m_G;
};

#endif
