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

private:
  std::pair<int,int> m_particles;
  // Gravitational constant
  scalar m_G;
};

#endif
