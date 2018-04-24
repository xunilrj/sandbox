#ifndef __IMPLICIT_EULER__
#define __IMPLICIT_EULER__

#include <Eigen/Dense>
#include <iostream>

#include "SceneStepper.h"

class ImplicitEuler : public SceneStepper
{
public:
  ImplicitEuler();
  
  virtual ~ImplicitEuler();
  
  virtual bool stepScene( TwoDScene& scene, scalar dt );
  
  virtual std::string getName() const;
  
  static void zeroFixedDoFs( const TwoDScene& scene, VectorXs& vec )
{
  int nprts = scene.getNumParticles();
  for( int i = 0; i < nprts; ++i ) if( scene.isFixed(i) ) vec.segment<2>(2*i).setZero();
}

static void setFixedRowsAndColsToIdentity( const TwoDScene& scene, MatrixXs& mat )
{
  int nprts = scene.getNumParticles();
  for( int i = 0; i < nprts; ++i ) if( scene.isFixed(i) )
  {
    mat.row(2*i).setZero();
    mat.row(2*i+1).setZero();
    mat.col(2*i).setZero();
    mat.col(2*i+1).setZero();
    // Set diagonal of fixed degrees of freedom to 1
    mat(2*i,2*i) = 1.0;
    mat(2*i+1,2*i+1) = 1.0;
  }  
}
};

#endif
