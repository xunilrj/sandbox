#ifndef __DRAG_DAMPING_FORCE_H__
#define __DRAG_DAMPING_FORCE_H__

#include <Eigen/Core>
#include <iostream>

#include "Force.h"
#include "MathDefs.h"

class DragDampingForce : public Force
{
public:

  DragDampingForce( const scalar& b );

  virtual ~DragDampingForce();
  
  virtual void addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E );
  
  virtual void addGradEToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, VectorXs& gradE );
  
  virtual void addHessXToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );
  
  virtual void addHessVToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );
  
  virtual Force* createNewCopy();

private:
  scalar m_b;
};

#endif
