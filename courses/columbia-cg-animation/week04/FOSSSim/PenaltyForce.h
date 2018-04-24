#ifndef __PENALTY_FORCE_H__
#define __PENALTY_FORCE_H__

#include <Eigen/Core>
#include "Force.h"
#include <iostream>

class TwoDScene;

class PenaltyForce : public Force
{
public:

  PenaltyForce( const TwoDScene &scene, const scalar stiffness, const scalar thickness );

  virtual ~PenaltyForce();
  
  virtual void addEnergyToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, scalar& E );
  
  virtual void addGradEToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, VectorXs& gradE );
  
  virtual void addHessXToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );
  
  virtual void addHessVToTotal( const VectorXs& x, const VectorXs& v, const VectorXs& m, MatrixXs& hessE );
  
  virtual Force* createNewCopy();

  void addParticleParticleGradEToTotal(const VectorXs &x, int idx1, int idx2, VectorXs &gradE);

  void addParticleEdgeGradEToTotal(const VectorXs &x, int vidx, int eidx, VectorXs &gradE);

  void addParticleHalfplaneGradEToTotal(const VectorXs &x, int vidx, int pidx, VectorXs &gradE);

private:
  const TwoDScene &m_scene;
  const scalar m_k;
  const scalar m_thickness;
};

#endif
