#include "LinearizedImplicitEuler.h"



bool LinearizedImplicitEuler::stepScene(TwoDScene& scene, scalar h )
{
  VectorXs& qn = scene.getX();
  VectorXs& qn1 = qn;
  VectorXs& qdotn = scene.getV();
  VectorXs& qdotn1 = qdotn; 
  const VectorXs& m = scene.getM();
  assert(qn.size() == qdotn.size());
  assert(qn.size() == m.size());
    
  int ndof = qn.size();

  // Ax = b
  // b
  VectorXs b = VectorXs::Zero(ndof);
  VectorXs stepq = h*qdotn;
  VectorXs stepqdot = VectorXs::Zero(ndof);
  scene.accumulateGradU(b,/*qn+*/stepq, /*qdotn+*/stepqdot);
  b *= -h;
  zeroFixedDoFs(scene, b);

  //A
  MatrixXs partialFpartialq = MatrixXs::Zero(ndof,ndof);
  scene.accumulateddUdxdx(partialFpartialq,/*qn+*/stepq,/*qdotn+*/stepqdot);
    
  MatrixXs partialFpartialqdot = MatrixXs::Zero(ndof,ndof);
  scene.accumulateddUdxdv(partialFpartialqdot,/*qn+*/stepq,/*qdotn+*/stepqdot);
  
  MatrixXs M = m.asDiagonal();
  MatrixXs A = M - (h*h*-partialFpartialq + h*-partialFpartialqdot);
  setFixedRowsAndColsToIdentity(scene,A);

  {
      //Ax = b
      VectorXs deltaqdot = A.fullPivLu().solve(b);
      qdotn1 = qdotn + deltaqdot;
      qn1 = qn + h*qdotn1;
  }
  
  return true;
}
