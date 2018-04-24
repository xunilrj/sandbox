#include "ImplicitEuler.h"

bool ImplicitEuler::stepScene(TwoDScene& scene, scalar h)
{
  VectorXs& qn = scene.getX();
  VectorXs& qn1 = scene.getX();
  VectorXs& qdotn = scene.getV();
  VectorXs& qdotn1 = scene.getV();
    
  const VectorXs& m = scene.getM();
  const MatrixXs M = m.asDiagonal();
  assert(qn.size() == qdotn.size());
  assert(qn.size() == m.size());

  int ndof = qn.size();

  VectorXs stepqn = h*qdotn;
  VectorXs& stepqn1 = stepqn;
  VectorXs stepqdotn = VectorXs::Zero(ndof);
  VectorXs& stepqdotn1 = stepqdotn;
  
  MatrixXs A(ndof,ndof);

  VectorXs b(ndof);
  VectorXs deltaqdoti1(ndof);
  scalar stplen = std::numeric_limits<scalar>::infinity();

  int iteration_i = 0;
  int MAXITERATIONS = 1000;
  while ((stplen >= 1.0e-9)&&(iteration_i < MAXITERATIONS))
  {
    //−M(qi˙n+1 − q˙n) + hF(qn + hqi˙n+1, qi˙n+1)
    b.setZero();
    scene.accumulateGradU(b,/*qn+*/stepqn,/*qdotn+*/stepqdotn);
    b *= -h;
    b += -M*stepqdotn;

    zeroFixedDoFs(scene, b);

    //M − (h2*∂F/∂qn+1 + h*∂F/∂q˙n+1 )
    //where ∂F/∂qn+1 and ∂F/∂q˙n+1 are evaluated at (qn+hqi˙n+1, qi˙n+1).
    A.setZero();
    scene.accumulateddUdxdx(A,/*qn+*/stepqn,/*qdotn+*/stepqdotn);
    A *= h;
    scene.accumulateddUdxdv(A,/*qn+*/stepqn,/*qdotn+*/stepqdotn);
    A *= -h;
    A = M - A;
    setFixedRowsAndColsToIdentity(scene, A);

    deltaqdoti1 = A.fullPivLu().solve(b);

    stepqdotn1 += deltaqdoti1;
    stepqn1 = h*(qdotn+stepqdotn);
    
    stplen = deltaqdoti1.norm();
    ++iteration_i;
  }

  qn1   = qn + stepqn;
  qdotn = qdotn + stepqdotn; 
  
  return true;
}
