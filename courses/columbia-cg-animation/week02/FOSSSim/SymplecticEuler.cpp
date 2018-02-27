#include "SymplecticEuler.h"
#include <Eigen/LU>
#include <Eigen/Core>

bool SymplecticEuler::stepScene( TwoDScene& scene, scalar dt )
{
    VectorXs& qn = scene.getX();
    VectorXs& qn1 = qn;
    VectorXs& qdotn = scene.getV();
    VectorXs& qdotn1 = qdotn;
    const VectorXs& m = scene.getM();
    MatrixXs M = m.asDiagonal();
    MatrixXs MMinus1 = M.inverse();
    
	//Symplectic Euler	
    VectorXs deltaU = VectorXs::Zero(qn.size());
    VectorXs& f = deltaU;
    scene.accumulateGradU(deltaU);	
	f = -1*deltaU;	
    
    qdotn1 = qdotn + dt*MMinus1*f;    
        
    for(int i = 0; i < scene.getNumParticles(); ++i)
        if(scene.isFixed(i))
            scene.setVelocity(i, {0,0});
  
    qn1 = qn + dt*qdotn1;
    
    return true;
}






