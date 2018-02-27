#include "ExplicitEuler.h"
#include <Eigen/LU>
#include <Eigen/Core>

bool ExplicitEuler::stepScene( TwoDScene& scene, scalar dt )
{
    // Your code goes here!
    
    // Some tips on getting data from TwoDScene:
    // A vector containing all of the system's position DoFs. x0, y0, x1, y1, ...
    //VectorXs& x = scene.getX();
    // A vector containing all of the system's velocity DoFs. v0, v0, v1, v1, ...
    //VectorXs& v = scene.getV();
    // A vector containing the masses associated to each DoF. m0, m0, m1, m1, ...
    //const VectorXs& m = scene.getM();
    // Determine if the ith particle is fixed
    // if( scene.isFixed(i) )
    
    VectorXs& qn = scene.getX();
    VectorXs& qn1 = qn;
    VectorXs& qdotn = scene.getV();
    VectorXs& qdotn1 = qdotn;
    
    const VectorXs& m = scene.getM();
    MatrixXs M = m.asDiagonal();
    MatrixXs MMinus1 = M.inverse();
    
    VectorXs f(qn.size());
    scene.accumulateGradU(f);
    
    //(n,1) = (n,n)*(n,1)
    VectorXs Pn =  M*qdotn;
    //(n,1) = (n,1) + (1,1)*(n,n)*(n,1)
    qn1 = qn + dt*MMinus1*Pn;
    qdotn1 = qdotn + dt*f;
    
    for(int i = 0; i < scene.getNumParticles(); ++i)
    {
        if(scene.isFixed(i))
        {
            scene.setVelocity(i, {0,0});
        }
    }
    
    return true;
}
