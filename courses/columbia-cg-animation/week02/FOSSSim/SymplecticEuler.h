#ifndef __SYMPLECTIC_EULER__
#define __SYMPLECTIC_EULER__

#include "SceneStepper.h"

class SymplecticEuler : public SceneStepper
{
public:
  SymplecticEuler();
  
  virtual ~SymplecticEuler();
  
  virtual bool stepScene( TwoDScene& scene, scalar dt );
  
  virtual std::string getName() const;
};

#endif
