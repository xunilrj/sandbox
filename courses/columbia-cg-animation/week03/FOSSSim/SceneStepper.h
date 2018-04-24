#ifndef __SCENE_STEPPER__
#define __SCENE_STEPPER__

#include "TwoDScene.h"

#include "MathDefs.h"

class SceneStepper
{
public:
  virtual ~SceneStepper();
  
  virtual bool stepScene( TwoDScene& scene, scalar dt ) = 0;
  
  virtual std::string getName() const = 0;
};

#endif
