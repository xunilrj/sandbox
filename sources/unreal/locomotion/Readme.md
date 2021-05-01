# Basic locomotion using Mixamo

# Model

https://www.turbosquid.com/3d-models/girl-model-1637866

# Mixamo

## Rigging

## Basic Idle, Walking, Running

# Exporting Mixamo to Unreal

Before we can use the models exported from Mixamo, we need to convert them to a format that is compatible with Unreal.

We will be using a Blender plugin called "Mixamo Converter" that contains a very usefull "Batch Convert". We just need to point to a folder and it will convert every fbx for us.

Mind the "Tranfer rotation disabled". In some cases this may be useful, but we will disable for now because it creates some problems with some Mixamo animations we chosen.

With this disabled our animations will only move straitgh forward (or left, or right etc...); but no rotations.

https://github.com/enziop/mixamo_converter

![Screenshot010.jpg](./Screenshot010.jpg)

This is our folder with Mixamo exported fbx. The plugin will convert everything to that "output" folder. Be warned that the plugin does not have a progress bar. So it looks like it hanged; just wait.

![Screenshot010.jpg](./Screenshot011.jpg)



# Inside Unreal

## Creating Project

Now we have everything that we need to create our project. We can start from a "black project". That will demand us to create everything. All the fun!

![Screenshot001.jpg](./Screenshot001.jpg)

We will chose the C++ project to be able to customize our game using C++ later.

![Screenshot002.jpg](./Screenshot002.jpg)

I have configured Unreal to use Visual Studio Code before (Edit -> Editor Preference -> Source Code -> Source Code Editor, if you wanna do the same)

![Screenshot003.jpg](./Screenshot003.jpg)

Back to Unreal I have created the following four folders; to organize what we are going to do here.

![Screenshot004.jpg](./Screenshot004.jpg)

I immediatelly hit Ctrl+S to save the current level.

![Screenshot005.jpg](./Screenshot005.jpg)

After this we can go to "Editor -> Project Settings -> Maps & Mode -> Default Maps" and we can choose the current leval as the defaul for both the game and the Editor.

In my case I just called it "Sandbox".

![Screenshot005.jpg](./Screenshot006.jpg)

Given that we are here, we can go to "Edit -> Project Settings -> Input -> Bindings" and create out input bidings.

We want W/S to walk/run; and D/A to walk sideways. Booster will allow us to unlock out "super fast run"! Probably not as exciting as it sounds...

Mind the scale chosen: from -2.0 to 2.0. This will make sense later.

![Screenshot005.jpg](./Screenshot007.jpg)

Before we jump into the character creation we need to create our GameMode. We don't need to configure it yet.

![Screenshot005.jpg](./Screenshot008.jpg)

Now we can start importing our model. First let us create two folders under "Character". Girl will contain everything related to this model we chosen. Mixamo will contain everything that is generic and specific to Mixamo; be it: Skeleton and Animations.

![Screenshot005.jpg](./Screenshot009.jpg)

## Import model in T-Pose

Now we can import the first model, where the girl is in the T-Pose. We will import this model to the Girl folder, because this will import the Girl model, its textures etc...

![Screenshot012.jpg](./Screenshot012.jpg)

![Screenshot013.jpg](./Screenshot013.jpg)

Import part here is to select "Import Mesh" and "Skeletal Mesh".

![Screenshot014.jpg](./Screenshot014.jpg)

This is what we should get.

![Screenshot015.jpg](./Screenshot015.jpg)

To keep things organized, let us move the Skeleton to the folder Mixamo and rename it.

![Screenshot015.jpg](./Screenshot016.jpg)

![Screenshot015.jpg](./Screenshot017.jpg)

## Create Character

Now we can start creating the Girl Character.

![Screenshot015.jpg](./Screenshot018.jpg)

First step is to select the Girl Model for the Girl's mesh.

![Screenshot015.jpg](./Screenshot019.jpg)

The model does not comes correctly aligned (multiple ways to fix this). Here we will just rotate the model here. It needs to be facing the blue arrow.

![Screenshot015.jpg](./Screenshot020.jpg)

![Screenshot015.jpg](./Screenshot021.jpg)

Then we decrease the capsule to more tigh embrace the model and we are fine.

![Screenshot015.jpg](./Screenshot022.jpg)

Also create a "Spring Arm" and a "Camera" below the Capsule. Do not crate them below the Mesh.

![Screenshot015.jpg](./Screenshot025.jpg)

With the character created we can go back to our "DefaultGameMode" and choose this Character as the "Default Pawn".

![Screenshot015.jpg](./Screenshot023.jpg)

And choose this GameMode as the "Default GameMode" at "Edit -> Project Settings -> Maps and Modes -> Default Modes".

![Screenshot015.jpg](./Screenshot024.jpg)

If we hit "Play" now, we should see our Character in its full glory. Static in its T-Pose. First milestone achieved!

![Screenshot015.jpg](./Screenshot026.jpg)


## Import Animations

We are ready to start importing our animations. We will start with the easiest one: Idle. GO back to the Mixamo folder and import the Idle fbx.

Do no import mesh. We want just the animation.

![Screenshot015.jpg](./Screenshot027.jpg)

![Screenshot015.jpg](./Screenshot028.jpg)

Inside the Girl folder, create an AnimationBlueprint. This will drive the model animation. Important to create an AnimationBlueprint to the Mixamo Skeleton we imported.

![Screenshot015.jpg](./Screenshot029.jpg)

![Screenshot015.jpg](./Screenshot030.jpg)

And create an "Blend Space" (not Blend Space 1D) inside the Mixamo folder. Also important to choose the correct Skeleton.

This will allow us the blend multiple animations together.

![Screenshot015.jpg](./Screenshot032.jpg)

Inside the Blend Space, configure both horizontal and vertical axis to go from -2.0 to 2.0. (Starting to realize why our input is mapped to this range?)

![Screenshot015.jpg](./Screenshot033.jpg)

From the righ pane, drag-and-drop the Idle animation to the very center of the space. You should see the Idle animation playing.

![Screenshot015.jpg](./Screenshot034.jpg)

Jump back to the Animation Blueprint and dang-and-drop the BlendSpace and connect it to the "Final Pose".

![Screenshot015.jpg](./Screenshot035.jpg)

![Screenshot015.jpg](./Screenshot036.jpg)

Last step is to switch to the Girl Character blueprint and apply this "Animation Blueprint" to it.

![Screenshot015.jpg](./Screenshot037.jpg)

If we hit play... Boom! Model animated.

![Screenshot015.jpg](./Screenshot038.jpg)

## Blending Walk, Run, Left and Right Animations

We have everything in place to blend out walk and run animation.

But first go to the Girl folder and create a Struct. This will contain shared data between "Girl Character" and "Girl Animation Blueprint".

![Screenshot015.jpg](./Screenshot039.jpg)

![Screenshot015.jpg](./Screenshot040.jpg)

For now we just need two properties.

![Screenshot015.jpg](./Screenshot041.jpg)

At the "Girl Animation BLueprint" create a variable and select this Struct as its type.

![Screenshot015.jpg](./Screenshot042.jpg)

Drang-and-drop this variable as "Get" and connect its variables to the "Blend Space" inputs.

![Screenshot015.jpg](./Screenshot043.jpg)

![Screenshot015.jpg](./Screenshot044.jpg)

![Screenshot015.jpg](./Screenshot045.jpg)

Now, we need to copy this data from the "Girl Character". To do this, go to the "Event Graph" tab, cast the "Pawn Onwer" to the "Girl Character" (better ways to do this, but this is fine for now), and copy both properties.

![Screenshot015.jpg](./Screenshot047.jpg)

And switch to the "Girl Character" and react to input. 

We chosen a very simple model here. We will LERP (interpolate) between where we are, and the axis value.

We don't need the "Print String" node, of course. It is there just to illustrate.

![Screenshot015.jpg](./Screenshot048.jpg)

![Screenshot015.jpg](./001.gif)

Go back to the Mixamo folder and import all other animations. Here we are importing: walking, running, strafe left, strafe right.

![Screenshot015.jpg](./Screenshot049.jpg)

Deselect "Import Mesh"; and hit "Import All"

![Screenshot015.jpg](./Screenshot050.jpg)

This is what you should get.

![Screenshot015.jpg](./Screenshot051.jpg)

At our Blend Space, drag-and-drop these animations as below.

![Screenshot015.jpg](./Screenshot052.jpg)

Hit "Play" and.... oh no. Our first bug!.

![Screenshot015.jpg](./002.gif)

The problem here is that we exported all Mixamo animations with Root Motion. So the animation  does not keep the model on its place, but we do not apply this transformation to the "Girl Character".

That is the reason of this loop.

What we need to is to tell Unreal to this for us. We can easily do this select all animations. And hitting "Asset Actions -> Bulk Edit".

![Screenshot015.jpg](./Screenshot053.jpg)

![Screenshot015.jpg](./Screenshot054.jpg)

Inside the "Property Matrix" we can enable "Enable Root Motion".

![Screenshot015.jpg](./Screenshot055.jpg)

If we hit Play now... and we have our second bug.
Now the model does not move. 

We still have one little thing to do.

![Screenshot015.jpg](./003.gif)

Go back to the "Girl Animation Blueprint" and under "Animation Preview" choose "Root Motion from Everything" and hit the "Apply" button.

Worst UI ever!

![Screenshot015.jpg](./Screenshot056.jpg)

![Screenshot015.jpg](./Screenshot057.jpg)

Well... what matters is now everything works.

![Screenshot015.jpg](./004.gif)
