# Bones, Deformed Mesh and Foward/Inverse Kinematics

# Bones

There easiest way to animate characters is to use sprites sheets of course. You just change which image is rendered according to the animation and time and it is done. 

// TODO sprite animation

But this is not the only way. And not necessarily the best way. Another famous technique is to use bones, skinned character, skinned mesh etc... Multiples names to the same technique.

Here you create a structure tha resemble a skeleton, that is why it is names bones. You choose how to render each bone and you animate to bones, not the images.

For example:

// TODO bones animation

This technique IS more complex, but creates much powerful characters. Using deformed meshes in some bones allows you to deliver more dynamic animations more easily. For example, a hair that follows wind, or a character that aims correctly where it should etc...

But to understand what is behind this we will start with a much simples version. First we will understand the math behind using a 2 bones arm. In this case we will calculate what is called the Foward Kinematics and the Inverve Kinematics. After having the bones behaving like we want, we will jump into more sophisticated "skinning", where we draw what we actually want to draw.

## Foward Kinematics

## Inverse Kinematics

Oh My God, I Inverted Kine!
http://www.cs.cmu.edu/~15464-s13/assignments/assignment2/jlander_gamedev_sept98.pdf

[TODO]
https://www.math.ucsd.edu/~sbuss/ResearchWeb/ikmethods/SdlsPaper.pdf

### Pseudo-Inverse Method

The pseudo-inverse method can be traced to the famous paper "Resolved Motion Rate Control of Manipulators and Human Prostheses". The paper is actually for robot arms, but can be adapted the character animations as we are using them here.

B. Characteristic Equation of Manipulator  
Resolved Motion Rate Control of Manipulators and Human Prostheses  
https://ieeexplore.ieee.org/document/4081862  
PDF - https://www.researchgate.net/publication/3298017_Resolved_Motion_Rate_Control_of_Space_Manipulators_with_Generalized_Jacobian_Matrix  

### Unreachable Targets

## Skinning

### Simple Skinning

### Deformed Meshes

### Deformed Meshes using GLSL

## Animation