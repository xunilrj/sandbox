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

We will start with a simple 2D Foward Kinematics, because it is simpler. This will avoid matrix math, at least for now.

The first thing we need to understand is the hierarchy. Bones are just trees. Where each transformation is applied in relation to its parent. So we can start with the simples possible meaningfull bones tree: two bones.

//TODO draw two bones.

We must first recognize the elements in this diagram. Bones what circles with crosses. The center of the cross is the bone position. As the bone rotate a small blue angle is shown. Change the root bone position and its angle to see what happens.

//TODO draw interactive bones

the triangle is the bone itself. It has a length. Its end is called the "effector" because is robotic it is where the tool that does something is positioned. The effector position is simply 

bone.x = parent.effector.x;
bone.y = parent.effector.y;
effector.x = bone.x + bone.length*cos(bone.angle);  
effector.y = bone.y + bone.length*sin(bone.angle);  

That is why when you hover the bone you see a dashed circle. This is where the effector can reach moving only that bone.

## Simple Render

Now we just pass all matrices to the shader and use them to transform the vertex

attribute vec2 v_pos;
attribute int v_bone;

uniform mat3 u_bone[20];
uniform mat3 u_world;

void main() {
    vec3 pos0 = vec3(v_pos, 1.0) * u_bone[v_bone);    
    vec3 pos = vec3(pos.xy, 1) * u_world;
    gl_Position = vec4(pos, 1.0);
}

// TODO show example

## Deformed Mesh

But we can achieve much more complex scenarios if we allow each vertex to be modified by more than one bone.

What we need is a distance in the bind pose for each bone that affects a vertex. We also need the how much that bone affects. The sum of these weights should be 1, of course.

With this we can render our mesh as:

attribute vec3 bind[5];
attribute int bones[5];

uniform mat3 u_bone[20];
uniform mat3 u_world;

void main() {
    ...
    vec3 pos0 = bind[0].z * (vec3(bind[0].xy, 1.0) * u_bone[bones[0]);
    vec3 pos1 = bind[1].z * (vec3(bind[1].xy, 1.0) * u_bone[bones[1]);
    ...

    vec3 pos = pos0 + pos1 + ...;
    pos = vec3(pos.xy, 1) * u_world;
    gl_Position = vec4(pos, 1.0);
}

In this case we can have up to 20 bones. And each vertex of a mesh can be affected by up to 5 bones.

//TODO show example

## Inverse Kinematics

In the FK case we basically pass a list of angles (for each bone) and receive a list of matrices and the effector position. But there are some cases where we want the inverse. We want to pass the effector position and generate the list of angles that will put the bones in that particular configuration.

This is a much harder problem. To be able to solve it we will first take a lot of simplifications. For example, there is no constraints on the angle rotation. And there is no constraint on the effector position.

The analytical solution depends on how many bones a skeleton has and its parameters. It get ugly fast. We will use a iterative approach. This means that we will choose a small modification to the angle list that will bring the effector closer to the point. And repeat this step multiple times until the effector is close enough.

The first assumption here is that the chosen position is reachable. If it isn't we will project this position to the closest viable one.

After this we will calculate how much the effector position change when we change the angle of each bone. With this modification list in hand we will choose the small step and apply to the current angle configuration.

And we will repeat this until we are close enough.

The catch here is that although it is complicated to calculate the change in the effector position it is very simple to calculate a linear approximation. Linear approximations are derivatives. And the derivatives os sin/cos are cos/sin.

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

## Animation

Now all we need to do is animate the angles and we will have model animation. The catch here is how we store the animation.

The easiest method is to use keyframes. At certain poses we store the angle configuration and when playing the animation we just interpolate these angles.