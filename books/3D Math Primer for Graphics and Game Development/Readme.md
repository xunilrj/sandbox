# 3D Math Primer for Graphics and Game Development

Implementation of the book "3D Math Primer for Graphics and Game Development"

## Philosophy

- Newest possible C++ version 2x;  
- Must compile to webassembly;  
- Any zero cost abstraction that help the development is well received (why? to have brace initializer working in all cases - https://stackoverflow.com/a/48402461/5397116);  
- Objects will be bag os properties with methods;  
- C-like API (just function, does not necessarily compile in a C compiler).  

## Roadmap

[x] 2.3 Specifying Vectors with Cartesian Coordinates 
[x] 2.5 Negating a Vector 
[x] 2.6 Vector Multiplication by a Scalar 
[x] 2.7 Vector Addition and Subtraction 
[x] 2.8 Vector Magnitude (Length) 
[x] 2.9 Unit Vectors 
[x] 2.10 The Distance Formula 
[x] 2.11 Vector Dot Product 
[x] 2.12 Vector Cross Product 
[x] 2.13 Linear Algebra Identities 

[] Vector Functional API using our "func.h"
[] Vector WASM

[x] 4 Introduction to Matrices
[x] 4.1.2 describes square matrices.
[x] 4.1.4 describes matrix transposition.
[x] 4.1.5 explains how to multiply a matrix by a scalar.
[x] 4.1.6 explains how to multiply a matrix by another matrix.
[x] 4.1.7 explains how to multiply a vector by a matrix.

[] 5 Matrices and Linear Transformations
[x] 5.1 Rotation 
[x] 5.1.2 3D Rotation about Cardinal Axes
[x] 5.1.3 3D Rotation about an Arbitrary Axis
[x] 5.2 Scale 
[x] 5.2.1 Scaling along the Cardinal Axes
[ ] 5.2.2 Scaling in an Arbitrary Direction
[] 5.3 Orthographic Projection 
[] 5.4 Reflection 
[] 5.5 Shearing 

[] 6 More on Matrices
[x] 6.1 Determinant of a Matrix 
[x] 6.2 Inverse of a Matrix 
[x] 6.3 Orthogonal Matrices 
[x] 6.4 4 × 4 Homogeneous Matrices 
[x] 6.5 4 × 4 Matrices and Perspective Projection 

[] 7 Polar Coordinate Systems
[] 7.1 2D Polar Space 
[] 7.2 Why Would Anybody Use Polar Coordinates? 
[] 7.3 3D Polar Space 
[] 7.4 Using Polar Coordinates to Specify Vectors 

[] 8 Rotation in Three Dimensions
[] 8.1 What Exactly is “Orientation”? 
[] 8.2 Matrix Form 
[] 8.3 Euler Angles 
[] Another reason to choose Euler angles when you need to save space is that the numbers you are storing are more easily compressed.
[] 8.4 Axis-Angle and Exponential Map Representations 
[x] 8.5 Quaternions 
[x] 8.5.1 Quaternion Notation
[x] 8.5.3 Quaternion Negation
[x] 8.5.4 Identity Quaternion(s)
[x] 8.5.5 Quaternion Magnitude
[x] 8.5.6 Quaternion Conjugate and Inverse
[x] 8.5.7 Quaternion Multiplication
[] 8.5.8 Quaternion “Difference”
[] 8.5.9 Quaternion Dot Product
[] 8.5.10 Quaternion log, exp, and Multiplication by a Scalar
[] 8.5.11 Quaternion Exponentiation
[x] 8.5.12 Quaternion Interpolation, a.k.a. Slerp
[] 8.6 Comparison of Methods 
[] 8.7 Converting between Representations 
[] 8.7.1 shows how to convert Euler angles to a matrix.
[] 8.7.2 shows how to convert a matrix to Euler angles.
[x] 8.7.3 shows how to convert a quaternion to a matrix.
[x] 8.7.4 shows how to convert a matrix to a quaternion.
[x] 8.7.5 shows how to convert Euler angles to a quaternion.
[x] 8.7.6 shows how to convert a quaternion to Euler angles.

[] 9 Geometric Primitives
[] 9.1 Representation Techniques 
[] 9.2 Lines and Rays 
[] 9.3 Spheres and Circles 
[] 9.4 Bounding Boxes 
[x] 9.4.1 Representing AABBs
[x] 9.4.2 Computing AABBs
[x] 9.4.4 Transforming AABBs
[] 9.5 Planes 
[x] 9.5.2 Defining a Plane by Using Three Points
[x] 9.5.4 Distance from Point to Plane
[] 9.6 Triangles 
[] 9.7 Polygons 

[] 10 Mathematical Topics from 3D Graphics
[] 10.1 How Graphics Works 
[] 10.2 Viewing in 3D 
[] 10.3 Coordinate Spaces 
[] 10.4 Polygon Meshes 
[] 10.5 Texture Mapping 
[] 10.6 The Standard Local Lighting Model 
[] 10.7 Light Sources 
[] 10.8 Skeletal Animation 
[] 10.9 Bump Mapping 
[] 10.10 The Real-Time Graphics Pipeline 
[] 10.11 Some HLSL Examples 
[] 10.12 Further Reading 

[] 11 Mechanics 1: Linear Kinematics and Calculus
[] 11.1 Overview and Other Expectation-Reducing Remarks 
[] 11.2 Basic Quantities and Units 
[] 11.3 Average Velocity 
[] 11.4 Instantaneous Velocity and the Derivative 
[] 11.5 Acceleration 
[] 11.6 Motion under Constant Acceleration 
[] 11.7 The Integral 
[] 11.8 Uniform Circular Motion 

[] 12 Mechanics 2: Linear and Rotational Dynamics
[] 12.1 Newton’s Three Laws 
[] 12.2 Some Simple Force Laws 
[] 12.3 Momentum 
[] 12.4 Impulsive Forces and Collisions 
[] 12.5 Rotational Dynamics 
[] 12.6 Real-Time Rigid Body Simulators 
[] 12.7 Suggested Reading 

[] 13 Curves in 3D
[] 13.1 Parametric Polynomial Curves 
[] 13.2 Polynomial Interpolation 
[] 13.3 Hermite Curves 
[] 13.4 Bezier Curves 
[] 13.5 Subdivision 
[] 13.6 Splines 
[] 13.7 Hermite and Bezier Splines 
[] 13.8 Continuity 
[] 13.9 Automatic Tangent Control 

[] A Geometric Tests
[] A.1 Closest Point on 2D Implicit Line 
[] A.2 Closest Point on a Parametric Ray 
[] A.3 Closest Point on a Plane 
[] A.4 Closest Point on a Circle or Sphere 
[] A.5 Closest Point in an AABB 
[] A.6 Intersection Tests 
[] A.7 Intersection of Two Implicit Lines in 2D 
[] A.8 Intersection of Two Rays in 3D 
[] A.9 Intersection of a Ray and Plane 
[] A.10 Intersection of an AABB and Plane 
[] A.11 Intersection of Three Planes 
[] A.12 Intersection of Ray and a Circle or Sphere 
[] A.13 Intersection of Two Circles or Spheres 
[] A.14 Intersection of a Sphere and AABB 
[] A.15 Intersection of a Sphere and a Plane 
[] A.16 Intersection of a Ray and a Triangle 
[] A.17 Intersection of Two AABBs 
[] A.18 Intersection of a Ray and an AABB 

TODO
Improve robust float
Improve Unit of Measures