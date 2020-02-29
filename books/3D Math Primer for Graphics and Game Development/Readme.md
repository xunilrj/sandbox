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

[] 3 Multiple Coordinate Spaces
[] 3.1 Why Bother with Multiple Coordinate Spaces? 
[] 3.2 Some Useful Coordinate Spaces 
[] 3.3 Basis Vectors and Coordinate Space Transformations 
[] 3.4 Nested Coordinate Spaces 
[] 3.5 In Defense of Upright Space 

[] 4 Introduction to Matrices
[] 4.1 Mathematical Definition of Matrix 
[] 4.2 Geometric Interpretation of Matrix 
[] 4.3 The Bigger Picture of Linear Algebra 

[] 5 Matrices and Linear Transformations
[] 5.1 Rotation 
[] 5.2 Scale 
[] 5.3 Orthographic Projection 
[] 5.4 Reflection 
[] 5.5 Shearing 
[] 5.6 Combining Transformations 
[] 5.7 Classes of Transformations 

[] 6 More on Matrices
[] 6.1 Determinant of a Matrix 
[] 6.2 Inverse of a Matrix 
[] 6.3 Orthogonal Matrices 
[] 6.4 4 × 4 Homogeneous Matrices 
[] 6.5 4 × 4 Matrices and Perspective Projection 

[] 7 Polar Coordinate Systems
[] 7.1 2D Polar Space 
[] 7.2 Why Would Anybody Use Polar Coordinates? 
[] 7.3 3D Polar Space 
[] 7.4 Using Polar Coordinates to Specify Vectors 

[] 8 Rotation in Three Dimensions
[] 8.1 What Exactly is “Orientation”? 
[] 8.2 Matrix Form 
[] 8.3 Euler Angles 
[] 8.4 Axis-Angle and Exponential Map Representations 
[] 8.5 Quaternions 
[] 8.6 Comparison of Methods 
[] 8.7 Converting between Representations 

[] 9 Geometric Primitives
[] 9.1 Representation Techniques 
[] 9.2 Lines and Rays 
[] 9.3 Spheres and Circles 
[] 9.4 Bounding Boxes 
[] 9.5 Planes 
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