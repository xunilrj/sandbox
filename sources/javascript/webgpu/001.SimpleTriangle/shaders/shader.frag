#version 450

layout (location = 0) in vec3 inColor;
layout (location = 0) out vec4 outFragColor;

void main()
{
  float grayScale = dot(vec3(.2126, .7152, .0722), inColor.rgb);
  outFragColor = vec4(vec3(grayScale), 1.0);  
}