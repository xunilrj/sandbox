#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inColor;

layout (location = 0) out vec3 outColor;

layout(binding = 0) uniform Uniforms {
    mat4 pvm;
};

void main()
{
    outColor = inColor;
    gl_Position = pvm[gl_VertexIndex];
}