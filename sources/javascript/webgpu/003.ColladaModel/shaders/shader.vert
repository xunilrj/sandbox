#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 0) out vec3 outColor;

layout(binding = 0) uniform Uniforms {
    mat4 pvm;
};

void main()
{
    outColor = vec3(inUV.rg, 0);        
    gl_Position = pvm * vec4(inPos.xyz, 1.0);
}