#version 450

layout (location = 0) in vec3 VERTEXPOSITION;
layout (location = 1) in vec3 NORMAL;
layout (location = 2) in vec2 TEXCOORD;
layout (location = 0) out vec2 out_TEXCOORD;

layout(binding = 0) uniform Uniforms {
    mat4 pvm;
};

void main()
{
    out_TEXCOORD = TEXCOORD;
    gl_Position = pvm * vec4(VERTEXPOSITION.xyz, 1.0);
}
