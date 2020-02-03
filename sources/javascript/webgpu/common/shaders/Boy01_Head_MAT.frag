#version 450
//Boy01_Head_MAT
//emissionColor: null
//ambientColor: null
//diffuseColor: null
//specularColor: 0.0248,0.0248,0.0248,1
//reflectiveColor: null
//transparentColor: null
//shininess: 20
//reflectivity: null
//transparency: 1
//diffuseTexture: {"texture":"file3-sampler","texture_safe":"file3_sampler","texcoord":"CHANNEL0"}
layout (location = 0) in vec2 CHANNEL0;
layout (location = 0) out vec4 fragment_color;
layout (binding = 1) uniform sampler sampler_file3_sampler;
layout (binding = 2) uniform texture2D tex_file3_sampler;
void main()
{
    /*AUTOGEN - DIFFUSE MAPS - START*/
    vec4 color_0 = texture(sampler2D(tex_file3_sampler, sampler_file3_sampler), CHANNEL0.st);
    /*AUTOGEN - DIFFUSE MAPS - END*/
    /*AUTOGEN - RETURN FRAGMENT COLOR - START*/
    fragment_color = color_0;
    /*AUTOGEN - RETURN FRAGMENT COLOR - END*/
}