vec4 borderColor = vec4(1.0,1.0,1.0,1.0);
vec4 textColor = vec4(1.0,0.0,0.0,1.0);
float borderStart = -0.50;
float borderEnd = 0.0;
float innerStart = -0.01;
float innerEnd = 0.0;

float smoothstep_zero(float edge0, float edge1, float x)
{
    if(x > edge1) return 0.0;
    return smoothstep(edge0, edge1, x);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = fragCoord/iResolution.xy;
    vec4 color = texture(iChannel0, uv);
    
    float sd = (color.r * 255.0) * 256.0 * 256.0;
    sd += (color.g * 255.0) * 256.0;
    sd += (color.b * 255.0);
    sd /= 16777215.0;
    sd = 1.0 - sd;
    sd = sd * 2.0 - 1.0;
    
    fragColor = vec4(0,0,0,1);
    
    float borderW = smoothstep_zero(borderStart, borderEnd, sd);
    fragColor += borderW * borderColor;
    
    float textW = smoothstep(innerStart, innerEnd, sd);
    fragColor += textW * textColor;
    
    //fragColor = vec4(sd,sd,sd,1);
}