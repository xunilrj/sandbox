struct VS_OUTPUT_QUANT
{
float4 Pos           : POSITION0;
float4 vDiffuse : COLOR;
float2 TexCoord      : TEXCOORD0;
float2 TexCoord1     : TEXCOORD1;
};
 float4x4  ViewProjX;   // obj->clip
 float4x4modelViewXf;   // obj->view
 float4    light0Pos;    
 float4    light1Pos;    
 float4    lightDir;    
 float4    materialColor;
 float     light0Intensity;
 float     light1Intensity;
 float     ambientLightIntensity;
 floatlightDirIntensity;
 float4x4WorldXf;   // obj->world
 float4x4ViewWorldXf;   // obj->viewWorld
 floatsilhouetteSize; 

// bone palette
 float4x4  bonesPalette[18];

VS_OUTPUT_QUANT VS_main( float4 inPos: POSITION0, float3 inNormal: NORMAL0,float2 inTexCoord : TEXCOORD0,
 
  float3 blendWeight: BLENDWEIGHT0, float4 blendIndices : COLOR, float3 inNomral2 : NORMAL1)

{
VS_OUTPUT_QUANT Out;
float4 inPosScale= ( 1.0f - silhouetteSize );

float4 scaler= {64.0f, 64.0f, 64.0f, 64.0f };
   float4 mapper= {0.5f , 0.5f , 0.0f , 0.0f  };
   float4 mapperA= {1.0f , 1.0f , 0.0f , 0.0f  };
float4 scaledIndices= blendIndices * scaler;

float4 skinPos = mul( inPos, bonesPalette[scaledIndices.z])* blendWeight.xxxx;
float4 skinPos1 = mul( inPos, bonesPalette[scaledIndices.y])* blendWeight.yyyy;
float4 skinPos2 = mul( inPos, bonesPalette[scaledIndices.x])* blendWeight.zzzz;

float4 skinFinalPos = skinPos + skinPos1 + skinPos2;

float4 skinNorm = mul( inNormal, bonesPalette[scaledIndices.z])* blendWeight.xxxx;
float4 skinNorm1 = mul( inNormal, bonesPalette[scaledIndices.y])* blendWeight.yyyy;
float4 skinNorm2 = mul( inNormal, bonesPalette[scaledIndices.x])* blendWeight.zzzz;
float4 skinFinalNorm = skinNorm + skinNorm1 + skinNorm2;

skinNorm = mul( inNormal, bonesPalette[scaledIndices.z])* blendWeight.xxxx;
skinNorm1 = mul( inNormal, bonesPalette[scaledIndices.y])* blendWeight.yyyy;
skinNorm2 = mul( inNormal, bonesPalette[scaledIndices.x])* blendWeight.zzzz;
float4 skinFinalNorm2 = skinNorm + skinNorm1 + skinNorm2;

float4 worldPos= mul( skinFinalPos,  WorldXf  );
float4 worldViewNormal= mul( skinFinalNorm,  modelViewXf  );
float4 worldViewNormal2= mul( skinFinalNorm2,  modelViewXf  );

float4 scaledNormal= -worldViewNormal * inPosScale;
   float4 worldNormalOffset= mul( scaledNormal,  ViewWorldXf  );

float4 finalWorldPos = worldPos + worldNormalOffset;

// to view projection
Out.Pos= mul(finalWorldPos, ViewProjX);

float4 normEnvN = normalize(worldViewNormal2);
normEnvN = normEnvN +mapperA;
normEnvN = normEnvN *mapper;
Out.TexCoord.x = normEnvN.x;
Out.TexCoord.y = (mapperA.x - normEnvN.y);
Out.vDiffuse= materialColor;
Out.TexCoord1= inTexCoord;
return Out;
}, 0
