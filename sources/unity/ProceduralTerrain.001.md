# Procedural Terrain 


## Mesh and Texture Generation

For this part we will generate a simple plane and apply a completely random texture to it.

```
[ExecuteInEditMode, RequireComponent(typeof(MeshFilter), typeof(MeshRenderer))]
public class TerrainComponent : MonoBehaviour
{
    public int Width = 100;
    public int Height = 100;

    public void UpdateMesh()
    {        
    }
}
```


```
[CustomEditor(typeof(TerrainComponent))]
public class TerrainEditor : Editor
{
    public override void OnInspectorGUI()
    {
        base.OnInspectorGUI();
        var gen = GUILayout.Button("Generate");
        if (gen)
        {
            foreach (TerrainComponent t in targets)
            {
                t.UpdateMesh();
            }
        }
    }
}
```


```
public void UpdateMesh()
{
    MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
    meshRenderer.sharedMaterial = new Material(Shader.Find("Standard"));

    var t2d = new Texture2D(Width, Height);
    var pixels = new Color[Width * Height];
    for (var x = 0; x < Width; ++x)
    {
        for (var y = 0; y < Height; ++y)
        {
            pixels[y * Width + x] = Color.Lerp(
                Color.black,
                Color.white,
                Random.Range(0, 1.0f));
        }
    }
    t2d.SetPixels(pixels);
    t2d.Apply();
}
```



```
public static class Noise
{
    public static float[,] Generate(int width, int height)
    {
        var values = new float[width, height];

        for (var x = 0; x < width; ++x)
        {
            for (var y = 0; y < height; ++y)
            {
                values[x, y] = Random.Range(0, 1.0f);
            }
        }

        return values;
    }
}
```


```
public void UpdateMesh()
{
    MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
    meshRenderer.sharedMaterial = new Material(Shader.Find("Standard"));

    var noise = Noise.Generate(Width, Height);

    var t2d = new Texture2D(Width, Height);
    var pixels = new Color[Width * Height];
    for (var x = 0; x < Width; ++x)
    {
        for (var y = 0; y < Height; ++y)
        {
            pixels[y * Width + x] = Color.Lerp(
                Color.black,
                Color.white,
                noise[x, y]);
        }
    }
    t2d.SetPixels(pixels);
    t2d.Apply();
}
```

Now we are ready to create the geometry.

```
public void UpdateMesh()
{
    MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
    meshRenderer.sharedMaterial = new Material(Shader.Find("Standard"));

    var noise = Noise.Generate(Width, Height);

    var t2d = new Texture2D(Width, Height);
    var pixels = new Color[Width * Height];
    for (var x = 0; x < Width; ++x)
    {
        for (var y = 0; y < Height; ++y)
        {
            pixels[y * Width + x] = Color.Lerp(
                Color.black,
                Color.white,
                noise[x, y]);
        }
    }
    t2d.SetPixels(pixels);
    t2d.Apply();
    meshRenderer.sharedMaterial.mainTexture = t2d;

    MeshFilter meshFilter = gameObject.GetComponent<MeshFilter>();

    Mesh mesh = new Mesh();

    Vector3[] vertices = new Vector3[4]
    {
        new Vector3(0, 0, 0),
        new Vector3(Width, 0, 0),
        new Vector3(0, Height, 0),
        new Vector3(Width, Height, 0)
    };
    mesh.vertices = vertices;

    int[] tris = new int[6]
    {
        // lower left triangle
        0, 2, 1,
        // upper right triangle
        2, 3, 1
    };
    mesh.triangles = tris;

    Vector3[] normals = new Vector3[4]
    {
        -Vector3.forward,
        -Vector3.forward,
        -Vector3.forward,
        -Vector3.forward
    };
    mesh.normals = normals;

    Vector2[] uv = new Vector2[4]
    {
        new Vector2(0, 0),
        new Vector2(1, 0),
        new Vector2(0, 1),
        new Vector2(1, 1)
    };
    mesh.uv = uv;

    meshFilter.mesh = mesh;
}
```

## Perlin Noise 1 - Scale

Now we are ready to implement our Perlin Noise.


```
public enum NoiseType
{
    Random = 0,
    Perlin = 1,
}
```

```
public struct PerlinNoiseOptions
{
    [Range(0, 10)]
    public float Scale;

    public PerlinNoiseOptions(float scale = 10.0f)
    {
        Scale = scale;
    }
}
```


```
public static float[,] Generate(int width, int height, PerlinNoiseOptions options)
    {
        var scale = Mathf.Max(options.Scale, 0.0001f);

        var values = new float[width, height];

        for (var x = 0; x < width; ++x)
        {
            for (var y = 0; y < height; ++y)
            {
                var pnx = x / scale;
                var pny = y / scale;
                values[x, y] = Mathf.PerlinNoise(pnx, pny);
            }
        }

        return values;
    }
```

```
public void UpdateMesh(object options = null)
    {
        MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
        meshRenderer.sharedMaterial = new Material(Shader.Find("Standard"));


        var noise = Noise.Generate(Width, Height, options);
```

```
public override void OnInspectorGUI()
    {
        serializedObject.Update();
        serializedObject.Show(nameof(TerrainComponent.Width));
        serializedObject.Show(nameof(TerrainComponent.Height));
        var noiseValue = serializedObject.Enum<NoiseType>(
            nameof(TerrainComponent.Type));

        if (noiseValue == NoiseType.Perlin)
        {
            EditorGUILayout.PropertyField(
                serializedObject.FindProperty(nameof(TerrainComponent.PerlinNoiseOptions))
            );
        }

        var r = serializedObject.ApplyModifiedProperties();
        var gen = GUILayout.Button("Generate");

        if (r || gen)
        {
            foreach (TerrainComponent t in targets)
            {
                if (noiseValue == NoiseType.Random)
                    t.UpdateMesh();
                else if (noiseValue == NoiseType.Perlin)
                    t.UpdateMesh(t.PerlinNoiseOptions);
            }
        }
    }
```


```
public static class SerializedObjectExtensions
{
    public static bool Show(this SerializedObject obj, string name)
    {
        return EditorGUILayout.PropertyField(
            obj.FindProperty(name)
        );
    }

    public static T Enum<T>(this SerializedObject obj, string name)
     where T : struct
    {
        var p = obj.FindProperty(name);
        EditorGUILayout.PropertyField(p);
        return (T)(object)p.enumValueIndex;
    }
}
```

## Perlin Noise 2 - Octaves and Lacunarity

```
[System.Serializable]
public struct PerlinNoiseOptions
{
    [Range(0, 10)]
    public float Scale;

    [Range(1, 100)]
    public int Octaves;

    [Range(1, 10)]
    public float Persistance;


    [Range(0, 1)]
    public float Lacunarity;
}
```

```
public static float[,] Generate(int width, int height, PerlinNoiseOptions options)
{
    var scale = Mathf.Max(options.Scale, 0.0001f);
    var octaves = options.Octaves;

    var values = new float[width, height];

    var max = float.MinValue;
    var min = float.MaxValue;
    for (var x = 0; x < width; ++x)
    {
        for (var y = 0; y < height; ++y)
        {
            var amp = 1.0f;
            var freq = 1.0f;
            for (var o = 0; o < octaves; ++o)
            {
                var pnx = x / scale * freq;
                var pny = y / scale * freq;
                values[x, y] += Mathf.PerlinNoise(pnx, pny)
                    * amp;

                amp *= options.Persistance;
                freq *= options.Lacunarity;
            }

            max = Mathf.Max(max, values[x, y]);
            min = Mathf.Min(min, values[x, y]);
        }
    }

    for (var x = 0; x < width; ++x)
    {
        for (var y = 0; y < height; ++y)
        {
            values[x, y] = Mathf.InverseLerp(min, max, values[x, y]);
        }
    }

    return values;
}
```

## Perlin Noise 3 - Seed and Offsets

```
[System.Serializable]
public struct PerlinNoiseOptions
{
    [Range(0, 100)]
    public float Scale;

    [Range(1, 32)]
    public int Octaves;

    [Range(0, 1)]
    public float Persistance;


    [Range(1, 3)]
    public float Lacunarity;

    public int Seed;

    public Vector2 Offset;
}
```

```
 public static float[,] Generate(int width, int height, PerlinNoiseOptions options)
{
    var scale = Mathf.Max(options.Scale, 0.0001f);
    var octaves = options.Octaves;
    var offx = options.Offset.x;
    var offy = options.Offset.y;

    var random = new System.Random(options.Seed);
    var octavesOffsets = new Vector2[octaves];
    for (var o = 0; o < octaves; ++o)
    {
        var x = random.Next(-100000, 100000) + offx;
        var y = random.Next(-100000, 100000) + offy;
        octavesOffsets[o] = new Vector2(x, y);
    }

    var values = new float[width, height];

    var max = float.MinValue;
    var min = float.MaxValue;

    var hw = width / 2f;
    var hh = height / 2f;

    for (var x = 0; x < width; ++x)
    {
        for (var y = 0; y < height; ++y)
        {
            var amp = 1.0f;
            var freq = 1.0f;
            var v = 0.0f;

            for (var o = 0; o < octaves; ++o)
            {
                float sx = (octavesOffsets[o].x + x - hw) / scale * freq;
                float sy = (octavesOffsets[o].y + y - hh) / scale * freq;

                float ov = Mathf.PerlinNoise(sx, sy) * 2 - 1;
                v += ov * amp;

                amp *= options.Persistance;
                freq *= options.Lacunarity;
            }

            values[x, y] = v;

            max = Mathf.Max(max, values[x, y]);
            min = Mathf.Min(min, values[x, y]);
        }
    }

    for (var x = 0; x < width; ++x)
        for (var y = 0; y < height; ++y)
            values[x, y] = Mathf.InverseLerp(min, max, values[x, y]);

    return values;
}
```

## Perlin Noise 3 - Grid and Height

```
public class TerrainMeshGenerator
{
    public static void Generate(Mesh mesh, float maxY, float[,] heights)
    {
        var width = heights.GetLength(0);
        var height = heights.GetLength(1);

        var vertCount = width * height;
        var idxCount = (width - 1) * (height - 1) * 6;

        var tris = new int[idxCount];
        var i = 0;
        for (var x = 0; x < width - 1; ++x)
        {
            for (var y = 0; y < height - 1; ++y)
            {
                var vi = y * width + x;

                tris[i + 0] = vi;
                tris[i + 1] = vi + width + 1;
                tris[i + 2] = vi + width;

                tris[i + 3] = vi + width + 1;
                tris[i + 4] = vi;
                tris[i + 5] = vi + 1;

                i += 6;
            }
        }

        var vertices = new Vector3[vertCount];
        var normals = new Vector3[vertCount];
        var uv = new Vector2[vertCount];

        var startx = -(width / 2f);
        var starty = -(height / 2f);

        i = 0;
        for (var x = 0; x < width; ++x)
        {
            for (var y = 0; y < height; ++y)
            {
                var h = heights[x, y] * maxY;

                vertices[i].x = startx + x;
                vertices[i].y = h;
                vertices[i].z = starty + y;

                normals[i].x = 0;
                normals[i].y = 1;
                normals[i].z = 0;

                uv[i].x = (float)x / (float)width;
                uv[i].y = (float)y / (float)height;

                i += 1;
            }
        }


        mesh.vertices = vertices;
        mesh.normals = normals;
        mesh.uv = uv;
        mesh.triangles = tris;
    }
}
```

```
public class TerrainComponent : MonoBehaviour
{
    public int Width = 100;
    public int Height = 100;

    public float MaxY = 1;

    ...
}
```


```
public void UpdateMesh(object options = null)
{
   
   ...

    MeshFilter meshFilter = gameObject.GetComponent<MeshFilter>();
    Mesh mesh = new Mesh();
    TerrainMeshGenerator.Generate(mesh, MaxY, noise);
    meshFilter.mesh = mesh;
}
```

## Perlin Noise 4 - Height Curve

```
public class TerrainComponent : MonoBehaviour
{
    public int Width = 100;
    public int Height = 100;

    public float MaxY = 1;
    public AnimationCurve HeightCurve;
    
    public void UpdateMesh(object options = null)
    {
        ...
        TerrainMeshGenerator.Generate(mesh, MaxY, HeightCurve, noise);
        ...
    }
}
```

```
public static void Generate(Mesh mesh, float maxY,
    AnimationCurve hcurve,
    float[,] heights)
{
    ...
    for (var x = 0; x < width; ++x)
    {
        for (var y = 0; y < height; ++y)
        {
            var h = hcurve.Evaluate(heights[x, y]) * maxY;

            vertices[i].x = startx + x;
            vertices[i].y = h;
            vertices[i].z = starty + y;

            normals[i].x = 0;
            normals[i].y = 1;
            normals[i].z = 0;

            uv[i].x = (float)x / (float)width;
            uv[i].y = (float)y / (float)height;

            i += 1;
        }
    }
    ...
}

## Perlin Noise 5 - Normals and Material


```
public class TerrainMeshGenerator
{
    public static void Generate(Mesh mesh, float maxY,
        AnimationCurve hcurve,
        float[,] heights)
    {
        ...
        mesh.vertices = vertices;
        mesh.normals = normals;
        mesh.uv = uv;
        mesh.triangles = tris;

        mesh.RecalculateNormals();
    }
}
```



```
Shader "Custom/TerrainShader"
{
    Properties
    {
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 200

        CGPROGRAM
        #pragma surface surf Standard fullforwardshadows
        #pragma target 3.0

        struct Input
        {
            float3 worldPos;
        };

        float minHeight;
        float maxHeight;

        float ilerp(float a, float b, float v)
        {
            return (v-a)/(b-a);
        }

        void surf (Input IN, inout SurfaceOutputStandard o)
        {
            float h = ilerp(minHeight, maxHeight, IN.worldPos.y);
            o.Albedo = float3(0,h,0);
        }
        ENDCG
    }
    FallBack "Diffuse"
}
```


```
public struct GenerateResult
{
    public float MaxHeight;
    public float MinHeight;
}
```

```
public static GenerateResult Generate(Mesh mesh, float maxY,
    AnimationCurve hcurve,
    float[,] heights)
{
    ...
    for (var x = 0; x < width; ++x)
    {
        for (var y = 0; y < height; ++y)
        {
            var h = hcurve.Evaluate(heights[x, y]) * maxY;

            result.MinHeight = Mathf.Min(result.MinHeight, h);
            result.MaxHeight = Mathf.Max(result.MaxHeight, h);
        }
    }
    ...

    mesh.RecalculateNormals();

    return result;
}
```


```
public class TerrainComponent : MonoBehaviour
{
    public float minHeight;
    public float maxHeight;

    public void UpdateMesh(object options = null)
    {
        ...
        MeshFilter meshFilter = gameObject.GetComponent<MeshFilter>();
        Mesh mesh = new Mesh();
        var r = TerrainMeshGenerator.Generate(mesh, MaxY, HeightCurve, noise);
        meshFilter.mesh = mesh;

        minHeight = r.MinHeight;
        maxHeight = r.MaxHeight;
    }

    void Update()
    {
        MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
        Material mat = meshRenderer.sharedMaterial as Material;
        mat.SetFloat("minHeight", minHeight);
        mat.SetFloat("maxHeight", maxHeight);
    }
}
```


```
float startX;
float startZ;
float endX;
float endZ;

void surf (Input IN, inout SurfaceOutputStandard o)
{
    float2 uv = float2(
        ilerp(startX, endX, IN.worldPos.x),
        ilerp(startZ, endZ, IN.worldPos.z)
    );
    float4 w = tex2D (_MainTex, uv);
    w /= w.x + w.y + w.z + w.w;

    o.Albedo = w;
}
```


```
public class TerrainComponent : MonoBehaviour
{
    void Update()
    {
        MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
        Material mat = meshRenderer.sharedMaterial as Material;
        mat.SetFloat("startX", -(Width / 2.0f));
        mat.SetFloat("startZ", -(Height / 2.0f));
        mat.SetFloat("endX", (Width / 2.0f));
        mat.SetFloat("endZ", (Height / 2.0f));
        mat.SetFloat("minHeight", minHeight);
        mat.SetFloat("maxHeight", maxHeight);
    }

    public void UpdateMesh(object options = null)
    {
        MeshRenderer meshRenderer = GetComponent<MeshRenderer>();
        if (meshRenderer.sharedMaterial == null)
            meshRenderer.sharedMaterial = new Material(Shader.Find("TerrainShader"));

        var noise = Noise.Generate(Width, Height, options);
        var meshFilter = gameObject.GetComponent<MeshFilter>();
        var mesh = new Mesh();
        var r = TerrainMeshGenerator.Generate(mesh, MaxY, HeightCurve, noise);
        meshFilter.mesh = mesh;

        minHeight = r.MinHeight;
        maxHeight = r.MaxHeight;

        var t2d = new Texture2D(Width, Height);
        var pixels = new Color[Width * Height];
        for (var x = 0; x < Width; ++x)
        {
            for (var y = 0; y < Height; ++y)
            {
                var f = noise[x, y];
                if (f >= 0.0 && f < 0.2)
                    pixels[y * Width + x] = new Color(1.0f, 0.0f, 0.0f, 0.0f);
                else if (f >= 0.2 && f < 0.4)
                    pixels[y * Width + x] = new Color(0.0f, 1.0f, 0.0f, 0.0f);
                else if (f >= 0.6 && f < 0.8)
                    pixels[y * Width + x] = new Color(0.0f, 0.0f, 1.0f, 0.0f);
                else if (f >= 0.8 && f <= 1.0)
                    pixels[y * Width + x] = new Color(0.0f, 0.0f, 0.0f, 1.0f);
                else
                    pixels[y * Width + x] = new Color(1.0f, 0.0f, 0.0f, 0.0f);
            }
        }
        t2d.SetPixels(pixels);
        t2d.Apply();
        meshRenderer.sharedMaterial.mainTexture = t2d;
    }
}
```


```
Shader "Custom/TerrainShader"
{
    Properties
    {
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 200

        CGPROGRAM
        #pragma surface surf Standard fullforwardshadows
        #pragma target 3.0

        struct appdata
        {
            float4 vertex : POSITION;
            float2 uv : TEXCOORD0;
        };

        struct Input
        {
            float3 worldPos;
        };

        float minHeight;
        float maxHeight;
        float startX;
        float startZ;
        float endX;
        float endZ;
        sampler2D _MainTex;
        sampler2D texture1; float4 texture1_ST;
        sampler2D texture2; float4 texture2_ST;
        sampler2D texture3; float4 texture3_ST;
        sampler2D texture4; float4 texture4_ST;

        float ilerp(float a, float b, float v)
        {
            return (v-a)/(b-a);
        }

        void surf (Input IN, inout SurfaceOutputStandard o)
        {
            float2 uv = float2(
                ilerp(startX, endX, IN.worldPos.x),
                ilerp(startZ, endZ, IN.worldPos.z)
            );
            float4 w = tex2D (_MainTex, uv);
            w /= w.x + w.y + w.z + w.w;

            o.Albedo = w;
        } 
        ENDCG
    }
    FallBack "Diffuse"
}
```



```
Shader "Custom/TerrainShader"
{
    Properties
    {
        texture1("Texture 1",2D) = "white"{}
        texture2("Texture 2",2D) = "white"{}
        texture3("Texture 3",2D) = "white"{}
        texture4("Texture 4",2D) = "white"{}
    }
    ...
    void surf (Input IN, inout SurfaceOutputStandard o)
    {
        float2 uv = float2(
            ilerp(startX, endX, IN.worldPos.x),
            ilerp(startZ, endZ, IN.worldPos.z)
        );
        float4 w = tex2D (_MainTex, uv);
        w /= w.x + w.y + w.z + w.w;

        float4 tex1 = tex2D(texture1, TRANSFORM_TEX(uv, texture1));
        float4 tex2 = tex2D(texture2, TRANSFORM_TEX(uv, texture2));
        float4 tex3 = tex2D(texture3, TRANSFORM_TEX(uv, texture3));
        float4 tex4 = tex2D(texture4, TRANSFORM_TEX(uv, texture4));

        o.Albedo = w.x * tex1
            + w.y * tex2
            + w.z * tex3
            + w.w * tex4;
    } 
}
```

## Perlin Noise 5 - Island with Falloff


```
[System.Serializable]
public class Falloff
{
    [Range(0, 5)]
    public float a;
    [Range(0, 5)]
    public float b;
}
```


```
public static void ApplyFalloff(float[,] heights, Falloff falloff)
{
    var width = heights.GetLength(0);
    var height = heights.GetLength(1);

    for (var x = 0; x < width; ++x)
        for (var y = 0; y < height; ++y)
        {
            float vx = x / (float)width * 2.0f - 1.0f;
            float vy = y / (float)height * 2.0f - 1.0f;

            float v = Mathf.Max(
                Mathf.Abs(vx),
                Mathf.Abs(vy)
            );
            v = Evaluate(v, falloff.a, falloff.b);

            heights[x, y] = Mathf.Clamp01(heights[x, y] - v);
        }
}

static float Evaluate(float value, float a, float b)
{
    var va = Mathf.Pow(value, a);
    var vb = Mathf.Pow(b - b * value, a);
    return Mathf.Pow(value, a) / (va + vb);
}
```


```
public class TerrainComponent : MonoBehaviour
{
    ...
    public Falloff Falloff;
    ...
    void Start()
    {
        UpdateMesh();
    }

    public void UpdateMesh()
    {
        ...

        float[,] noise = null;
        if (Type == NoiseType.Random)
            noise = Noise.Generate(Width, Height);
        else if (Type == NoiseType.Perlin)
            noise = Noise.Generate(Width, Height, PerlinNoiseOptions);

        if (Falloff != null)
            Noise.ApplyFalloff(noise, Falloff);

        ...
    }
}
```