

// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

char ** fcn.00493d80(char **param_1)
{
    param_1[4] = (char *)((uint32_t)param_1[4] | 0x200000);
    param_1[10] = (char *)0x89b2b4;
    *param_1 = "d3dmesh";
    *(undefined4 *)0x8c3528 = 0x1b;
    *(undefined4 *)0x8c352c = 0x5c11c0;
    fcn.00434ec0(0x8c3528);
    *(undefined4 *)0x8c351c = 0x1a;
    *(undefined4 *)0x8c3520 = 0x5c11d0;
    fcn.00434ec0(0x8c351c);
    *(undefined4 *)0x8c3510 = 0;
    *(undefined4 *)0x8c3514 = 0x5c1cf0;
    fcn.00434ec0(0x8c3510);
    *(undefined4 *)0x8c3504 = 0x14;
    *(undefined4 *)0x8c3508 = 0x5c4b10;
    fcn.00434ec0(0x8c3504);
    *(code **)0x8c3500 = method.DCArray_class_String_.virtual_4;
    *(undefined4 *)0x8c34e8 = 0x7e3ce0;
    *(undefined4 *)0x8c34ec = 0x44;
    *(char ***)0x8c34f4 = param_1;
    param_1[7] = (char *)0x8c34e8;
    *(char **)0x8c330c = "mToolAnimatedVertexGroupEntries";
    *(undefined4 *)0x8c3310 = 0x17c;
    *(char ***)0x8c3318 = param_1;
    *(undefined4 *)0x8c3324 = 0x493c60;
    *(char **)0x8c3328 = "mToolAnimatedVertexEntries";
    *(undefined4 *)0x8c332c = 0x16c;
    *(char ***)0x8c3334 = param_1;
    *(undefined4 *)0x8c3338 = 0x8c330c;
    *(undefined4 *)0x8c3340 = 0x48e790;
    *(char **)0x8c3344 = "mFlags";
    *(undefined4 *)0x8c3348 = 100;
    *(char ***)0x8c3350 = param_1;
    *(undefined4 *)0x8c3354 = 0x8c3328;
    *(undefined4 *)0x8c335c = 0x40d890;
    *(char **)0x8c3360 = "mAnimatedVertexCount";
    *(undefined4 *)0x8c3364 = 0x74;
    *(char ***)0x8c336c = param_1;
    *(undefined4 *)0x8c3370 = 0x8c3344;
    *(code **)0x8c3378 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;
    *(char **)0x8c337c = "mTriangleStripState";
    *(undefined4 *)0x8c3380 = 0x70;
    *(char ***)0x8c3388 = param_1;
    *(undefined4 *)0x8c338c = 0x8c3398;
    *(code **)0x8c3394 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;
    *(char **)0x8c3398 = "mbMeshHasSmoothNormalsSupport";
    *(undefined4 *)0x8c339c = 0x16a;
    *(char ***)0x8c33a4 = param_1;
    *(undefined4 *)0x8c33a8 = 0x8c3360;
    *(code **)0x8c33b0 = method.Function1_bool_.virtual_12;
    *(char **)0x8c33b4 = "mbMeshHasVertexAlpha";
    *(undefined4 *)0x8c33b8 = 0x169;
    *(char ***)0x8c33c0 = param_1;
    *(undefined4 *)0x8c33c4 = 0x8c337c;
    *(code **)0x8c33cc = method.Function1_bool_.virtual_12;
    *(char **)0x8c33d0 = "mbVertexAlphaSupport";
    *(undefined4 *)0x8c33d4 = 0x168;
    *(char ***)0x8c33dc = param_1;
    *(undefined4 *)0x8c33e0 = 0x8c33b4;
    *(code **)0x8c33e8 = method.Function1_bool_.virtual_12;
    *(char **)0x8c33ec = "mbLowQualityRender";
    *(undefined4 *)0x8c33f0 = 0x167;
    *(char ***)0x8c33f8 = param_1;
    *(undefined4 *)0x8c33fc = 0x8c33d0;
    *(code **)0x8c3404 = method.Function1_bool_.virtual_12;
    *(char **)0x8c3408 = "mbLightmaps";
    *(undefined4 *)0x8c340c = 0x165;
    *(char ***)0x8c3414 = param_1;
    *(undefined4 *)0x8c3418 = 0x8c33ec;
    *(code **)0x8c3420 = method.Function1_bool_.virtual_12;
    *(char **)0x8c3424 = "mSkinningData";
    *(undefined4 *)0x8c3428 = 0xf4;
    *(char ***)0x8c3430 = param_1;
    *(undefined4 *)0x8c3434 = 0x8c3408;
    *(undefined4 *)0x8c343c = 0x4875f0;
    *(char **)0x8c3440 = "mBonePalettes";
    *(undefined4 *)0x8c3444 = 0xe4;
    *(char ***)0x8c344c = param_1;
    *(undefined4 *)0x8c3450 = 0x8c3424;
    *(undefined4 *)0x8c3458 = 0x489ca0;
    *(char **)0x8c345c = "mVertexAnimations";
    *(undefined4 *)0x8c3460 = 0xd4;
    *(char ***)0x8c3468 = param_1;
    *(undefined4 *)0x8c346c = 0x8c3440;
    *(undefined4 *)0x8c3474 = 0x48e750;
    *(char **)0x8c3478 = "mTriangleSets";
    *(undefined4 *)0x8c347c = 0xc4;
    *(char ***)0x8c3484 = param_1;
    *(undefined4 *)0x8c3488 = 0x8c345c;
    *(undefined4 *)0x8c3490 = 0x4916a0;
    *(char **)0x8c3494 = "mBoundingBox";
    *(undefined4 *)0x8c3498 = 0x78;
    *(char ***)0x8c34a0 = param_1;
    *(undefined4 *)0x8c34a4 = 0x8c3478;
    *(undefined4 *)0x8c34ac = 0x480dc0;
    *(char **)0x8c34b0 = "mbDeformable";
    *(undefined4 *)0x8c34b4 = 0x164;
    *(char ***)0x8c34bc = param_1;
    *(undefined4 *)0x8c34c0 = 0x8c3494;
    *(code **)0x8c34c8 = method.Function1_bool_.virtual_12;
    *(char **)0x8c34cc = "mVersion";
    *(undefined4 *)0x8c34d0 = 0x60;
    *(char ***)0x8c34d8 = param_1;
    *(undefined4 *)0x8c34dc = 0x8c34b0;
    *(code **)0x8c34e4 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;
    *(undefined4 *)0x8c34f8 = 0x8c34cc;
    return param_1;
}


// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// TirangleSet
int32_t fcn.0048f2a0(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x899a34;
    *(undefined4 *)0x8c2b8c = 0x14;
    *(undefined4 *)0x8c2b90 = 0x5c36b0;
    fcn.00434ec0(0x8c2b8c);
    *(code **)0x8c2b88 = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4;
    *(char **)0x8c2b70 = "mVertexShaderName";

    *(undefined4 *)0x8c2b74 = 0x18;
    *(int32_t *)0x8c2b7c = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8c2b70;
    *(undefined4 *)0x8c2670 = *(undefined4 *)0x8c265c;
    *(char **)0x8c2458 = "TriangleSet::eUVGENDefault";


    *(char **)0x8c2484 = "mUVGenMode";
    *(undefined4 *)0x8c2488 = 0x14c;
    *(undefined4 *)0x8c248c = 0x40;
    *(int32_t *)0x8c2490 = param_1;
    *(undefined4 *)0x8c2494 = 0x8c2468;
    *(undefined4 *)0x8c2498 = 0x8c24f4;
    *(code **)0x8c249c = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8c24bc = "mUVScreenSpaceZoom";
    *(undefined4 *)0x8c24c0 = 0x150;
    *(int32_t *)0x8c24c8 = param_1;
    *(undefined4 *)0x8c24cc = 0x8c24a0;
    *(code **)0x8c24d4 = method.Function1_bool_.virtual_12;


    *(char **)0x8c24f4 = "TriangleSet::eUVGENPositionCameraSpace";
    *(undefined4 *)0x8c24fc = 1;
    *(undefined4 *)0x8c2500 = 0x8c2458;


    *(char **)0x8c2520 = "TriangleSet::kNumToonShadesUseTexture";
    *(undefined4 *)0x8c2528 = 6;
    *(undefined4 *)0x8c252c = 0x8c2530;
    *(char **)0x8c2530 = "TriangleSet::kNumToonShades2";
    *(undefined4 *)0x8c2538 = 5;
    *(undefined4 *)0x8c253c = 0x8c2540;
    *(char **)0x8c2540 = "TriangleSet::kNumToonShades3";
    *(undefined4 *)0x8c2548 = 4;
    *(undefined4 *)0x8c254c = 0x8c2550;
    *(char **)0x8c2550 = "TriangleSet::kNumToonShades4";
    *(undefined4 *)0x8c2558 = 3;
    *(undefined4 *)0x8c255c = 0x8c2560;
    *(char **)0x8c2560 = "TriangleSet::kNumToonShades5";
    *(undefined4 *)0x8c2568 = 2;
    *(undefined4 *)0x8c256c = 0x8c2570;
    *(char **)0x8c2570 = "TriangleSet::kNumToonShades6";
    *(undefined4 *)0x8c2578 = 1;
    *(undefined4 *)0x8c257c = 0x8c2664;


    *(char **)0x8c259c = "RenderDevice::kBlendAlphaSubtract";
    *(undefined4 *)0x8c25a4 = 9;
    *(undefined4 *)0x8c25a8 = 0x8c25ac;
    *(char **)0x8c25ac = "RenderDevice::kBlendAlphaAdd";
    *(undefined4 *)0x8c25b4 = 8;
    *(undefined4 *)0x8c25b8 = 0x8c25bc;
    *(char **)0x8c25bc = "RenderDevice::kBlendInvMultiply";
    *(undefined4 *)0x8c25c4 = 7;
    *(undefined4 *)0x8c25c8 = 0x8c25cc;
    *(char **)0x8c25cc = "RenderDevice::kBlendMultiply";
    *(undefined4 *)0x8c25d4 = 6;
    *(undefined4 *)0x8c25d8 = 0x8c25dc;
    *(char **)0x8c25dc = "RenderDevice::kBlendAdd";
    *(undefined4 *)0x8c25e4 = 5;
    *(undefined4 *)0x8c25e8 = 0x8c25ec;
    *(char **)0x8c25ec = "RenderDevice::kBlendInvAlphaTest";
    *(undefined4 *)0x8c25f4 = 4;
    *(undefined4 *)0x8c25f8 = 0x8c25fc;
    *(char **)0x8c25fc = "RenderDevice::kBlendAlphaTest";
    *(undefined4 *)0x8c2604 = 3;
    *(undefined4 *)0x8c2608 = 0x8c260c;
    *(char **)0x8c260c = "RenderDevice::kBlendAlphaAlphaTest";
    *(undefined4 *)0x8c2614 = 2;
    *(undefined4 *)0x8c2618 = 0x8c261c;
    *(char **)0x8c261c = "RenderDevice::kBlendAlpha";
    *(undefined4 *)0x8c2624 = 1;
    *(undefined4 *)0x8c2628 = 0x8c2674;
    

    *(char **)0x8c2648 = "mToonNumShades";
    *(undefined4 *)0x8c264c = 200;
    *(undefined4 *)0x8c2650 = 0x40;
    *(int32_t *)0x8c2654 = param_1;
    *(undefined4 *)0x8c2658 = 0x8c2504;
    *(undefined4 *)0x8c265c = 0x8c2520;
    *(code **)0x8c2660 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8c2664 = "TriangleSet::kNumToonShades7";
    *(undefined4 *)0x8c266c = 0;
    *(char **)0x8c2674 = "RenderDevice::kBlendNormal";
    *(undefined4 *)0x8c267c = 0;
    *(undefined4 *)0x8c2680 = *(undefined4 *)0x8c2698;

    *(char **)0x8c2684 = "mAlphaMode";
    *(undefined4 *)0x8c2688 = 0x140;
    *(undefined4 *)0x8c268c = 0x40;
    *(int32_t *)0x8c2690 = param_1;
    *(undefined4 *)0x8c2694 = 0x8c2580;
    *(undefined4 *)0x8c2698 = 0x8c259c;
    *(code **)0x8c269c = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8c2b54 = "mPixelShaderName";
    *(undefined4 *)0x8c2b58 = 0x20;
    *(int32_t *)0x8c2b60 = param_1;
    *(undefined4 *)0x8c2b64 = 0x8c2b38;
    *(code **)0x8c2b6c = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4;

    *(char **)0x8c2b38 = "mpVertexShader";
    *(undefined4 *)0x8c2b3c = 0x28;
    *(int32_t *)0x8c2b44 = param_1;
    *(undefined4 *)0x8c2b48 = 0x8c2b1c;
    *(code **)0x8c2b50 = method.DCArray_class_Ptr_class_AnimationValueInterfaceBase__.virtual_4;

    *(char **)0x8c2b1c = "mBonePaletteIndex";
    *(undefined4 *)0x8c2b20 = 0x2c;
    *(int32_t *)0x8c2b28 = param_1;
    *(undefined4 *)0x8c2b2c = 0x8c2b00;
    *(code **)0x8c2b34 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;
    
    *(char **)0x8c2b00 = "mGeometryFormat";
    *(undefined4 *)0x8c2b04 = 0x30;
    *(int32_t *)0x8c2b0c = param_1;
    *(undefined4 *)0x8c2b10 = 0x8c2ae4;
    *(code **)0x8c2b18 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8c2ae4 = "mMinVertIndex";
    *(undefined4 *)0x8c2ae8 = 0x34;
    *(int32_t *)0x8c2af0 = param_1;
    *(undefined4 *)0x8c2af4 = 0x8c2ac8;
    *(code **)0x8c2afc = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8c2ac8 = "mMaxVertIndex";
    *(undefined4 *)0x8c2acc = 0x38;
    *(int32_t *)0x8c2ad4 = param_1;
    *(undefined4 *)0x8c2ad8 = 0x8c2aac;
    *(code **)0x8c2ae0 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;




    *(char **)0x8c2aac = "mStartIndex";
    *(undefined4 *)0x8c2ab0 = 0x3c;
    *(int32_t *)0x8c2ab8 = param_1;
    *(undefined4 *)0x8c2abc = 0x8c2a90;
    *(code **)0x8c2ac4 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8c2a90 = "mNumPrimitives";
    *(undefined4 *)0x8c2a94 = 0x40;
    *(int32_t *)0x8c2a9c = param_1;
    *(undefined4 *)0x8c2aa0 = 0x8c2a74;
    *(code **)0x8c2aa8 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8c2a74 = "mLightingGroup";
    *(undefined4 *)0x8c2a78 = 0x48;
    *(int32_t *)0x8c2a80 = param_1;
    *(undefined4 *)0x8c2a84 = 0x8c2a58;
    *(code **)0x8c2a8c = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4;


    *(char **)0x8c2a58 = "mBoundingBox";
    *(undefined4 *)0x8c2a5c = 0x50;
    *(int32_t *)0x8c2a64 = param_1;
    *(undefined4 *)0x8c2a68 = 0x8c2a3c;
    *(undefined4 *)0x8c2a70 = 0x480dc0;

    *(char **)0x8c2a3c = "mBoundingSphere";
    *(undefined4 *)0x8c2a40 = 0x68;
    *(int32_t *)0x8c2a48 = param_1;
    *(undefined4 *)0x8c2a4c = 0x8c2a20;
    *(undefined4 *)0x8c2a54 = 0x480e00;


    *(char **)0x8c24a0 = "mbHasOctree";
    *(undefined4 *)0x8c24a4 = 0x7c;
    *(int32_t *)0x8c24ac = param_1;
    *(code **)0x8c24b8 = method.Function1_bool_.virtual_12;


    *(char **)0x8c2a20 = "mhDiffuseMap";
    *(undefined4 *)0x8c2a24 = 0x80;
    *(int32_t *)0x8c2a2c = param_1;
    *(undefined4 *)0x8c2a30 = 0x8c2a04;
    *(code **)0x8c2a38 = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;



    *(char **)0x8c29e8 = "mhDetailMap";
    *(undefined4 *)0x8c29ec = 0x84;
    *(int32_t *)0x8c29f4 = param_1;
    *(undefined4 *)0x8c29f8 = 0x8c29cc;
    *(code **)0x8c2a00 = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;

    *(char **)0x8c29b0 = "mhBumpMap";
    *(undefined4 *)0x8c29b4 = 0x8c;
    *(int32_t *)0x8c29bc = param_1;
    *(undefined4 *)0x8c29c0 = 0x8c2994;
    *(code **)0x8c29c8 = method.Animat


    *(char **)0x8c287c = "mhEnvMap";
    *(undefined4 *)0x8c2880 = 0x90;
    *(int32_t *)0x8c2888 = param_1;
    *(undefined4 *)0x8c288c = 0x8c2860;
    *(code **)0x8c2894 = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;

    *(char **)0x8c2a04 = "mhSpecularColorMap";
    *(undefined4 *)0x8c2a08 = 0x94;
    *(int32_t *)0x8c2a10 = param_1;
    *(undefined4 *)0x8c2a14 = 0x8c29e8;
    *(code **)0x8c2a1c = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;


    *(char **)0x8c2994 = "mhAmbientMap";
    *(undefined4 *)0x8c2998 = 0x98;
    *(int32_t *)0x8c29a0 = param_1;
    *(undefined4 *)0x8c29a4 = 0x8c2978;
    *(code **)0x8c29ac = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;


    *(char **)0x8c2504 = "mbToonRendering";
    *(undefined4 *)0x8c2508 = 0x9c;
    *(int32_t *)0x8c2510 = param_1;
    *(undefined4 *)0x8c2514 = 0x8c24d8;
    *(code **)0x8c251c = method.Function1_bool_.virtual_12;


    *(char **)0x8c2978 = "mhToonLightQuantized";
    *(undefined4 *)0x8c297c = 0xa0;
    *(int32_t *)0x8c2984 = param_1;
    *(undefined4 *)0x8c2988 = 0x8c295c;
    *(code **)0x8c2990 = method.AnimationMixer_class_Handle_class_T3Texture___0_.virtual_12;


    *(char **)0x8c27f0 = "mToonMaterialColor";
    *(undefined4 *)0x8c27f4 = 0xa4;
    *(int32_t *)0x8c27fc = param_1;
    *(undefined4 *)0x8c2800 = 0x8c27d4;
    *(code **)0x8c2808 = method.AnimationMixer_class_Color__0_.virtual_12;


*(char **)0x8c27d4 = "mToonOutlineColor";
    *(undefined4 *)0x8c27d8 = 0xb4;
    *(int32_t *)0x8c27e0 = param_1;
    *(undefined4 *)0x8c27e4 = 0x8c27b8;
    *(code **)0x8c27ec = method.AnimationMixer_class_Color__0_.virtual_12;

    *(char **)0x8c27b8 = "mToonOutlineSize";
    *(undefined4 *)0x8c27bc = 0xc4;
    *(int32_t *)0x8c27c4 = param_1;
    *(undefined4 *)0x8c27c8 = 0x8c279c;
    *(code **)0x8c27d0 = method.Function1_float_.virtual_12;

    *(char **)0x8c279c = "mToonOffset";
    *(undefined4 *)0x8c27a0 = 0xcc;
    *(int32_t *)0x8c27a8 = param_1;
    *(undefined4 *)0x8c27ac = 0x8c2780;
    *(code **)0x8c27b4 = method.Function1_class_Vector2_.virtual_12;



    *(char **)0x8c2780 = "mToonEnvLighting";
    *(undefined4 *)0x8c2784 = 0xd4;
    *(int32_t *)0x8c278c = param_1;
    *(undefined4 *)0x8c2790 = 0x8c2764;
    *(code **)0x8c2798 = method.Function1_bool_.virtual_12;
    
    *(char **)0x8c2764 = "mToonNoNormalDeform";
    *(undefined4 *)0x8c2768 = 0xd5;
    *(int32_t *)0x8c2770 = param_1;
    *(undefined4 *)0x8c2774 = 0x8c2748;
    *(code **)0x8c277c = method.Function1_bool_.virtual_12;


    *(char **)0x8c26f4 = "mToonMaxZConstOutlineSize";
    *(undefined4 *)0x8c26f8 = 0xd8;
    *(int32_t *)0x8c2700 = param_1;
    *(undefined4 *)0x8c2704 = 0x8c26d8;
    *(code **)0x8c270c = method.Function1_float_.virtual_12;


    *(char **)0x8c26d8 = "mToonMinZConstOutlineSize";
    *(undefined4 *)0x8c26dc = 0xdc;
    *(int32_t *)0x8c26e4 = param_1;
    *(undefined4 *)0x8c26e8 = 0x8c26bc;
    *(code **)0x8c26f0 = method.Function1_float_.virtual_12;



    *(char **)0x8c2748 = "mNeedSWSkinning";
    *(undefined4 *)0x8c274c = 0xe0;
    *(int32_t *)0x8c2754 = param_1;
    *(undefined4 *)0x8c2758 = 0x8c272c;
    *(code **)0x8c2760 = method.Function1_bool_.virtual_12;


    *(char **)0x8c24d8 = "mbVertexAnimation";
    *(undefined4 *)0x8c24dc = 0xe3;
    *(int32_t *)0x8c24e4 = param_1;
    *(undefined4 *)0x8c24e8 = 0x8c2484;
    *(code **)0x8c24f0 = method.Function1_bool_.virtual_12;

    *(char **)0x8c272c = "mNeedComputeOutline";
    *(undefined4 *)0x8c2730 = 0xe1;
    *(int32_t *)0x8c2738 = param_1;
    *(undefined4 *)0x8c273c = 0x8c2710;
    *(code **)0x8c2744 = method.Function1_bool_.virtual_12;


    *(char **)0x8c2710 = "mNeedRenderOutline";
    *(undefined4 *)0x8c2714 = 0xe2;
    *(int32_t *)0x8c271c = param_1;
    *(undefined4 *)0x8c2720 = 0x8c26f4;
    *(code **)0x8c2728 = method.Function1_bool_.virtual_12;

        *(char **)0x8c295c = "mpPixelShader";
    *(undefined4 *)0x8c2960 = 0xe4;
    *(int32_t *)0x8c2968 = param_1;
    *(undefined4 *)0x8c296c = 0x8c2940;
    *(code **)0x8c2974 = method.DCArray_class_Ptr_class_AnimationValueInterfaceBase__.virtual_4; 

    *(undefined4 *)0x8c2944 = 0xe8;
    *(int32_t *)0x8c294c = param_1;
    *(undefined4 *)0x8c2950 = 0x8c2924;
    *(code **)0x8c2958 = method.Function1_bool_.virtual_12;

      *(char **)0x8c2924 = "mTriStrips";
    *(undefined4 *)0x8c2928 = 0xec;
    *(int32_t *)0x8c2930 = param_1;
    *(undefined4 *)0x8c2934 = 0x8c2908;
    *(undefined4 *)0x8c293c = 0x482610;


    *(char **)0x8c2908 = "mNumTotalIndices";
    *(undefined4 *)0x8c290c = 0xfc;
    *(int32_t *)0x8c2914 = param_1;
    *(undefined4 *)0x8c2918 = 0x8c28ec;
    *(code **)0x8c2920 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;





    *(char **)0x8c28ec = "mbDoubleSided";
    *(undefined4 *)0x8c28f0 = 0x100;
    *(int32_t *)0x8c28f8 = param_1;
    *(undefined4 *)0x8c28fc = 0x8c28d0;
    *(code **)0x8c2904 = method.Function1_bool_.virtual_12;



    *(char **)0x8c28d0 = "mbBumpEffectsSpecular";
    *(undefined4 *)0x8c28d4 = 0x101;
    *(int32_t *)0x8c28dc = param_1;
    *(undefined4 *)0x8c28e0 = 0x8c28b4;
    *(code **)0x8c28e8 = method.Function1_bool_.virtual_12;

    *(char **)0x8c28b4 = "mfBumpHeight";
    *(undefined4 *)0x8c28b8 = 0x104;
    *(int32_t *)0x8c28c0 = param_1;
    *(undefined4 *)0x8c28c4 = 0x8c2898;
    *(code **)0x8c28cc = method.Function1_float_.virtual_12;


    *(char **)0x8c2898 = "mbBumpAsNormalMap";
    *(undefined4 *)0x8c289c = 0x108;
    *(int32_t *)0x8c28a4 = param_1;
    *(undefined4 *)0x8c28a8 = 0x8c287c;
    *(code **)0x8c28b0 = method.Function1_bool_.virtual_12;


    *(char **)0x8c2860 = "mfEccentricity";
    *(undefined4 *)0x8c2864 = 0x10c;
    *(int32_t *)0x8c286c = param_1;
    *(undefined4 *)0x8c2870 = 0x8c2844;
    *(code **)0x8c2878 = method.Function1_float_.virtual_12;


    *(char **)0x8c26a0 = "mReceiveShadowIntensity";
    *(undefined4 *)0x8c26a4 = 0x110;
    *(int32_t *)0x8c26ac = param_1;
    *(undefined4 *)0x8c26b0 = 0x8c262c;
    *(code **)0x8c26b8 = method.Function1_float_.virtual_12;


    *(char **)0x8c262c = "mReceiveShadows";
    *(undefined4 *)0x8c2630 = 0x114;
    *(int32_t *)0x8c2638 = param_1;
    *(undefined4 *)0x8c263c = 0x8c2684;
    *(code **)0x8c2644 = method.Function1_bool_.virtual_12;


    *(char **)0x8c26bc = "mGlowIntensity";
    *(undefined4 *)0x8c26c0 = 0x118;
    *(int32_t *)0x8c26c8 = param_1;
    *(undefined4 *)0x8c26cc = 0x8c26a0;
    *(code **)0x8c26d4 = method.Function1_float_.virtual_12;

    *(char **)0x8c2844 = "mSpecularColor";
    *(undefined4 *)0x8c2848 = 0x11c;
    *(int32_t *)0x8c2850 = param_1;
    *(undefined4 *)0x8c2854 = 0x8c2828;
    *(code **)0x8c285c = method.AnimationMixer_class_Color__0_.virtual_12;


    *(char **)0x8c280c = "mbSelfIlluminated";
    *(undefined4 *)0x8c2810 = 0x13c;
    *(int32_t *)0x8c2818 = param_1;
    *(undefined4 *)0x8c281c = 0x8c27f0;
    *(code **)0x8c2824 = method.Function1_bool_.virtual_12;

    *(char **)0x8c2580 = "mfReflectivity";
    *(undefined4 *)0x8c2584 = 0x144;
    *(int32_t *)0x8c258c = param_1;
    *(undefined4 *)0x8c2590 = 0x8c2648;
    *(code **)0x8c2598 = method.Function1_float_.virtual_12;


    *(undefined4 *)0x8c2460 = 0;
    *(undefined4 *)0x8c2464 = *(undefined4 *)0x8c2498;
    *(char **)0x8c2468 = "mUVScreenSpaceScaling";
    *(undefined4 *)0x8c246c = 0x148;
    *(int32_t *)0x8c2474 = param_1;
    *(undefined4 *)0x8c2478 = 0x8c24bc;
    *(code **)0x8c2480 = method.Function1_float_.virtual_12;



    *(char **)0x8c2828 = "mAmbientColor";
    *(undefined4 *)0x8c282c = 300;
    *(int32_t *)0x8c2834 = param_1;
    *(undefined4 *)0x8c2838 = 0x8c280c;
    *(code **)0x8c2840 = method.AnimationMixer_class_Color__0_.virtual_12;

    *(undefined4 *)0x8c2b80 = 0x8c2b54;
    return param_1;
}



// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_1ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

void __cdecl fcn.005c2d90(int32_t arg_8h)
{
    bool bVar1;
    bool bVar2;
    char cVar3;
    int32_t iVar4;
    int32_t in_ECX;
    uint32_t uVar5;
    uint32_t uVar6;
    char *pcVar7;
    undefined auStack28 [2];
    char cStack26;
    char cStack25;
    char cStack24;
    char cStack23;
    char cStack22;
    char cStack21;
    int32_t iStack20;
    float fStack16;
    
    if ((*(int32_t *)(arg_8h + 0x90) == 0) ||
       (cStack24 = '\x01', (~(uint8_t)(*(uint32_t *)(*(int32_t *)(arg_8h + 0x90) + 0x38) >> 0xd) & 1) == 0)) {
        cStack24 = '\0';
    }
    cStack25 = 0.01 < *(float *)(arg_8h + 0x144);
    cStack21 = *(char *)(arg_8h + 0x101);
    fStack16 = *(float *)(arg_8h + 0x124) * 0.11 + *(float *)(arg_8h + 0x11c) * 0.3 + *(float *)(arg_8h + 0x120) * 0.59;
    if ((*(int32_t *)(arg_8h + 0x8c) == 0) ||
       (cStack23 = '\x01', (~(uint8_t)(*(uint32_t *)(*(int32_t *)(arg_8h + 0x8c) + 0x38) >> 0xd) & 1) == 0)) {
        cStack23 = '\0';
    }
    if ((*(int32_t *)(arg_8h + 0x88) == 0) ||
       (cStack26 = '\x01', (~(uint8_t)(*(uint32_t *)(*(int32_t *)(arg_8h + 0x88) + 0x38) >> 0xd) & 1) == 0)) {
        cStack26 = '\0';
    }
    cStack22 = *(char *)(in_ECX + 0x167);
    if (((*(int32_t *)(arg_8h + 0xa0) == 0) ||
        ((~(uint8_t)(*(uint32_t *)(*(int32_t *)(arg_8h + 0xa0) + 0x38) >> 0xd) & 1) == 0)) &&
       (*(char *)(arg_8h + 0x9c) == '\0')) {
        bVar1 = false;
    } else {
        bVar1 = true;
    }
    iStack20 = in_ECX;
    cVar3 = fcn.0045f740();
    if ((cVar3 == '\0') || (*(char *)0x8e5dc4 == '\0')) {
        bVar2 = false;
    } else {
        bVar2 = true;
    }
    if (bVar1) {
        bVar2 = false;
    }
    *(undefined *)(arg_8h + 8) = 0;
    if (bVar2) {
        uVar5 = 0;
        if (cStack26 != '\0') {
            uVar5 = 8;
        }
        if (*(char *)(iStack20 + 0x164) != '\0') {
            uVar5 = uVar5 | 1;
        }
        if (cStack23 != '\0') {
            uVar5 = uVar5 | 2;
        }
        iVar4 = (**(code **)(*(int32_t *)arg_8h + 0x30))();
        if (iVar4 != 0) {
            uVar5 = uVar5 | 0x20;
        }
        if (cStack24 != '\0') {
            uVar6 = uVar5;
            if (cStack25 == '\0') goto code_r0x005c2f30;
            uVar5 = uVar5 | 0x10;
        }
        uVar6 = uVar5;
        if ((cStack25 != '\0') && ((iVar4 = (**(code **)(*(int32_t *)arg_8h + 0x2c))(), iVar4 != 0 || (0.0 < fStack16)))
           ) {
            uVar6 = uVar5 | 0x40;
            iVar4 = (**(code **)(*(int32_t *)arg_8h + 0x2c))();
            if (iVar4 != 0) {
                uVar6 = uVar5 | 0x44;
            }
        }
code_r0x005c2f30:
        cVar3 = fcn.005c1e80(0x8ef4b0, uVar6);
        if (cVar3 == '\0') {
            return;
        }
        *(undefined *)(arg_8h + 8) = 1;
        iVar4 = *(int32_t *)(arg_8h + 4);
        if (((iVar4 != 0) && (*(undefined4 *)(iVar4 + 0x3c) = *(undefined4 *)0x8ba770, *(int32_t *)(iVar4 + 0x30) == 0))
           && (*(int32_t *)(iVar4 + 0x28) != 0)) {
            if ((*(uint32_t *)(iVar4 + 0x38) & 0x9000) != 0) {
                fcn.0041a5e0(0x8951a0);
            }
            fcn.006ca6c0();
            return;
        }
        fcn.006ca6c0();
        return;
    }
    if (!bVar1) {
        if (*(char *)(in_ECX + 0x164) != '\0') {
            if (cStack22 == '\0') {
                if ((cStack24 != '\0') && (cStack25 != '\0')) {
                    fcn.004966c0("EnvDeform");
                    fcn.005c1f40(&iStack20);
                    fcn.004966c0(0x7f5224);
                    fcn.005c1f70(auStack28);
                    return;
                }
                if (cStack23 != '\0') {
                    fcn.004966c0("BumpDeform");
                    fcn.005c1f40(&iStack20);
                    pcVar7 = "Bump";
                    goto code_r0x005c330e;
                }
            }
            fcn.004966c0("SimpleDeform");
            fcn.005c1f40(&iStack20);
            pcVar7 = "Simple";
            goto code_r0x005c3031;
        }
        if (cStack22 == '\0') {
            if ((cStack24 != '\0') && (cStack25 != '\0')) {
                if (cStack26 != '\0') {
                    fcn.004966c0("EnvLightmap");
                    fcn.005c1f40(&iStack20);
                    fcn.004966c0("EnvLightmap");
                    fcn.005c1f70(auStack28);
                    return;
                }
                fcn.004966c0(0x7f5224);
                fcn.005c1f40(&iStack20);
                pcVar7 = "Env";
                goto code_r0x005c330e;
            }
            if (cStack23 != '\0') {
                if (cStack21 == '\0') {
                    if (cStack26 != '\0') {
                        fcn.004966c0("BumpDiffuseLightmap");
                        fcn.005c1f40(&iStack20);
                        pcVar7 = "BumpDiffuseLightmap";
                        goto code_r0x005c330e;
                    }
                    fcn.004966c0("BumpDiffuse");
                    fcn.005c1f40(&iStack20);
                    pcVar7 = "BumpDiffuse";
                } else {
                    if (cStack26 == '\0') {
                        fcn.004966c0("Bump");
                        fcn.005c1f40(&iStack20);
                        fcn.004966c0("Bump");
                        fcn.005c1f70(auStack28);
                        return;
                    }
                    fcn.004966c0("BumpLightmap");
                    fcn.005c1f40(&iStack20);
                    pcVar7 = "BumpLightmap";
                }
                goto code_r0x005c3031;
            }
        }
        if (cStack26 != '\0') {
            fcn.004966c0("Lightmap");
            fcn.005c1f40(&iStack20);
            fcn.004966c0("Lightmap");
            fcn.005c1f70(auStack28);
            return;
        }
        fcn.004966c0("Simple");
        fcn.005c1f40(&iStack20);
        pcVar7 = "Simple";
code_r0x005c330e:
        fcn.004966c0(pcVar7);
        fcn.005c1f70(auStack28);
        return;
    }
    if (*(char *)(in_ECX + 0x164) == '\0') {
        if (*(char *)(arg_8h + 0xd4) != '\0') {
            fcn.004966c0("CartoonEnv");
            fcn.005c1f40(&iStack20);
            fcn.004966c0("Cartoon");
            fcn.005c1f70(auStack28);
            return;
        }
        if (*(float *)(arg_8h + 0xc4) == 0.0) {
            fcn.004966c0("CartoonNoOutline");
            fcn.005c1f40(&iStack20);
            pcVar7 = "CartoonNoOutline";
            goto code_r0x005c330e;
        }
        pcVar7 = "Cartoon";
code_r0x005c301b:
        fcn.004966c0(pcVar7);
    } else {
        if (*(float *)(arg_8h + 0xc4) == 0.0) {
            fcn.004966c0("CartoonDeformNoOutline");
            fcn.005c1f40(&iStack20);
            fcn.004966c0("CartoonNoOutline");
            fcn.005c1f70(auStack28);
            return;
        }
        if (*(char *)(arg_8h + 0xd4) == '\0') {
            pcVar7 = "CartoonDeform";
            goto code_r0x005c301b;
        }
        fcn.004966c0("CartoonDeformEnv");
    }
    fcn.005c1f40(&iStack20);
    pcVar7 = "Cartoon";
code_r0x005c3031:
    fcn.004966c0(pcVar7);
    fcn.005c1f70(auStack28);
    return;
}
