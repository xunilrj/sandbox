
// WARNING: [rz-ghidra] Var arg_1ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_1ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

code ** __fastcall method.DCArray_struct_SkeletonInstance::SklNodeData_.virtual_0(code **param_1)
{
    int32_t iVar1;
    undefined4 *in_FS_OFFSET;
    uint8_t unaff_retaddr;
    undefined4 uStack12;
    undefined4 uStack8;
    undefined4 uStack4;
    
    uStack8 = 0x791f28;
    uStack12 = *in_FS_OFFSET;
    *in_FS_OFFSET = &uStack12;
    *param_1 = vtable.DCArray_struct_SkeletonInstance::SklNodeData_.0;
    iVar1 = 0;
    uStack4 = 0;
    if (0 < (int32_t)param_1[1]) {
        do {
            fcn.005bea20();
            iVar1 = iVar1 + 1;
        } while (iVar1 < (int32_t)param_1[1]);
    }
    param_1[1] = (code *)0x0;
    if (param_1[3] != (code *)0x0) {
        fcn.0042dca0(param_1[3]);
    }
    uStack8 = 0xffffffff;
    fcn.00496a40();
    if ((unaff_retaddr & 1) != 0) {
        fcn.0042dca0(param_1);
    }
    *in_FS_OFFSET = param_1;
    return param_1;
}


// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

// 0x0048ef30
undefined4 method.DCArray_class_Skeleton::Entry_.virtual_4(void)
{
    if ((*(uint32_t *)0x8c22ac & 0x20000000) == 0) {
        fcn.00438160(0x89acc4);
        *(undefined4 *)0x8c22b0 = 0xa8;
        fcn.0048dd40(0x8c229c);
    }
    return 0x8c229c;
}


// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_78h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.

void __fastcall fcn.00438160(int32_t param_1)
{
    undefined4 uVar1;
    undefined4 *puVar2;
    undefined4 *in_FS_OFFSET;
    undefined4 uStack48;
    undefined auStack44 [12];
    undefined4 uStack32;
    uint32_t uStack28;
    undefined4 uStack16;
    undefined4 uStack12;
    undefined4 uStack8;
    undefined4 uStack4;
    
    uStack4 = 0xffffffff;
    uStack8 = 0x78f57a;
    uStack12 = *in_FS_OFFSET;
    *in_FS_OFFSET = &uStack12;
    uVar1 = fcn.00761923(0x8fa964);
    *(undefined4 *)(param_1 + 4) = uVar1;
    uVar1 = fcn.00436730(auStack44);
    uStack12 = 0;
    puVar2 = (undefined4 *)fcn.004967a0(uVar1);
    *(undefined4 *)(param_1 + 8) = *puVar2;
    *(undefined4 *)(param_1 + 0xc) = puVar2[1];
    uStack16 = 0xffffffff;
    if (0xf < uStack28) {
        fcn.004a99b0(uStack48, uStack28 + 1);
    }
    *(uint32_t *)(param_1 + 0x10) = *(uint32_t *)(param_1 + 0x10) | 0x20000000;
    *(int32_t *)(param_1 + 0x24) = *(undefined4 *)0x8bad18;
    *(int32_t *)0x8bad18 = param_1;
    *in_FS_OFFSET = uStack32;
    return;
}


// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

void fcn.0048dd40(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x89a75c;
    *(code **)0x8c2298 = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4; // 8 bytes
    *(char **)0x8c2280 = "mJointName";
    *(undefined4 *)0x8c2284 = 0;
    *(int32_t *)0x8c228c = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8c2280;

     *(char **)0x8c2248 = "mParentName";
    *(undefined4 *)0x8c224c = 8; 
    *(int32_t *)0x8c2254 = param_1;
    *(undefined4 *)0x8c2258 = 0x8c222c;
    *(code **)0x8c2260 = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4; // 8 bytes

     *(char **)0x8c222c = "mParentIndex";
    *(undefined4 *)0x8c2230 = 0x10;
    *(int32_t *)0x8c2238 = param_1;
    *(undefined4 *)0x8c223c = 0x8c2264;
    *(code **)0x8c2244 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8c2264 = "mBoneLength";
    *(undefined4 *)0x8c2268 = 0x14;
    *(int32_t *)0x8c2270 = param_1;
    *(undefined4 *)0x8c2274 = 0x8c2210;
    *(code **)0x8c227c = method.Function1_float_.virtual_12;
    *(undefined4 *)0x8c2290 = 0x8c2248;

    *(uint32_t *)0x8c226c = *(uint32_t *)0x8c226c | 1;
    *(char **)0x8c214c = "mFlags";
    *(undefined4 *)0x8c2150 = 0xa0;
    *(int32_t *)0x8c2158 = param_1;
    *(undefined4 *)0x8c2164 = 0x40d890;

    *(char **)0x8c2210 = "mLocalPos";
    *(undefined4 *)0x8c2214 = 0x34;
    *(int32_t *)0x8c221c = param_1;
    *(undefined4 *)0x8c2220 = 0x8c21f4;
    *(code **)0x8c2228 = method.AnimationMixer_class_Vector3__0_.virtual_12;
    
    *(char **)0x8c21f4 = "mLocalQuat";
    *(undefined4 *)0x8c21f8 = 0x40;
    *(int32_t *)0x8c2200 = param_1;
    *(undefined4 *)0x8c2204 = 0x8c21d8;
    *(code **)0x8c220c = method.AnimationMixer_class_Quaternion__0_.virtual_12;

    *(char **)0x8c21d8 = "mRestXform";
    *(undefined4 *)0x8c21dc = 0x50;
    *(int32_t *)0x8c21e4 = param_1;
    *(undefined4 *)0x8c21e8 = 0x8c21bc;
    *(code **)0x8c21f0 = method.AnimatedValueInterface_class_Transform_.virtual_12;

    *(char **)0x8c21bc = "mGlobalTranslationScale";
    *(undefined4 *)0x8c21c0 = 0x6c;
    *(int32_t *)0x8c21c8 = param_1;
    *(undefined4 *)0x8c21cc = 0x8c21a0;
    *(code **)0x8c21d4 = method.AnimationMixer_class_Vector3__0_.virtual_12;

    *(char **)0x8c21a0 = "mLocalTranslationScale";
    *(undefined4 *)0x8c21a4 = 0x78;
    *(int32_t *)0x8c21ac = param_1;
    *(undefined4 *)0x8c21b0 = 0x8c2184;
    *(code **)0x8c21b8 = method.AnimationMixer_class_Vector3__0_.virtual_12;

    *(char **)0x8c2184 = "mAnimTranslationScale";
    *(undefined4 *)0x8c2188 = 0x84;
    *(int32_t *)0x8c2190 = param_1;
    *(undefined4 *)0x8c2194 = 0x8c2168;
    *(code **)0x8c219c = method.AnimationMixer_class_Vector3__0_.virtual_12;

    *(char **)0x8c2168 = "mResourceGroupMembership";
    *(undefined4 *)0x8c216c = 0x90;
    *(int32_t *)0x8c2174 = param_1;
    *(undefined4 *)0x8c2178 = 0x8c214c;
    *(undefined4 *)0x8c2180 = 0x48c450;

    return;
}


// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
//  ;-- method.AnimatedValueInterface_class_Transform_.virtual_12:
//  ;-- method.CompressedTransformKeys.virtual_12:
//  ;-- method.Procedural_LookAt_Value.virtual_12:
//  ;-- method.Procedural_Eyes_Value.virtual_12:
//  ;-- method.CompressedKeys_class_Transform_.virtual_12:
//  ;-- method.SingleValue_class_Transform_.virtual_12:
//  ;-- method.KeyframedValue_class_Transform_.virtual_12:
//  ;-- method.KeyframedValue_class_Transform_.1.virtual_20:
//  ;-- method.AnimatedValueInterface_class_Transform_.1.virtual_12:
//  ;-- method.CompressedTransformKeys.1.virtual_12:
//  ;-- method.Procedural_LookAt_Value.1.virtual_12:
//  ;-- method.Procedural_Eyes_Value.1.virtual_12:
//  ;-- method.CompressedKeys_class_Transform_.1.virtual_12:
//  ;-- method.SingleValue_class_Transform_.1.virtual_12:
//  ;-- method.KeyframedValue_class_Transform_.2.virtual_12:
//method.KeyframedValue_class_Transform_.3.virtual_20 ();
undefined4 method.AnimatedValueInterface_class_Transform_.virtual_12(void)
{
    if ((*(uint32_t *)0x8bfb5c & 0x20000000) == 0) {
        fcn.00438160(0x897358);
        *(undefined4 *)0x8bfb60 = 0x1c;
        sub.KeyframedValue_class_Quaternion_.3.virtual_20_4458e0(0x8bfb4c);
    }
    return 0x8bfb4c;
}
