// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

int32_t sub.Animation.1.virtual_4_54f3c0(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x8a47dc;
    *(code **)0x8ec028 = method.Animation.virtual_4;
    *(char **)0x8ec010 = "Baseclass_Animation";
    *(undefined4 *)0x8ec014 = 0;
    *(undefined4 *)0x8ec018 = 0x10;
    *(int32_t *)0x8ec01c = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8ec010;
    *(undefined4 *)0x8ec004 = 0x14;
    *(undefined4 *)0x8ec008 = 0x6c0ac0;
    fcn.00434ec0(0x8ec004);
    *(undefined4 *)0x8ebff8 = 0xb;
    *(undefined4 *)0x8ebffc = 0x496ce0;
    fcn.00434ec0(0x8ebff8);
    *(undefined4 *)0x8ebfec = 2;
    *(undefined4 *)0x8ebff0 = 0x6c0390;
    fcn.00434ec0(0x8ebfec);
    *(undefined4 *)0x8ebfe0 = 3;
    *(undefined4 *)0x8ebfe4 = 0x6c1690;
    fcn.00434ec0(0x8ebfe0);
    *(char **)0x8ebf54 = "mbUsePrivateNode";
    *(undefined4 *)0x8ebf58 = 0xa0;
    *(int32_t *)0x8ebf60 = param_1;
    *(code **)0x8ebf6c = method.Function1_bool_.virtual_12;

    *(char **)0x8ebf70 = "mTargetOffset";
    *(undefined4 *)0x8ebf74 = 0xa4;
    *(int32_t *)0x8ebf7c = param_1;
    *(undefined4 *)0x8ebf80 = 0x8ebf54;
    *(code **)0x8ebf88 = method.AnimationMixer_class_Vector3__0_.virtual_12;

    *(char **)0x8ebf8c = "mTargetNode";
    *(undefined4 *)0x8ebf90 = 0x84;
    *(int32_t *)0x8ebf98 = param_1;
    *(undefined4 *)0x8ebf9c = 0x8ebf70;
    *(code **)0x8ebfa4 = method.DCArray_class_String_.virtual_4;

    *(char **)0x8ebfa8 = "mTargetAgent";
    *(undefined4 *)0x8ebfac = 0x68;
    *(int32_t *)0x8ebfb4 = param_1;
    *(undefined4 *)0x8ebfb8 = 0x8ebf8c;
    *(code **)0x8ebfc0 = method.DCArray_class_String_.virtual_4;

    *(char **)0x8ebfc4 = "mHostNode";
    *(undefined4 *)0x8ebfc8 = 0x4c;
    *(int32_t *)0x8ebfd0 = param_1;
    *(undefined4 *)0x8ebfd4 = 0x8ebfa8;
    *(code **)0x8ebfdc = method.DCArray_class_String_.virtual_4;
    
    *(undefined4 *)0x8ec020 = 0x8ebfc4;
    return param_1;
}

// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

void sub.KeyframedValue_class_Vector3_.3.virtual_20_621ba0(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x8adb78;
    *(code **)0x8f1f84 = method.DCArray_class_Ptr_class_AnimationValueInterfaceBase__.virtual_4;
    *(char **)0x8f1f6c = "mpAgent";
    *(undefined4 *)0x8f1f70 = 0x58;
    *(int32_t *)0x8f1f78 = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8f1f6c;
    *(char **)0x8f1dac = "mbOrbitRelative";
    *(undefined4 *)0x8f1db0 = 0x138;
    *(int32_t *)0x8f1db8 = param_1;
    *(code **)0x8f1dc4 = method.Function1_bool_.virtual_12;
    *(char **)0x8f1dc8 = "mMinRotateAmount";
    *(undefined4 *)0x8f1dcc = 0x110;
    *(int32_t *)0x8f1dd4 = param_1;
    *(undefined4 *)0x8f1dd8 = 0x8f1dac;
    *(code **)0x8f1de0 = method.Function1_float_.virtual_12;
    *(char **)0x8f1de4 = "mMinMoveAmount";
    *(undefined4 *)0x8f1de8 = 0x10c;
    *(int32_t *)0x8f1df0 = param_1;
    *(undefined4 *)0x8f1df4 = 0x8f1dc8;
    *(code **)0x8f1dfc = method.Function1_float_.virtual_12;
    *(char **)0x8f1e00 = "mOrbitMin";
    *(undefined4 *)0x8f1e04 = 300;
    *(int32_t *)0x8f1e0c = param_1;
    *(undefined4 *)0x8f1e10 = 0x8f1de4;
    *(code **)0x8f1e18 = method.Method1_class_NavCam__class_Polar__class_Ptr_class_NavCam__.virtual_12;
    *(char **)0x8f1e1c = "mOrbitMax";
    *(undefined4 *)0x8f1e20 = 0x120;
    *(int32_t *)0x8f1e28 = param_1;
    *(undefined4 *)0x8f1e2c = 0x8f1e00;
    *(code **)0x8f1e34 = method.Method1_class_NavCam__class_Polar__class_Ptr_class_NavCam__.virtual_12;
    *(char **)0x8f1e38 = "mOrbitPos";
    *(undefined4 *)0x8f1e3c = 0x114;
    *(int32_t *)0x8f1e44 = param_1;
    *(undefined4 *)0x8f1e48 = 0x8f1e1c;
    *(code **)0x8f1e50 = method.Method1_class_NavCam__class_Polar__class_Ptr_class_NavCam__.virtual_12;
    *(char **)0x8f1e54 = "mDampenValue";
    *(undefined4 *)0x8f1e58 = 0x108;
    *(int32_t *)0x8f1e60 = param_1;
    *(undefined4 *)0x8f1e64 = 0x8f1e38;
    *(code **)0x8f1e6c = method.Function1_float_.virtual_12;
    *(char **)0x8f1e70 = "mVerticalTriggerPercentage";
    *(undefined4 *)0x8f1e74 = 0x104;
    *(int32_t *)0x8f1e7c = param_1;
    *(undefined4 *)0x8f1e80 = 0x8f1e54;
    *(code **)0x8f1e88 = method.Function1_float_.virtual_12;
    *(char **)0x8f1e8c = "mHorizontalTriggerPercentage";
    *(undefined4 *)0x8f1e90 = 0x100;
    *(int32_t *)0x8f1e98 = param_1;
    *(undefined4 *)0x8f1e9c = 0x8f1e70;
    *(code **)0x8f1ea4 = method.Function1_float_.virtual_12;
    *(char **)0x8f1ea8 = "mAnimationTime";
    *(undefined4 *)0x8f1eac = 0xf0;
    *(int32_t *)0x8f1eb4 = param_1;
    *(undefined4 *)0x8f1eb8 = 0x8f1e8c;
    *(code **)0x8f1ec0 = method.Function1_float_.virtual_12;
    *(char **)0x8f1ec4 = "mhNavCamAnim";
    *(undefined4 *)0x8f1ec8 = 0xe0;
    *(int32_t *)0x8f1ed0 = param_1;
    *(undefined4 *)0x8f1ed4 = 0x8f1ea8;
    *(code **)0x8f1edc = method.AnimationMixer_class_AnimOrChore__0_.virtual_12;
    *(char **)0x8f1ee0 = "mbMoving";
    *(undefined4 *)0x8f1ee4 = 0xdf;
    *(int32_t *)0x8f1eec = param_1;
    *(undefined4 *)0x8f1ef0 = 0x8f1ec4;
    *(code **)0x8f1ef8 = method.Function1_bool_.virtual_12;
    *(char **)0x8f1efc = "mpTarget";
    *(undefined4 *)0x8f1f00 = 0xa8;
    *(int32_t *)0x8f1f08 = param_1;
    *(undefined4 *)0x8f1f0c = 0x8f1f34;
    *(undefined4 *)0x8f1f14 = 0x621a20;
    *(char **)0x8f1f18 = "mMode";
    *(undefined4 *)0x8f1f1c = 0x5c;
    *(int32_t *)0x8f1f24 = param_1;
    *(undefined4 *)0x8f1f28 = 0x8f1f50;
    *(code **)0x8f1f30 = method.Method1_class_NavCam__struct_NavCam::EnumMode__class_Ptr_class_NavCam__.virtual_12;
    *(char **)0x8f1f34 = "mTargetOffset";
    *(undefined4 *)0x8f1f38 = 0xac;
    *(int32_t *)0x8f1f40 = param_1;
    *(undefined4 *)0x8f1f44 = 0x8f1ee0;
    *(code **)0x8f1f4c = method.AnimationMixer_class_Vector3__0_.virtual_12;
    *(char **)0x8f1f50 = "mHomePos";
    *(undefined4 *)0x8f1f54 = 0x60;
    *(int32_t *)0x8f1f5c = param_1;
    *(undefined4 *)0x8f1f60 = 0x8f1efc;
    *(code **)0x8f1f68 = method.AnimationMixer_class_Vector3__0_.virtual_12;
    *(undefined4 *)0x8f1f7c = 0x8f1f18;
    return;
}


// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

int32_t sub.Baseclass_AnimatedValueInterface_T_6ef8c0(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x8b1cbc;
    *(undefined4 *)0x8f790c = 0x14;
    *(undefined4 *)0x8f7910 = 0x6cfff0;
    fcn.00434ec0(0x8f790c);
    *(undefined4 *)0x8f78f8 = 0x10;
    *(int32_t *)0x8f78fc = param_1;
    *(undefined4 *)0x8f7908 = 0x6eabf0;
    *(char **)0x8f78f0 = "Baseclass_AnimatedValueInterface<T>";
    *(undefined4 *)0x8f78f4 = 8;
    *(undefined4 *)(param_1 + 0x1c) = 0x8f78f0;
    *(char **)0x8f7880 = "mSamples";
    *(undefined4 *)0x8f7884 = 0x40;
    *(int32_t *)0x8f788c = param_1;
    *(undefined4 *)0x8f7898 = 0x6ee8d0;
    *(char **)0x8f789c = "mMaxVal";
    *(undefined4 *)0x8f78a0 = 0x30;
    *(int32_t *)0x8f78a8 = param_1;
    *(undefined4 *)0x8f78ac = 0x8f7880;
    *(code **)0x8f78b4 = method.AnimationMixer_class_Quaternion__0_.virtual_12;
    *(char **)0x8f78b8 = "mMinVal";
    *(undefined4 *)0x8f78bc = 0x20;
    *(int32_t *)0x8f78c4 = param_1;
    *(undefined4 *)0x8f78c8 = 0x8f789c;
    *(code **)0x8f78d0 = method.AnimationMixer_class_Quaternion__0_.virtual_12;
    *(char **)0x8f78d4 = "Baseclass_KeyframedValueInterface";
    *(undefined4 *)0x8f78d8 = 0;
    *(undefined4 *)0x8f78dc = 0x10;
    *(int32_t *)0x8f78e0 = param_1;
    *(undefined4 *)0x8f78e4 = 0x8f78b8;
    *(code **)0x8f78ec = method.KeyframedValueInterface.virtual_32;
    *(undefined4 *)0x8f7900 = 0x8f78d4;
    return param_1;
}


// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_18h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_450h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_454h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_458h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_450h_2 is stack pointer based, which is not supported for decompilation.

void __thiscall method.CompressedKeys_class_Transform_.virtual_16(int32_t param_1, int32_t *param_2)
{
    uint32_t *puVar1;
    int32_t iVar2;
    uint32_t uVar3;
    undefined4 uVar4;
    undefined4 *puVar5;
    code *pcVar6;
    uint32_t uVar7;
    int32_t iVar8;
    undefined4 unaff_ESI;
    uint32_t uVar9;
    int32_t iVar10;
    undefined4 unaff_EDI;
    uint32_t *puVar11;
    uint32_t uStack8;
    uint32_t uStack4;
    
    iVar2 = *param_2;
    param_2 = (int32_t *)0x0;
    if (*(int32_t *)(iVar2 + 100) == 0) {
        puVar11 = (uint32_t *)&param_2;
        fcn.0044a3a0(puVar11);
    } else {
        puVar11 = &uStack4;
        uStack4 = 0;
        fcn.0044a280(puVar11, 2);
    }
    *(uint16_t *)(param_1 + 0x26) = *(uint16_t *)(param_1 + 0x26) & 0xfff9;
    uVar9 = uStack4 & 0xffff;
    uVar3 = (int32_t)(uVar9 + 3) >> 2;
    *(int16_t *)(param_1 + 0x24) = (int16_t)uStack4;
    uVar7 = uStack4;
    if (uVar9 != 0) {
        uVar4 = fcn.005f13c0(uVar9 * 0x1c);
        *(uint16_t *)(param_1 + 0x26) = *(uint16_t *)(param_1 + 0x26) & 0xfffe;
        *(undefined4 *)(param_1 + 0x18) = uVar4;
        *(int16_t *)(param_1 + 0x24) = (int16_t)uVar9;
        uVar7 = uStack8;
    }
    uVar4 = fcn.005f13c0((uVar7 & 0xffff) * 4);
    *(undefined4 *)(param_1 + 0x1c) = uVar4;
    uVar4 = fcn.005f13c0(unaff_ESI);
    iVar8 = 0;
    *(undefined4 *)(param_1 + 0x20) = uVar4;
    if ((int16_t)uVar3 != 0) {
        iVar10 = 0;
        do {
            puVar5 = (undefined4 *)(*(int32_t *)(param_1 + 0x18) + iVar10);
            if (puVar5 != (undefined4 *)0x0) {
                *puVar5 = 0;
                puVar5[1] = 0;
                puVar5[2] = 0;
                puVar5[3] = 0x3f800000;
                puVar5[4] = 0;
                puVar5[5] = 0;
                puVar5[6] = 0;
            }
            iVar8 = iVar8 + 1;
            iVar10 = iVar10 + 0x1c;
        } while (iVar8 < (int32_t)(uVar3 & 0xffff));
    }
    if ((int16_t)uVar3 != 0) {
        iVar8 = 0;
        do {
            iVar10 = *(int32_t *)(param_1 + 0x18) + iVar8;
            uVar7 = uVar3;
            if ((*(uint32_t *)0x8bfb5c & 0x20000000) == 0) {
                puVar11 = (uint32_t *)0x897358;
                fcn.00438160();
                *(undefined4 *)0x8bfb60 = 0x1c;
                *(undefined4 *)0x8bfb74 = 0x8970dc;
                *(code **)0x8bfaa4 = method.AnimationMixer_class_Quaternion__0_.virtual_12;
                *(char **)0x8bfa8c = "mRot";
                *(undefined4 *)0x8bfa90 = 0;
                *(undefined4 *)0x8bfa98 = 0x8bfb4c;
                *(undefined4 *)0x8bfb68 = 0x8bfa8c;
                *(code **)0x8bfa88 = method.AnimationMixer_class_Vector3__0_.virtual_12;
                *(char **)0x8bfa70 = "mTrans";
                *(undefined4 *)0x8bfa74 = 0x10;
                *(undefined4 *)0x8bfa7c = 0x8bfb4c;
                *(undefined4 *)0x8bfa9c = 0x8bfa70;
                uVar7 = uVar3;
            }
            pcVar6 = (code *)fcn.00434e10(0x14, puVar11);
            if (pcVar6 == (code *)0x0) {
                fcn.0043d4f0(iVar10, 0x8bfb4c, 0, iVar2);
            } else {
                (*pcVar6)(iVar10, 0x8bfb4c, 0, iVar2);
            }
            puVar1 = (uint32_t *)(*(int32_t *)(param_1 + 0x1c) + uVar7 * 4);
            if (*(int32_t *)(iVar2 + 100) == 0) {
                uVar3 = uVar7;
                fcn.0044a3a0(puVar1);
            } else {
                uVar3 = *puVar1;
                fcn.0044a280(&stack0xfffffff0, 4);
            }
            iVar8 = iVar8 + 0x1c;
        } while ((int32_t)(uVar7 + 1) < (int32_t)(uVar3 & 0xffff));
    }
    if (*(int32_t *)(iVar2 + 100) == 0) {
        fcn.0044a3a0(*(undefined4 *)(param_1 + 0x20), unaff_EDI);
        return;
    }
    fcn.0044a280(*(undefined4 *)(param_1 + 0x20), unaff_EDI);
    return;
}



// WARNING: Could not reconcile some variable overlaps
// WARNING: [rz-ghidra] Var arg_b4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_9ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_18h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_20h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_20h_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_24h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_2ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_28h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_28h_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_1ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.

float * __thiscall fcn.006c1af0(int32_t param_1, float *param_2)
{
    undefined8 *puVar1;
    int32_t iVar2;
    float fVar3;
    float fVar4;
    float fVar5;
    float fVar6;
    float fVar7;
    float fVar8;
    float fVar9;
    float fVar10;
    float fVar11;
    float fVar12;
    float fVar13;
    int32_t iVar14;
    float fVar15;
    char cVar16;
    int32_t iVar17;
    int32_t *piVar18;
    undefined4 uVar19;
    float *pfVar20;
    float *pfVar21;
    float unaff_EBX;
    int32_t unaff_EBP;
    int32_t iVar22;
    int32_t unaff_EDI;
    undefined4 *in_FS_OFFSET;
    bool bVar23;
    unkfloat10 Var24;
    undefined8 *puVar25;
    float fVar26;
    float *pfStack212;
    undefined8 *puStack208;
    float *pfStack204;
    float *pfStack200;
    undefined4 uStack196;
    float fStack192;
    float *pfStack184;
    float fVar27;
    float fStack168;
    float fStack164;
    float fStack160;
    float fStack156;
    float fStack152;
    float fStack148;
    float fStack144;
    float fStack140;
    float fStack136;
    undefined8 uStack132;
    float fStack124;
    undefined8 uStack120;
    undefined4 uStack112;
    undefined4 uStack108;
    float fStack100;
    float fStack96;
    float fStack92;
    float fStack88;
    float fStack84;
    float fStack80;
    float fStack76;
    float fStack72;
    undefined4 uStack68;
    undefined4 uStack64;
    float fStack52;
    float fStack48;
    float fStack44;
    undefined4 uStack40;
    undefined4 uStack36;
    undefined uStack32;
    undefined4 uStack28;
    undefined uStack24;
    float *pfStack20;
    undefined4 uStack12;
    undefined4 uStack8;
    undefined4 uStack4;
    
    uStack12 = *in_FS_OFFSET;
    uStack4 = 0xffffffff;
    uStack8 = 0x7bcecd;
    *in_FS_OFFSET = &uStack12;
    if ((*(int32_t **)(param_1 + 0x48) == (int32_t *)0x0) || (**(int32_t **)(param_1 + 0x48) == 0)) {
        *param_2 = 0.0;
        param_2[1] = 0.0;
        param_2[2] = 0.0;
        param_2[3] = 1.0;
        goto code_r0x006c2897;
    }
    pfStack184 = &fStack92;
    fcn.006c1850();
    uStack8 = 0;
    if (fStack96 == 0.0) {
        if (*(int32_t **)(param_1 + 0x48) == (int32_t *)0x0) {
            iVar17 = 0;
        } else {
            iVar17 = **(int32_t **)(param_1 + 0x48);
        }
        fStack96 = *(float *)(iVar17 + 0x5c);
        if (fStack96 != 0.0) {
            uStack196 = 0x6c1b6d;
            fStack192 = fStack96;
            fcn.004d3a40();
        }
    }
    fVar15 = fStack96;
    uStack132 = (double)((uint64_t)(uint32_t)uStack132._4_4_ << 0x20);
    pfStack200 = *(float **)(param_1 + 0x7c);
    fStack192 = 4.203895e-45;
    uStack196 = 0x7e5a2c;
    pfStack204 = (float *)0x0;
    uStack8 = CONCAT31(uStack8._1_3_, 1);
    puStack208 = (undefined8 *)0x6c1b97;
    iVar17 = fcn.0040a410();
    if (iVar17 == 0) {
        puStack208 = (undefined8 *)&stack0xffffff54;
        pfStack212 = (float *)0x6c1ba9;
        fcn.0046a110();
        uStack24 = 2;
        if (unaff_EBX != 0.0) {
            puStack208 = (undefined8 *)&fStack168;
            pfStack212 = (float *)0x6c1bc6;
            piVar18 = (int32_t *)fcn.0046fec0();
            if (*piVar18 != 0) {
                pfStack212 = (float *)&stack0xffffff44;
                fcn.0046fec0();
                uStack32 = 3;
                uVar19 = fcn.0047abb0(&stack0xffffff50);
                uStack36 = CONCAT31(uStack36._1_3_, 4);
                fcn.004266a0(uVar19);
                pfVar20 = pfStack184;
                bVar23 = pfStack184 != (float *)0x0;
                uStack40 = CONCAT31(uStack40._1_3_, 3);
                pfStack184 = (float *)0x0;
                if (bVar23) {
                    fcn.005621f0(pfVar20, 0xffffffff);
                }
            }
        }
        uStack28._0_1_ = 1;
        if (unaff_EBP != 0) {
            pfStack212 = (float *)0xffffffff;
            fcn.00469ad0(unaff_EBP);
        }
    } else {
        pfStack212 = (float *)0x6c1c4f;
        puStack208 = (undefined8 *)(param_1 + 0x68);
        fcn.004967a0();
        pfStack212 = &fStack72;
        pfVar20 = (float *)fcn.005629b0(&stack0xffffff54);
        fStack152 = *pfVar20;
        uStack28._0_1_ = 5;
        if (fStack152 != 0.0) {
            pfStack212 = (float *)0x1;
            fcn.005621f0(fStack152);
        }
        uStack28._0_1_ = 1;
        if (unaff_EBX != 0.0) {
            pfStack212 = (float *)0xffffffff;
            fcn.005621f0(unaff_EBX);
        }
    }
    if (fStack152 == 0.0) {
        *pfStack20 = 0.0;
        pfStack20[1] = 0.0;
        uStack28 = 0xffffffff;
        pfStack20[2] = 0.0;
        pfStack20[3] = 1.0;
        param_2 = pfStack20;
        if (fVar15 != 0.0) {
            pfStack212 = (float *)0xffffffff;
            fcn.004d3a40(fVar15);
        }
        goto code_r0x006c2897;
    }
    fStack72 = 0.0;
    iVar17 = *(int32_t *)((int32_t)fStack152 + 0x5c);
    uStack28 = CONCAT31(uStack28._1_3_, 6);
    if ((*(uint8_t *)(iVar17 + 0x18) & 1) == 0) {
        if (*(int32_t *)(iVar17 + 0x5c) == 0) {
            *(undefined4 *)(iVar17 + 0x38) = *(undefined4 *)(iVar17 + 0x1c);
            *(undefined4 *)(iVar17 + 0x3c) = *(undefined4 *)(iVar17 + 0x20);
            *(undefined4 *)(iVar17 + 0x40) = *(undefined4 *)(iVar17 + 0x24);
            *(undefined4 *)(iVar17 + 0x44) = *(undefined4 *)(iVar17 + 0x28);
            *(undefined4 *)(iVar17 + 0x48) = *(undefined4 *)(iVar17 + 0x2c);
            *(undefined4 *)(iVar17 + 0x4c) = *(undefined4 *)(iVar17 + 0x30);
            fVar26 = *(float *)(iVar17 + 0x34);
        } else {
            pfStack212 = (float *)0x6c1d0d;
            pfVar20 = (float *)fcn.004698a0();
            pfStack212 = pfVar20;
            pfVar21 = (float *)fcn.004aee20(&fStack100, iVar17 + 0x2c);
            fStack164 = pfVar20[4] + *pfVar21;
            fVar27 = pfVar20[5];
            fVar3 = pfVar21[1];
            fVar26 = pfVar20[6] + pfVar21[2];
            fVar4 = pfVar20[3];
            fVar5 = pfVar20[1];
            fVar6 = pfVar20[2];
            fVar7 = *pfVar20;
            fStack192 = (*pfVar20 * *(float *)(iVar17 + 0x20) +
                        pfVar20[3] * *(float *)(iVar17 + 0x24) + *(float *)(iVar17 + 0x28) * pfVar20[2]) -
                        *(float *)(iVar17 + 0x1c) * pfVar20[1];
            fStack160 = ((pfVar20[3] * *(float *)(iVar17 + 0x28) - *(float *)(iVar17 + 0x1c) * *pfVar20) -
                        pfVar20[1] * *(float *)(iVar17 + 0x20)) - *(float *)(iVar17 + 0x24) * pfVar20[2];
            *(float *)(iVar17 + 0x38) =
                 (*(float *)(iVar17 + 0x24) * pfVar20[1] +
                 *(float *)(iVar17 + 0x28) * *pfVar20 + pfVar20[3] * *(float *)(iVar17 + 0x1c)) -
                 *(float *)(iVar17 + 0x20) * pfVar20[2];
            *(float *)(iVar17 + 0x3c) =
                 (*(float *)(iVar17 + 0x1c) * fVar6 +
                 *(float *)(iVar17 + 0x28) * fVar5 + fVar4 * *(float *)(iVar17 + 0x20)) -
                 *(float *)(iVar17 + 0x24) * fVar7;
            *(float *)(iVar17 + 0x40) = fStack192;
            *(float *)(iVar17 + 0x44) = fStack160;
            *(float *)(iVar17 + 0x48) = fStack164;
            *(float *)(iVar17 + 0x4c) = fVar27 + fVar3;
            fStack156 = fVar26;
        }
        *(float *)(iVar17 + 0x50) = fVar26;
        *(uint32_t *)(iVar17 + 0x18) = *(uint32_t *)(iVar17 + 0x18) | 1;
    }
    pfStack212 = (float *)(iVar17 + 0x38);
    pfVar20 = (float *)fcn.004aee20(&fStack144, param_1 + 0xa4);
    fStack144 = *(float *)(iVar17 + 0x48) + *pfVar20;
    fStack140 = *(float *)(iVar17 + 0x4c) + pfVar20[1];
    fStack192 = *(float *)(iVar17 + 0x50) + pfVar20[2];
    fStack136 = fStack192;
    if (*(int32_t *)(param_1 + 0x98) != 0) {
        pfStack212 = (float *)0x0;
        fVar26 = 1.287806e-38;
        fStack156 = (float)fcn.004946d0(0x8c3ac8);
        uStack36 = CONCAT31(uStack36._1_3_, 7);
        if (fStack156 != 0.0) {
            puVar1 = (undefined8 *)(param_1 + 0x84);
            puVar25 = puVar1;
            fcn.004967a0(puVar1);
            piVar18 = (int32_t *)fcn.005beb10(&pfStack184, 0);
            if (piVar18 == (int32_t *)0x0) {
                fcn.004967a0(puVar1);
                piVar18 = (int32_t *)fcn.005be3e0(&pfStack212);
                if (piVar18 != (int32_t *)0x0) goto code_r0x006c1ee7;
                iVar17 = *(int32_t *)(unaff_EDI + 0x5c);
                if ((*(uint8_t *)(iVar17 + 0x18) & 1) == 0) {
                    if (*(int32_t *)(iVar17 + 0x5c) == 0) {
                        *(undefined4 *)(iVar17 + 0x38) = *(undefined4 *)(iVar17 + 0x1c);
                        *(undefined4 *)(iVar17 + 0x3c) = *(undefined4 *)(iVar17 + 0x20);
                        *(undefined4 *)(iVar17 + 0x40) = *(undefined4 *)(iVar17 + 0x24);
                        *(undefined4 *)(iVar17 + 0x44) = *(undefined4 *)(iVar17 + 0x28);
                        *(undefined4 *)(iVar17 + 0x48) = *(undefined4 *)(iVar17 + 0x2c);
                        *(undefined4 *)(iVar17 + 0x4c) = *(undefined4 *)(iVar17 + 0x30);
                        pfVar20 = *(float **)(iVar17 + 0x34);
                    } else {
                        pfVar21 = (float *)fcn.004698a0();
                        pfVar20 = (float *)fcn.004aee20((int32_t)&uStack132 + 4, iVar17 + 0x2c, pfVar21);
                        fStack192 = pfVar21[4] + *pfVar20;
                        pfStack204 = (float *)(pfVar21[5] + pfVar20[1]);
                        pfVar20 = (float *)(pfVar21[6] + pfVar20[2]);
                        pfStack200 = (float *)((*(float *)(iVar17 + 0x24) * pfVar21[1] +
                                               *(float *)(iVar17 + 0x1c) * pfVar21[3] +
                                               *(float *)(iVar17 + 0x28) * *pfVar21) -
                                              *(float *)(iVar17 + 0x20) * pfVar21[2]);
                        fVar26 = (*(float *)(iVar17 + 0x1c) * pfVar21[2] +
                                 *(float *)(iVar17 + 0x28) * pfVar21[1] + pfVar21[3] * *(float *)(iVar17 + 0x20)) -
                                 *pfVar21 * *(float *)(iVar17 + 0x24);
                        puVar25 = (undefined8 *)
                                  ((*pfVar21 * *(float *)(iVar17 + 0x20) +
                                   pfVar21[3] * *(float *)(iVar17 + 0x24) + *(float *)(iVar17 + 0x28) * pfVar21[2]) -
                                  *(float *)(iVar17 + 0x1c) * pfVar21[1]);
                        fVar27 = pfVar21[3];
                        fVar3 = *pfVar21;
                        fVar4 = pfVar21[1];
                        fVar5 = pfVar21[2];
                        *(float **)(iVar17 + 0x38) = pfStack200;
                        *(float *)(iVar17 + 0x3c) = fVar26;
                        *(undefined8 **)(iVar17 + 0x40) = puVar25;
                        *(float *)(iVar17 + 0x44) =
                             ((*(float *)(iVar17 + 0x28) * fVar27 - *(float *)(iVar17 + 0x1c) * fVar3) -
                             fVar4 * *(float *)(iVar17 + 0x20)) - *(float *)(iVar17 + 0x24) * fVar5;
                        *(float *)(iVar17 + 0x48) = fStack192;
                        *(float **)(iVar17 + 0x4c) = pfStack204;
                        pfStack184 = pfVar20;
                    }
                    *(float **)(iVar17 + 0x50) = pfVar20;
                    *(uint32_t *)(iVar17 + 0x18) = *(uint32_t *)(iVar17 + 0x18) | 1;
                }
                pfVar20 = (float *)(iVar17 + 0x38);
            } else {
code_r0x006c1ee7:
                iVar17 = *piVar18;
                if ((*(uint8_t *)(iVar17 + 0x18) & 1) == 0) {
                    if (*(int32_t *)(iVar17 + 0x5c) == 0) {
                        *(undefined4 *)(iVar17 + 0x38) = *(undefined4 *)(iVar17 + 0x1c);
                        *(undefined4 *)(iVar17 + 0x3c) = *(undefined4 *)(iVar17 + 0x20);
                        *(undefined4 *)(iVar17 + 0x40) = *(undefined4 *)(iVar17 + 0x24);
                        *(undefined4 *)(iVar17 + 0x44) = *(undefined4 *)(iVar17 + 0x28);
                        *(undefined4 *)(iVar17 + 0x48) = *(undefined4 *)(iVar17 + 0x2c);
                        *(undefined4 *)(iVar17 + 0x4c) = *(undefined4 *)(iVar17 + 0x30);
                        fVar26 = *(float *)(iVar17 + 0x34);
                    } else {
                        pfStack212 = (float *)0x6c1f03;
                        pfVar20 = (float *)fcn.004698a0();
                        pfStack212 = pfVar20;
                        pfVar21 = (float *)fcn.004aee20(&fStack100, iVar17 + 0x2c);
                        fStack164 = pfVar20[4] + *pfVar21;
                        fVar27 = pfVar20[5];
                        fVar3 = pfVar21[1];
                        fVar26 = pfVar20[6] + pfVar21[2];
                        fVar4 = pfVar20[3];
                        fVar5 = pfVar20[1];
                        fVar6 = pfVar20[2];
                        fVar7 = *pfVar20;
                        fStack192 = (*pfVar20 * *(float *)(iVar17 + 0x20) +
                                    pfVar20[3] * *(float *)(iVar17 + 0x24) + *(float *)(iVar17 + 0x28) * pfVar20[2]) -
                                    *(float *)(iVar17 + 0x1c) * pfVar20[1];
                        fStack160 = ((pfVar20[3] * *(float *)(iVar17 + 0x28) - *(float *)(iVar17 + 0x1c) * *pfVar20) -
                                    pfVar20[1] * *(float *)(iVar17 + 0x20)) - *(float *)(iVar17 + 0x24) * pfVar20[2];
                        *(float *)(iVar17 + 0x38) =
                             (*(float *)(iVar17 + 0x24) * pfVar20[1] +
                             pfVar20[3] * *(float *)(iVar17 + 0x1c) + *(float *)(iVar17 + 0x28) * *pfVar20) -
                             *(float *)(iVar17 + 0x20) * pfVar20[2];
                        *(float *)(iVar17 + 0x3c) =
                             (*(float *)(iVar17 + 0x1c) * fVar6 +
                             *(float *)(iVar17 + 0x28) * fVar5 + fVar4 * *(float *)(iVar17 + 0x20)) -
                             *(float *)(iVar17 + 0x24) * fVar7;
                        *(float *)(iVar17 + 0x40) = fStack192;
                        *(float *)(iVar17 + 0x44) = fStack160;
                        *(float *)(iVar17 + 0x48) = fStack164;
                        *(float *)(iVar17 + 0x4c) = fVar27 + fVar3;
                        fStack156 = fVar26;
                    }
                    *(float *)(iVar17 + 0x50) = fVar26;
                    *(uint32_t *)(iVar17 + 0x18) = *(uint32_t *)(iVar17 + 0x18) | 1;
                }
                pfVar20 = (float *)(iVar17 + 0x38);
                fVar26 = (float)(param_1 + 0xa4);
                puVar25 = &uStack132;
                pfStack212 = pfVar20;
            }
            pfVar21 = (float *)fcn.004aee20(puVar25, fVar26);
            fStack144 = pfVar20[4] + *pfVar21;
            fStack140 = pfVar20[5] + pfVar21[1];
            fStack192 = pfVar20[6] + pfVar21[2];
            fStack136 = fStack192;
        }
        uStack28 = CONCAT31(uStack28._1_3_, 6);
    }
    uStack132 = *(double *)((int32_t)fVar15 + 0x1c);
    fStack124 = *(float *)((int32_t)fVar15 + 0x24);
    uStack120 = (double)CONCAT44(uStack120._4_4_, *(undefined4 *)((int32_t)fVar15 + 0x28));
    *(undefined4 *)((int32_t)fVar15 + 0x1c) = *(undefined4 *)(param_1 + 0xb0);
    *(undefined4 *)((int32_t)fVar15 + 0x20) = *(undefined4 *)(param_1 + 0xb4);
    *(undefined4 *)((int32_t)fVar15 + 0x24) = *(undefined4 *)(param_1 + 0xb8);
    *(undefined4 *)((int32_t)fVar15 + 0x28) = *(undefined4 *)(param_1 + 0xbc);
    if ((*(uint32_t *)((int32_t)fVar15 + 0x18) & 1) != 0) {
        piVar18 = *(int32_t **)((int32_t)fVar15 + 0x6c);
        *(uint32_t *)((int32_t)fVar15 + 0x18) = *(uint32_t *)((int32_t)fVar15 + 0x18) & 0xfffffffe;
        while (piVar18 != (int32_t *)0x0) {
            iVar17 = *piVar18;
            piVar18 = (int32_t *)piVar18[2];
            pfStack200 = (float *)0x6c221c;
            (**(code **)(iVar17 + 4))();
        }
        for (iVar17 = *(int32_t *)((int32_t)fVar15 + 0x60); iVar17 != 0; iVar17 = *(int32_t *)(iVar17 + 0x68)) {
            pfStack200 = (float *)0x6c2237;
            fcn.004699e0();
        }
    }
    if ((*(uint8_t *)((int32_t)fVar15 + 0x18) & 1) == 0) {
        if (*(int32_t *)((int32_t)fVar15 + 0x5c) == 0) {
            *(undefined4 *)((int32_t)fVar15 + 0x38) = *(undefined4 *)((int32_t)fVar15 + 0x1c);
            *(undefined4 *)((int32_t)fVar15 + 0x3c) = *(undefined4 *)((int32_t)fVar15 + 0x20);
            *(undefined4 *)((int32_t)fVar15 + 0x40) = *(undefined4 *)((int32_t)fVar15 + 0x24);
            *(undefined4 *)((int32_t)fVar15 + 0x44) = *(undefined4 *)((int32_t)fVar15 + 0x28);
            *(undefined4 *)((int32_t)fVar15 + 0x48) = *(undefined4 *)((int32_t)fVar15 + 0x2c);
            *(undefined4 *)((int32_t)fVar15 + 0x4c) = *(undefined4 *)((int32_t)fVar15 + 0x30);
            fVar26 = *(float *)((int32_t)fVar15 + 0x34);
        } else {
            pfStack200 = (float *)0x6c2258;
            pfVar20 = (float *)fcn.004698a0();
            pfStack204 = (float *)((int32_t)fVar15 + 0x2c);
            puStack208 = (undefined8 *)&uStack36;
            pfStack212 = (float *)0x6c226c;
            pfStack200 = pfVar20;
            pfVar21 = (float *)fcn.004aee20();
            fStack152 = pfVar20[4] + *pfVar21;
            fStack164 = pfVar20[5] + pfVar21[1];
            fVar26 = pfVar20[6] + pfVar21[2];
            fVar27 = pfVar20[3];
            fVar3 = pfVar20[1];
            fVar4 = pfVar20[2];
            fVar5 = *pfVar20;
            fVar6 = pfVar20[3];
            fVar7 = pfVar20[2];
            fVar8 = *pfVar20;
            fVar9 = pfVar20[1];
            fVar10 = pfVar20[3];
            fVar11 = *pfVar20;
            fVar12 = pfVar20[1];
            fVar13 = pfVar20[2];
            *(float *)((int32_t)fVar15 + 0x38) =
                 (*(float *)((int32_t)fVar15 + 0x24) * pfVar20[1] +
                 *pfVar20 * *(float *)((int32_t)fVar15 + 0x28) + *(float *)((int32_t)fVar15 + 0x1c) * pfVar20[3]) -
                 *(float *)((int32_t)fVar15 + 0x20) * pfVar20[2];
            *(float *)((int32_t)fVar15 + 0x3c) =
                 (fVar4 * *(float *)((int32_t)fVar15 + 0x1c) +
                 *(float *)((int32_t)fVar15 + 0x28) * fVar3 + *(float *)((int32_t)fVar15 + 0x20) * fVar27) -
                 fVar5 * *(float *)((int32_t)fVar15 + 0x24);
            *(float *)((int32_t)fVar15 + 0x40) =
                 (*(float *)((int32_t)fVar15 + 0x20) * fVar8 +
                 fVar7 * *(float *)((int32_t)fVar15 + 0x28) + fVar6 * *(float *)((int32_t)fVar15 + 0x24)) -
                 *(float *)((int32_t)fVar15 + 0x1c) * fVar9;
            *(float *)((int32_t)fVar15 + 0x44) =
                 ((fVar10 * *(float *)((int32_t)fVar15 + 0x28) - *(float *)((int32_t)fVar15 + 0x1c) * fVar11) -
                 *(float *)((int32_t)fVar15 + 0x20) * fVar12) - fVar13 * *(float *)((int32_t)fVar15 + 0x24);
            *(float *)((int32_t)fVar15 + 0x48) = fStack152;
            *(float *)((int32_t)fVar15 + 0x4c) = fStack164;
            fStack144 = fVar26;
        }
        *(float *)((int32_t)fVar15 + 0x50) = fVar26;
        *(uint32_t *)((int32_t)fVar15 + 0x18) = *(uint32_t *)((int32_t)fVar15 + 0x18) | 1;
    }
    pfStack200 = &fStack52;
    fStack88 = (float)uStack132 - *(float *)((int32_t)fVar15 + 0x48);
    pfStack204 = &fStack88;
    puStack208 = (undefined8 *)&fStack100;
    fStack84 = uStack132._4_4_ - *(float *)((int32_t)fVar15 + 0x4c);
    fStack80 = fStack124 - *(float *)((int32_t)fVar15 + 0x50);
    fStack160 = -*(float *)((int32_t)fVar15 + 0x38);
    fStack48 = -*(float *)((int32_t)fVar15 + 0x3c);
    fStack44 = -*(float *)((int32_t)fVar15 + 0x40);
    fStack148 = *(float *)((int32_t)fVar15 + 0x44);
    pfStack212 = (float *)0x6c240f;
    fStack52 = fStack160;
    uStack40 = fStack148;
    fcn.004aee20();
    *(float *)((int32_t)fVar15 + 0x1c) = (float)uStack120;
    *(undefined4 *)((int32_t)fVar15 + 0x20) = uStack120._4_4_;
    *(undefined4 *)((int32_t)fVar15 + 0x24) = uStack112;
    *(undefined4 *)((int32_t)fVar15 + 0x28) = uStack108;
    if ((*(uint32_t *)((int32_t)fVar15 + 0x18) & 1) != 0) {
        piVar18 = *(int32_t **)((int32_t)fVar15 + 0x6c);
        *(uint32_t *)((int32_t)fVar15 + 0x18) = *(uint32_t *)((int32_t)fVar15 + 0x18) & 0xfffffffe;
        while (piVar18 != (int32_t *)0x0) {
            iVar17 = *piVar18;
            piVar18 = (int32_t *)piVar18[2];
            pfStack200 = (float *)0x6c244c;
            (**(code **)(iVar17 + 4))();
        }
        for (iVar17 = *(int32_t *)((int32_t)fVar15 + 0x60); iVar17 != 0; iVar17 = *(int32_t *)(iVar17 + 0x68)) {
            pfStack200 = (float *)0x6c2467;
            fcn.004699e0();
        }
    }
    fStack136 = *(float *)(param_1 + 0xc0);
    fStack148 = 99999.0;
    fStack160 = 99999.0;
    fStack144 = 0.0;
    fStack164 = 0.0;
    uStack120 = (double)(fStack100 * fStack100);
    uStack132 = (double)(fStack92 * fStack92);
    pfStack200 = (float *)0x6c24b9;
    Var24 = (unkfloat10)fcn.00764280();
    fVar26 = (float)Var24 / 1.0;
    fStack168 = fVar26 * fStack92;
    fVar26 = fStack168 * *(float *)0x895248 +
             fVar26 * 0.0 * *(float *)0x895244 + fVar26 * fStack100 * *(float *)0x895240;
    if ((uint16_t)((uint16_t)(fVar26 < 1.0) << 8 | (uint16_t)(fVar26 == 1.0) << 0xe) == 0) {
        fVar26 = 1.0;
    }
    pfStack200 = (float *)0x6c253d;
    Var24 = (unkfloat10)fcn.00765a10();
    if (fStack100 <= 0.0) {
        fVar27 = -1.0;
    } else {
        fVar27 = 1.0;
    }
    fStack152 = (float)Var24 * fVar27 * 57.29578;
    pfStack200 = (float *)0x6c2591;
    Var24 = (unkfloat10)fcn.00764280();
    uStack132 = (double)((uint64_t)uStack132 & 0xffffffff |
                        (uint64_t)(uint32_t)((1.0 / (float)Var24) * fStack96) << 0x20);
    pfStack200 = (float *)0x6c25be;
    Var24 = (unkfloat10)fcn.00765b60();
    iVar17 = 0;
    uStack120._0_4_ = (float)Var24 * 57.29578;
    uStack120 = (double)((uint64_t)uStack120 & 0xffffffff00000000 | (uint64_t)(uint32_t)(float)uStack120);
    if (0 < *(int32_t *)(param_1 + 200)) {
        iVar22 = 0;
        do {
            if ((*(int32_t *)(param_1 + 0xc0) == -1) || (*(int32_t *)(param_1 + 0xc0) == iVar17)) {
                if ((0.0 < fStack148) &&
                   ((iVar14 = *(int32_t *)(param_1 + 0xd0),
                    *(float *)(iVar14 + iVar22) != *(float *)(iVar14 + 4 + iVar22) && (fVar26 <= 1.0)))) {
                    pfStack200 = (float *)(iVar14 + iVar22);
                    pfStack204 = (float *)((int32_t)pfStack200 + 4);
                    puStack208 = (undefined8 *)&fStack152;
                    pfStack212 = (float *)0x6c2641;
                    cVar16 = fcn.0044f310();
                    if (cVar16 == '\0') {
                        fVar27 = *(float *)(*(int32_t *)(param_1 + 0xd0) + 4 + iVar22);
                        pfVar20 = (float *)(*(int32_t *)(param_1 + 0xd0) + iVar22);
                        if ((uint16_t)((uint16_t)(fVar27 < fStack152) << 8 | (uint16_t)(fVar27 == fStack152) << 0xe) ==
                            0) {
                            fVar27 = pfVar20[1] - fStack152;
                        } else {
                            fVar27 = fStack152 - *pfVar20;
                        }
                        if ((uint16_t)((uint16_t)(fStack148 < fVar27) << 8 | (uint16_t)(fStack148 == fVar27) << 0xe) ==
                            0) {
                            pfStack200 = (float *)(*(int32_t *)(param_1 + 0xd0) + iVar22);
                            pfStack204 = (float *)((int32_t)pfStack200 + 4);
                            puStack208 = (undefined8 *)&fStack152;
                            pfStack212 = (float *)0x6c2734;
                            fStack136 = (float)iVar17;
                            Var24 = (unkfloat10)fcn.00463ba0();
                            fStack144 = (float)Var24;
                            fStack148 = fVar27;
                        }
                    } else {
                        fStack148 = 0.0;
                        fStack144 = fStack152;
                        fStack136 = (float)iVar17;
                    }
                }
                if (((uint16_t)((uint16_t)(fStack160 < 0.0) << 8 | (uint16_t)(fStack160 == 0.0) << 0xe) == 0) &&
                   (iVar14 = *(int32_t *)(param_1 + 0xd0),
                   *(float *)(iVar14 + iVar22 + 0xc) != *(float *)(iVar14 + 8 + iVar22))) {
                    fVar27 = *(float *)(iVar14 + 0xc + iVar22);
                    iVar2 = iVar14 + iVar22;
                    if ((fVar27 < (float)uStack120 == (fVar27 == (float)uStack120)) ||
                       (*(float *)(iVar2 + 8) < (float)uStack120)) {
                        if ((uint16_t)
                            ((uint16_t)(*(float *)(iVar2 + 0xc) < (float)uStack120) << 8 |
                            (uint16_t)(*(float *)(iVar2 + 0xc) == (float)uStack120) << 0xe) == 0) {
                            fVar27 = *(float *)(iVar2 + 0xc) - (float)uStack120;
                        } else {
                            fVar27 = (float)uStack120 - *(float *)(iVar2 + 8);
                        }
                        if (fVar27 < fStack160) {
                            pfStack200 = (float *)(iVar14 + iVar22 + 8);
                            pfStack204 = (float *)(iVar14 + iVar22 + 0xc);
                            puStack208 = &uStack120;
                            pfStack212 = (float *)0x6c2799;
                            fStack136 = (float)iVar17;
                            Var24 = (unkfloat10)fcn.00463ba0();
                            fStack164 = (float)Var24;
                            fStack160 = fVar27;
                        }
                    } else {
                        fStack160 = 0.0;
                        fStack164 = (float)uStack120;
                        fStack136 = (float)iVar17;
                    }
                }
            }
            iVar17 = iVar17 + 1;
            iVar22 = iVar22 + 0x18;
        } while (iVar17 < *(int32_t *)(param_1 + 200));
    }
    if (-1 < (int32_t)fStack136) {
        fStack144 = *(float *)(*(int32_t *)(param_1 + 0xd0) + 0x10 + (int32_t)fStack136 * 0x18) + fStack144;
        fStack164 = *(float *)(*(int32_t *)(param_1 + 0xd0) + (int32_t)fStack136 * 0x18 + 0x14) + fStack164;
    }
    fStack76 = 0.0;
    fStack72 = 0.0;
    uStack68 = 0;
    uStack64 = 0x3f800000;
    pfStack200 = (float *)0x0;
    pfStack204 = (float *)(fStack144 * 0.01745329);
    puStack208 = (undefined8 *)(fStack164 * -0.01745329);
    pfStack212 = (float *)0x6c2841;
    fStack160 = (float)puStack208;
    fcn.004ac910();
    *pfStack20 = fStack88;
    pfStack212 = (float *)0xffffffff;
    pfStack20[1] = fStack84;
    uStack28 = uStack28 & 0xffffff00;
    pfStack20[2] = fStack80;
    pfStack20[3] = fStack76;
    fcn.005621f0(fStack152);
    uStack28 = 0xffffffff;
    fcn.004d3a40(fVar15, 0xffffffff);
    param_2 = pfStack20;
code_r0x006c2897:
    *in_FS_OFFSET = uStack36;
    return param_2;
}
