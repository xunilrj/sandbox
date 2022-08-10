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
