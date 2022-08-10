
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

void fcn.00449620(undefined4 *param_1)
{
    int32_t *piVar1;
    
    param_1[10] = 0x8972f0;
    *param_1 = 0x7e6b10;
    *(undefined4 *)0x8c0204 = 0xb;
    *(undefined4 *)0x8c0208 = 0x58ea00;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0xb) goto code_r0x00449669;
    }
    *(int32_t **)0x8c020c = (int32_t *)param_1[8];
    param_1[8] = 0x8c0204;
code_r0x00449669:
    *(undefined4 *)0x8c01f8 = 3;
    *(undefined4 *)0x8c01fc = 0x5903b0;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 3) goto code_r0x0044969f;
    }
    *(int32_t **)0x8c0200 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01f8;
code_r0x0044969f:
    *(undefined4 *)0x8c01ec = 0xd;
    *(undefined4 *)0x8c01f0 = 0x58ec70;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0xd) goto code_r0x004496d9;
    }
    *(int32_t **)0x8c01f4 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01ec;
code_r0x004496d9:
    *(undefined4 *)0x8c01e0 = 0x16;
    *(undefined4 *)0x8c01e4 = 0x58fd20;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0x16) goto code_r0x0044970f;
    }
    *(int32_t **)0x8c01e8 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01e0;
code_r0x0044970f:
    *(undefined4 *)0x8c01d4 = 0xf;
    *(undefined4 *)0x8c01d8 = 0x590d10;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0xf) goto code_r0x00449749;
    }
    *(int32_t **)0x8c01dc = (int32_t *)param_1[8];
    param_1[8] = 0x8c01d4;
code_r0x00449749:
    *(undefined4 *)0x8c01c8 = 0x14;
    *(undefined4 *)0x8c01cc = 0x590d80;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0x14) goto code_r0x0044977f;
    }
    *(int32_t **)0x8c01d0 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01c8;
code_r0x0044977f:
    *(undefined4 *)0x8c01bc = 0;
    *(undefined4 *)0x8c01c0 = 0x58ed30;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0) goto code_r0x004497b9;
    }
    *(int32_t **)0x8c01c4 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01bc;
code_r0x004497b9:
    *(undefined4 *)0x8c01b0 = 0x1c;
    *(undefined4 *)0x8c01b4 = 0x590bb0;
    for (piVar1 = (int32_t *)param_1[8]; piVar1 != (int32_t *)0x0; piVar1 = (int32_t *)piVar1[2]) {
        if (*piVar1 == 0x1c) goto code_r0x004497ef;
    }
    *(int32_t **)0x8c01b8 = (int32_t *)param_1[8];
    param_1[8] = 0x8c01b0;
code_r0x004497ef:
    *(code **)0x8c01ac = method.DCArray_class_String_.virtual_4;
    *(undefined4 *)0x8c0194 = 0x7e3ce0;
    *(undefined4 *)0x8c0198 = 0;
    *(undefined4 **)0x8c01a0 = param_1;
    param_1[7] = 0x8c0194;

    *(char **)0x8c0178 = "mbResetNavCamsOnExit";
    *(undefined4 *)0x8c017c = 0x1c;
    *(undefined4 **)0x8c0184 = param_1;
    *(undefined4 *)0x8c0188 = 0x8c015c;
    *(code **)0x8c0190 = method.Function1_bool_.virtual_12;
    *(undefined4 *)0x8c01a4 = 0x8c0178;

    *(char **)0x8c015c = "mLength";
    *(undefined4 *)0x8c0160 = 0x20;
    *(undefined4 **)0x8c0168 = param_1;
    *(undefined4 *)0x8c016c = 0x8c0140;
    *(code **)0x8c0174 = method.Function1_float_.virtual_12;

    *(char **)0x8c0140 = "mNumResources";
    *(undefined4 *)0x8c0144 = 0x24;
    *(undefined4 **)0x8c014c = param_1;
    *(undefined4 *)0x8c0150 = 0x8c0124;
    *(code **)0x8c0158 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8c0124 = "mNumAgents";
    *(undefined4 *)0x8c0128 = 0x28;
    *(undefined4 **)0x8c0130 = param_1;
    *(undefined4 *)0x8c0134 = 0x8c0108;
    *(code **)0x8c013c = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    // 0x2c .. 0x4C = 32 bytes (8 ints)


    *(char **)0x8c0108 = "mEditorProps";
    *(undefined4 *)0x8c010c = 0x4c;
    *(undefined4 **)0x8c0114 = param_1;
    *(undefined4 *)0x8c0118 = 0x8c00ec;
    *(code **)0x8c0120 = fcn.004185b0;


    *(char **)0x8c00ec = "mbChoreBackgroundFade";
    *(undefined4 *)0x8c00f0 = 0x98;
    *(undefined4 **)0x8c00f8 = param_1;
    *(undefined4 *)0x8c00fc = 0x8c00d0;
    *(code **)0x8c0104 = method.Function1_bool_.virtual_12;


    *(char **)0x8c00d0 = "mbChoreBackgroundLoop";
    *(undefined4 *)0x8c00d4 = 0x99;
    *(undefined4 **)0x8c00dc = param_1;
    *(undefined4 *)0x8c00e0 = 0x8c00b4;
    *(code **)0x8c00e8 = method.Function1_bool_.virtual_12;


    *(char **)0x8c0098 = "mbEndPause";
    *(undefined4 *)0x8c009c = 0x9a;
    *(undefined4 **)0x8c00a4 = param_1;
    *(undefined4 *)0x8c00a8 = 0x8c007c;
    *(code **)0x8c00b0 = method.Function1_bool_.virtual_12;


    *(char **)0x8c007c = "mRenderDelay";
    *(undefined4 *)0x8c0080 = 0x9c;
    *(undefined4 **)0x8c0088 = param_1;
    *(undefined4 *)0x8c008c = 0x8c0060;
    *(code **)0x8c0094 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8c0060 = "mSynchronizedToLocalization";
    *(undefined4 *)0x8c0064 = 0xa8;
    *(undefined4 **)0x8c006c = param_1;
    *(undefined4 *)0x8c0070 = 0x8c0044;
    *(code **)0x8c0078 = method.Set_class_Symbol__struct_std::less_class_Symbol__.virtual_4;


    *(char **)0x8c00b4 = "mChoreSceneFile";
    *(undefined4 *)0x8c00b8 = 0xb0;
    *(undefined4 **)0x8c00c0 = param_1;
    *(undefined4 *)0x8c00c4 = 0x8c0098;
    *(code **)0x8c00cc = method.DCArray_class_String_.virtual_4;

     *(char **)0x8c0044 = "mDependencies";
    *(undefined4 *)0x8c0048 = 0xd0;
    *(undefined4 **)0x8c0050 = param_1;
    *(undefined4 *)0x8c005c = 0x4495e0;



    return;
}




// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

int32_t fcn.00592c30(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x8a9a40;
    *(undefined4 *)0x8ef2d8 = 0x14;
    *(undefined4 *)0x8ef2dc = 0x5f49e0;
    fcn.00434ec0(0x8ef2d8);
    *(code **)0x8ef2d4 = method.DCArray_class_Ptr_class_AnimationValueInterfaceBase__.virtual_4;

    *(char **)0x8ef2bc = "mpChore";
    *(undefined4 *)0x8ef2c0 = 0;
    *(int32_t *)0x8ef2c8 = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8ef2bc;
    *(undefined4 *)0x8ef08c = 0x7f26f8;
    *(undefined4 *)0x8ef090 = 0xac;
    *(undefined4 *)0x8ef094 = 0x40;
    *(int32_t *)0x8ef098 = param_1;
    *(code **)0x8ef0a4 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8ef268 = "mVersion";
    *(undefined4 *)0x8ef26c = 4;
    *(int32_t *)0x8ef274 = param_1;
    *(undefined4 *)0x8ef278 = 0x8ef2a0;
    *(code **)0x8ef280 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;

    *(char **)0x8ef2a0 = "mResName";
    *(undefined4 *)0x8ef2a4 = 8;
    *(int32_t *)0x8ef2ac = param_1;
    *(undefined4 *)0x8ef2b0 = 0x8ef24c;
    *(code **)0x8ef2b8 = method.DCArray_class_String_.virtual_4;

    *(char **)0x8ef24c = "mResLength";
    *(undefined4 *)0x8ef250 = 0x24;
    *(int32_t *)0x8ef258 = param_1;
    *(undefined4 *)0x8ef25c = 0x8ef230;
    *(code **)0x8ef264 = method.Function1_float_.virtual_12;

    *(char **)0x8ef230 = "mPriority";
    *(undefined4 *)0x8ef234 = 0x28;
    *(int32_t *)0x8ef23c = param_1;
    *(undefined4 *)0x8ef240 = 0x8ef214;
    *(code **)0x8ef248 = method.Map_int__class_LanguageResource__struct_std::less_int__.virtual_28;


    *(char **)0x8ef214 = "mFlags";
    *(undefined4 *)0x8ef218 = 0x2c;
    *(int32_t *)0x8ef220 = param_1;
    *(undefined4 *)0x8ef224 = 0x8ef284;
    *(undefined4 *)0x8ef22c = 0x40d890;


    *(char **)0x8ef284 = "mResourceGroup";
    *(undefined4 *)0x8ef288 = 0x30;
    *(int32_t *)0x8ef290 = param_1;
    *(undefined4 *)0x8ef294 = 0x8ef1f8;
    *(code **)0x8ef29c = method.DCArray_class_String_.virtual_4;

    *(char **)0x8ef1f8 = "mhObject";
    *(undefined4 *)0x8ef1fc = 0x4c;
    *(int32_t *)0x8ef204 = param_1;
    *(undefined4 *)0x8ef208 = 0x8ef1dc;
    *(undefined4 *)0x8ef210 = 0x499f70;

    *(char **)0x8ef1dc = "mControlAnimation";
    *(undefined4 *)0x8ef1e0 = 0x50;
    *(int32_t *)0x8ef1e8 = param_1;
    *(undefined4 *)0x8ef1ec = 0x8ef1c0;
    *(code **)0x8ef1f4 = method.Animation.virtual_4;


    *(char **)0x8ef1c0 = "mBlocks";
    *(undefined4 *)0x8ef1c4 = 0x90;
    *(int32_t *)0x8ef1cc = param_1;
    *(undefined4 *)0x8ef1d0 = 0x8ef1a4;
    *(undefined4 *)0x8ef1d8 = 0x592420;

    *(char **)0x8ef1a4 = "mbNoPose";
    *(undefined4 *)0x8ef1a8 = 0xa0;
    *(int32_t *)0x8ef1b0 = param_1;
    *(undefined4 *)0x8ef1b4 = 0x8ef188;
    *(code **)0x8ef1bc = method.Function1_bool_.virtual_12;

    *(char **)0x8ef188 = "mbEmbedded";
    *(undefined4 *)0x8ef18c = 0xa1;
    *(int32_t *)0x8ef194 = param_1;
    *(undefined4 *)0x8ef198 = 0x8ef16c;
    *(code **)0x8ef1a0 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef16c = "mbEnabled";
    *(undefined4 *)0x8ef170 = 0xa2;
    *(int32_t *)0x8ef178 = param_1;
    *(undefined4 *)0x8ef17c = 0x8ef150;
    *(code **)0x8ef184 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef150 = "mbIsAgentResource";
    *(undefined4 *)0x8ef154 = 0xa3;
    *(int32_t *)0x8ef15c = param_1;
    *(undefined4 *)0x8ef160 = 0x8ef134;
    *(code **)0x8ef168 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef134 = "mbViewGraphs";
    *(undefined4 *)0x8ef138 = 0xa4;
    *(int32_t *)0x8ef140 = param_1;
    *(undefined4 *)0x8ef144 = 0x8ef118;
    *(code **)0x8ef14c = method.Function1_bool_.virtual_12;

    *(char **)0x8ef118 = "mbViewEmptyGraphs";
    *(undefined4 *)0x8ef11c = 0xa5;
    *(int32_t *)0x8ef124 = param_1;
    *(undefined4 *)0x8ef128 = 0x8ef0fc;
    *(code **)0x8ef130 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef0fc = "mbViewProperties";
    *(undefined4 *)0x8ef100 = 0xa6;
    *(int32_t *)0x8ef108 = param_1;
    *(undefined4 *)0x8ef10c = 0x8ef0e0;
    *(code **)0x8ef114 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef0e0 = "mbViewResourceGroups";
    *(undefined4 *)0x8ef0e4 = 0xa7;
    *(int32_t *)0x8ef0ec = param_1;
    *(undefined4 *)0x8ef0f0 = 0x8ef0c4;
    *(code **)0x8ef0f8 = method.Function1_bool_.virtual_12;

    *(char **)0x8ef0c4 = "mResourceProperties";
    *(undefined4 *)0x8ef0c8 = 0xb0;
    *(int32_t *)0x8ef0d0 = param_1;
    *(undefined4 *)0x8ef0d4 = 0x8ef0a8;
    *(code **)0x8ef0dc = fcn.004185b0;

    *(char **)0x8ef0a8 = "mResourceGroupInclude";
    *(undefined4 *)0x8ef0ac = 0xfc;
    *(int32_t *)0x8ef0b4 = param_1;
    *(undefined4 *)0x8ef0b8 = 0x8ef08c;
    *(undefined4 *)0x8ef0c0 = 0x48c450;






    *(undefined4 *)0x8ef2cc = 0x8ef268;
    return param_1;
}


// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// ChoreResource::Block
void fcn.005919d0(int32_t param_1)
{
    *(undefined4 *)(param_1 + 0x28) = 0x8a9944;
    *(code **)0x8eed4c = method.Function1_float_.virtual_12;
    *(char **)0x8eed34 = "mStartTime";
    *(undefined4 *)0x8eed38 = 0;
    *(int32_t *)0x8eed40 = param_1;
    *(undefined4 *)(param_1 + 0x1c) = 0x8eed34;
    *(uint32_t *)0x8eed20 = *(uint32_t *)0x8eed20 | 1;

    *(char **)0x8eecfc = "mEndTime";
    *(undefined4 *)0x8eed00 = 4;
    *(int32_t *)0x8eed08 = param_1;
    *(undefined4 *)0x8eed0c = 0x8eece0;
    *(code **)0x8eed14 = method.Function1_float_.virtual_12;

    *(char **)0x8eece0 = "mbLoopingBlock";
    *(undefined4 *)0x8eece4 = 8;
    *(int32_t *)0x8eecec = param_1;
    *(undefined4 *)0x8eecf0 = 0x8eecc4;
    *(code **)0x8eecf8 = method.Function1_bool_.virtual_12;

    *(char **)0x8eecc4 = "mScale";
    *(undefined4 *)0x8eecc8 = 0xc;
    *(int32_t *)0x8eecd0 = param_1;
    *(undefined4 *)0x8eecd4 = 0x8eed18;
    *(code **)0x8eecdc = method.Function1_float_.virtual_12;

    *(char **)0x8eed18 = "mbSelected";
    *(undefined4 *)0x8eed1c = 0x10;
    *(int32_t *)0x8eed24 = param_1;
    *(code **)0x8eed30 = method.Function1_bool_.virtual_12;

    *(undefined4 *)0x8eed44 = 0x8eecfc;
    return;
}