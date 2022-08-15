// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

undefined4 __thiscall method.CompressedTransformKeys.virtual_0(undefined4 param_1, uint8_t param_2)
{
    fcn.0053ae60();
    if ((param_2 & 1) != 0) {
        fcn.0042dca0(param_1);
    }
    return param_1;
}

undefined4 __thiscall method.CompressedTransformKeys.virtual_4()
{
}

undefined4 __thiscall method.CompressedTransformKeys.virtual_8()
{
}


// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

undefined4 method.CompressedTransformKeys.virtual_12(void)
{
    if ((*(uint32_t *)0x8bfb5c & 0x20000000) == 0) {
        fcn.00438160(0x897358);
        *(undefined4 *)0x8bfb60 = 0x1c;
        fcn.004458e0(0x8bfb4c);
    }
    return 0x8bfb4c;
}


// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_18h is stack pointer based, which is not supported for decompilation.

// 0x006c6500
void __thiscall method.CompressedTransformKeys.virtual_16(int32_t param_1, int32_t *param_2)
{
    int32_t iVar1;
    int32_t *piVar2;
    undefined4 uVar3;
    undefined4 *puVar4;
    uint32_t unaff_ESI;
    undefined4 uStack12;
    uint8_t uStack8;
    char cStack4;
    
    piVar2 = param_2;
    iVar1 = *param_2;
    param_2 = (int32_t *)((uint32_t)param_2 & 0xffffff00);
    if (*(int32_t *)(iVar1 + 100) == 0) {
        fcn.0044a3a0(&param_2);                                 // Reads file. End up at ;-- method.FileWin32.virtual_32: 0x00738460 
    } else {
        uStack12 = uStack12 & 0xffffff;
        fcn.0044a280((int32_t)&uStack12 + 3, 1);
    }
    if (cStack4 == -1) {
        if (*(int32_t *)(iVar1 + 100) == 0) {
            puVar4 = (undefined4 *)&stack0xfffffff0;
            fcn.0044a3a0(puVar4);
        } else {
            puVar4 = &uStack12;
            uStack12 = 0;
            fcn.0044a280(puVar4, 2);
        }
        uVar3 = fcn.005f13c0(unaff_ESI & 0xffff);
    } else {
        uVar3 = fcn.005f13c0(cStack4);
        puVar4 = (undefined4 *)(uint32_t)uStack8;
    }
    fcn.005f04e0(uVar3, puVar4);                        // <- code below
    if (*(int32_t *)(iVar1 + 100) == 0) {
        fcn.0044a3a0();
    } else {
        fcn.0044a280(*(undefined4 *)(param_1 + 0x40), *(int32_t *)(param_1 + 0x44) + 7U >> 3);
    }
    fcn.0074aa80(piVar2); // <- code below
    *(uint16_t *)(param_1 + 0x50) = (uint16_t)**(undefined4 **)(undefined4 *)(param_1 + 0x40) & 0x3fff;
    fcn.006c6230();
    return;
}


// WARNING: [rz-ghidra] Var arg_f8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_f4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h_3 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_1ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_20h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_1ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_28h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_30h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_48h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

// 0x006c65f0
// param_2 = output transform
void __thiscall method.CompressedTransformKeys.virtual_20(int32_t param_1, undefined4 *param_2, undefined4 *param_3, undefined4 param_4)
{
    undefined4 *puVar1;
    undefined4 *puVar2;
    float *pfVar3;
    uint8_t uVar4;
    float fVar5;
    undefined4 uVar6;
    uint32_t uVar7;
    float *pfVar8;
    uint32_t uVar9;
    float unaff_EBX;
    float fVar10;
    float *pfVar11;
    float unaff_EBP;
    float *unaff_ESI;
    uint32_t uVar12;
    undefined *puVar13;
    float fVar14;
    undefined4 *puVar15;
    undefined4 *puVar16;
    float fVar17;
    float fStack236;
    float fStack232;
    float fStack228;
    float fStack224;
    float fStack220;
    float fStack216;
    float fStack212;
    float fStack208;
    float *pfStack204;
    float fStack196;
    float fStack192;
    float fStack188;
    float fStack184;
    float fStack180;
    float *pfStack176;
    float fStack172;
    float fStack168;
    float fStack164;
    float fStack160;
    float fStack156;
    undefined4 uStack152;
    undefined4 uStack148;
    undefined4 uStack144;
    float fStack140;
    float fStack136;
    int32_t iStack132;
    undefined4 uStack124;
    undefined4 auStack120 [24];
    float *pfStack24;
    float *pfStack12;
    undefined4 *puStack8;
    float fStack4;
    
    fVar10 = (float)(uint32_t)*(uint16_t *)(param_1 + 0x50);
    if (fVar10 != 0.0) {
        auStack120[0] = 0;
        puVar16 = &uStack124;
        uStack124 = 0;
        puVar15 = auStack120;
        fVar14 = fVar10;
        fVar5 = (float)fcn.0074a990(param_4); // this function is in this file
        fVar17 = fVar5;
        if ((int32_t)fVar5 + 2 < (int32_t)*(int16_t *)(param_1 + 0xe8)) {
            fcn.006c6230();
        }
        if ((int32_t)*(int16_t *)(param_1 + 0xe8) < (int32_t)fVar5 + 2) {
            do {
                uVar4 = *(char *)(param_1 + 0xf5) + 1U & 3;
                *(uint8_t *)(param_1 + 0xf5) = uVar4;
                puVar1 = (undefined4 *)(param_1 + (((int32_t)(char)uVar4 - 1U & 3) + 3) * 0x1c);
                puVar2 = (undefined4 *)(param_1 + ((char)uVar4 + 3) * 0x1c);
                *puVar2 = *puVar1;
                puVar2[1] = puVar1[1];
                puVar2[2] = puVar1[2];
                puVar2[3] = puVar1[3];
                puVar2[4] = puVar1[4];
                puVar2[5] = puVar1[5];
                puVar2[6] = puVar1[6];
                *(int16_t *)(param_1 + 0xe8) = *(int16_t *)(param_1 + 0xe8) + 1;
                *(int32_t *)(param_1 + 0xe0) = *(int32_t *)(param_1 + 0xe0) << 2;
                *(char *)(param_1 + 0xf4) = *(char *)(param_1 + 0xf4) + '\x01';
                if ((int32_t)*(int16_t *)(param_1 + 0xe8) < (int32_t)fVar10) {
                    if (*(uint8_t *)(param_1 + 0xf2) <= *(uint8_t *)(param_1 + 0xf4)) {
                        fcn.006c5f40();
                    }
                    fVar5 = *(float *)(param_1 + 0x48);
                    fStack168 = 0.0;
                    fStack164 = 0.0;
                    fStack160 = 0.0;
                    fStack156 = 0.0;
                    uStack152 = 0;
                    uStack148 = 0;
                    uStack144 = 0;
                    uVar6 = fcn.005f06a0(fVar5, &fStack168, param_1 + 0xeb, 7);
                    *(undefined4 *)(param_1 + 0x48) = uVar6;
                    puVar13 = &stack0xffffff04;
                    fcn.006c6460();
                    if (*(char *)(param_1 + 0xea) == '\0') {
                        *(undefined **)(param_1 + 0xc4) = puVar13;
                        *(float *)(param_1 + 200) = fVar14;
                        *(undefined4 **)(param_1 + 0xcc) = puVar15;
                        *(undefined4 **)(param_1 + 0xd0) = puVar16;
                        *(float *)(param_1 + 0xd4) = fVar17;
                        *(float *)(param_1 + 0xd8) = unaff_EBP;
                        *(float **)(param_1 + 0xdc) = unaff_ESI;
                    } else {
                        *(float *)(param_1 + 0xd4) = fVar17 + *(float *)(param_1 + 0xd4);
                        *(float *)(param_1 + 0xd8) = unaff_EBP + *(float *)(param_1 + 0xd8);
                        *(float *)(param_1 + 0xdc) = *(float *)(param_1 + 0xdc) + (float)unaff_ESI;
                        *(float *)(param_1 + 0xc4) = *(float *)(param_1 + 0xc4) + (float)puVar13;
                        *(float *)(param_1 + 200) = *(float *)(param_1 + 200) + fVar14;
                        *(float *)(param_1 + 0xcc) = *(float *)(param_1 + 0xcc) + (float)puVar15;
                        *(float *)(param_1 + 0xd0) = (float)puVar16 + *(float *)(param_1 + 0xd0);
                    }
                    pfVar8 = (float *)(param_1 + (*(char *)(param_1 + 0xf5) + 3) * 0x1c);
                    pfVar8[4] = pfVar8[4] + *(float *)(param_1 + 0xd4);
                    pfVar8[5] = *(float *)(param_1 + 0xd8) + pfVar8[5];
                    pfVar8[6] = *(float *)(param_1 + 0xdc) + pfVar8[6];
                    *pfVar8 = *(float *)(param_1 + 0xc4) + *pfVar8;
                    pfVar8[1] = *(float *)(param_1 + 200) + pfVar8[1];
                    pfVar8[2] = *(float *)(param_1 + 0xcc) + pfVar8[2];
                    pfVar8[3] = *(float *)(param_1 + 0xd0) + pfVar8[3];
                    fcn.004ac6f0();
                    *(uint32_t *)(param_1 + 0xe0) =
                         *(uint32_t *)(param_1 + 0xe0) | (uint32_t)*(uint8_t *)(param_1 + 0xf3);
                    if (*(int16_t *)(param_1 + 0xe8) == 0) {
                        pfVar8 = (float *)(param_1 + (*(char *)(param_1 + 0xf5) + 3) * 0x1c);
                        fStack228 = *pfVar8;
                        fStack224 = pfVar8[1];
                        fStack220 = pfVar8[2];
                        fStack216 = pfVar8[3];
                        fStack212 = pfVar8[4];
                        fStack208 = pfVar8[5];
                        pfStack204 = (float *)pfVar8[6];
                        *(float *)(param_1 + 0x54) = fStack228;
                        *(float *)(param_1 + 0x58) = fStack224;
                        *(float *)(param_1 + 0x5c) = fStack220;
                        *(float *)(param_1 + 0x60) = fStack216;
                        *(float *)(param_1 + 100) = fStack212;
                        *(float *)(param_1 + 0x68) = fStack208;
                        *(float **)(param_1 + 0x6c) = pfStack204;
                        *(uint32_t *)(param_1 + 0xe0) =
                             *(int32_t *)(param_1 + 0xe0) * 4 | (uint32_t)*(uint8_t *)(param_1 + 0xf3);
                        *(float *)(param_1 + 0x70) = fStack228;
                        *(float *)(param_1 + 0x74) = fStack224;
                        *(float *)(param_1 + 0x78) = fStack220;
                        *(float *)(param_1 + 0x7c) = fStack216;
                        *(float *)(param_1 + 0x80) = fStack212;
                        *(float *)(param_1 + 0x84) = fStack208;
                        *(float **)(param_1 + 0x88) = pfStack204;
                        *(uint32_t *)(param_1 + 0xe0) =
                             *(int32_t *)(param_1 + 0xe0) * 4 | (uint32_t)*(uint8_t *)(param_1 + 0xf3);
                        *(float *)(param_1 + 0x8c) = fStack228;
                        *(float *)(param_1 + 0x90) = fStack224;
                        *(float *)(param_1 + 0x94) = fStack220;
                        *(float *)(param_1 + 0x98) = fStack216;
                        *(float *)(param_1 + 0x9c) = fStack212;
                        *(float *)(param_1 + 0xa0) = fStack208;
                        *(float **)(param_1 + 0xa4) = pfStack204;
                        *(uint32_t *)(param_1 + 0xe0) =
                             *(int32_t *)(param_1 + 0xe0) * 4 | (uint32_t)*(uint8_t *)(param_1 + 0xf3);
                        *(float *)(param_1 + 0xa8) = fStack228;
                        *(float *)(param_1 + 0xac) = fStack224;
                        *(float *)(param_1 + 0xb0) = fStack220;
                        *(float *)(param_1 + 0xb4) = fStack216;
                        *(float *)(param_1 + 0xb8) = fStack212;
                        *(float *)(param_1 + 0xbc) = fStack208;
                        *(float **)(param_1 + 0xc0) = pfStack204;
                        *(uint32_t *)(param_1 + 0xe0) =
                             *(int32_t *)(param_1 + 0xe0) * 4 | (uint32_t)*(uint8_t *)(param_1 + 0xf3);
                    }
                } else {
                    *(uint32_t *)(param_1 + 0xe0) =
                         (uint32_t)*(uint8_t *)(param_1 + 0xf3) | *(uint32_t *)(param_1 + 0xe0);
                }
            } while ((int32_t)*(int16_t *)(param_1 + 0xe8) < (int32_t)fVar5 + 2);
        }
        fStack4 = fStack4 - fStack136;
        uVar7 = ((int32_t)*(char *)(param_1 + 0xf5) - (int32_t)*(int16_t *)(param_1 + 0xe8)) + (int32_t)fVar5;
        iStack132 = param_1 + ((uVar7 - 2 & 3) + 3) * 0x1c;
        pfVar8 = (float *)(param_1 + ((uVar7 & 3) + 3) * 0x1c);
        pfVar3 = (float *)(param_1 + ((uVar7 + 1 & 3) + 3) * 0x1c);
        uVar4 = ((char)*(int16_t *)(param_1 + 0xe8) - SUB41(fVar5, 0)) * '\x02';
        uVar9 = *(uint32_t *)(param_1 + 0xe0) >> (uVar4 & 0x1f) & 3;
        uVar12 = *(uint32_t *)(param_1 + 0xe0) >> (uVar4 - 2 & 0x1f) & 3;
        if ((uint16_t)((uint16_t)(fStack4 < 0.0) << 8 | (uint16_t)(fStack4 == 0.0) << 0xe) != 0) {
            fStack4 = 0.0;
        }
        if (fStack136 == fStack140) {
            if ((uint16_t)((uint16_t)(fStack4 < 1.0) << 8 | (uint16_t)(fStack4 == 1.0) << 0xe) == 0) {
                fStack4 = 1.0;
            }
        } else {
            fStack4 = (fStack140 - fStack136) / fStack4;
        }
        if ((uVar12 == 1) && (uVar9 == 1)) {
            fStack236 = *pfVar8;
            fStack232 = pfVar8[1];
            fStack228 = pfVar8[2];
            fStack224 = pfVar8[3];
            fStack220 = pfVar8[4];
            fStack216 = pfVar8[5];
            fStack212 = pfVar8[6];
            fcn.004aef90(pfVar3, fStack4, &fStack236);
            fVar10 = pfVar3[4];
            fVar5 = pfVar3[5];
            fVar17 = pfVar3[6];
            *pfStack24 = unaff_EBP;
            pfStack24[1] = (float)unaff_ESI;
            pfStack24[2] = unaff_EBX;
            pfStack24[3] = fStack236;
            pfStack24[4] = fVar14 * (fVar10 - fStack232) + fStack232;
            pfStack24[5] = fStack228 + (fVar5 - fStack228) * fVar14;
            pfStack24[6] = fStack224 + fVar14 * (fVar17 - fStack224);
        } else if (uVar9 == 0) {
            *pfStack12 = *pfVar8;
            pfStack12[1] = pfVar8[1];
            pfStack12[2] = pfVar8[2];
            pfStack12[3] = pfVar8[3];
            pfStack12[4] = pfVar8[4];
            pfStack12[5] = pfVar8[5];
            pfStack12[6] = pfVar8[6];
        } else {
            fStack168 = 0.0;
            fStack164 = 0.0;
            fStack160 = 0.0;
            uStack152 = 0;
            uStack148 = 0;
            uStack144 = 0;
            fStack196 = 0.0;
            fStack192 = 0.0;
            fStack188 = 0.0;
            fStack180 = 0.0;
            pfStack176 = (float *)0x0;
            fStack172 = 0.0;
            fStack156 = 1.0;
            fStack184 = 1.0;
            pfVar11 = pfVar3;
            if ((uVar9 != 3) && (pfVar11 = (float *)(param_1 + ((uVar7 - 1 & 3) + 3) * 0x1c), uVar9 != 2)) {
                fStack236 = *pfVar3;
                fStack232 = pfVar3[1];
                fStack228 = pfVar3[2];
                fStack224 = pfVar3[3];
                fStack220 = pfVar3[4];
                fStack216 = pfVar3[5];
                fStack212 = pfVar3[6];
                fcn.004aef90(pfVar8, 0x40000000, &fStack236);
                fStack220 = (pfVar8[4] - fStack232) * 2.0;
                fStack216 = (pfVar8[5] - fStack228) * 2.0;
                fStack212 = (pfVar8[6] - fStack224) * 2.0;
                fStack164 = fStack220 + fStack232;
                fStack160 = fStack228 + fStack216;
                fStack156 = fStack224 + fStack212;
                fStack168 = fStack236;
                pfVar11 = &fStack180;
                fStack180 = unaff_EBP;
                pfStack176 = unaff_ESI;
                fStack172 = unaff_EBX;
            }
            if ((uVar12 != 3) && (uVar12 != 2)) {
                fStack236 = *pfVar8;
                fStack232 = pfVar8[1];
                fStack228 = pfVar8[2];
                fStack224 = pfVar8[3];
                fStack220 = pfVar8[4];
                fStack216 = pfVar8[5];
                fStack212 = pfVar8[6];
                unaff_ESI = pfVar3;
                fcn.004aef90();
                fStack220 = (pfVar3[4] - fStack232) * 2.0;
                fStack216 = (pfVar3[5] - fStack228) * 2.0;
                fStack212 = (pfVar3[6] - fStack224) * 2.0;
                fStack192 = fStack220 + fStack232;
                fStack188 = fStack228 + fStack216;
                fStack184 = fStack224 + fStack212;
                fStack196 = fStack236;
                fStack208 = unaff_EBP;
                pfStack204 = unaff_ESI;
            }
            fcn.006c7050(pfVar11, pfVar8);
            pfVar8 = (float *)fcn.006c70c0(&fStack188, unaff_ESI);
            *pfStack12 = *pfVar8;
            pfStack12[1] = pfVar8[1];
            pfStack12[2] = pfVar8[2];
            pfStack12[3] = pfVar8[3];
            pfStack12[4] = pfVar8[4];
            pfStack12[5] = pfVar8[5];
            pfStack12[6] = pfVar8[6];
            fcn.004ac6f0();
        }
        *puStack8 = 0x3f800000;
        puStack8[1] = 0x3f800000;
        return;
    }
    // if param_1 + 0x50 == 0
    *param_2 = *(undefined4 *)0x895184;         // 0.0
    param_2[1] = *(undefined4 *)0x895188;       // 0.0
    param_2[2] = *(undefined4 *)0x89518c;       // 0.0
    param_2[3] = *(undefined4 *)0x895190;       // 1.0
    param_2[4] = *(undefined4 *)0x895194;       // 0.0
    param_2[5] = *(undefined4 *)0x895198;       // 0.0
    param_2[6] = *(undefined4 *)0x89519c;       // 0.0
    *param_3 = 0;
    param_3[1] = 0;
    return;
}

// 0x00895183      add     byte [eax], al
// 0x00895185      add     byte [eax], al
// 0x00895187      add     byte [eax], al
// 0x00895189      add     byte [eax], al
// 0x0089518b      add     byte [eax], al
// 0x0089518d      add     byte [eax], al
// 0x0089518f      add     byte [eax], al
// 0x00895191      add     byte [eax + 0x3f], al
// 0x00895197      add     byte [eax], al
// 0x00895199      add     byte [eax], al
// 0x0089519b      add     byte [eax], al
// 0x0089519d      add     byte [eax], al
// 0x0089519f      add     byte [eax], al
// 0x008951a1      add     byte [eax], al
// 0x008951a3      add     byte [eax], al
// 0x008951a5      add     byte [eax], al
// 0x008951a7      add     byte [eax], al
// 0x008951a9      add     byte [eax], al
// 0x008951ab      add     byte [eax], al
// 0x008951ad      add     byte [eax], al
// 0x008951af      add     byte [eax], al
// 0x008951b1      add     byte [eax], al
// 0x008951b3      add     byte [eax], al
// 0x008951b5      add     byte [eax], al
// 0x008951b7      add     byte [edi], cl

void __thiscall method.CompressedTransformKeys.virtual_24()
{
}

;-- method.CompressedTransformKeys.virtual_40:
0x00548470      jmp     fcn.005462e0


// WARNING: [rz-ghidra] Var arg_38h is stack pointer based, which is not supported for decompilation.

undefined4 fcn.005462e0(void)
{
    if ((*(uint32_t *)0x8eb584 & 0x20000000) == 0) {
        fcn.00438160(0x8a3f04);
        *(undefined4 *)0x8eb588 = 0xf8;
        *(undefined4 *)0x8eb59c = 0x8a4168;
        *(code **)0x8eb314 = method.AnimationMixerBase.virtual_40;
        *(char **)0x8eb2fc = "Baseclass_AnimationValueInterfaceBase";
        *(undefined4 *)0x8eb300 = 0;
        *(undefined4 *)0x8eb304 = 0x10;
        *(undefined4 *)0x8eb308 = 0x8eb574;
        *(undefined4 *)0x8eb590 = 0x8eb2fc;
    }
    return 0x8eb574;
}

// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h_3 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.

uint32_t __thiscall fcn.0074a990(int32_t param_1, undefined4 *param_2, undefined4 *param_3)
{
    int32_t iVar1;
    uint32_t uVar2;
    undefined4 *unaff_ESI;
    undefined4 *unaff_EDI;
    undefined4 *puVar3;
    
    puVar3 = param_2;
    if ((uint16_t)
        ((uint16_t)(*(float *)(param_1 + 0x10) < (float)param_2) << 8 |
        (uint16_t)(*(float *)(param_1 + 0x10) == (float)param_2) << 0xe) == 0) {
        if (0 < *(int32_t *)(param_1 + 0x1c)) {
            *(undefined4 *)(param_1 + 0x1c) = 0xffffffff;
            *(undefined4 *)(param_1 + 0x18) = 0x3d088889;
            fcn.005f03b0(0xd);
            *(undefined4 *)(param_1 + 0x14) = 0xbd088889;
            *(undefined *)(param_1 + 0x25) = 1;
            *(undefined4 *)(param_1 + 0x10) = 0xbd088889;
            puVar3 = unaff_ESI;
            fcn.0074a6c0(param_3);
            *(undefined4 *)(param_1 + 0x10) = *(undefined4 *)(param_1 + 0x14);
        }
        if ((float)puVar3 <= *(float *)(param_1 + 0x10)) {
            *param_2 = *(undefined4 *)(param_1 + 0x10);
            *param_3 = *(undefined4 *)(param_1 + 0x14);
            return 0;
        }
    }
    iVar1 = *(int32_t *)(param_1 + 0x1c);
    while ((iVar1 < (int32_t)param_3 &&
           (((uint16_t)
             ((uint16_t)(*(float *)(param_1 + 0x10) < (float)puVar3) << 8 |
             (uint16_t)(*(float *)(param_1 + 0x10) == (float)puVar3) << 0xe) == 0 ||
            (*(float *)(param_1 + 0x14) < (float)puVar3))))) {
        *(undefined4 *)(param_1 + 0x10) = *(undefined4 *)(param_1 + 0x14);
        fcn.0074a6c0();
        iVar1 = *(int32_t *)(param_1 + 0x1c);
        puVar3 = unaff_EDI;
    }
    uVar2 = *(int32_t *)(param_1 + 0x1c) - 1;
    *param_2 = *(undefined4 *)(param_1 + 0x10);
    *param_3 = *(undefined4 *)(param_1 + 0x14);
    return uVar2 & ((int32_t)uVar2 < 0) - 1;
}


// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

void __thiscall fcn.005f04e0(undefined4 *param_1, int32_t param_2)
{
    undefined4 unaff_retaddr;
    
    if (*(char *)(param_1 + 3) != '\0') {
        fcn.0042dca0(*param_1);
        *param_1 = 0;
        param_1[1] = 0;
        param_1[2] = 0;
        *(undefined *)(param_1 + 3) = 0;
    }
    *(undefined *)(param_1 + 3) = 0;
    *param_1 = unaff_retaddr;
    param_1[1] = param_2 * 8;
    return;
}


// WARNING: [rz-ghidra] Var arg_18h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch_2 is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.

void __thiscall fcn.0074aa80(undefined4 *param_1, int32_t *param_2)
{
    int32_t iVar1;
    undefined4 uVar2;
    uint32_t unaff_ESI;
    uint32_t unaff_EDI;
    uint32_t uVar3;
    undefined4 uStack12;
    undefined uStack8;
    char cStack4;
    
    iVar1 = *param_2;
    param_2 = (int32_t *)((uint32_t)param_2 & 0xffffff00);
    uVar3 = 1;
    if (*(int32_t *)(iVar1 + 100) == 0) {
        fcn.0044a3a0(&param_2);
    } else {
        uStack12 = uStack12 & 0xffffff;
        fcn.0044a280((int32_t)&uStack12 + 3, 1);
    }
    if (cStack4 == -1) {
        if (*(int32_t *)(iVar1 + 100) == 0) {
            fcn.0044a3a0(&stack0xfffffff0);
        } else {
            uStack12 = 0;
            fcn.0044a280(&uStack12, 2);
        }
        fcn.005f0580(unaff_ESI & 0xffff);
        uVar2 = fcn.005f13c0(unaff_EDI & 0xffff);
        uVar3 = uVar3 & 0xffff;
    } else {
        fcn.005f0580(cStack4);
        uVar2 = fcn.005f13c0(uStack8);
        uVar3 = uStack12 & 0xff;
    }
    fcn.005f04e0(uVar2, uVar3);
    if (*(int32_t *)(iVar1 + 100) == 0) {
        fcn.0044a3a0();
    } else {
        fcn.0044a280(*param_1, param_1[1] + 7 >> 3);
    }
    param_1[6] = 0x3d088889;    // 0.0333333350718
    param_1[7] = 0xffffffff;    // nan
    fcn.005f03b0(0xd);
    param_1[5] = 0xbd088889;    // -0.0333333350718
    param_1[4] = 0xbd088889;    // -0.0333333350718
    *(undefined *)((int32_t)param_1 + 0x25) = 1;
    fcn.0074a6c0(1);  // Code below
    param_1[4] = param_1[5];
    return;
}


// WARNING: [rz-ghidra] Var arg_14h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

void __thiscall fcn.0074a6c0(uint32_t **param_1, uint32_t param_2)
{
    char *pcVar1;
    uint32_t *puVar2;
    undefined4 uVar3;
    uint8_t uVar4;
    uint32_t uVar5;
    char extraout_DL;
    uint32_t uVar6;
    uint32_t *puVar7;
    uint32_t *puVar8;
    uint32_t uVar9;
    uint32_t *puVar10;
    unkfloat10 Var11;
    uint8_t uStack12;
    
    param_1[7] = (uint32_t *)((int32_t)param_1[7] + 1);
    if ((int32_t)param_1[7] < (int32_t)param_2) {
        pcVar1 = (char *)((int32_t)param_1 + 0x25);
        *pcVar1 = *pcVar1 + -1;
        if (*pcVar1 == '\0') {
            puVar7 = param_1[2];
            puVar10 = *param_1;
            puVar2 = (uint32_t *)((int32_t)puVar7 + 1);
            param_1[2] = puVar2;
            *(bool *)((int32_t)param_1 + 0x26) =
                 (*(uint8_t *)(((uint32_t)puVar7 >> 3) + (int32_t)puVar10) & (uint8_t)(1 << ((uint8_t)puVar7 & 7))) != 0
            ;
            uVar6 = *puVar10 & 7;
            puVar7 = (uint32_t *)((int32_t)puVar2 + uVar6);
            param_2 = 0x20 - ((uint32_t)puVar2 & 0x1f);
            puVar8 = (uint32_t *)(((uint32_t)puVar2 >> 3 & 0xfffffffc) + (int32_t)puVar10);
            param_1[2] = puVar7;
            if (uVar6 <= param_2) {
                param_2 = uVar6;
            }
            if (param_2 == 0x20) {
                uStack12 = 0xff;
            } else {
                uStack12 = (char)(1 << ((uint8_t)param_2 & 0x1f)) - 1;
            }
            uVar4 = (uint8_t)(*puVar8 >> (int8_t)((uint32_t)puVar2 & 0x1f)) & uStack12;
            if (uVar6 - param_2 != 0) {
                uVar4 = uVar4 | (uint8_t)(((1 << ((uint8_t)(uVar6 - param_2) & 0x1f)) - 1U & puVar8[1]) <<
                                         ((uint8_t)param_2 & 0x1f));
            }
            *(uint8_t *)(param_1 + 9) = uVar4;
            uVar6 = (*puVar10 >> 3 & 7) + 1;
            puVar2 = (uint32_t *)((int32_t)puVar7 + uVar6);
            uVar5 = 0x20 - ((uint32_t)puVar7 & 0x1f);
            puVar8 = (uint32_t *)(((uint32_t)puVar7 >> 3 & 0xfffffffc) + (int32_t)puVar10);
            param_1[2] = puVar2;
            uVar9 = uVar6;
            if (uVar5 < uVar6) {
                uVar9 = uVar5;
            }
            if (uVar9 == 0x20) {
                uStack12 = 0xff;
            } else {
                uStack12 = (char)(1 << ((uint8_t)uVar9 & 0x1f)) - 1;
            }
            uStack12 = (uint8_t)(*puVar8 >> (int8_t)((uint32_t)puVar7 & 0x1f)) & uStack12;
            if (uVar6 - uVar9 != 0) {
                uStack12 = uStack12 |
                           (uint8_t)(((1 << ((uint8_t)(uVar6 - uVar9) & 0x1f)) - 1U & puVar8[1]) <<
                                    ((uint8_t)uVar9 & 0x1f));
            }
            *(uint8_t *)((int32_t)param_1 + 0x25) = uStack12;
            if (uVar4 != 0) {
                uVar6 = *puVar10 >> 10 & 7;
                if (uVar6 == 7) {
                    Var11 = (unkfloat10)fcn.005f05a0(); //code below
                } else {
                    uVar3 = *(undefined4 *)(uVar6 * 4 + 0x817b80);
                    uVar9 = *puVar10 >> 6 & 0xf;
                    param_1[2] = (uint32_t *)((int32_t)puVar2 + uVar9);
                    uVar5 = 0x20 - ((uint32_t)puVar2 & 0x1f);
                    puVar10 = (uint32_t *)(((uint32_t)puVar2 >> 3 & 0xfffffffc) + (int32_t)puVar10);
                    uVar6 = uVar9;
                    if (uVar5 < uVar9) {
                        uVar6 = uVar5;
                    }
                    if (uVar6 == 0x20) {
                        uVar5 = 0xffffffff;
                    } else {
                        uVar5 = (1 << ((uint8_t)uVar6 & 0x1f)) - 1;
                    }
                    uVar5 = *puVar10 >> (int8_t)((uint32_t)puVar2 & 0x1f) & uVar5;
                    if (uVar9 - uVar6 != 0) {
                        uVar5 = uVar5 | ((1 << ((uint8_t)(uVar9 - uVar6) & 0x1f)) - 1U & puVar10[1]) <<
                                        ((uint8_t)uVar6 & 0x1f);
                    }
                    Var11 = (unkfloat10)fcn.006cb330(uVar5, uVar9, uVar3);
                }
                param_1[8] = (uint32_t *)(float)Var11;
            }
        }
        uVar9 = (uint32_t)*(uint8_t *)(param_1 + 9);
        puVar2 = param_1[2];
        param_1[2] = (uint32_t *)((int32_t)puVar2 + uVar9);
        puVar7 = (uint32_t *)(((uint32_t)puVar2 >> 3 & 0xfffffffc) + (int32_t)*param_1);
        uVar6 = 0x20 - ((uint32_t)puVar2 & 0x1f);
        if (uVar9 <= uVar6) {
            uVar6 = uVar9;
        }
        if (uVar6 == 0x20) {
            uVar5 = 0xffffffff;
        } else {
            uVar5 = (1 << ((uint8_t)uVar6 & 0x1f)) - 1;
        }
        uVar5 = *puVar7 >> (int8_t)((uint32_t)puVar2 & 0x1f) & uVar5;
        if (uVar9 - uVar6 != 0) {
            uVar5 = uVar5 | ((1 << ((uint8_t)(uVar9 - uVar6) & 0x1f)) - 1U & puVar7[1]) << ((uint8_t)uVar6 & 0x1f);
        }
        Var11 = (unkfloat10)
                fcn.0074a560(uVar5, uVar9, param_1[8], 
                             uVar6 & 0xffffff00 | (uint32_t)(*(char *)((int32_t)param_1 + 0x26) != '\0'));
        if (extraout_DL != '\0') {
            param_1[6] = (uint32_t *)((float)param_1[6] + (float)(uint32_t *)(float)Var11);
            param_1[5] = (uint32_t *)((float)param_1[6] + (float)param_1[5]);
            return;
        }
        param_1[6] = (uint32_t *)(float)Var11;
        param_1[5] = (uint32_t *)((float)param_1[6] + (float)param_1[5]);
    }
    return;
}

unkfloat10 __fastcall fcn.005f05a0(int32_t *param_1)
{
    uint32_t uVar1;
    uint32_t *puVar2;
    uint32_t uVar3;
    float fVar4;
    uint32_t uVar5;
    
    uVar1 = param_1[2];
    param_1[2] = uVar1 + 0x20;
    puVar2 = (uint32_t *)((uVar1 >> 3 & 0xfffffffc) + *param_1);
    uVar3 = 0x20 - (uVar1 & 0x1f);
    if (uVar3 < 0x20) {
        uVar5 = (1 << ((uint8_t)uVar3 & 0x1f)) - 1;
    } else {
        uVar3 = 0x20;
        uVar5 = 0xffffffff;
    }
    fVar4 = (float)(*puVar2 >> (int8_t)(uVar1 & 0x1f) & uVar5);
    if (0x20 - uVar3 != 0) {
        fVar4 = (float)((uint32_t)fVar4 |
                       ((1 << ((uint8_t)(0x20 - uVar3) & 0x1f)) - 1U & puVar2[1]) << ((uint8_t)uVar3 & 0x1f));
    }
    return (unkfloat10)fVar4;
}


// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.

unkfloat10 fcn.006cb330(uint32_t param_1, uint8_t param_2, float param_3)
{
    float fVar1;
    float fVar2;
    uint32_t uVar3;
    
    uVar3 = (1 << (param_2 & 0x1f)) - 1;
    if (param_1 == uVar3) {
        return (unkfloat10)param_3;
    }
    if (param_1 != 0) {
        fVar1 = (float)(uVar3 & param_1);
        if ((int32_t)(uVar3 & param_1) < 0) {
            fVar1 = fVar1 + 4.294967e+09;
        }
        fVar2 = (float)uVar3;
        if ((int32_t)uVar3 < 0) {
            fVar2 = fVar2 + 4.294967e+09;
        }
        fVar2 = fVar2 / (param_3 * fVar1);
        if ((uint16_t)((uint16_t)(fVar2 < 0.0) << 8 | (uint16_t)(fVar2 == 0.0) << 0xe) != 0) {
            fVar2 = 0.0;
        }
        if (param_3 <= fVar2) {
            return (unkfloat10)param_3;
        }
        return (unkfloat10)fVar2;
    }
    return (unkfloat10)0;
}


// WARNING: [rz-ghidra] Var arg_8h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_10h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_4h is stack pointer based, which is not supported for decompilation.
// WARNING: [rz-ghidra] Var arg_ch is stack pointer based, which is not supported for decompilation.

unkfloat10 fcn.0074a560(uint32_t param_1, float param_2, float param_3, char param_4)
{
    float fVar1;
    float fVar2;
    int32_t iVar3;
    uint32_t uVar4;
    
    iVar3 = (int32_t)param_2;
    if (0 < (int32_t)param_2) {
        if (param_4 == '\0') {
            param_2 = 0.0;
        } else {
            param_2 = -param_3;
            param_3 = param_3 + param_3;
        }
        uVar4 = (1 << ((uint8_t)iVar3 & 0x1f)) - 1;
        fVar1 = (float)(uVar4 & param_1);
        if ((int32_t)(uVar4 & param_1) < 0) {
            fVar1 = fVar1 + 4.294967e+09;
        }
        fVar2 = (float)uVar4;
        if ((int32_t)uVar4 < 0) {
            fVar2 = fVar2 + 4.294967e+09;
        }
        return (unkfloat10)((fVar1 / fVar2) * param_3 + param_2);
    }
    return (unkfloat10)0;
}
