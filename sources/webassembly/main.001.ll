target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-linux-gnu"
declare ccc i8* @memcpy$def(i8*, i8*, i64)
declare ccc i8* @memmove$def(i8*, i8*, i64)
declare ccc i8* @memset$def(i8*, i64, i64)
declare ccc i64 @newSpark$def(i8*, i8*)
!0 = metadata !{metadata !"top", i8* null}
!1 = metadata !{metadata !"stack", metadata !0}
!2 = metadata !{metadata !"heap", metadata !0}
!3 = metadata !{metadata !"rx", metadata !2}
!4 = metadata !{metadata !"base", metadata !0}
!5 = metadata !{metadata !"other", metadata !0}
%__stginit_HSMod_struct = type <{}>
@__stginit_HSMod$def = internal global %__stginit_HSMod_struct<{}>
@__stginit_HSMod = alias i8* bitcast (%__stginit_HSMod_struct* @__stginit_HSMod$def to i8*)
%HSMod_sum_closure_struct = type <{i64}>
@HSMod_sum_closure$def = internal global %HSMod_sum_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @HSMod_sum_info$def to i64)}>
@HSMod_sum_closure = alias i8* bitcast (%HSMod_sum_closure_struct* @HSMod_sum_closure$def to i8*)
@HSMod_sum_info = alias i8* bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @HSMod_sum_info$def to i8*)
%HSMod_sum_entry_struct = type <{i64, i64, i64}>
@HSMod_sum_info_itable$def = internal constant %HSMod_sum_entry_struct<{i64 12884901911, i64 0, i64 15}>, section "X98A__STRIP,__me1", align 8
@HSMod_sum_info_itable = alias i8* bitcast (%HSMod_sum_entry_struct* @HSMod_sum_info_itable$def to i8*)
define cc 10 void @HSMod_sum_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind section "X98A__STRIP,__me2"
{
cxs:
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R4_Var = alloca i64, i32 1
  store i64 %R4_Arg, i64* %R4_Var
  %R3_Var = alloca i64, i32 1
  store i64 %R3_Arg, i64* %R3_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %lnxv = load i64** %Sp_Var
  %lnxw = getelementptr inbounds i64* %lnxv, i32 -3
  %lnxx = ptrtoint i64* %lnxw to i64
  %lnxy = icmp ult i64 %lnxx, %SpLim_Arg
  br i1 %lnxy, label %cxt, label %cxu
cxu:
  %lnxz = load i64* %R2_Var
  store i64 %lnxz, i64* %R2_Var
  %lnxB = ptrtoint i8* @stg_ap_pp_info to i64
  %lnxA = load i64** %Sp_Var
  %lnxC = getelementptr inbounds i64* %lnxA, i32 -3
  store i64 %lnxB, i64* %lnxC, !tbaa !1
  %lnxE = load i64* %R3_Var
  %lnxD = load i64** %Sp_Var
  %lnxF = getelementptr inbounds i64* %lnxD, i32 -2
  store i64 %lnxE, i64* %lnxF, !tbaa !1
  %lnxH = load i64* %R4_Var
  %lnxG = load i64** %Sp_Var
  %lnxI = getelementptr inbounds i64* %lnxG, i32 -1
  store i64 %lnxH, i64* %lnxI, !tbaa !1
  %lnxJ = load i64** %Sp_Var
  %lnxK = getelementptr inbounds i64* %lnxJ, i32 -3
  %lnxL = ptrtoint i64* %lnxK to i64
  %lnxM = inttoptr i64 %lnxL to i64*
  store i64* %lnxM, i64** %Sp_Var
  %lnxN = bitcast i8* @base_GHCziNum_zp_info to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %lnxO = load i64** %Sp_Var
  %lnxP = load i64* %R1_Var
  %lnxQ = load i64* %R2_Var
  tail call cc 10 void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* %lnxN( i64* %Base_Arg, i64* %lnxO, i64* %Hp_Arg, i64 %lnxP, i64 %lnxQ, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
cxt:
  %lnxR = load i64* %R4_Var
  store i64 %lnxR, i64* %R4_Var
  %lnxS = load i64* %R3_Var
  store i64 %lnxS, i64* %R3_Var
  %lnxT = load i64* %R2_Var
  store i64 %lnxT, i64* %R2_Var
  %lnxU = ptrtoint %HSMod_sum_closure_struct* @HSMod_sum_closure$def to i64
  store i64 %lnxU, i64* %R1_Var
  %lnxV = getelementptr inbounds i64* %Base_Arg, i32 -1
  %lnxW = bitcast i64* %lnxV to i64*
  %lnxX = load i64* %lnxW, !tbaa !4
  %lnxY = inttoptr i64 %lnxX to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %lnxZ = load i64** %Sp_Var
  %lny0 = load i64* %R1_Var
  %lny1 = load i64* %R2_Var
  %lny2 = load i64* %R3_Var
  %lny3 = load i64* %R4_Var
  tail call cc 10 void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* %lnxY( i64* %Base_Arg, i64* %lnxZ, i64* %Hp_Arg, i64 %lny0, i64 %lny1, i64 %lny2, i64 %lny3, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}
%Sy5_srt_struct = type <{}>
@Sy5_srt$def = internal constant %Sy5_srt_struct<{}>
@Sy5_srt = alias internal i8* bitcast (%Sy5_srt_struct* @Sy5_srt$def to i8*)
@stg_ap_pp_info = external global i8
@base_GHCziNum_zp_info = external global i8
@llvm.used = appending constant [4 x i8*] [i8* bitcast (%Sy5_srt_struct* @Sy5_srt$def to i8*), i8* bitcast (%HSMod_sum_entry_struct* @HSMod_sum_info_itable$def to i8*), i8* bitcast (%HSMod_sum_closure_struct* @HSMod_sum_closure$def to i8*), i8* bitcast (%__stginit_HSMod_struct* @__stginit_HSMod$def to i8*)], section "llvm.metadata"
