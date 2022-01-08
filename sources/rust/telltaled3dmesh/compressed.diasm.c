void* sub_557030(int32_t arg1)

00557035  *(arg1 + 0x28) = 0x8a488c
00557043  *data_8ed43c = 0x14
0055704d  *data_8ed440 = 0x6cfff0
00557057  sub_434ec0(arg1, data_8ed43c)
00557061  *data_8ed428 = 0x10
00557066  *data_8ed42c = arg1
0055706c  *data_8ed438 = 0x5469b0
00557076  *data_8ed420 = 0x7f08b0  {"Baseclass_AnimatedValueInterface…"}
00557080  *data_8ed424 = 8
0055708a  *(arg1 + 0x1c) = 0x8ed420
00557091  *data_8ed40c = 0x10
0055709b  *data_8ed400 = 0x6f1d60
005570a0  *data_8ed3e4 = 0x6f1d60
005570a5  *data_8ed410 = arg1
005570ab  *data_8ed3f4 = arg1
005570b1  *data_8ed3d8 = arg1
005570b7  *data_8ed3bc = arg1
005570bf  *data_8ed41c = 0x540710
005570c9  *data_8ed404 = 0x7f088c  {"Baseclass_KeyframedValueInterfac…"}
005570d3  *data_8ed408 = 0
005570dd  *data_8ed430 = 0x8ed404
005570e7  *data_8ed3e8 = 0x7f0884  {"mMinVal"}
005570f1  *data_8ed3ec = 0x20
005570fb  *data_8ed414 = 0x8ed3e8
00557105  *data_8ed3cc = 0x7f087c  {"mMaxVal"}
0055710f  *data_8ed3d0 = 0x24
00557119  *data_8ed3f8 = 0x8ed3cc
00557123  *data_8ed3c8 = 0x5566f0
0055712d  *data_8ed3b0 = 0x7f0870  {"mSamples"}
00557137  *data_8ed3b4 = 0x28
00557141  *data_8ed3dc = 0x8ed3b0
0055714c  return arg1

-----------------------------------------------------------------------------


void sub_6cb3d0(int32_t arg1 @ ebp, int32_t* arg2, void* arg3, void* arg4)

006cb3dd  int32_t* fsbase
006cb3dd  int32_t var_c = *fsbase
006cb3de  *fsbase = &var_c
006cb3eb  float* var_54 = nullptr
006cb3f0  void* esi = arg_10
006cb403  bool var_69 = *(**(esi + 0xc) + 0x58)() == 0
006cb421  if (*(esi + 0x64) == 1)
006cb421      sub_43d4f0(arg1, arg2, arg3, arg4, esi)
006cb42f      *fsbase = var_c
006cb439      return 
006cb443  void* ebp = *(arg3 + 0x18)
006cb446  if (ebp == 0)
006cb453      ebp = sub_43a730(arg3)
006cb45d  void* var_50
006cb45d  sub_44a760(esi, &var_50, arg3)
006cb462  void* eax_5 = var_50
006cb466  if (eax_5 != 0)
006cb474      eax_5.b = *(eax_5 + 8) == *(ebp + 0x28)
006cb4b7      if (eax_5.b != 0)
006cb4b7          void* eax_8 = *(*arg2 + 0x14)()
006cb4c1          int32_t eax_9 = sub_434e10(eax_8, 0x14)
006cb4c8          int32_t var_60 = eax_9
006cb4c6          if (eax_9 == 0)
006cb4ce              var_60 = 0x43d4f0
006cb4d6          if ((*data_8b9240 & 0x20000000) == 0)
006cb4ec              *data_8b9240 = 6
006cb4f6              sub_438160(0x8b9230, 0x8935b0)
006cb4fb              *data_8b9244 = 1
006cb505              *data_8b9258 = 0x893434
006cb50f          float var_68
006cb50f          if (var_69 == 0)
006cb515              if (*(esi + 0x64) != 0)
006cb538                  arg_10 = var_68
006cb53f                  sub_44a280(&arg_10, 4)
006cb523              else
006cb523                  sub_44a3a0(esi, &var_68, 4)
006cb544              if (*(esi + 0x64) != 0)
006cb567                  arg_10 = var_68
006cb56e                  sub_44a280(&arg_10, 4)
006cb552              else
006cb552                  sub_44a3a0(esi, &var_68, 4)
006cb573          if ((*data_8b9408 & 0x20000000) == 0)
006cb589              sub_438160(0x8b93f8, 0x8937b4)
006cb593              *data_8b940c = 8
006cb59d              sub_40c960(0x8b93f8)
006cb5b0          sub_496900(arg2, arg2 + 0x10, data_8b93f8, nullptr)
006cb5bb          void* eax_10 = arg2 + 0x18
006cb5b8          if (*(esi + 0x64) != 0)
006cb5d6              arg_10 = *eax_10
006cb5dd              sub_44a280(&arg_10, 4)
006cb5c5          else
006cb5c5              sub_44a3a0(esi, eax_10, 4)
006cb5fe          void var_44
006cb5fe          sub_40d9b0(&var_44, "mMinVal", 7)
006cb60f          int32_t var_4
006cb60f          var_4.b = 1
006cb617          var_54 = 1
006cb62b          void var_28
006cb62b          sub_40d8d0(&var_28, &var_44, nullptr, __gfids_table[0xbae7688].rvAddr+3)
006cb666          int32_t var_4_1
006cb666          var_4_1.b = 6
006cb66b          int32_t* eax_11
006cb66b          int32_t edx_7
006cb66b          eax_11, edx_7 = sub_436a80(arg3, &var_28)
006cb679          var_4_1.b = 0
006cb6a9          void* edi_3 = *(eax_11 + 4) + arg2
006cb6b2          uint8_t eax_13 = not.b((*(eax_8 + 0x10) u>> 1).b) & 1
006cb6b8          arg_10.b = eax_13
006cb6b4          if (var_69 != 0)
006cb6c1              arg_10.b = 0
006cb6ca          else if (eax_13 != 0)
006cb6ce              if (*(esi + 0x64) != 0)
006cb6eb                  var_54 = var_68
006cb6f2                  sub_44a280(&var_54, 4)
006cb6dc              else
006cb6dc                  sub_44a3a0(esi, &var_68, 4)
006cb6fc          sub_434d10(eax_8, edi_3)
006cb709          var_60(edi_3, eax_8, 0, esi)
006cb711          void* edi_4 = edi_3 + *(eax_8 + 0x14)
006cb717          if (arg_10.b != 0)
006cb720              if (*(esi + 0x64) != 0)
006cb740                  var_54 = var_68
006cb744                  sub_44a280(&var_54, 4)
006cb72e              else
006cb72e                  sub_44a3a0(esi, &var_68, 4)
006cb74e          sub_434d10(eax_8, edi_4)
006cb75b          var_60(edi_4, eax_8, 0, esi)
006cb779          sub_40d9b0(&var_28, "mSamples", 8)
006cb78a          var_4_1.b = 7
006cb792          var_54 = 4
006cb7a6          sub_40d8d0(&var_44, &var_28, nullptr, __gfids_table[0xbae7688].rvAddr+3)
006cb7e6          int32_t var_4_2
006cb7e6          var_4_2.b = 0xc
006cb7eb          int32_t* eax_16
006cb7eb          int32_t edx_11
006cb7eb          eax_16, edx_11 = sub_436a80(arg3, &var_44)
006cb7f9          var_4_2.b = 0
006cb820          void* edi_7 = *(eax_16 + 4) + arg2
006cb827          if (var_69 == 0)
006cb831              if (*(esi + 0x64) != 0)
006cb851                  var_54 = var_68
006cb855                  sub_44a280(&var_54, 4)
006cb83f              else
006cb83f                  sub_44a3a0(esi, &var_68, 4)
006cb85a          float* var_5c
006cb85a          if (*(esi + 0x64) != 0)
006cb87a              var_54 = var_5c
006cb87e              sub_44a280(&var_54, 4)
006cb868          else
006cb868              sub_44a3a0(esi, &var_5c, 4)
006cb883          float* eax_17 = var_5c
006cb887          if (eax_17 s> 0)
006cb892              *(*edi_7 + 0x20)(eax_17)
006cb8a1              void* eax_20 = *(*edi_7 + 0x10)(0)
006cb8a3              if (*(esi + 0x64) != 0)
006cb8bd                  var_54 = fconvert.s(fconvert.t(*eax_20))
006cb8c3                  sub_44a280(&var_54, 4)
006cb8af              else
006cb8af                  sub_44a3a0(esi, eax_20, 4)
006cb8d6              var_69 = *(eax_20 + 8) != 0 + 0x30
006cb8d3              if (*(esi + 0x64) != 0)
006cb8f1                  sub_44a280(&var_69, 1)
006cb8e3              else
006cb8e3                  sub_44a3a0(esi, &var_69, 1)
006cb8fb              void* eax_21 = eax_20 + 0xc
006cb901              *(eax_20 + 8) = var_69 == 0x31
006cb904              if (*(esi + 0x64) != 0)
006cb91e                  var_54 = *eax_21
006cb922                  sub_44a280(&var_54, 4)
006cb90e              else
006cb90e                  sub_44a3a0(esi, eax_21, 4)
006cb927              if (arg_10.b != 0)
006cb930                  if (*(esi + 0x64) != 0)
006cb950                      var_54 = var_68
006cb954                      sub_44a280(&var_54, 4)
006cb93e                  else
006cb93e                      sub_44a3a0(esi, &var_68, 4)
006cb961              sub_434d10(eax_8, eax_20 + 0x10)
006cb96e              var_60(eax_20 + 0x10, eax_8, 0, esi)
006cb97e              var_54 = eax_20
006cb982              int32_t var_58_1 = 1
006cb97a              if (var_5c s> 1)
006cb99e                  bool cond:28_1
006cb99e                  do
006cb99e                      float* eax_26 = *(*edi_7 + 0x10)(var_58_1)
006cb9a0                      float var_48
006cb9a0                      if (*(esi + 0x64) != 0)
006cb9ba                          var_48 = fconvert.s(fconvert.t(*eax_26))
006cb9c0                          sub_44a280(&var_48, 4)
006cb9ac                      else
006cb9ac                          sub_44a3a0(esi, eax_26, 4)
006cb9d3                      var_69 = *(eax_26 + 8) != 0 + 0x30
006cb9d0                      if (*(esi + 0x64) != 0)
006cb9ee                          sub_44a280(&var_69, 1)
006cb9e0                      else
006cb9e0                          sub_44a3a0(esi, &var_69, 1)
006cb9f8                      void* eax_27 = eax_26 + 0xc
006cb9fe                      *(eax_26 + 8) = var_69 == 0x31
006cba01                      if (*(esi + 0x64) != 0)
006cba1b                          var_48 = *eax_27
006cba1f                          sub_44a280(&var_48, 4)
006cba0b                      else
006cba0b                          sub_44a3a0(esi, eax_27, 4)
006cba24                      if (arg_10.b != 0)
006cba2d                          if (*(esi + 0x64) != 0)
006cba4d                              var_48 = var_68
006cba51                              sub_44a280(&var_48, 4)
006cba3b                          else
006cba3b                              sub_44a3a0(esi, &var_68, 4)
006cba5e                      sub_434d10(eax_8, eax_26 + 0x10)
006cba6b                      var_60(eax_26 + 0x10, eax_8, 0, esi)
006cba71                      float* ecx_53 = var_54
006cba7a                      var_48 = fconvert.s(fconvert.t(*eax_26) - fconvert.t(*ecx_53))
006cba7e                      long double x87_r7_5 = fconvert.t(var_48)
006cba82                      long double temp0_1 = fconvert.t(9.9999997473787516e-05)
006cba82                      x87_r7_5 - temp0_1
006cba8a                      long double x87_r7_6
006cba8a                      if (((x87_r7_5 < temp0_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r7_5, temp0_1) ? 1 : 0 << 0xa) | ((x87_r7_5 == temp0_1 ? 1 : 0 << 0xe) | 0x3800))):1.b & 0x41) != 0)
006cba97                          x87_r7_6 = float.t(0)
006cba8f                      else
006cba8f                          long double x87_r6_1 = float.t(1)
006cba91                          x87_r7_6 = x87_r6_1 / x87_r6_1
006cba9d                      *(ecx_53 + 4) = fconvert.s(x87_r7_6)
006cbaa3                      cond:28_1 = var_58_1 + 1 s< var_5c
006cbaa7                      var_54 = eax_26
006cbaab                      var_58_1 = var_58_1 + 1
006cbaa0                  while (cond:28_1)
006cbabb              *(var_54 + 4) = fconvert.s(float.t(1))
006cbacb          *fsbase = var_c
006cbad5          return 
006cb48d  sub_43d4f0(ebp, arg2, arg3, arg4, esi)
006cb49d  *fsbase = var_c




-----------------------------------------------------------------------------


void* sub_552820(int32_t arg1)

00552824  *(arg1 + 0x28) = 0x8a451c
0055282d  *data_8ec614 = 0
00552838  *data_8ec628 = 0x5d5f20
0055283e  *data_8ec610 = 0x7e5f88  {"mTime"}
00552848  *data_8ec61c = arg1
0055284d  *(arg1 + 0x1c) = 0x8ec610
00552854  *data_8ec608 = 0
00552860  *data_8ec60c = *data_8ec5f8
0055286b  *data_8ec5dc = 4
00552871  *data_8ec5bc = 4
0055287d  int32_t ecx_2 = *data_8ec5c0 | 0x21
00552880  *data_8ec5b4 = 0x7397b0
0055288a  *data_8ec59c = 0x7f0674  {"mbInterpolateToNextKey"}
00552894  *data_8ec5a0 = 8
0055289e  *data_8ec5a8 = arg1
005528a3  *data_8ec620 = 0x8ec59c
005528ad  *data_8ec5fc = 0x4f33e0
005528b7  *data_8ec5e4 = 0x7f0664  {"mTangentMode"}
005528c1  *data_8ec5e8 = 0xc
005528cb  *data_8ec5ec = 0x40
005528d5  *data_8ec5f0 = arg1
005528da  *data_8ec5ac = 0x8ec5e4
005528e4  *data_8ec600 = 0x7f0654  {"eTangentUnknown"}
005528ee  *data_8ec58c = 0x7f0644  {"eTangentStepped"}
005528f8  *data_8ec594 = 1
00552902  *data_8ec598 = 0x8ec600
0055290c  *data_8ec57c = 0x7f0634  {"eTangentKnot"}
00552916  *data_8ec584 = 2
00552920  *data_8ec588 = 0x8ec58c
0055292a  *data_8ec56c = 0x7f0624  {"eTangentSmooth"}
00552934  *data_8ec574 = 3
0055293e  *data_8ec578 = 0x8ec57c
00552948  *data_8ec5d4 = 0x7f0614  {"eTangentFlat"}
00552952  *data_8ec5e0 = 0x8ec56c
0055295c  *data_8ec5f8 = 0x8ec5d4
00552966  *data_8ec568 = 0x6f1d60
00552970  *data_8ec550 = 0x7e3f64  {"mValue"}
0055297a  *data_8ec554 = 0x10
00552984  *data_8ec55c = arg1
00552989  *data_8ec5f4 = 0x8ec550
00552993  *data_8ec5d0 = 0x5d5f20
00552999  *data_8ec5b8 = 0x7f05fc  {"mRecipTimeToNextSample"}
005529a3  *data_8ec5c4 = arg1
005529a8  *data_8ec560 = 0x8ec5b8
005529b2  *data_8ec5c0 = ecx_2
005529b8  return arg1

---------------------------------------------------------------------------------


int32_t* sub_537610(int32_t* arg1)

00537617  int32_t* fsbase
00537617  int32_t* eax = *fsbase
0053761d  int32_t* var_c = eax
0053761e  *fsbase = &var_c
0053762d  if (arg1 != 0)
00537639      eax = sub_6c5e00(arg1)
00537642  *fsbase = var_c
0053764c  return eax


-------------------------------------------------------------------------------------


int32_t* __fastcall sub_6c5e00(int32_t* arg1)

006c5e0d  int32_t* fsbase
006c5e0d  int32_t var_c = *fsbase
006c5e0e  *fsbase = &var_c
006c5e21  *arg1 = 0x7e3a94
006c5e27  sub_4966b0(arg1 + 8)
006c5e2e  *(arg1 + 0x10) = 0
006c5e38  *arg1 = 0x7ef8f8
006c5e3e  sub_5f03a0(arg1 + 0x18)
006c5e46  int32_t var_4
006c5e46  var_4.b = 1
006c5e4b  sub_5f03a0(arg1 + 0x40)
006c5e50  long double x87_r7 = float.t(0)
006c5e52  *(arg1 + 0x54) = fconvert.s(x87_r7)
006c5e55  int32_t ecx_3 = var_c
006c5e59  *(arg1 + 0x58) = fconvert.s(x87_r7)
006c5e5e  *(arg1 + 0x5c) = fconvert.s(x87_r7)
006c5e61  long double x87_r6 = float.t(1)
006c5e63  *(arg1 + 0x60) = fconvert.s(x87_r6)
006c5e68  *(arg1 + 0x64) = fconvert.s(x87_r7)
006c5e6b  *(arg1 + 0x68) = fconvert.s(x87_r7)
006c5e6e  *(arg1 + 0x6c) = fconvert.s(x87_r7)
006c5e71  *(arg1 + 0x70) = fconvert.s(x87_r7)
006c5e74  *(arg1 + 0x74) = fconvert.s(x87_r7)
006c5e77  *(arg1 + 0x78) = fconvert.s(x87_r7)
006c5e7c  *(arg1 + 0x7c) = fconvert.s(x87_r6)
006c5e81  *(arg1 + 0x80) = fconvert.s(x87_r7)
006c5e87  *(arg1 + 0x84) = fconvert.s(x87_r7)
006c5e8d  *(arg1 + 0x88) = fconvert.s(x87_r7)
006c5e93  *(arg1 + 0x8c) = fconvert.s(x87_r7)
006c5e99  *(arg1 + 0x90) = fconvert.s(x87_r7)
006c5e9f  *(arg1 + 0x94) = fconvert.s(x87_r7)
006c5ea7  *(arg1 + 0x98) = fconvert.s(x87_r6)
006c5eaf  *(arg1 + 0x9c) = fconvert.s(x87_r7)
006c5eb5  *(arg1 + 0xa0) = fconvert.s(x87_r7)
006c5ebb  *(arg1 + 0xa4) = fconvert.s(x87_r7)
006c5ec1  *(arg1 + 0xa8) = fconvert.s(x87_r7)
006c5ec7  *(arg1 + 0xac) = fconvert.s(x87_r7)
006c5ecd  *(arg1 + 0xb0) = fconvert.s(x87_r7)
006c5ed5  *(arg1 + 0xb4) = fconvert.s(x87_r6)
006c5edd  *(arg1 + 0xb8) = fconvert.s(x87_r7)
006c5ee3  *(arg1 + 0xbc) = fconvert.s(x87_r7)
006c5ee9  *(arg1 + 0xc0) = fconvert.s(x87_r7)
006c5eef  *(arg1 + 0xc4) = fconvert.s(x87_r7)
006c5ef5  *(arg1 + 0xc8) = fconvert.s(x87_r7)
006c5efb  *(arg1 + 0xcc) = fconvert.s(x87_r7)
006c5f03  *(arg1 + 0xd0) = fconvert.s(x87_r6)
006c5f09  *(arg1 + 0xd4) = fconvert.s(x87_r7)
006c5f0f  *(arg1 + 0xd8) = fconvert.s(x87_r7)
006c5f15  *(arg1 + 0xdc) = fconvert.s(x87_r7)
006c5f1b  *(arg1 + 0xf4) = 0
006c5f21  *(arg1 + 0xf5) = 0
006c5f29  *fsbase = ecx_3
006c5f33  return arg1

-------------------------------------------------------------------------------------

int32_t* __fastcall sub_4966b0(int32_t* arg1)

004966b2  *arg1 = 0
004966b8  *(arg1 + 4) = 0
004966bf  return arg1


-----------------------------------------------------------------------------------------

int32_t* __fastcall sub_5f03a0(int32_t* arg1)

005f03a4  *arg1 = 0
005f03a6  *(arg1 + 4) = 0
005f03a9  *(arg1 + 8) = 0
005f03ac  *(arg1 + 0xc) = 0
005f03af  return arg1


------------------------------------------------------------------------------------------------


int32_t __stdcall sub_6c6500(void* arg1 @ ecx, int32_t* arg2)

006c6504  int32_t* ebx = arg2
006c6509  void* ebp = *ebx
006c650f  arg2.b = 0
006c6514  if (*(ebp + 0x64) != 0)
006c6532      char var_9 = 0
006c6537      sub_44a280(&var_9, 1)
006c6524  else
006c6524      sub_44a3a0(ebp, &arg2, 1)
006c653c  char eax = arg2.b
006c6540  uint32_t var_20_1
006c6540  int32_t eax_1
006c6540  if (eax != 0xff)
006c6586      eax_1 = sub_5f13c0(ebx, zx.d(eax))
006c6590      var_20_1 = zx.d(arg2.b)
006c6547  else
006c6547      int32_t var_8 = 0
006c6544      if (*(ebp + 0x64) != 0)
006c6562          int32_t var_4 = 0
006c6566          sub_44a280(&var_4, 2)
006c6556      else
006c6556          sub_44a3a0(ebp, &var_8, 2)
006c6573      eax_1 = sub_5f13c0(ebx, zx.d(var_8.w))
006c657d      var_20_1 = zx.d(var_8.w)
006c6591  void** edi = arg1 + 0x40
006c6597  sub_5f04e0(edi, eax_1, var_20_1)
006c659f  void* ecx_8 = *edi
006c65a4  char* eax_5 = (*(arg1 + 0x44) + 7) u>> 3
006c65a7  if (*(ebp + 0x64) != 0)
006c65b8      sub_44a280(ecx_8, eax_5)
006c65b1  else
006c65b1      sub_44a3a0(ebp, ecx_8, eax_5)
006c65c1  sub_74aa80(arg1 + 0x18, ebx)
006c65d1  *(arg1 + 0x50) = (**edi).w & 0x3fff
006c65e1  return sub_6c6230(arg1)

--------------------------------------------------------


int32_t sub_434e60(int32_t arg1, int32_t arg2)

00434e60  int32_t edx_1 = *data_8bad18
00434e66  int32_t eax = edx_1
00434e69  int32_t ecx = 0
00434e6b  if (eax != 0)
00434e80      while (not(arg1 == *(eax + 8) && arg2 == *(eax + 0xc)))
00434e82          ecx = eax
00434e84          eax = *(eax + 0x24)
00434e89          if (eax == 0)
00434e89              break
00434e6b  if (eax == 0 || (eax != 0 && not(arg1 == *(eax + 8) && arg2 == *(eax + 0xc))))
00434e8b      eax = 0
00434e94  if (((eax != 0 && arg1 == *(eax + 8)) && arg2 == *(eax + 0xc)) && edx_1 != eax)
00434e94      if (ecx != 0)
00434e9b          *(ecx + 0x24) = *(eax + 0x24)
00434e9e          edx_1 = *data_8bad18
00434ea5      *(eax + 0x24) = edx_1
00434ea8      *data_8bad18 = eax
00434eae      return eax
00434e8f  return eax

-----------------------------------------------

int32_t* sub_53a950()

0053a95d  int32_t* fsbase
0053a95d  int32_t var_c = *fsbase
0053a95e  *fsbase = &var_c
0053a96e  int32_t* eax_1 = sub_42dc60(0x28)
0053a9c4  if (eax_1 == 0)
0053a9c4      *fsbase = var_c
0053a9ce      return 0
0053a988  *eax_1 = 0x7e3a94
0053a98e  sub_4966b0(eax_1 + 8)
0053a993  long double x87_r7 = float.t(0)
0053a995  *(eax_1 + 0x10) = 0
0053a99c  *eax_1 = 0x7ef2e4
0053a9a2  *(eax_1 + 0x18) = fconvert.s(x87_r7)
0053a9a5  *(eax_1 + 0x1c) = fconvert.s(x87_r7)
0053a9aa  *(eax_1 + 0x20) = fconvert.s(x87_r7)
0053a9b2  *fsbase = var_c
0053a9bc  return eax_1

---------------------------------------------------------------------------


int32_t* __stdcall sub_53b1c0(int32_t* arg1 @ ecx, void* arg2)

0053b1c6  *arg1 = 0x7e3a94
0053b1cf  *(arg1 + 8) = *(arg2 + 8)
0053b1d5  *(arg1 + 0xc) = *(arg2 + 0xc)
0053b1db  *(arg1 + 0x10) = *(arg2 + 0x10)
0053b1de  *arg1 = 0x7ef92c
0053b1e6  void* esi = arg2 + 0x18
0053b1e9  void* edi = arg1 + 0x18
0053b1f1  for (int32_t ecx_3 = 0xa; ecx_3 != 0; ecx_3 = ecx_3 - 1)
0053b1f1      *edi = *esi
0053b1f1      edi = edi + 4
0053b1f1      esi = esi + 4
0053b1f6  *(arg1 + 0x40) = *(arg2 + 0x40)
0053b1fc  *(arg1 + 0x44) = *(arg2 + 0x44)
0053b202  *(arg1 + 0x48) = *(arg2 + 0x48)
0053b208  *(arg1 + 0x4c) = *(arg2 + 0x4c)
0053b20e  *(arg1 + 0x50) = *(arg2 + 0x50)
0053b214  *(arg1 + 0x54) = *(arg2 + 0x54)
0053b21c  *(arg1 + 0x58) = fconvert.s(fconvert.t(*(arg2 + 0x58)))
0053b223  *(arg1 + 0x5c) = *(arg2 + 0x5c)
0053b22a  *(arg1 + 0x5d) = *(arg2 + 0x5d)
0053b231  *(arg1 + 0x5e) = *(arg2 + 0x5e)
0053b238  *(arg1 + 0x5f) = *(arg2 + 0x5f)
0053b23b  void* edx
0053b23b  edx.b = *(arg2 + 0x60)
0053b23e  *(arg1 + 0x60) = edx.b
0053b241  return arg1

------------------------------------------------------------------------------

void sub_53a000(int32_t* arg1)

0053a004  if (arg1 != 0)
0053a00a      **arg1(1)

----------------------------------------------------------------

int32_t* sub_537610(int32_t* arg1)

00537617  int32_t* fsbase
00537617  int32_t* eax = *fsbase
0053761d  int32_t* var_c = eax
0053761e  *fsbase = &var_c
0053762d  if (arg1 != 0)
00537639      eax = sub_6c5e00(arg1)
00537642  *fsbase = var_c
0053764c  return eax

---------------------------------------------------------------------

int32_t* __fastcall sub_6c5e00(int32_t* arg1)

006c5e0d  int32_t* fsbase
006c5e0d  int32_t var_c = *fsbase
006c5e0e  *fsbase = &var_c
006c5e21  *arg1 = 0x7e3a94
006c5e27  sub_4966b0(arg1 + 8)
006c5e2e  *(arg1 + 0x10) = 0
006c5e38  *arg1 = 0x7ef8f8
006c5e3e  sub_5f03a0(arg1 + 0x18)
006c5e46  int32_t var_4
006c5e46  var_4.b = 1
006c5e4b  sub_5f03a0(arg1 + 0x40)
006c5e50  long double x87_r7 = float.t(0)
006c5e52  *(arg1 + 0x54) = fconvert.s(x87_r7)
006c5e55  int32_t ecx_3 = var_c
006c5e59  *(arg1 + 0x58) = fconvert.s(x87_r7)
006c5e5e  *(arg1 + 0x5c) = fconvert.s(x87_r7)
006c5e61  long double x87_r6 = float.t(1)
006c5e63  *(arg1 + 0x60) = fconvert.s(x87_r6)
006c5e68  *(arg1 + 0x64) = fconvert.s(x87_r7)
006c5e6b  *(arg1 + 0x68) = fconvert.s(x87_r7)
006c5e6e  *(arg1 + 0x6c) = fconvert.s(x87_r7)
006c5e71  *(arg1 + 0x70) = fconvert.s(x87_r7)
006c5e74  *(arg1 + 0x74) = fconvert.s(x87_r7)
006c5e77  *(arg1 + 0x78) = fconvert.s(x87_r7)
006c5e7c  *(arg1 + 0x7c) = fconvert.s(x87_r6)
006c5e81  *(arg1 + 0x80) = fconvert.s(x87_r7)
006c5e87  *(arg1 + 0x84) = fconvert.s(x87_r7)
006c5e8d  *(arg1 + 0x88) = fconvert.s(x87_r7)
006c5e93  *(arg1 + 0x8c) = fconvert.s(x87_r7)
006c5e99  *(arg1 + 0x90) = fconvert.s(x87_r7)
006c5e9f  *(arg1 + 0x94) = fconvert.s(x87_r7)
006c5ea7  *(arg1 + 0x98) = fconvert.s(x87_r6)
006c5eaf  *(arg1 + 0x9c) = fconvert.s(x87_r7)
006c5eb5  *(arg1 + 0xa0) = fconvert.s(x87_r7)
006c5ebb  *(arg1 + 0xa4) = fconvert.s(x87_r7)
006c5ec1  *(arg1 + 0xa8) = fconvert.s(x87_r7)
006c5ec7  *(arg1 + 0xac) = fconvert.s(x87_r7)
006c5ecd  *(arg1 + 0xb0) = fconvert.s(x87_r7)
006c5ed5  *(arg1 + 0xb4) = fconvert.s(x87_r6)
006c5edd  *(arg1 + 0xb8) = fconvert.s(x87_r7)
006c5ee3  *(arg1 + 0xbc) = fconvert.s(x87_r7)
006c5ee9  *(arg1 + 0xc0) = fconvert.s(x87_r7)
006c5eef  *(arg1 + 0xc4) = fconvert.s(x87_r7)
006c5ef5  *(arg1 + 0xc8) = fconvert.s(x87_r7)
006c5efb  *(arg1 + 0xcc) = fconvert.s(x87_r7)
006c5f03  *(arg1 + 0xd0) = fconvert.s(x87_r6)
006c5f09  *(arg1 + 0xd4) = fconvert.s(x87_r7)
006c5f0f  *(arg1 + 0xd8) = fconvert.s(x87_r7)
006c5f15  *(arg1 + 0xdc) = fconvert.s(x87_r7)
006c5f1b  *(arg1 + 0xf4) = 0
006c5f21  *(arg1 + 0xf5) = 0
006c5f29  *fsbase = ecx_3
006c5f33  return arg1

-------------------------------------------------------------

int32_t* sub_53d700(int32_t* arg1, void* arg2)

0053d707  int32_t* fsbase
0053d707  int32_t* eax = *fsbase
0053d70d  int32_t* var_c = eax
0053d70e  *fsbase = &var_c
0053d71d  if (arg1 != 0)
0053d72e      eax = sub_53b030(arg1, arg2)
0053d737  *fsbase = var_c
0053d741  return eax

--------------------------------------------------------------

int32_t* __stdcall sub_53b030(int32_t* arg1 @ ecx, void* arg2)

0053b03d  int32_t* fsbase
0053b03d  int32_t var_c = *fsbase
0053b03e  *fsbase = &var_c
0053b04e  *arg1 = 0x7e3a94
0053b057  *(arg1 + 8) = *(arg2 + 8)
0053b05d  *(arg1 + 0xc) = *(arg2 + 0xc)
0053b069  *(arg1 + 0x10) = *(arg2 + 0x10)
0053b06c  void* esi = arg2 + 0x18
0053b06f  void* edi = arg1 + 0x18
0053b072  int32_t ecx_1 = 0xa
0053b077  *arg1 = 0x7ef8f8
0053b085  for (; ecx_1 != 0; ecx_1 = ecx_1 - 1)
0053b085      *edi = *esi
0053b085      edi = edi + 4
0053b085      esi = esi + 4
0053b08a  *(arg1 + 0x40) = *(arg2 + 0x40)
0053b090  *(arg1 + 0x44) = *(arg2 + 0x44)
0053b096  *(arg1 + 0x48) = *(arg2 + 0x48)
0053b09c  *(arg1 + 0x4c) = *(arg2 + 0x4c)
0053b09f  int32_t ecx_2
0053b09f  ecx_2.w = *(arg2 + 0x50)
0053b0b4  int32_t var_4
0053b0b4  var_4.b = 2
0053b0b9  *(arg1 + 0x50) = ecx_2.w
0053b0bd  sub_40a940(arg1 + 0x54, arg2 + 0x54, 0x1c, 4, 0x474580)
0053b0c8  *(arg1 + 0xc4) = fconvert.s(fconvert.t(*(arg2 + 0xc4)))
0053b0d6  *(arg1 + 0xc8) = fconvert.s(fconvert.t(*(arg2 + 0xc8)))
0053b0e2  *(arg1 + 0xcc) = fconvert.s(fconvert.t(*(arg2 + 0xcc)))
0053b0ee  *(arg1 + 0xd0) = fconvert.s(fconvert.t(*(arg2 + 0xd0)))
0053b0fa  *(arg1 + 0xd4) = fconvert.s(fconvert.t(*(arg2 + 0xd4)))
0053b106  *(arg1 + 0xd8) = fconvert.s(fconvert.t(*(arg2 + 0xd8)))
0053b112  *(arg1 + 0xdc) = fconvert.s(fconvert.t(*(arg2 + 0xdc)))
0053b11e  *(arg1 + 0xe0) = *(arg2 + 0xe0)
0053b12a  *(arg1 + 0xe4) = fconvert.s(fconvert.t(*(arg2 + 0xe4)))
0053b137  *(arg1 + 0xe8) = *(arg2 + 0xe8)
0053b145  *(arg1 + 0xea) = *(arg2 + 0xea)
0053b151  *(arg1 + 0xeb) = *(arg2 + 0xeb)
0053b15e  *(arg1 + 0xef) = *(arg2 + 0xef)
0053b16c  *(arg1 + 0xf1) = *(arg2 + 0xf1)
0053b179  *(arg1 + 0xf2) = *(arg2 + 0xf2)
0053b17f  int16_t edx_4
0053b17f  edx_4.b = *(arg2 + 0xf3)
0053b185  *(arg1 + 0xf3) = edx_4.b
0053b192  *(arg1 + 0xf4) = *(arg2 + 0xf4)
0053b19f  *(arg1 + 0xf5) = *(arg2 + 0xf5)
0053b1ad  *fsbase = var_c
0053b1b7  return arg1

--------------------------------------------------------

void __stdcall sub_40a940(int32_t arg1, int32_t arg2, int32_t arg3, int32_t arg4, int32_t arg5)

0040a945  int32_t ebp_1 = arg4 - 1
0040a945  if (arg4 - 1 s>= 0)
0040a950      int32_t esi_1 = arg1
0040a955      int32_t edi_1 = arg2
0040a963      int32_t temp2_1
0040a963      do
0040a963          arg5(edi_1)
0040a967          esi_1 = esi_1 + arg3
0040a969          edi_1 = edi_1 + arg3
0040a96b          temp2_1 = ebp_1
0040a96b          ebp_1 = ebp_1 - 1
0040a96b      while (temp2_1 - 1 s>= 0)

---------------------------------------------------------

int32_t sub_5d5c50(int32_t* arg1)

005d5c5c  return **arg1(0)


---------------------------------------------------------


int32_t sub_4972f0(int32_t* arg1 @ ebp, void* arg2, void* arg3, void* arg4, int32_t* arg5)

004972fd  int32_t* fsbase
004972fd  int32_t var_c = *fsbase
004972fe  *fsbase = &var_c
00497314  int32_t edi
00497314  int32_t var_d0 = edi
00497321  if (*(arg5 + 0x64) == 1)
00497326      *(arg2 + 0xc) = 4
0049733b  int32_t* var_d4 = arg5
0049733f  sub_43d4f0(arg1, arg2, arg5, arg2, arg3, arg4, var_d4)
0049733f  int16_t top = 0xffff
00497349  sub_44a6e0(arg5)
00497352  int32_t eax_1 = *(arg2 + 0x20)
00497355  int32_t var_b0 = eax_1
00497359  var_d4 = 4
0049734e  int32_t var_c0
0049734e  int32_t* esp_1
0049734e  if (*(arg5 + 0x64) != 0)
0049736b      var_c0 = eax_1
00497374      sub_44a280(&var_c0)
00497374      esp_1 = &var_d0
00497364  else
00497364      sub_44a3a0(arg5, &var_b0, var_d4)
00497364      esp_1 = &var_d0
00497379  if (*(arg5 + 0x64) != 1)
00497382      if ((*data_8b98d0 & 0x20000000) == 0)
0049738e          var_d4 = data_893304
00497398          sub_438160(0x8b98c0, 0x893304)
0049739d          var_d4 = data_8b98c0
004973a2          *data_8b98d4 = 0x18
004973ac          sub_40e610(data_8b98c0)
004973b4      void* eax_2 = *(arg2 + 0x24)
004973bb      var_d4 = arg1
004973bc      void* ebp = eax_2 + var_b0
004973bf      void* ebx_1 = nullptr
004973c1      void* var_a8
004973c1      if (ebp != eax_2)
004973cb          int32_t* edi_2 = *(arg2 + 0x28)
004973c9          if (ebp s> 0)
004973e6              ebx_1 = sub_42dc60(ebp << 2)
004973e8          void* ecx_5 = *(arg2 + 0x20)
004973eb          if (ecx_5 s>= ebp)
004973f5              ecx_5 = ebp
004973f7              var_a8 = ebp
004973ef          else
004973ef              var_a8 = ecx_5
004973fd          int32_t eax_5 = *(arg2 + 0x20)
00497400          var_c0 = eax_5
004973fb          if (ecx_5 s> 0)
00497408              void* eax_6 = ebx_1
0049740a              void* edx_2 = edi_2 - ebx_1
0049740c              void* var_b4_1 = ecx_5
00497410              void* temp0_1
00497410              do
00497410                  if (eax_6 != 0)
00497414                      *eax_6 = 0
0049741d                      *eax_6 = *(edx_2 + eax_6)
0049741f                  eax_6 = eax_6 + 4
00497422                  temp0_1 = var_b4_1
00497422                  var_b4_1 = var_b4_1 - 1
00497422              while (temp0_1 != 1)
00497429              eax_5 = var_c0
0049742d          if (eax_5 s> 0)
00497431              int32_t* edi_4 = edi_2
00497439              for (int32_t ecx_6 = eax_5; ecx_6 != 0; ecx_6 = ecx_6 - 1)
00497439                  *edi_4 = 0
00497439                  edi_4 = edi_4 + 4
0049743b              ecx_5 = var_a8
00497443          *(arg2 + 0x20) = ecx_5
0049744c          *(arg2 + 0x24) = ebp
0049744f          *(arg2 + 0x28) = ebx_1
00497452          sub_42dca0(edi_2)
00497457          ebx_1 = nullptr
0049745d      int32_t* edi_5 = arg5
00497459      if (*(arg2 + 0xc) s< 4)
00497867          int32_t var_bc = 0
00497864          if (*(edi_5 + 0x64) != 0)
00497882              int32_t var_a4 = 0
00497886              sub_44a280(&var_a4, 4)
00497886              esp_1 = &var_d4
00497874          else
00497874              sub_44a3a0(edi_5, &var_bc, 4)
00497874              esp_1 = &var_d4
0049788b          if (var_bc s> 0)
004978b0              do
004978b0                  *(esp_1 + 0x18) = *(esp_1 + 0x18) - 1
004978b5                  void* esp_39
004978b5                  if (*(arg2 + 0xc) != 0)
0049796b                      sub_4966b0(esp_1 + 0x5c)
00497970                      if ((*data_8b9408 & 0x20000000) == 0)
0049797c                          *(esp_1 - 4) = 0x8937b4
00497986                          sub_438160(0x8b93f8)
0049798b                          *(esp_1 - 4) = 0x8b93f8
00497990                          *data_8b940c = 8
0049799a                          sub_40c960()
0049799f                          esp_1 = esp_1
004979a2                      *(esp_1 - 4) = 0x14
004979a2                      esp_1 = esp_1 - 4
004979a9                      int32_t eax_38 = sub_434e10(0x8b93f8)
004979a9                      void* esp_45 = esp_1 + 4
004979ae                      if (eax_38 != 0)
004979b9                          *(esp_45 - 4) = *(esp_45 + 0xe4)
004979ba                          *(esp_45 - 8) = 0
004979c0                          *(esp_45 - 0xc) = 0x8b93f8
004979c5                          *(esp_45 - 0x10) = esp_45 + 0x5c
004979c6                          eax_38()
004979c6                          esp_45 = esp_45
004979ae                      if (eax_38 == 0)
004979da                          *(esp_45 - 4) = *(esp_45 + 0xe4)
004979db                          *(esp_45 - 8) = 0
004979e1                          *(esp_45 - 0xc) = 0x8b93f8
004979e6                          *(esp_45 - 0x10) = esp_45 + 0x5c
004979e6                          esp_45 = esp_45 - 0x10
004979e7                          sub_43d4f0(ebp, arg2, edi_5)
004979e7                          top = top + 0xffff
004979e7                          unimplemented  {call sub_43d4f0}
004979ae                      if (eax_38 != 0 || eax_38 == 0)
004979ec                          esp_45 = esp_45 + 0x10
004979f3                      int32_t eax_40 = *(esp_45 + 0x5c)
004979f7                      *(esp_45 - 4) = *(esp_45 + 0x60)
004979f8                      *(esp_45 - 8) = eax_40
004979f9                      void* eax_41 = sub_434e60()
004979fe                      edi_5 = *(esp_45 + 0xe4)
00497a05                      esp_39 = esp_45
00497a08                      ebp = eax_41
00497a0a                      *(esp_39 + 0x50) = eax_41
004978c4                  else
004978c4                      *(esp_1 + 0x90) = 0xf
004978cb                      *(esp_1 + 0x8c) = 0
004978d6                      *(esp_1 + 0x7c) = 0
004978db                      edi_5 = *(esp_1 + 0xe4)
004978e6                      *(esp_1 - 4) = esp_1 + 0x78
004978e9                      *(esp_1 + 0xd0) = 4
004978f4                      sub_44a810(edi_5)
004978fd                      *(esp_1 - 4) = esp_1 + 0x78
00497902                      sub_4967a0(esp_1 + 0x64)
0049790b                      int32_t eax_34 = *(esp_1 + 0x64)
0049790f                      *(esp_1 - 4) = *(esp_1 + 0x68)
00497910                      *(esp_1 - 8) = eax_34
00497916                      ebp = sub_434e60()
00497918                      int32_t eax_36 = *(esp_1 + 0x90)
0049791f                      esp_39 = esp_1
00497925                      *(esp_39 + 0x50) = ebp
00497929                      *(esp_39 + 0xd0) = 0xffffffff
00497922                      if (eax_36 u>= 0x10)
00497936                          int32_t ecx_49 = *(esp_39 + 0x7c)
0049793d                          *(esp_39 - 4) = eax_36 + 1
0049793e                          *(esp_39 - 8) = ecx_49
00497946                          sub_4a99b0()
00497946                          esp_39 = esp_39
0049794b                      *(esp_39 + 0x90) = 0xf
00497952                      *(esp_39 + 0x8c) = 0
0049795d                      *(esp_39 + 0x7c) = 0
00497a0e                  bool cond:14_1 = *(edi_5 + 0x64) != 0
00497a12                  *(esp_39 - 4) = 4
00497a12                  void* esp_55 = esp_39 - 4
00497a14                  if (cond:14_1)
00497a24                      int32_t edx_21 = *(esp_55 + 0x14)
00497a2c                      *(esp_55 - 4) = esp_55 + 0x34
00497a2f                      *(esp_55 + 0x34) = edx_21
00497a33                      sub_44a280()
00497a33                      esp_1 = esp_55 + 4
00497a1a                  else
00497a1a                      *(esp_55 - 4) = esp_55 + 0x14
00497a1d                      sub_44a3a0(edi_5)
00497a1d                      esp_1 = esp_55 + 4
00497a38                  if (*(esp_1 + 0x10) s> 0)
00497a43                      bool cond:25_1
00497a43                      do
00497a43                          *(esp_1 + 0x10) = *(esp_1 + 0x10) - 1
00497a4a                          int32_t eax_43 = sub_434cf0(ebp)
00497a4f                          *(esp_1 - 4) = 0x8b98c0
00497a54                          *(esp_1 - 8) = eax_43
00497a5c                          edi_5 = sub_434db0(ebp)
00497a60                          int32_t edx_23 = *(*edi_5 + 0x10)
00497a63                          *(esp_1 - 4) = 0
00497a69                          *(esp_1 - 8) = esp_1 + 0x40
00497a6c                          *(esp_1 + 0x58) = edi_5
00497a70                          edx_23()
00497a70                          esp_1 = esp_1
00497a72                          *(esp_1 + 0x38) = edi_5
00497a76                          int32_t eax_46 = *(arg2 + 0x24)
00497a79                          bool cond:21_1 = *(arg2 + 0x20) != eax_46
00497a7c                          *(esp_1 + 0xd0) = 5
00497a87                          if (not(cond:21_1))
00497a8d                              int32_t ecx_62 = eax_46
00497a8f                              if (ecx_62 s< 0xa)
00497a94                                  ecx_62 = 0xa
00497a99                              int32_t ebx_5 = ecx_62 + eax_46
00497a9e                              *(esp_1 + 0x14) = ebx_5
00497a9c                              if (ebx_5 != eax_46)
00497aab                                  int32_t* ebp_3 = nullptr
00497aaf                                  *(esp_1 + 0x20) = *(arg2 + 0x28)
00497aad                                  if (ebx_5 s> 0)
00497abc                                      *(esp_1 - 4) = ebx_5 << 2
00497ac2                                      esp_1 = esp_1
00497ac7                                      ebp_3 = sub_42dc60()
00497ac9                                  int32_t eax_50 = *(arg2 + 0x20)
00497ace                                  int32_t ebx_6 = eax_50
00497acc                                  if (eax_50 s>= ebx_5)
00497ad2                                      ebx_6 = *(esp_1 + 0x14)
00497ad8                                  int32_t ecx_64 = eax_50
00497ada                                  *(esp_1 + 0x30) = ecx_64
00497ad6                                  if (ebx_6 s> 0)
00497ae4                                      int32_t* eax_51 = ebp_3
00497ae6                                      int32_t* edx_25 = *(esp_1 + 0x20) - ebp_3
00497ae8                                      int32_t edi_10 = ebx_6
00497aea                                      ebx_6 = ebx_6
00497af0                                      int32_t temp1_1
00497af0                                      do
00497af0                                          if (eax_51 != 0)
00497af4                                              *eax_51 = 0
00497afd                                              *eax_51 = *(edx_25 + eax_51)
00497aff                                          eax_51 = eax_51 + 4
00497b02                                          temp1_1 = edi_10
00497b02                                          edi_10 = edi_10 - 1
00497b02                                      while (temp1_1 != 1)
00497b07                                      ecx_64 = *(esp_1 + 0x30)
00497b0b                                  if (ecx_64 s> 0)
00497b0f                                      int32_t* edi_11 = *(esp_1 + 0x20)
00497b15                                      for (; ecx_64 != 0; ecx_64 = ecx_64 - 1)
00497b15                                          *edi_11 = 0
00497b15                                          edi_11 = edi_11 + 4
00497b1b                                  int32_t edx_26 = *(esp_1 + 0x14)
00497b1f                                  *(esp_1 - 4) = *(esp_1 + 0x20)
00497b25                                  *(arg2 + 0x20) = ebx_6
00497b28                                  *(arg2 + 0x24) = edx_26
00497b2b                                  *(arg2 + 0x28) = ebp_3
00497b2e                                  sub_42dca0()
00497b2e                                  esp_1 = esp_1
00497b33                                  edi_5 = *(esp_1 + 0x58)
00497b37                                  ebp = *(esp_1 + 0x50)
00497b41                          void** eax_53 = *(arg2 + 0x28) + (*(arg2 + 0x20) << 2)
00497b44                          if (eax_53 != 0)
00497b48                              *eax_53 = edi_5
00497b4a                          *(arg2 + 0x20) = *(arg2 + 0x20) + 1
00497b4e                          cond:25_1 = *(esp_1 + 0x10) s> 0
00497b53                          *(esp_1 + 0xd0) = 0xffffffff
00497b53                      while (cond:25_1)
00497b64                  *(esp_1 + 0x10) = *(esp_1 + 0x10) - 1
00497b64              while (*(esp_1 + 0x18) s> 0)
0049746f      else
0049746f          var_a8 = nullptr
0049746c          if (*(edi_5 + 0x64) != 0)
0049748a              var_c0 = 0
0049748e              sub_44a280(&var_c0, 4)
0049747c          else
0049747c              sub_44a3a0(edi_5, &var_a8, 4)
00497496          int32_t var_c4 = 0
00497493          if (*(edi_5 + 0x64) != 0)
004974b1              var_c0 = 0
004974b5              sub_44a280(&var_c0, 4)
004974a5          else
004974a5              sub_44a3a0(edi_5, &var_c4, 4)
004974f0          int32_t var_4_1
004974f0          var_4_1.b = 1
004974f8          esp_1 = &var_d4
00497501          var_4_1.b = 2
004974fd          if (var_c4 s> 0)
00497514              do
00497514                  sub_4966b0(esp_1 + 0x38)
00497519                  if ((*data_8b9408 & 0x20000000) == 0)
00497525                      *(esp_1 - 4) = 0x8937b4
0049752f                      sub_438160(0x8b93f8)
00497534                      *(esp_1 - 4) = 0x8b93f8
00497539                      *data_8b940c = 8
00497543                      sub_40c960()
00497548                      esp_1 = esp_1
0049754b                  *(esp_1 - 4) = 0x14
0049754b                  esp_1 = esp_1 - 4
00497552                  int32_t eax_8 = sub_434e10(0x8b93f8)
00497552                  void* esp_5 = esp_1 + 4
00497557                  if (eax_8 != 0)
0049755b                      *(esp_5 - 4) = edi_5
0049755c                      *(esp_5 - 8) = 0
00497562                      *(esp_5 - 0xc) = 0x8b93f8
00497567                      *(esp_5 - 0x10) = esp_5 + 0x38
00497568                      eax_8()
00497568                      esp_5 = esp_5
00497557                  if (eax_8 == 0)
00497575                      *(esp_5 - 4) = edi_5
00497576                      *(esp_5 - 8) = 0
0049757c                      *(esp_5 - 0xc) = 0x8b93f8
00497581                      *(esp_5 - 0x10) = esp_5 + 0x38
00497581                      esp_5 = esp_5 - 0x10
00497582                      sub_43d4f0(ebp, arg2, edi_5)
00497582                      top = top + 0xffff
00497582                      unimplemented  {call sub_43d4f0}
00497557                  if (eax_8 != 0 || eax_8 == 0)
00497587                      esp_5 = esp_5 + 0x10
0049758e                  *(esp_5 - 4) = esp_5 + 0x38
00497597                  bool cond:16_1 = *(edi_5 + 0x64) != 0
0049759b                  ebp = sub_435660()
0049759d                  *(esp_5 - 4) = 2
0049759d                  void* esp_15 = esp_5 - 4
0049759f                  void* esp_17
0049759f                  if (cond:16_1)
004975af                      uint32_t eax_11 = zx.d(*(esp_15 + 0x2c))
004975b8                      *(esp_15 - 4) = esp_15 + 0x18
004975bb                      *(esp_15 + 0x18) = eax_11
004975bf                      sub_44a280()
004975bf                      esp_17 = esp_15 + 4
004975a5                  else
004975a5                      *(esp_15 - 4) = esp_15 + 0x2c
004975a8                      sub_44a3a0(edi_5)
004975a8                      esp_17 = esp_15 + 4
004975c4                  bool cond:22_1 = *(edi_5 + 0x64) != 0
004975c8                  *(esp_17 - 4) = 2
004975c8                  void* esp_19 = esp_17 - 4
004975ca                  if (cond:22_1)
004975da                      uint32_t eax_12 = zx.d(*(esp_19 + 0x1c))
004975e3                      *(esp_19 - 4) = esp_19 + 0x18
004975e6                      *(esp_19 + 0x18) = eax_12
004975ea                      sub_44a280()
004975ea                      esp_1 = esp_19 + 4
004975d0                  else
004975d0                      *(esp_19 - 4) = esp_19 + 0x1c
004975d3                      sub_44a3a0(edi_5)
004975d3                      esp_1 = esp_19 + 4
004975f8                  int32_t eax_13 = *(esp_1 + 0x6c)
004975fc                  *(esp_1 + 0x1c) = *(esp_1 + 0x1c) + (zx.d(*(esp_1 + 0x28)) * *(ebp + 0x14))
00497600                  *(eax_13 + (ebx_1 << 2)) = ebp
0049760f                  *(*(esp_1 + 0xa0) + (ebx_1 << 2)) = zx.d(*(esp_1 + 0x28))
0049761e                  *(*(esp_1 + 0x94) + (ebx_1 << 2)) = zx.d(*(esp_1 + 0x18))
00497621                  ebx_1 = ebx_1 + 1
00497621              while (ebx_1 s< *(esp_1 + 0x10))
0049762e              if (*(esp_1 + 0x1c) s> 0)
00497635                  int32_t eax_15 = *(esp_1 + 0x1c)
00497639                  *(esp_1 - 4) = eax_15
0049763f                  *(arg2 + 0x30) = eax_15
00497642                  esp_1 = esp_1
00497647                  *(arg2 + 0x2c) = sub_42dc60()
0049764a          int32_t ecx_26 = *(esp_1 + 0x2c)
0049764e          if (ecx_26 s> 0)
00497652              *(esp_1 - 4) = ecx_26
00497658              esp_1 = esp_1
0049765d              ecx_26 = *(esp_1 + 0x2c)
00497661              *(arg2 + 0x34) = sub_42dc60()
00497667          int32_t ebx_2 = *(arg2 + 0x2c)
0049766a          *(esp_1 + 0x44) = *(arg2 + 0x34)
00497670          bool cond:12_1 = *(esp_1 + 0x10) s<= 0
00497674          *(esp_1 + 0x40) = edi_5
00497678          *(esp_1 + 0x48) = ecx_26
0049767c          *(esp_1 + 0x4c) = 0
00497680          *(esp_1 + 0x20) = 0
00497684          if (not(cond:12_1))
0049768a              int32_t ebx_3 = ebx_2
00497690              bool cond:15_1
00497690              do
00497690                  int32_t eax_19 = *(esp_1 + 0x20)
0049769b                  int32_t edx_11 = *(esp_1 + 0xa0)
004976a2                  *(esp_1 + 0x64) = *(*(esp_1 + 0x6c) + (eax_19 << 2))
004976a6                  int32_t ecx_29 = *(edx_11 + (eax_19 << 2))
004976b2                  int32_t eax_20 = *(*(esp_1 + 0x94) + (eax_19 << 2))
004976b5                  *(esp_1 + 0x28) = ecx_29
004976b9                  *(esp_1 + 0x5c) = eax_20
004976a9                  if (ecx_29 s> 0)
004976d0                      bool cond:23_1
004976d0                      do
004976d0                          *(esp_1 + 0x28) = *(esp_1 + 0x28) - 1
004976de                          int32_t ebp_1 = *(esp_1 + 0x64)
004976e2                          *(esp_1 - 4) = ebx_3
004976e5                          int32_t edi_6 = ebx_3
004976e7                          sub_434d10(ebp_1)
004976ec                          ebx_3 = ebx_3 + *(ebp_1 + 0x14)
004976ef                          *(esp_1 - 4) = 0x8b98c0
004976f4                          *(esp_1 - 8) = edi_6
004976f7                          *(esp_1 + 0x30) = ebx_3
004976fb                          int32_t* eax_21 = sub_434db0(ebp_1)
00497709                          int32_t edx_14 = *(*eax_21 + 0x10)
0049770c                          *(esp_1 - 4) = *(esp_1 + 0x5c)
00497711                          *(esp_1 - 8) = esp_1 + 0x40
00497714                          edx_14()
00497714                          esp_1 = esp_1
00497716                          *(esp_1 + 0x38) = eax_21
0049771a                          int32_t eax_23 = *(arg2 + 0x24)
0049771d                          bool cond:20_1 = *(arg2 + 0x20) != eax_23
00497720                          *(esp_1 + 0xd0) = 3
00497728                          if (not(cond:20_1))
0049772e                              int32_t ecx_35 = eax_23
00497730                              if (ecx_35 s< 0xa)
00497735                                  ecx_35 = 0xa
0049773a                              ecx_35 = ecx_35 + eax_23
0049773e                              *(esp_1 + 0x1c) = ecx_35
0049773c                              if (ecx_35 != eax_23)
0049774b                                  int32_t edi_7 = ecx_35
0049774d                                  int32_t* ebx_4 = nullptr
00497751                                  *(esp_1 + 0x18) = *(arg2 + 0x28)
0049774f                                  if (edi_7 s> 0)
0049775e                                      *(esp_1 - 4) = edi_7 << 2
00497764                                      esp_1 = esp_1
00497769                                      ebx_4 = sub_42dc60()
0049776b                                  int32_t ecx_37 = *(arg2 + 0x20)
0049776e                                  if (ecx_37 s>= edi_7)
00497778                                      ecx_37 = edi_7
0049777a                                      *(esp_1 + 0x14) = edi_7
00497772                                  else
00497772                                      *(esp_1 + 0x14) = ecx_37
00497780                                  int32_t eax_27 = *(arg2 + 0x20)
00497783                                  *(esp_1 + 0x58) = eax_27
0049777e                                  if (ecx_37 s> 0)
0049778d                                      int32_t* eax_28 = ebx_4
0049778f                                      int32_t* edx_16 = *(esp_1 + 0x18) - ebx_4
00497791                                      *(esp_1 + 0x50) = ecx_37
00497795                                      int32_t temp2_1
00497795                                      do
00497795                                          if (eax_28 != 0)
00497799                                              *eax_28 = 0
004977a2                                              *eax_28 = *(edx_16 + eax_28)
004977a4                                          eax_28 = eax_28 + 4
004977a7                                          temp2_1 = *(esp_1 + 0x50)
004977a7                                          *(esp_1 + 0x50) = *(esp_1 + 0x50) - 1
004977a7                                      while (temp2_1 != 1)
004977ae                                      eax_27 = *(esp_1 + 0x58)
004977b2                                      edi_7 = *(esp_1 + 0x1c)
004977b6                                  if (eax_27 s> 0)
004977ba                                      int32_t* edi_9 = *(esp_1 + 0x18)
004977c2                                      for (int32_t ecx_38 = eax_27; ecx_38 != 0; ecx_38 = ecx_38 - 1)
004977c2                                          *edi_9 = 0
004977c2                                          edi_9 = edi_9 + 4
004977c4                                      edi_7 = *(esp_1 + 0x1c)
004977c8                                      ecx_37 = *(esp_1 + 0x14)
004977cc                                  *(arg2 + 0x20) = ecx_37
004977d3                                  *(esp_1 - 4) = *(esp_1 + 0x18)
004977d9                                  *(arg2 + 0x24) = edi_7
004977dc                                  *(arg2 + 0x28) = ebx_4
004977df                                  sub_42dca0()
004977df                                  esp_1 = esp_1
004977e4                                  ebx_3 = *(esp_1 + 0x30)
004977ee                          int32_t** eax_30 = *(arg2 + 0x28) + (*(arg2 + 0x20) << 2)
004977f1                          if (eax_30 != 0)
004977f5                              *eax_30 = eax_21
004977f7                          *(arg2 + 0x20) = *(arg2 + 0x20) + 1
004977fb                          cond:23_1 = *(esp_1 + 0x28) s> 0
00497800                          *(esp_1 + 0xd0) = 2
00497800                      while (cond:23_1)
00497815                  cond:15_1 = *(esp_1 + 0x20) + 1 s< *(esp_1 + 0x10)
00497819                  *(esp_1 + 0x20) = *(esp_1 + 0x20) + 1
00497812              while (cond:15_1)
0049782a          *(esp_1 + 0xd0) = 1
00497832          sub_560b90(esp_1 + 0x94)
0049783e          *(esp_1 + 0xd0) = 0
00497846          sub_560b90(esp_1 + 0xa0)
0049784f          *(esp_1 + 0xd0) = 0xffffffff
0049785a          sub_560b90(esp_1 + 0x6c)
00497b74      int32_t eax_54 = *(arg2 + 0xc)
00497b77      if (eax_54 s>= 2)
00497b80          if (eax_54 s>= 3)
00497b85              int32_t ebx_7 = 0
00497b87              if (*(esp_1 + 0x24) s> 0)
00497b93                  bool cond:18_1
00497b93                  do
00497b93                      void* edi_12 = *(*(arg2 + 0x28) + (ebx_7 << 2))
00497b99                      *(esp_1 + 0x38) = edi_12
00497b9d                      void* ecx_68 = *(esp_1 + 0xe4)
00497ba4                      bool cond:17_1 = *(ecx_68 + 0x64) != 0
00497ba8                      *(esp_1 + 0xd0) = 6
00497baf                      *(esp_1 + 0x14) = 0
00497bb7                      *(esp_1 - 4) = 4
00497bb7                      void* esp_65 = esp_1 - 4
00497bb9                      if (cond:17_1)
00497bcb                          *(esp_65 - 4) = esp_65 + 0x34
00497bcc                          *(esp_65 + 0x34) = 0
00497bd4                          sub_44a280()
00497bd4                          esp_1 = esp_65 + 4
00497bbf                      else
00497bbf                          *(esp_65 - 4) = esp_65 + 0x18
00497bc0                          sub_44a3a0(ecx_68)
00497bc0                          esp_1 = esp_65 + 4
00497bd9                      *(edi_12 + 0x10) = 0
00497be4                      *(edi_12 + 0x10) = *(edi_12 + 0x10) | *(esp_1 + 0x14)
00497be7                      ebx_7 = ebx_7 + 1
00497bea                      cond:18_1 = ebx_7 s< *(esp_1 + 0x24)
00497bee                      *(esp_1 + 0xd0) = 0xffffffff
00497bee                  while (cond:18_1)
00497bfb          void* ebp_4 = *(esp_1 + 0xe4)
00497c02          int32_t edi_13 = 0
00497c04          bool cond:10_1 = *(ebp_4 + 0x64) != 0
00497c07          *(esp_1 + 0x10) = 0
00497c0b          *(esp_1 - 4) = 2
00497c0b          void* esp_68 = esp_1 - 4
00497c0f          if (cond:10_1)
00497c21              *(esp_68 - 4) = esp_68 + 0x34
00497c22              *(esp_68 + 0x34) = 0
00497c26              sub_44a280()
00497c26              esp_1 = esp_68 + 4
00497c15          else
00497c15              *(esp_68 - 4) = esp_68 + 0x14
00497c16              sub_44a3a0(ebp_4)
00497c16              esp_1 = esp_68 + 4
00497c2b          if (*(esp_1 + 0x10) u<= 0)
00497d16              int32_t ebx_9 = 0
00497d18              if (*(esp_1 + 0x24) s> 0)
00497d26                  do
00497d26                      sub_4966b0(esp_1 + 0x38)
00497d2b                      if ((*data_8b9408 & 0x20000000) == 0)
00497d37                          *(esp_1 - 4) = 0x8937b4
00497d41                          sub_438160(0x8b93f8)
00497d46                          *(esp_1 - 4) = 0x8b93f8
00497d4b                          *data_8b940c = 8
00497d55                          sub_40c960()
00497d5a                          esp_1 = esp_1
00497d5d                      *(esp_1 - 4) = 0x14
00497d5d                      esp_1 = esp_1 - 4
00497d64                      int32_t eax_66 = sub_434e10(0x8b93f8)
00497d64                      void* esp_86 = esp_1 + 4
00497d69                      if (eax_66 != 0)
00497d6d                          *(esp_86 - 4) = ebp_4
00497d6e                          *(esp_86 - 8) = 0
00497d73                          *(esp_86 - 0xc) = 0x8b93f8
00497d78                          *(esp_86 - 0x10) = esp_86 + 0x38
00497d79                          eax_66()
00497d79                          esp_86 = esp_86
00497d69                      if (eax_66 == 0)
00497d86                          *(esp_86 - 4) = ebp_4
00497d87                          *(esp_86 - 8) = 0
00497d8c                          *(esp_86 - 0xc) = 0x8b93f8
00497d91                          *(esp_86 - 0x10) = esp_86 + 0x38
00497d91                          esp_86 = esp_86 - 0x10
00497d92                          sub_43d4f0(ebp_4, arg2, edi_13)
00497d92                          top = top + 0xffff
00497d92                          unimplemented  {call sub_43d4f0}
00497d69                      if (eax_66 != 0 || eax_66 == 0)
00497d97                          esp_86 = esp_86 + 0x10
00497d9d                      int32_t eax_68 = *(*(arg2 + 0x28) + (ebx_9 << 2))
00497da4                      *(esp_86 - 4) = esp_86 + 0x38
00497da8                      sub_4966e0(eax_68 + 8)
00497da8                      esp_1 = esp_86
00497dad                      ebx_9 = ebx_9 + 1
00497dad                  while (ebx_9 s< *(esp_1 + 0x24))
00497c3e          else
00497c3e              int32_t eax_60 = *(**(ebp_4 + 0xc) + 0x5c)()
00497c46              sub_5f03a0(esp_1 + 0x40)
00497c50              *(esp_1 - 4) = zx.d(*(esp_1 + 0x10))
00497c55              *(esp_1 + 0xd0) = 7
00497c60              sub_560bb0(esp_1 + 0x6c)
00497c6a              int32_t eax_61 = *(esp_1 + 0x6c)
00497c6e              *(esp_1 - 4) = zx.d(*(esp_1 + 0x10))
00497c6f              *(esp_1 - 8) = eax_61
00497c74              *(esp_1 + 0xd0) = 8
00497c7c              sub_5f04e0(esp_1 + 0x40)
00497c81              bool cond:13_1 = *(ebp_4 + 0x64) != 0
00497c89              *(esp_1 - 4) = zx.d(*(esp_1 + 0x10))
00497c89              void* esp_76 = esp_1 - 4
00497c8a              if (cond:13_1)
00497c9e                  *(esp_76 - 4) = *(esp_76 + 0x44)
00497ca1                  sub_44a280()
00497ca1                  esp_1 = esp_76 + 4
00497c90              else
00497c90                  *(esp_76 - 4) = *(esp_76 + 0x44)
00497c93                  sub_44a3a0(ebp_4)
00497c93                  esp_1 = esp_76 + 4
00497ca6              if (*(esp_1 + 0x24) s> 0)
00497cac                  esp_1 = esp_1
00497cb3                  do
00497cb3                      int32_t ebp_5 = *(*(arg2 + 0x28) + (edi_13 << 2))
00497cb9                      *(esp_1 + 0x38) = ebp_5
00497cc1                      *(esp_1 - 4) = esp_1 + 0x40
00497cc6                      *(esp_1 - 8) = esp_1 + 0x30
00497cc9                      *(esp_1 + 0xd0) = 9
00497cd1                      sub_5f0980(eax_60)
00497cda                      *(esp_1 - 4) = esp_1 + 0x30
00497cde                      sub_4966e0(ebp_5 + 8)
00497cde                      esp_1 = esp_1
00497ce3                      edi_13 = edi_13 + 1
00497ce3                  while (edi_13 s< *(esp_1 + 0x24))
00497cf0              *(esp_1 + 0xd0) = 7
00497cf8              sub_560b90(esp_1 + 0x6c)
00497d01              *(esp_1 + 0xd0) = 0xffffffff
00497d0c              sub_5f0830(esp_1 + 0x40)
00497dba      *esp_1
00497dba      esp_1 = esp_1 + 4
00497dbb  void* edi_14 = *(esp_1 + 0xe0)
00497dc4  sub_44a5c0(edi_14)
00497dcc  void* esi_2 = arg2 + 0x10
00497dcf  if ((*(arg2 + 0x10) | *(esi_2 + 4)) == 0)
00497ddb      *(esp_1 - 4) = esp_1 + 0xa8
00497de4      *(esp_1 - 4) = sub_4aaf20(edi_14 + 0x10)
00497de9      *(esp_1 + 0xcc) = 0xa
00497df4      sub_4967a0(esp_1 + 0x4c)
00497dfd      *(esp_1 - 4) = esp_1 + 0x4c
00497e00      sub_4966e0(esi_2)
00497e00      esp_1 = esp_1
00497e05      int32_t eax_72 = *(esp_1 + 0xc0)
00497e0f      *(esp_1 + 0xcc) = 0xffffffff
00497e0c      if (eax_72 u>= 0x10)
00497e1f          *(esp_1 - 4) = eax_72 + 1
00497e27          *(esp_1 - 8) = *(esp_1 + 0xac)
00497e2f          sub_4a99b0()
00497e2f          esp_1 = esp_1
00497e3b  *esp_1
00497e3c  *(esp_1 + 4)
00497e42  *(esp_1 + 8)
00497e43  *fsbase = *(esp_1 + 0xc4)
00497e50  return 1

--------------------------------------------------------

void* sub_76be70(char* arg1)

0076be70  char* ecx = arg1
0076be74  if ((ecx & 3) != 0)
0076be7c      do
0076be7c          int32_t eax
0076be7c          eax.b = *ecx
0076be7e          ecx = ecx + 1
0076bedc          if (eax.b == 0)
0076bedc              return (ecx + 0xffffffff) - arg1
0076bedc      while ((ecx & 3) != 0)
0076bea0  while (true)
0076bea0      int32_t eax_2 = *ecx
0076beae      ecx = ecx + 4
0076beb1      if ((((eax_2 ^ 0xffffffff) ^ (0x7efefeff + eax_2)) & 0x81010100) != 0)
0076beb8          int32_t eax_5 = *(ecx + 0xfffffffc)
0076befa          if (eax_5.b == 0)
0076befa              return (ecx + 0xfffffffc) - arg1
0076bef0          if (eax_5:1.b == 0)
0076bef0              return (ecx + 0xfffffffd) - arg1
0076bee6          if ((eax_5 & 0xff0000) == 0)
0076bee6              return (ecx + 0xfffffffe) - arg1
0076becf          if ((eax_5 & 0xff000000) == 0)
0076becf              break
0076bedc  return (ecx + 0xffffffff) - arg1


007ff290  float data_7ff290 = 0.00999999978 // 1
007ff294  float data_7ff294 = 0.0500000007  // 2
007ff298  float data_7ff298 = 0.100000001   // 3
007ff29c  float data_7ff29c = 0.5           // 4
007ff2a0  float data_7ff2a0 = 0.699999988   // 5
007ff2a4  float data_7ff2a4 = 0.800000012   // 6
007ff2a8  float data_7ff2a8 = 1             // 7
007ff2ac  float data_7ff2ac = 1.5           // 8
007ff2b0  float data_7ff2b0 = 2             // 9
007ff2b4  float data_7ff2b4 = 3             // 10
007ff2b8  float data_7ff2b8 = 4             // 11
007ff2bc  float data_7ff2bc = 5.5           // 12
007ff2c0  float data_7ff2c0 = 7             // 13
007ff2c4  float data_7ff2c4 = 8.5           // 14
007ff2c8  float data_7ff2c8 = 10            // 15


---------------------------------------------------------------

int32_t __fastcall sub_6c5f40(void* arg1)

006c5f47  void* edi = arg1
006c5f49  int32_t eax = *(edi + 0x48)
006c5f4c  void* esi = edi + 0x40
006c5f52  *(esi + 8) = eax + 1
006c5f66  uint32_t eax_1
006c5f66  eax_1.b = *((eax u>> 3) + *esi)
006c5f6b  int32_t ecx_3
006c5f6b  ecx_3.b = (eax_1.b & (1 << (eax.b & 7)).b) != 0
006c5f6e  void* var_c = edi
006c5f72  *(edi + 0xea) = ecx_3.b
006c5f78  int32_t var_4 = 0
006c5f8b  while (true)
006c5f8b      int32_t eax_2 = sub_6c6fe0(edi, var_4)
006c5f90      int32_t ecx_5 = *(esi + 8)
006c5f9d      int32_t ecx_6 = ecx_5 & 0x1f
006c5fa0      *(esi + 8) = ecx_5 + eax_2
006c5fa6      int32_t* edx_5 = ((ecx_5 u>> 3) & 0xfffffffc) + *esi
006c5faf      int32_t eax_4 = 0x20 - ecx_6
006c5fb1      int32_t var_8_1
006c5fb1      if (eax_4 u>= eax_2)
006c5fbb          var_8_1 = eax_2
006c5fbf          eax_4 = eax_2
006c5fb5      else
006c5fb5          var_8_1 = eax_4
006c5fc1      int32_t ebx_1
006c5fc1      if (eax_4 != 0x20)
006c5fd4          ebx_1 = (1 << eax_4.b) - 1
006c5fc6      else
006c5fc6          ebx_1 = 0xffffffff
006c5fdd      int32_t eax_7 = (*edx_5 u>> ecx_6.b) & ebx_1
006c5fe3      char edi_2 = eax_2.b - var_8_1.b
006c5fe3      if (eax_2 != var_8_1)
006c5ffa          eax_7 = eax_7 | ((((1 << edi_2) - 1) & *(edx_5 + 4)) << var_8_1.b)
006c6004      *(var_c + var_4 + 0xeb) = eax_7.b
006c600e      bool cond:0_1 = var_4 + 1 s< 7
006c6011      var_4 = var_4 + 1
006c6015      if (not(cond:0_1))
006c6015          break
006c5f82      edi = var_c
006c6028  void* edi_3 = ((*(*esi + 4) u>> 0xb) & 7) + 1                         // bits_per_sample?
006c602b  void* eax_13 = *(esi + 8)
006c6036  *(esi + 8) = eax_13 + edi_3
006c6039  void* eax_14 = eax_13 & 0x1f
006c603f  int32_t* edx_10 = ((eax_13 u>> 3) & 0xfffffffc) + *esi
006c6046  void* ecx_14 = 0x20 - eax_14
006c6048  void* var_4_1
006c6048  if (ecx_14 u>= edi_3)
006c6052      var_4_1 = edi_3
006c6056      ecx_14 = edi_3
006c604c  else
006c604c      var_4_1 = ecx_14
006c6058  int32_t ebp_6
006c6058  if (ecx_14 != 0x20)
006c6069      ebp_6 = (1 << ecx_14.b) - 1
006c605d  else
006c605d      ebp_6 = 0xffffffff
006c6070  void* eax_15 = var_4_1
006c6076  int32_t ebx_6 = (*edx_10 u>> eax_14.b) & ebp_6
006c6078  char edi_4 = edi_3.b - eax_15.b
006c6078  if (edi_3 != eax_15)
006c608f      ebx_6 = ebx_6 | ((((1 << edi_4) - 1) & *(edx_10 + 4)) << eax_15.b)
006c6091  void* ebp_12 = var_c
006c6095  bool cond:1 = *(ebp_12 + 0xeb) u> 0
006c609c  *(ebp_12 + 0xf2) = ebx_6.b
006c60a2  if (cond:1)
006c60e7      label_6c60e7:
006c60e7      void* eax_16 = *esi
006c60ef      int32_t ecx_20 = (*(eax_16 + 4) u>> 7) & 0xf                              // max_bounds_index
006c60f2      if (ecx_20 == 0xf)
006c60f9          long double st0_1
006c60f9          st0_1, eax_15 = sub_5f05a0(esi)
006c60fe          *(ebp_12 + 0xe4) = fconvert.s(st0_1)
006c6113      else
006c6113          int32_t ecx_22 = *(esi + 8)
006c6116          float var_8_2 = fconvert.s(fconvert.t(*((ecx_20 << 2) + 0x7ff290)))
006c611d          float eax_19 = (*(eax_16 + 4) u>> 3) & 0xf                            // bits_per_bounds
006c6123          *(esi + 8) = ecx_22 i+ eax_19
006c6128          int32_t ecx_23 = ecx_22 & 0x1f
006c6133          int32_t* edx_15 = ((ecx_22 u>> 3) & 0xfffffffc) + *esi
006c613a          float ecx_24 = 0x20 - ecx_23
006c613c          float var_4_2
006c613c          if (ecx_24 u>= eax_19)
006c6146              var_4_2 = eax_19
006c614a              ecx_24 = eax_19
006c6140          else
006c6140              var_4_2 = ecx_24
006c614c          int32_t ebp_13
006c614c          if (ecx_24 != 0x20)
006c615d              ebp_13 = (1 << ecx_24.b) - 1
006c6151          else
006c6151              ebp_13 = 0xffffffff
006c616c          int32_t edi_7 = (*edx_15 u>> ecx_23.b) & ebp_13
006c616e          float ecx_27 = eax_19 i- var_4_2
006c616e          if (eax_19 != var_4_2)
006c6183              edi_7 = edi_7 | ((((1 << ecx_27.b) - 1) & *(edx_15 + 4)) << var_4_2.b)
006c6194          eax_15 = var_c
006c6198          *(eax_15 + 0xe4) = fconvert.s(sub_6cb330(edi_7, eax_19, fconvert.s(fconvert.t(var_8_2))))
006c61a1          ebp_12 = eax_15
006c60ab  else
006c60ab      if (*(ebp_12 + 0xec) u> 0)
006c60ab          goto label_6c60e7
006c60b4      if (*(ebp_12 + 0xed) u> 0)
006c60b4          goto label_6c60e7
006c60bd      if (*(ebp_12 + 0xee) u> 0)
006c60bd          goto label_6c60e7
006c60c6      if (*(ebp_12 + 0xef) u> 0)
006c60c6          goto label_6c60e7
006c60cf      if (*(ebp_12 + 0xf0) u> 0)
006c60cf          goto label_6c60e7
006c60d8      if (*(ebp_12 + 0xf1) != 0)
006c60d8          goto label_6c60e7
006c60dc      *(ebp_12 + 0xe4) = fconvert.s(float.t(0))
006c61a5  if ((*(*esi + 6) & 1) != 0)                                       // tangent?
006c61ab      int32_t eax_20 = *(esi + 8)
006c61b1      *(esi + 8) = eax_20 + 2
006c61b9      eax_15 = eax_20 & 0x1f
006c61bf      int32_t* edx_20 = ((eax_20 u>> 3) & 0xfffffffc) + *esi
006c61c6      void* ecx_29 = 0x20 - eax_15
006c61c8      void* edi_8
006c61c8      if (ecx_29 u>= 2)
006c61d9          edi_8 = 2
006c61d0      else
006c61d0          edi_8 = ecx_29
006c61c8      int32_t esi_1
006c61c8      if (ecx_29 u>= 2 || (ecx_29 u< 2 && ecx_29 != 0x20))
006c61e7          esi_1 = (1 << edi_8.b) - 1
006c61c8      if (ecx_29 == 0x20)
006c61d4          esi_1 = 0xffffffff
006c61f5      int32_t ebx_11 = (*edx_20 u>> eax_15.b) & esi_1
006c61f7      char ecx_32 = 2 - edi_8.b
006c61f7      if (2 != edi_8)
006c620a          eax_15 = (((1 << ecx_32) - 1) & *(edx_20 + 4)) << edi_8.b
006c620c          ebx_11 = ebx_11 | eax_15
006c620e      *(ebp_12 + 0xf3) = ebx_11.b
006c6216  *(ebp_12 + 0xf4) = 0
006c6222  return eax_15

-----------------------------------------------------------------------------------------------------

0x6C6189 push ecx - ECX=0
0x6C618D push eax - EAX=10 // arg2
0x6C618E push edi - EDI=926 // arg1

long double sub_6cb330(int32_t arg1, float arg2, float arg3)

006cb33f  float eax_1 = (1 << arg2.b) - 1                               // 2**arg2 - 1
006cb34a  if (arg1 == eax_1)                                            
006cb34a      return fconvert.t(arg3)
006cb351  if (arg1 == 0)
006cb351      return float.t(0)
006cb354  float edx_1 = eax_1 & arg1
006cb35c  long double x87_r7_2 = float.t(edx_1)
006cb356  if (edx_1 s< 0)
006cb362      x87_r7_2 = x87_r7_2 + fconvert.t(4.2949673e+09f)
006cb36a  long double x87_r6 = fconvert.t(arg3)
006cb374  long double x87_r7_3 = x87_r7_2 * x87_r6
006cb376  long double x87_r5_1 = float.t(eax_1)
006cb368  if (eax_1 s< 0)
006cb37c      x87_r5_1 = x87_r5_1 + fconvert.t(4.2949673e+09f)
006cb384  long double x87_r7_5 = x87_r6
006cb38a  long double x87_r6_2 = float.t(0)
006cb38c  long double x87_r5_2 = fconvert.t(fconvert.s(x87_r7_3 / x87_r5_1))
006cb390  x87_r5_2 - x87_r6_2
006cb392  eax_1.w = (x87_r5_2 < x87_r6_2 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r5_2, x87_r6_2) ? 1 : 0 << 0xa) | ((x87_r5_2 == x87_r6_2 ? 1 : 0 << 0xe) | 0x2800)))
006cb394  if ((eax_1:1.b & 0x41) != 0)
006cb39d      x87_r6_2 = x87_r5_2
006cb399  else
006cb399      x87_r7_5 = x87_r5_2
006cb3a3  long double x87_r6_3 = fconvert.t(fconvert.s(x87_r6_2))
006cb3a7  x87_r6_3 - x87_r7_5
006cb3a9  eax_1.w = (x87_r6_3 < x87_r7_5 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_3, x87_r7_5) ? 1 : 0 << 0xa) | ((x87_r6_3 == x87_r7_5 ? 1 : 0 << 0xe) | 0x3000)))
006cb3ab  eax_1:1.b & 5
006cb3ab  bool p = unimplemented  {test ah, 0x5}
006cb3c5  if (p)
006cb3c5      return fconvert.t(fconvert.s(x87_r6_3))
006cb3ba  return fconvert.t(fconvert.s(x87_r7_5))

---------------------------------------------------------------------

// arg1 = CompressedKey struct
// arg1 + 0x40 is header buffer
int32_t __stdcall sub_6c6fe0(int32_t arg1 @ ecx, int32_t arg2)

006c6ff1  int32_t* edx_3 = (((arg2 + (arg2 << 1) + 0xe) u>> 3) & 0xfffffffc) + *(arg1 + 0x40)
006c6ff5  int32_t eax_1 = (arg2 + (arg2 << 1) + 0xe) & 0x1f
006c7000  int32_t ecx = 0x20 - eax_1
006c7002  int32_t ebx
006c7002  if (ecx u>= 3)
006c7013      ebx = 3
006c700a  else
006c700a      ebx = ecx
006c7002  int32_t esi
006c7002  if (ecx u>= 3 || (ecx u< 3 && ecx != 0x20))
006c7021      esi = (1 << ebx.b) - 1
006c7002  if (ecx == 0x20)
006c700e      esi = 0xffffffff
006c702f  int32_t eax_4 = (*edx_3 u>> eax_1.b) & esi                            // reader 3 bits buffer
006c7031  char ecx_3 = 3 - ebx.b
006c7031  if (3 != ebx)
006c7046      eax_4 = eax_4 | ((((1 << ecx_3) - 1) & *(edx_3 + 4)) << ebx.b)
006c704b  return eax_4


---------------------------------------------------------


float* __stdcall sub_6c65f0(void* arg1 @ ecx, float* arg2, float* arg3, float arg4)

006c65fa  uint32_t ebx = zx.d(*(arg1 + 0x50))
006c660f  if (ebx == 0)
006c660f      *arg2 = fconvert.s(fconvert.t(*data_895184))                          // 0
006c6619      *(arg2 + 4) = fconvert.s(fconvert.t(*data_895188))                    // 0
006c6622      *(arg2 + 8) = fconvert.s(fconvert.t(*data_89518c))                    // 0
006c662b      *(arg2 + 0xc) = fconvert.s(fconvert.t(*data_895190))                  // 1
006c6634      *(arg2 + 0x10) = fconvert.s(fconvert.t(*data_895194))                 // 0
006c663d      *(arg2 + 0x14) = fconvert.s(fconvert.t(*data_895198))                 // 0
006c6646      *(arg2 + 0x18) = fconvert.s(fconvert.t(*data_89519c))                 // 0
006c6650      long double x87_r7_7 = float.t(0)
006c6652      *arg3 = fconvert.s(x87_r7_7)
006c6654      *(arg3 + 4) = fconvert.s(x87_r7_7)
006c665d      return arg3
006c6660  long double x87_r7_8 = float.t(0)
006c6664  float var_78 = fconvert.s(x87_r7_8)
006c6672  float var_7c = fconvert.s(x87_r7_8)
006c6691  int32_t eax_2
006c6691  int80_t st0
006c6691  st0, eax_2 = sub_74a990(arg1 + 0x18, fconvert.s(fconvert.t(arg4)), ebx, &var_78, &var_7c)
006c669d  int32_t edi = eax_2
006c669f  int32_t ebp = edi + 2
006c66a4  int32_t var_ec = edi
006c66a2  if (ebp s< sx.d(*(arg1 + 0xe8)))
006c66ac      sub_6c6230(arg1)
006c66b8  float var_dc
006c66b8  float var_b4
006c66b8  float var_98
006c66b8  if (sx.d(*(arg1 + 0xe8)) s< ebp)
006c66c0      do
006c66c0          int32_t eax_3
006c66c0          eax_3.b = *(arg1 + 0xf5)
006c66c6          eax_3.b = eax_3.b + 1
006c66c8          eax_3.b = eax_3.b & 3
006c66ca          *(arg1 + 0xf5) = eax_3.b
006c66d0          int32_t ecx_2 = sx.d(eax_3.b)
006c66e5          void* eax_7 = arg1 + ((((((ecx_2 + 0xffffffff) & 3) + 3) << 3) - (((ecx_2 + 0xffffffff) & 3) + 3)) << 2)
006c66f6          float* ecx_4 = arg1 + ((((ecx_2 + 3) << 3) - (ecx_2 + 3)) << 2)
006c66f9          *ecx_4 = fconvert.s(fconvert.t(*eax_7))
006c66fe          *(ecx_4 + 4) = fconvert.s(fconvert.t(*(eax_7 + 4)))
006c6704          *(ecx_4 + 8) = fconvert.s(fconvert.t(*(eax_7 + 8)))
006c670a          *(ecx_4 + 0xc) = fconvert.s(fconvert.t(*(eax_7 + 0xc)))
006c6710          *(ecx_4 + 0x10) = fconvert.s(fconvert.t(*(eax_7 + 0x10)))
006c6716          *(ecx_4 + 0x14) = fconvert.s(fconvert.t(*(eax_7 + 0x14)))
006c671c          *(ecx_4 + 0x18) = fconvert.s(fconvert.t(*(eax_7 + 0x18)))
006c671f          *(arg1 + 0xe8) = *(arg1 + 0xe8) + 1
006c6727          int16_t eax_8 = *(arg1 + 0xe8)
006c672e          *(arg1 + 0xe0) = *(arg1 + 0xe0) << 2
006c6735          *(arg1 + 0xf4) = *(arg1 + 0xf4) + 1
006c673c          int32_t ecx_5 = *(arg1 + 0xe0)
006c6742          int32_t edx_4
006c6742          edx_4.b = *(arg1 + 0xf4)
006c674b          if (sx.d(eax_8) s>= ebx)
006c6b6c              *(arg1 + 0xe0) = zx.d(*(arg1 + 0xf3)) | ecx_5
006c6753          else
006c6753              if (edx_4.b u>= *(arg1 + 0xf2))
006c675d                  sub_6c5f40(arg1)
006c677b              var_98 = 0f
006c67a3              int32_t eax_10
006c67a3              int32_t ecx_8
006c67a3              eax_10, ecx_8 = sub_5f06a0(arg1 + 0x40, *(arg1 + 0x48), &var_98, arg1 + 0xeb, 7)
006c67a9              *(arg1 + 0x48) = eax_10
006c67c2              sub_6c6460(&var_dc, &var_98, arg1 + 0xeb, fconvert.s(fconvert.t(*(arg1 + 0xe4))))
006c67c7              float var_d8
006c67c7              float var_d4
006c67c7              float var_d0
006c67c7              float var_cc
006c67c7              float var_c8
006c67c7              float var_c4
006c67c7              if (*(arg1 + 0xea) == 0)
006c6846                  *(arg1 + 0xc4) = fconvert.s(fconvert.t(var_dc))
006c6850                  *(arg1 + 0xc8) = fconvert.s(fconvert.t(var_d8))
006c685a                  *(arg1 + 0xcc) = fconvert.s(fconvert.t(var_d4))
006c6864                  *(arg1 + 0xd0) = fconvert.s(fconvert.t(var_d0))
006c686e                  *(arg1 + 0xd4) = fconvert.s(fconvert.t(var_cc))
006c6878                  *(arg1 + 0xd8) = fconvert.s(fconvert.t(var_c8))
006c6882                  *(arg1 + 0xdc) = fconvert.s(fconvert.t(var_c4))
006c67da              else
006c67da                  *(arg1 + 0xd4) = fconvert.s(fconvert.t(var_cc) + fconvert.t(*(arg1 + 0xd4)))
006c67ea                  *(arg1 + 0xd8) = fconvert.s(fconvert.t(var_c8) + fconvert.t(*(arg1 + 0xd8)))
006c67fa                  *(arg1 + 0xdc) = fconvert.s(fconvert.t(*(arg1 + 0xdc)) + fconvert.t(var_c4))
006c680a                  *(arg1 + 0xc4) = fconvert.s(fconvert.t(*(arg1 + 0xc4)) + fconvert.t(var_dc))
006c681a                  *(arg1 + 0xc8) = fconvert.s(fconvert.t(*(arg1 + 0xc8)) + fconvert.t(var_d8))
006c682a                  *(arg1 + 0xcc) = fconvert.s(fconvert.t(*(arg1 + 0xcc)) + fconvert.t(var_d4))
006c683a                  *(arg1 + 0xd0) = fconvert.s(fconvert.t(var_d0) + fconvert.t(*(arg1 + 0xd0)))
006c689b              void* eax_13 = arg1 + ((((sx.d(*(arg1 + 0xf5)) + 3) << 3) - (sx.d(*(arg1 + 0xf5)) + 3)) << 2)
006c68a7              *(eax_13 + 0x10) = fconvert.s(fconvert.t(*(eax_13 + 0x10)) + fconvert.t(*(arg1 + 0xd4)))
006c68b3              *(eax_13 + 0x14) = fconvert.s(fconvert.t(*(arg1 + 0xd8)) + fconvert.t(*(eax_13 + 0x14)))
006c68bf              *(eax_13 + 0x18) = fconvert.s(fconvert.t(*(arg1 + 0xdc)) + fconvert.t(*(eax_13 + 0x18)))
006c68ca              *eax_13 = fconvert.s(fconvert.t(*(arg1 + 0xc4)) + fconvert.t(*eax_13))
006c68d5              *(eax_13 + 4) = fconvert.s(fconvert.t(*(arg1 + 0xc8)) + fconvert.t(*(eax_13 + 4)))
006c68e1              *(eax_13 + 8) = fconvert.s(fconvert.t(*(arg1 + 0xcc)) + fconvert.t(*(eax_13 + 8)))
006c68ed              *(eax_13 + 0xc) = fconvert.s(fconvert.t(*(arg1 + 0xd0)) + fconvert.t(*(eax_13 + 0xc)))
006c6906              sub_4ac6f0(arg1 + ((((sx.d(*(arg1 + 0xf5)) + 3) << 3) - (sx.d(*(arg1 + 0xf5)) + 3)) << 2))
006c6912              *(arg1 + 0xe0) = *(arg1 + 0xe0) | zx.d(*(arg1 + 0xf3))
006c6918              if (*(arg1 + 0xe8) == 0)
006c6939                  void* eax_18 = arg1 + ((((sx.d(*(arg1 + 0xf5)) + 3) << 3) - (sx.d(*(arg1 + 0xf5)) + 3)) << 2)
006c693e                  var_b4 = fconvert.s(fconvert.t(*eax_18))
006c6945                  float var_b0_1 = fconvert.s(fconvert.t(*(eax_18 + 4)))
006c694c                  float var_ac_1 = fconvert.s(fconvert.t(*(eax_18 + 8)))
006c6953                  float var_a8_1 = fconvert.s(fconvert.t(*(eax_18 + 0xc)))
006c695a                  float var_a4_1 = fconvert.s(fconvert.t(*(eax_18 + 0x10)))
006c6961                  float var_a0_1 = fconvert.s(fconvert.t(*(eax_18 + 0x14)))
006c6968                  float var_9c_1 = fconvert.s(fconvert.t(*(eax_18 + 0x18)))
006c696c                  long double x87_r7_60 = fconvert.t(var_b4)
006c6970                  *(arg1 + 0x54) = fconvert.s(x87_r7_60)
006c6973                  long double x87_r6_1 = fconvert.t(var_b0_1)
006c6977                  *(arg1 + 0x58) = fconvert.s(x87_r6_1)
006c697a                  long double x87_r5_1 = fconvert.t(var_ac_1)
006c697e                  *(arg1 + 0x5c) = fconvert.s(x87_r5_1)
006c6981                  long double x87_r4_1 = fconvert.t(var_a8_1)
006c6985                  *(arg1 + 0x60) = fconvert.s(x87_r4_1)
006c6988                  long double x87_r3_1 = fconvert.t(var_a4_1)
006c698c                  *(arg1 + 0x64) = fconvert.s(x87_r3_1)
006c698f                  long double x87_r2_1 = fconvert.t(var_a0_1)
006c6993                  *(arg1 + 0x68) = fconvert.s(x87_r2_1)
006c6996                  long double x87_r1_1 = fconvert.t(var_9c_1)
006c699a                  *(arg1 + 0x6c) = fconvert.s(x87_r1_1)
006c699d                  int32_t edx_9 = *(arg1 + 0xe0)
006c69ac                  int32_t edx_10 = edx_9 + edx_9
006c69b2                  *(arg1 + 0xe0) = (edx_10 + edx_10) | zx.d(*(arg1 + 0xf3))
006c69b8                  *(arg1 + 0x70) = fconvert.s(x87_r7_60)
006c69bd                  *(arg1 + 0x74) = fconvert.s(x87_r6_1)
006c69c2                  *(arg1 + 0x78) = fconvert.s(x87_r5_1)
006c69c7                  *(arg1 + 0x7c) = fconvert.s(x87_r4_1)
006c69cc                  *(arg1 + 0x80) = fconvert.s(x87_r3_1)
006c69d4                  *(arg1 + 0x84) = fconvert.s(x87_r2_1)
006c69dc                  *(arg1 + 0x88) = fconvert.s(x87_r1_1)
006c69e2                  int32_t ecx_15 = *(arg1 + 0xe0)
006c69f1                  int32_t ecx_16 = ecx_15 + ecx_15
006c69f7                  *(arg1 + 0xe0) = (ecx_16 + ecx_16) | zx.d(*(arg1 + 0xf3))
006c69fd                  *(arg1 + 0x8c) = fconvert.s(x87_r7_60)
006c6a05                  *(arg1 + 0x90) = fconvert.s(x87_r6_1)
006c6a0d                  *(arg1 + 0x94) = fconvert.s(x87_r5_1)
006c6a15                  *(arg1 + 0x98) = fconvert.s(x87_r4_1)
006c6a1d                  *(arg1 + 0x9c) = fconvert.s(x87_r3_1)
006c6a25                  *(arg1 + 0xa0) = fconvert.s(x87_r2_1)
006c6a2d                  *(arg1 + 0xa4) = fconvert.s(x87_r1_1)
006c6a33                  int32_t eax_20 = *(arg1 + 0xe0)
006c6a42                  int32_t eax_21 = eax_20 + eax_20
006c6a48                  *(arg1 + 0xe0) = (eax_21 + eax_21) | zx.d(*(arg1 + 0xf3))
006c6a4e                  *(arg1 + 0xa8) = fconvert.s(x87_r7_60)
006c6a56                  *(arg1 + 0xac) = fconvert.s(x87_r6_1)
006c6a5c                  *(arg1 + 0xb0) = fconvert.s(x87_r5_1)
006c6a62                  *(arg1 + 0xb4) = fconvert.s(x87_r4_1)
006c6a6a                  *(arg1 + 0xb8) = fconvert.s(x87_r3_1)
006c6a70                  *(arg1 + 0xbc) = fconvert.s(x87_r2_1)
006c6a76                  *(arg1 + 0xc0) = fconvert.s(x87_r1_1)
006c6a7c                  int32_t edx_14 = *(arg1 + 0xe0)
006c6a89                  int32_t edx_15 = edx_14 + edx_14
006c6a8f                  *(arg1 + 0xe0) = (edx_15 + edx_15) | zx.d(*(arg1 + 0xf3))
006c6a95              edi = var_ec
006c6a95      while (sx.d(*(arg1 + 0xe8)) s< edi + 2)
006c6aab  int32_t ecx_21 = sx.d(*(arg1 + 0xe8))
006c6ac0  long double x87_r6_5 = fconvert.t(var_78)
006c6acf  int32_t eax_27 = (sx.d(*(arg1 + 0xf5)) - ecx_21) + edi
006c6adb  long double x87_r6_7 = float.t(0)
006c6ae0  long double x87_r5_7 = fconvert.t(fconvert.s(fconvert.t(arg4) - x87_r6_5))
006c6ae4  int32_t ecx_22 = ecx_21 - edi
006c6ae6  x87_r5_7 - x87_r6_7
006c6af9  void* ebx_1 = arg1 + ((((((eax_27 + 0xffffffff) & 3) + 3) << 3) - (((eax_27 + 0xffffffff) & 3) + 3)) << 2)
006c6b2c  void* edi_6 = arg1 + (((((eax_27 & 3) + 3) << 3) - ((eax_27 & 3) + 3)) << 2)
006c6b2f  void* ebp_4 = arg1 + ((((((eax_27 + 1) & 3) + 3) << 3) - (((eax_27 + 1) & 3) + 3)) << 2)
006c6b32  int32_t esi_1 = *(arg1 + 0xe0)
006c6b3a  int32_t ecx_23 = ecx_22 + ecx_22
006c6b43  void* var_74 = arg1 + ((((((eax_27 + 0xfffffffe) & 3) + 3) << 3) - (((eax_27 + 0xfffffffe) & 3) + 3)) << 2)
006c6b4d  void* eax_31
006c6b4d  eax_31.w = (x87_r5_7 < x87_r6_7 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r5_7, x87_r6_7) ? 1 : 0 << 0xa) | ((x87_r5_7 == x87_r6_7 ? 1 : 0 << 0xe) | 0x2800)))
006c6b4f  int32_t edx_32 = ((esi_1 u>> ecx_23.b) & 3) + 1
006c6b55  int32_t esi_4 = ((esi_1 u>> ecx_23 + 0xfffffffe.b) & 3) + 1
006c6b58  float var_ec_2
006c6b58  if ((eax_31:1.b & 0x41) != 0)
006c6b77      x87_r6_7 = x87_r5_7
006c6b79      var_ec_2 = fconvert.s(x87_r6_7)
006c6b5d  else
006c6b5d      var_ec_2 = fconvert.s(x87_r5_7)
006c6b7d  long double x87_r5_8 = fconvert.t(var_7c)
006c6b88  x87_r6_5 - x87_r5_8
006c6b8a  eax_31.w = (x87_r6_5 < x87_r5_8 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_5, x87_r5_8) ? 1 : 0 << 0xa) | ((x87_r6_5 == x87_r5_8 ? 1 : 0 << 0xe) | 0x2800)))
006c6b8e  eax_31:1.b & 0x44
006c6b8e  bool p = unimplemented  {test ah, 0x44}
006c6b91  if (p)
006c6b93      long double x87_r3_5 = fconvert.t(var_ec_2)
006c6ba1      var_ec_2 = fconvert.s(x87_r3_5 / x87_r3_5)
006c6ba9  else
006c6ba9      x87_r6_7 = x87_r5_8
006c6bab      long double x87_r5_12 = fconvert.t(var_ec_2)
006c6baf      long double temp1_1 = fconvert.t(1.0)
006c6baf      x87_r5_12 - temp1_1
006c6bb5      eax_31.w = (x87_r5_12 < temp1_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r5_12, temp1_1) ? 1 : 0 << 0xa) | ((x87_r5_12 == temp1_1 ? 1 : 0 << 0xe) | 0x3000)))
006c6bb7      if ((eax_31:1.b & 0x41) == 0)
006c6bbe          var_ec_2 = fconvert.s(x87_r6_5)
006c6bc2          x87_r6_7 = x87_r6_7
006c6bc4  if (esi_4 == 2 && edx_32 == esi_4)
006c6be1      var_dc = fconvert.s(fconvert.t(*edi_6))
006c6be8      float var_d8_1 = fconvert.s(fconvert.t(*(edi_6 + 4)))
006c6bef      float var_d4_1 = fconvert.s(fconvert.t(*(edi_6 + 8)))
006c6bf6      float var_d0_1 = fconvert.s(fconvert.t(*(edi_6 + 0xc)))
006c6bfd      float var_cc_1 = fconvert.s(fconvert.t(*(edi_6 + 0x10)))
006c6c04      float var_c8_1 = fconvert.s(fconvert.t(*(edi_6 + 0x14)))
006c6c0b      float var_c4_1 = fconvert.s(fconvert.t(*(edi_6 + 0x18)))
006c6c17      sub_4aef90(ebp_4, fconvert.s(fconvert.t(var_ec_2)), &var_dc)
006c6c1f      long double x87_r6_9 = fconvert.t(var_cc_1)
006c6c37      long double x87_r5_14 = fconvert.t(var_c8_1)
006c6c48      long double x87_r4_9 = fconvert.t(var_c4_1)
006c6c5a      long double x87_r3_8 = fconvert.t(var_ec_2)
006c6c82      float var_c0_2 = fconvert.s(x87_r6_9 + fconvert.t(fconvert.s(fconvert.t(fconvert.s(fconvert.t(*(ebp_4 + 0x10)) - x87_r6_9)) * x87_r3_8)))
006c6c8a      float var_bc_2 = fconvert.s(x87_r5_14 + fconvert.t(fconvert.s(fconvert.t(fconvert.s(fconvert.t(*(ebp_4 + 0x14)) - x87_r5_14)) * x87_r3_8)))
006c6c92      float var_b8_2 = fconvert.s(x87_r4_9 + fconvert.t(fconvert.s(x87_r3_8 * fconvert.t(fconvert.s(fconvert.t(*(ebp_4 + 0x18)) - x87_r4_9)))))
006c6c9a      *arg2 = fconvert.s(fconvert.t(var_dc))
006c6ca0      *(arg2 + 4) = fconvert.s(fconvert.t(var_d8_1))
006c6ca7      *(arg2 + 8) = fconvert.s(fconvert.t(var_d4_1))
006c6cae      *(arg2 + 0xc) = fconvert.s(fconvert.t(var_d0_1))
006c6cb5      *(arg2 + 0x10) = fconvert.s(fconvert.t(var_c0_2))
006c6cbc      *(arg2 + 0x14) = fconvert.s(fconvert.t(var_bc_2))
006c6cc3      *(arg2 + 0x18) = fconvert.s(fconvert.t(var_b8_2))
006c6bc4  long double x87_r7_93
006c6bc4  if (esi_4 != 2 || (esi_4 == 2 && edx_32 != esi_4))
006c6bc4      if (edx_32 == 1)
006c6cd7          x87_r7_93 = x87_r6_7
006c6cdb          *arg2 = fconvert.s(fconvert.t(*edi_6))
006c6ce0          *(arg2 + 4) = fconvert.s(fconvert.t(*(edi_6 + 4)))
006c6ce6          *(arg2 + 8) = fconvert.s(fconvert.t(*(edi_6 + 8)))
006c6cec          *(arg2 + 0xc) = fconvert.s(fconvert.t(*(edi_6 + 0xc)))
006c6cf2          *(arg2 + 0x10) = fconvert.s(fconvert.t(*(edi_6 + 0x10)))
006c6cf8          *(arg2 + 0x14) = fconvert.s(fconvert.t(*(edi_6 + 0x14)))
006c6cfe          *(arg2 + 0x18) = fconvert.s(fconvert.t(*(edi_6 + 0x18)))
006c6d09      else
006c6d09          var_98 = fconvert.s(x87_r6_7)
006c6d21          var_b4 = fconvert.s(x87_r6_7)
006c6d06          if (edx_32 == 4)
006c6d43              ebx_1 = ebp_4
006c6d4a          else if (edx_32 != 3)
006c6d5a              var_dc = fconvert.s(fconvert.t(*ebp_4))
006c6d5f              *(ebp_4 + 4)
006c6d69              *(ebp_4 + 8)
006c6d70              *(ebp_4 + 0xc)
006c6d77              *(ebp_4 + 0x10)
006c6d7e              *(ebp_4 + 0x14)
006c6d85              *(ebp_4 + 0x18)
006c6d96              sub_4aef90(edi_6, fconvert.s(fconvert.t(2f)), &var_dc)
006c6da2              ebx_1 = &var_98
006c6e16              var_98 = fconvert.s(fconvert.t(var_dc))
006c6d9b              *(edi_6 + 0x10)
006c6db0              *(edi_6 + 0x14)
006c6dc1              *(edi_6 + 0x18)
006c6e4a          float* eax_34
006c6e4a          if (esi_4 == 4)
006c6e4f              eax_34 = edi_6
006c6e56          else if (esi_4 != 3)
006c6e6d              var_dc = fconvert.s(fconvert.t(*edi_6))
006c6e72              *(edi_6 + 4)
006c6e7c              *(edi_6 + 8)
006c6e83              *(edi_6 + 0xc)
006c6e8a              *(edi_6 + 0x10)
006c6e91              *(edi_6 + 0x14)
006c6e98              *(edi_6 + 0x18)
006c6ea9              sub_4aef90(ebp_4, fconvert.s(fconvert.t(2f)), &var_dc)
006c6eb5              eax_34 = &var_b4
006c6f29              var_b4 = fconvert.s(fconvert.t(var_dc))
006c6eae              *(ebp_4 + 0x10)
006c6ec3              *(ebp_4 + 0x14)
006c6ed4              *(ebp_4 + 0x18)
006c6e5b          else
006c6e5b              eax_34 = var_74
006c6f68          void var_70
006c6f68          sub_6c7050(&var_70, ebx_1, edi_6, ebp_4, eax_34)
006c6f81          float* eax_35 = sub_6c70c0(&var_70, &var_b4, fconvert.s(fconvert.t(var_ec_2)))
006c6f8f          *arg2 = fconvert.s(fconvert.t(*eax_35))
006c6f94          *(arg2 + 4) = fconvert.s(fconvert.t(*(eax_35 + 4)))
006c6f9a          *(arg2 + 8) = fconvert.s(fconvert.t(*(eax_35 + 8)))
006c6fa0          *(arg2 + 0xc) = fconvert.s(fconvert.t(*(eax_35 + 0xc)))
006c6fa6          *(arg2 + 0x10) = fconvert.s(fconvert.t(*(eax_35 + 0x10)))
006c6fac          *(arg2 + 0x14) = fconvert.s(fconvert.t(*(eax_35 + 0x14)))
006c6fb2          *(arg2 + 0x18) = fconvert.s(fconvert.t(*(eax_35 + 0x18)))
006c6fb5          sub_4ac6f0(arg2)
006c6bc4  if ((esi_4 == 2 && edx_32 == esi_4) || ((esi_4 != 2 || (esi_4 == 2 && edx_32 != esi_4)) && edx_32 != 1))
006c6fba      x87_r7_93 = float.t(1)
006c6fc4  *arg3 = fconvert.s(x87_r7_93)
006c6fc7  *(arg3 + 4) = fconvert.s(x87_r7_93)
006c6fd2  return arg3

---------------------------------------------------------------------

int32_t __stdcall sub_5f06a0(int32_t arg1 @ ecx, int32_t arg2, void* arg3, void* arg4, int32_t arg5)

005f06a3  int32_t esi = arg2
005f06af  int32_t* ebx_3 = ((esi u>> 3) & 0xfffffffc) + *arg1
005f06b5  int32_t eax = *ebx_3
005f06b7  void* ebx_4 = ebx_3 + 4
005f06ba  bool cond:0 = arg5 u<= 0
005f06bc  int32_t var_4 = eax
005f06c0  arg5 = arg5
005f076b  if (cond:0)
005f076b      return esi
005f06d8  while (true)
005f06d8      uint32_t edx_1 = zx.d(*arg4)
005f06de      arg4 = arg4 + 1
005f06e4      int32_t ecx_4 = esi & 0x1f
005f06ec      uint32_t edi_1 = 0x20 - ecx_4
005f06f0      uint32_t ebp_1 = edi_1
005f06ee      if (edi_1 u>= edx_1)
005f06f4          ebp_1 = edx_1
005f06f6      arg2 = arg2 + edx_1
005f070a      int32_t esi_3 = (eax u>> ecx_4.b) & ((1 << ebp_1.b) - 1)
005f070c      char edx_2 = edx_1.b - ebp_1.b
005f070c      if (edx_1 != ebp_1)
005f0710          eax = *ebx_4
005f071d          ebx_4 = ebx_4 + 4
005f0720          var_4 = eax
005f072b          esi_3 = esi_3 | ((((1 << edx_2) - 1) & eax) << ebp_1.b)
005f072f      else if (edi_1 != ebp_1)
005f073e          eax = var_4
005f0733      else
005f0733          eax = *ebx_4
005f0735          var_4 = eax
005f0739          ebx_4 = ebx_4 + 4
005f0746      *arg3 = esi_3
005f074b      int32_t temp2_1 = arg5
005f074b      arg5 = arg5 - 1
005f0750      arg3 = arg3 + 4
005f0754      if (temp2_1 == 1)
005f0754          break
005f06d0      esi = arg2
005f0763  return arg2

-----------------------------------------------------------------

float* __stdcall sub_6c6460(float* arg1, int32_t arg2, char* arg3, float arg4)

006c648e  void var_1c
006c648e  float* eax_2 = sub_6c4250(&var_1c, arg2 + 0x10, zx.d(*(arg3 + 4)), zx.d(*(arg3 + 5)), zx.d(*(arg3 + 6)), fconvert.s(fconvert.t(arg4)))
006c64b9  void var_10
006c64b9  float* eax_4 = sub_6c53f0(&var_10, arg2, zx.d(*arg3), zx.d(*(arg3 + 1)), zx.d(*(arg3 + 2)), zx.d(*(arg3 + 3)), fconvert.s(fconvert.t(arg4)))
006c64c9  *arg1 = fconvert.s(fconvert.t(*eax_4))
006c64ce  *(arg1 + 4) = fconvert.s(fconvert.t(*(eax_4 + 4)))
006c64d4  *(arg1 + 8) = fconvert.s(fconvert.t(*(eax_4 + 8)))
006c64da  *(arg1 + 0xc) = fconvert.s(fconvert.t(*(eax_4 + 0xc)))
006c64df  *(arg1 + 0x10) = fconvert.s(fconvert.t(*eax_2))
006c64e5  *(arg1 + 0x14) = fconvert.s(fconvert.t(*(eax_2 + 4)))
006c64ed  *(arg1 + 0x18) = fconvert.s(fconvert.t(*(eax_2 + 8)))
006c64f4  return arg1

---------------------------------------------------------------

void __stdcall sub_4a33c0(void* arg1 @ ecx, float* arg2, float* arg3)

004a33d6  int32_t esi = *(arg1 + 0x24)
004a33e7  if (esi == 0)
004a33e7      *arg2 = fconvert.s(fconvert.t(*data_8bad30))
004a33f1      *(arg2 + 4) = fconvert.s(fconvert.t(*data_8bad34))
004a33fa      *(arg2 + 8) = fconvert.s(fconvert.t(*data_8bad38))
004a3403      *(arg2 + 0xc) = fconvert.s(fconvert.t(*data_8bad3c))
004a340c      *(arg2 + 0x10) = fconvert.s(fconvert.t(*data_8bad40))
004a3415      *(arg2 + 0x14) = fconvert.s(fconvert.t(*data_8bad44))
004a341e      *(arg2 + 0x18) = fconvert.s(fconvert.t(*data_8bad48))
004a3425      long double x87_r7_7 = float.t(0)
004a3427      *arg3 = fconvert.s(x87_r7_7)
004a3429      *(arg3 + 4) = fconvert.s(x87_r7_7)
004a342f      return 
004a3434  int32_t eax_4 = sub_4027d0()
004a3439  int32_t ecx = esi + (esi << 3)
004a343c  int32_t ecx_1 = ecx + ecx
004a343e  int32_t ecx_2 = ecx_1 + ecx_1
004a3447  void* eax_5 = sub_4027c0(ecx_2 + ecx_2)
004a344c  long double x87_r7_8 = float.t(0)
004a344e  void* ebx = *(arg1 + 0x28)
004a3451  float var_28 = fconvert.s(x87_r7_8)
004a3458  float var_2c = fconvert.s(x87_r7_8)
004a345c  void* ecx_4 = eax_5 + ((esi + (esi << 3)) << 2)
004a345f  float var_34 = fconvert.s(x87_r7_8)
004a3466  float var_30 = fconvert.s(x87_r7_8)
004a3474  int32_t ebp = 0
004a347a  int32_t esi_1 = 0
004a347c  int32_t var_1c = *(*(ebx + 0x14) + 0x24)
004a3480  void* var_24 = eax_5
004a3484  while (true)
004a3484      void* edx_2 = *(ebx + 0x14)
004a3487      int32_t eax_6 = *(edx_2 + 0x24)
004a348a      if (eax_6 != var_1c)
004a349c          if (esi_1 s> 0)
004a34a4              void* edi_1 = var_24
004a34b2              var_24 = var_24 + 0x24
004a34be              ebp = ebp + 1
004a34cb              sub_4a2f30(ecx_4, esi_1, edi_1, fconvert.s(fconvert.t(var_28)), fconvert.s(fconvert.t(var_2c)))
004a34d0              long double x87_r7_11 = fconvert.t(var_34)
004a34d4              long double x87_r6_1 = fconvert.t(*(edi_1 + 0x1c))
004a34da              x87_r6_1 - x87_r7_11
004a34dc              (x87_r6_1 < x87_r7_11 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_1, x87_r7_11) ? 1 : 0 << 0xa) | ((x87_r6_1 == x87_r7_11 ? 1 : 0 << 0xe) | 0))):1.b & 5
004a34de              bool p_1 = unimplemented  {test ah, 0x5}
004a34e1              if (p_1)
004a34e6                  var_34 = fconvert.s(fconvert.t(*(edi_1 + 0x1c)))
004a34ea              long double x87_r7_13 = fconvert.t(var_30)
004a34ee              long double x87_r6_2 = fconvert.t(*(edi_1 + 0x20))
004a34f1              x87_r6_2 - x87_r7_13
004a34f3              (x87_r6_2 < x87_r7_13 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_2, x87_r7_13) ? 1 : 0 << 0xa) | ((x87_r6_2 == x87_r7_13 ? 1 : 0 << 0xe) | 0))):1.b & 5
004a34f5              bool p_2 = unimplemented  {test ah, 0x5}
004a34f8              if (p_2)
004a34fd                  var_30 = fconvert.s(fconvert.t(*(edi_1 + 0x20)))
004a3501              long double x87_r7_15 = fconvert.t(var_34)
004a3505              long double x87_r6_3 = fconvert.t(0.99998998641967773)
004a350b              x87_r6_3 - x87_r7_15
004a350d              (x87_r6_3 < x87_r7_15 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_3, x87_r7_15) ? 1 : 0 << 0xa) | ((x87_r6_3 == x87_r7_15 ? 1 : 0 << 0xe) | 0x3000))):1.b & 5
004a3511              bool p_3 = unimplemented  {test ah, 0x5}
004a3514              if (not(p_3))
004a3516                  long double x87_r6_4 = fconvert.t(var_30)
004a351a                  x87_r6_4 - x87_r7_15
004a3521                  if (((x87_r6_4 < x87_r7_15 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_4, x87_r7_15) ? 1 : 0 << 0xa) | ((x87_r6_4 == x87_r7_15 ? 1 : 0 << 0xe) | 0))):1.b & 0x41) == 0)
004a3521                      break
004a352b              x87_r7_8 = float.t(0)
004a352d              eax_6 = eax_6
004a3531              edx_2 = edx_2
004a3535          var_28 = fconvert.s(x87_r7_8)
004a3539          var_1c = eax_6
004a353d          var_2c = fconvert.s(x87_r7_8)
004a3541          esi_1 = 0
004a3546      int32_t* ecx_6 = *(ebx + 0x18)
004a3550      long double x87_r5_1 = fconvert.t(fconvert.s(fconvert.t(*(edx_2 + 0x20))))
004a355a      float var_18_2 = fconvert.s(fconvert.t(*(ebx + 0x1c)) * x87_r5_1)
004a355e      long double x87_r6_9 = x87_r7_8
004a355e      x87_r7_8 = x87_r5_1
004a3560      long double temp1_1 = fconvert.t(var_18_2)
004a3560      x87_r6_9 - temp1_1
004a3564      eax_6.w = (x87_r6_9 < temp1_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_9, temp1_1) ? 1 : 0 << 0xa) | ((x87_r6_9 == temp1_1 ? 1 : 0 << 0xe) | 0x3000)))
004a3566      eax_6:1.b & 5
004a3566      bool p_4 = unimplemented  {test ah, 0x5}
004a3569      if (not(p_4))
004a3576          float* edi_3 = ecx_4 + ((esi_1 + (esi_1 << 3)) << 2)
004a3579          if (edi_3 == 0)
004a3593              x87_r7_8 = x87_r6_9
004a357d          else
004a357d              *edi_3 = fconvert.s(x87_r6_9)
004a357f              *(edi_3 + 4) = fconvert.s(x87_r6_9)
004a3582              *(edi_3 + 8) = fconvert.s(x87_r6_9)
004a3585              *(edi_3 + 0xc) = fconvert.s(x87_r6_9)
004a3588              *(edi_3 + 0x10) = fconvert.s(x87_r6_9)
004a358b              *(edi_3 + 0x14) = fconvert.s(x87_r6_9)
004a358e              *(edi_3 + 0x18) = fconvert.s(x87_r6_9)
004a35ad          float var_8
004a3597          *(*ecx_6 + 0x14)(edi_3, &var_8, fconvert.s(fconvert.t(*(edx_2 + 0x18))), fconvert.s(x87_r7_8))
004a35b3          long double x87_r6_10 = fconvert.t(var_18_2)
004a35b7          esi_1 = esi_1 + 1
004a35cc          long double x87_r7_21 = fconvert.t(fconvert.s(fconvert.t(var_8) * x87_r6_10))
004a35d6          var_28 = fconvert.s(x87_r7_21 + fconvert.t(var_28))
004a35da          float var_4
004a35da          long double x87_r6_14 = fconvert.t(fconvert.s(x87_r6_10 * fconvert.t(var_4)))
004a35e4          var_2c = fconvert.s(x87_r6_14 + fconvert.t(var_2c))
004a35ea          *(edi_3 + 0x1c) = fconvert.s(x87_r7_21)
004a35ed          *(edi_3 + 0x20) = fconvert.s(x87_r6_14)
004a35f0          x87_r7_8 = float.t(0)
004a35f6      ebx = *(ebx + 4)
004a3601      if (ebx == 0)
004a3601          if (esi_1 s> 0)
004a3622              void* edi_4 = eax_5 + ((ebp + (ebp << 3)) << 2)
004a362e              sub_4a2f30(ecx_4, esi_1, edi_4, fconvert.s(fconvert.t(var_28)), fconvert.s(fconvert.t(var_2c)))
004a3633              long double x87_r7_25 = fconvert.t(var_34)
004a3637              long double x87_r6_16 = fconvert.t(*(edi_4 + 0x1c))
004a363d              x87_r6_16 - x87_r7_25
004a363f              (x87_r6_16 < x87_r7_25 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_16, x87_r7_25) ? 1 : 0 << 0xa) | ((x87_r6_16 == x87_r7_25 ? 1 : 0 << 0xe) | 0))):1.b & 5
004a3641              bool p_5 = unimplemented  {test ah, 0x5}
004a3644              if (p_5)
004a3649                  var_34 = fconvert.s(fconvert.t(*(edi_4 + 0x1c)))
004a364d              long double x87_r7_27 = fconvert.t(var_30)
004a3651              long double x87_r6_17 = fconvert.t(*(edi_4 + 0x20))
004a3654              x87_r6_17 - x87_r7_27
004a3656              (x87_r6_17 < x87_r7_27 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_17, x87_r7_27) ? 1 : 0 << 0xa) | ((x87_r6_17 == x87_r7_27 ? 1 : 0 << 0xe) | 0))):1.b & 5
004a3658              bool p_6 = unimplemented  {test ah, 0x5}
004a365b              if (p_6)
004a3660                  var_30 = fconvert.s(fconvert.t(*(edi_4 + 0x20)))
004a3601          break
004a3692  sub_4027e0(eax_4)
004a36a2  *arg3 = fconvert.s(fconvert.t(var_34))
004a36aa  *(arg3 + 4) = fconvert.s(fconvert.t(var_30))

----------------------------------------------------------------

void __stdcall sub_5be530(int32_t* arg1 @ ecx, char arg2, float* arg3)

005be533  int0_t x87_r0
005be533  if (arg2 == 0 || (arg2 != 0 && (*(arg1 + 0x70) & 1) != 0))
005be547      long double x87_r7_1 = float.t(0)
005be54c      float var_1c_1 = fconvert.s(x87_r7_1)
005be554      float var_18_1 = fconvert.s(x87_r7_1)
005be558      float var_14_1 = fconvert.s(x87_r7_1)
005be55c      long double x87_r6_1 = float.t(1)
005be55e      float var_10_1 = fconvert.s(x87_r6_1)
005be564      float var_c_1 = fconvert.s(x87_r7_1)
005be568      float var_8_1 = fconvert.s(x87_r7_1)
005be56c      float var_4_1 = fconvert.s(x87_r7_1)
005be552      if (((*(arg1 + 0x70) u>> 1).b & 1) != 0)
005be572          void* eax_3 = *arg1
005be57a          var_1c_1 = fconvert.s(fconvert.t(*(eax_3 + 0x1c)))
005be581          var_18_1 = fconvert.s(fconvert.t(*(eax_3 + 0x20)))
005be588          var_14_1 = fconvert.s(fconvert.t(*(eax_3 + 0x24)))
005be58f          var_10_1 = fconvert.s(fconvert.t(*(eax_3 + 0x28)))
005be596          var_c_1 = fconvert.s(fconvert.t(*(eax_3 + 0x2c)))
005be59d          var_8_1 = fconvert.s(fconvert.t(*(eax_3 + 0x30)))
005be5a4          var_4_1 = fconvert.s(fconvert.t(*(eax_3 + 0x34)))
005be5ab      float var_38 = fconvert.s(x87_r7_1)
005be5b2      float var_34_1 = fconvert.s(x87_r7_1)
005be5b6      float var_30_1 = fconvert.s(x87_r7_1)
005be5bb      float var_2c_1 = fconvert.s(x87_r7_1)
005be5c2      float var_28 = fconvert.s(x87_r7_1)
005be5ca      float var_24_1 = fconvert.s(x87_r7_1)
005be5d1      float var_20_1 = fconvert.s(x87_r7_1)
005be5dd      float var_5c = fconvert.s(x87_r7_1)
005be5af      *(*(arg1 + 0x40) + 0x14)(&var_38, &var_5c, fconvert.s(x87_r7_1), fconvert.s(x87_r6_1))
005be5f2      long double x87_r7_4 = float.t(0)
005be5f4      long double x87_r6_4 = fconvert.t(var_5c)
005be5f8      x87_r6_4 - x87_r7_4
005be5fa      int32_t eax_5
005be5fa      eax_5.w = (x87_r6_4 < x87_r7_4 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_4, x87_r7_4) ? 1 : 0 << 0xa) | ((x87_r6_4 == x87_r7_4 ? 1 : 0 << 0xe) | 0x3000)))
005be5fc      x87_r0 = x87_r6_4
005be5fe      float var_48
005be5fe      if ((eax_5:1.b & 0x41) != 0)
005be768          x87_r0 = x87_r7_4
005be60d      else
005be60d          if (((*(arg1 + 0x50) u>> 0xb).w.b & 1) == 0)
005be684              var_28 = fconvert.s(fconvert.t(*arg3) * fconvert.t(var_28))
005be68f              var_24_1 = fconvert.s(fconvert.t(*(arg3 + 4)) * fconvert.t(var_24_1))
005be69a              var_20_1 = fconvert.s(fconvert.t(*(arg3 + 8)) * fconvert.t(var_20_1))
005be611          else
005be611              long double x87_r6_5 = fconvert.t(9.99999997e-07f)
005be617              long double temp2_1 = fconvert.t(*(arg1 + 0x20))
005be617              x87_r6_5 - temp2_1
005be61a              (x87_r6_5 < temp2_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6_5, temp2_1) ? 1 : 0 << 0xa) | ((x87_r6_5 == temp2_1 ? 1 : 0 << 0xe) | 0x3800))):1.b & 5
005be61c              bool p_1 = unimplemented  {test ah, 0x5}
005be61f              if (not(p_1))
005be621                  x87_r0 = x87_r7_4
005be62a                  arg2.d = fconvert.s(fconvert.t(*(arg1 + 0x20)))
005be642                  var_28 = fconvert.s(fconvert.t(arg2.d) * fconvert.t(var_28))
005be658                  float* eax_8 = sub_4aee20(&var_48, &var_28, arg1 + 0x30)
005be662                  var_28 = fconvert.s(fconvert.t(*eax_8))
005be669                  var_24_1 = fconvert.s(fconvert.t(*(eax_8 + 4)))
005be670                  var_20_1 = fconvert.s(fconvert.t(*(eax_8 + 8)))
005be674                  x87_r7_4 = fconvert.t(var_5c)
005be6a2          void* esi_1 = *arg1
005be6a4          long double x87_r5_8 = fconvert.t(var_c_1)
005be6b6          long double x87_r4_2 = fconvert.t(var_8_1)
005be6c8          long double x87_r3_2 = fconvert.t(var_4_1)
005be6dc          var_48 = fconvert.s(fconvert.t(fconvert.s(fconvert.t(var_28) - x87_r5_8)) * x87_r7_4)
005be716          *(esi_1 + 0x2c) = fconvert.s(fconvert.t(fconvert.s(x87_r5_8 + fconvert.t(var_48))))
005be71d          *(esi_1 + 0x30) = fconvert.s(fconvert.t(fconvert.s(x87_r4_2 + fconvert.t(fconvert.s(fconvert.t(fconvert.s(fconvert.t(var_24_1) - x87_r4_2)) * x87_r7_4)))))
005be724          *(esi_1 + 0x34) = fconvert.s(fconvert.t(fconvert.s(x87_r3_2 + fconvert.t(fconvert.s(x87_r7_4 * fconvert.t(fconvert.s(fconvert.t(var_20_1) - x87_r3_2)))))))
005be727          eax_5 = *(esi_1 + 0x18)
005be72a          if ((eax_5.b & 1) != 0)
005be72e              int32_t* ecx_2 = *(esi_1 + 0x6c)
005be731              eax_5 = eax_5 & 0xfffffffe
005be736              *(esi_1 + 0x18) = eax_5
005be734              if (ecx_2 != 0)
005be745                  int32_t* ebx_1
005be745                  do
005be745                      ebx_1 = *(ecx_2 + 8)
005be742                      *(*ecx_2 + 4)(0)
005be74c                      ecx_2 = ebx_1
005be74c                  while (ebx_1 != 0)
005be751              void* esi_2 = *(esi_1 + 0x60)
005be754              if (esi_2 != 0)
005be75a                  do
005be75a                      sub_4699e0(esi_2)
005be75f                      esi_2 = *(esi_2 + 0x68)
005be75f                  while (esi_2 != 0)
005be76a      long double x87_r7_17 = fconvert.t(0f)
005be76e      long double temp1_1 = fconvert.t(0.99999898672103882)
005be76e      x87_r7_17 - temp1_1
005be774      eax_5.w = (x87_r7_17 < temp1_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r7_17, temp1_1) ? 1 : 0 << 0xa) | ((x87_r7_17 == temp1_1 ? 1 : 0 << 0xe) | 0x3800)))
005be776      if ((eax_5:1.b & 0x41) != 0)
005be7ee          var_48 = fconvert.s(fconvert.t(var_1c_1))
005be814          sub_4aef90(&var_48, &var_38, fconvert.s(x87_r7_17), &var_48)
005be81d          void* esi_6 = *arg1
005be81f          *(esi_6 + 0x1c) = fconvert.s(fconvert.t(var_48))
005be826          *(esi_6 + 0x20) = fconvert.s(fconvert.t(fconvert.s(fconvert.t(var_18_1))))
005be82d          *(esi_6 + 0x24) = fconvert.s(fconvert.t(fconvert.s(fconvert.t(var_14_1))))
005be834          *(esi_6 + 0x28) = fconvert.s(fconvert.t(fconvert.s(fconvert.t(var_10_1))))
005be837          int32_t eax_14 = *(esi_6 + 0x18)
005be83a          if ((eax_14.b & 1) != 0)
005be83e              int32_t* ecx_7 = *(esi_6 + 0x6c)
005be846              *(esi_6 + 0x18) = eax_14 & 0xfffffffe
005be844              if (ecx_7 != 0)
005be855                  int32_t* edi_2
005be855                  do
005be855                      edi_2 = *(ecx_7 + 8)
005be852                      *(*ecx_7 + 4)()
005be85c                      ecx_7 = edi_2
005be85c                  while (edi_2 != 0)
005be860              void* esi_7 = *(esi_6 + 0x60)
005be863              if (esi_7 != 0)
005be869                  do
005be869                      sub_4699e0(esi_7)
005be86e                      esi_7 = *(esi_7 + 0x68)
005be86e                  while (esi_7 != 0)
005be77b      else
005be77b          void* esi_3 = *arg1
005be77d          x87_r0 = x87_r7_17
005be783          *(esi_3 + 0x1c) = fconvert.s(fconvert.t(var_38))
005be78a          *(esi_3 + 0x20) = fconvert.s(fconvert.t(var_34_1))
005be791          *(esi_3 + 0x24) = fconvert.s(fconvert.t(var_30_1))
005be798          *(esi_3 + 0x28) = fconvert.s(fconvert.t(var_2c_1))
005be79b          int32_t eax_11 = *(esi_3 + 0x18)
005be79e          if ((eax_11.b & 1) != 0)
005be7a6              int32_t* ecx_4 = *(esi_3 + 0x6c)
005be7ae              *(esi_3 + 0x18) = eax_11 & 0xfffffffe
005be7ac              if (ecx_4 != 0)
005be7b8                  int32_t* edi
005be7b8                  do
005be7b8                      edi = *(ecx_4 + 8)
005be7b5                      *(*ecx_4 + 4)()
005be7bf                      ecx_4 = edi
005be7bf                  while (edi != 0)
005be7c3              void* esi_4 = *(esi_3 + 0x60)
005be7d2              if (esi_4 != 0)
005be7d2                  do
005be7d2                      sub_4699e0(esi_4)
005be7d7                      esi_4 = *(esi_4 + 0x68)
005be7d7                  while (esi_4 != 0)
005be7e3                  return x87_r0
005be87a  return x87_r0

------------------------------------------------------------


BOOL sub_5992d0(int32_t arg1 @ esi)

005992dd  int32_t* fsbase
005992dd  int32_t var_c = *fsbase
005992de  *fsbase = &var_c
005992ef  if ((*data_8ef470 & 1) == 0)
005992fd      *data_8ef470 = *data_8ef470 | 1
00599330      void var_28
00599330      sub_40d9b0(&var_28, "GameEngine::Loop", 0x10)
00599349      int32_t var_4
00599349      var_4.b = 1
00599359      void var_54
00599359      sub_40d8d0(&var_54, &var_28, nullptr, __gfids_table[0xbae7688].rvAddr+3)
00599386      int32_t var_4_1
00599386      var_4_1.b = 0
0059938a      sub_560e20(data_8ef430, var_54, 0)
00599394      int32_t edi
00599394      sub_7623b0(&var_54, edi, 0x7ded20)
005993a4  int32_t ecx_3
005993a4  int80_t st0
005993a4  st0, ecx_3 = sub_4b5a60()
005993c9  sub_560550()
005993ce  long double x87_r7_3 = fconvert.t(*data_8bace8)
005993d4  *data_8ba770 = *data_8ba770 + 1
005993db  float var_30_3 = fconvert.s(x87_r7_3)
005993ed  float var_2c = fconvert.s(fconvert.t(*data_8bad00))
005993f1  *data_8ef428 = 0
005993f7  sub_5354b0(data_8ea74c)
005993fc  *data_8bad20
00599402  sub_4a9170()
00599419  int16_t eax_1
00599419  int32_t ecx_5
00599419  long double st0_1
00599419  st0_1, eax_1, ecx_5 = sub_4e3ee0(fconvert.s(fconvert.t(var_30_3)), fconvert.s(fconvert.t(var_2c)))
0059941e  long double x87_r6 = float.t(0)
00599423  long double temp0 = fconvert.t(var_30_3)
00599423  x87_r6 - temp0
00599423  int16_t top = 0xffff
00599427  (x87_r6 < temp0 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((is_unordered.t(x87_r6, temp0) ? 1 : 0 << 0xa) | ((x87_r6 == temp0 ? 1 : 0 << 0xe) | 0x3800))):1.b & 5
00599429  bool p = unimplemented  {test ah, 0x5}
0059942c  int32_t ebx
0059942c  if (p)
00599505      *data_8ef424 = *data_8ef424 + 1
00599432  else
00599432      *data_8ef424 = 0
00599438      sub_4b60a0(ecx_5, st0_1)
0059943d      sub_421340()
00599442      sub_5182c0(arg1, ebx, var_30_3, var_2c)
00599447      sub_421340()
0059944c      sub_471020()
00599451      sub_563730()
00599456      sub_4d96b0()
00599463      sub_50c540()
0059946b      sub_421340()
00599475      sub_4ecb90(data_8bb100)
0059947a      sub_421340()
0059947f      sub_712620()
00599486      sub_5eec20(0xfe)                                  // <- update animations 0xfe = flags read at sub_5edf30(
0059948b      sub_421340()
00599490      sub_61f210()
0059949c      sub_71a470(fconvert.s(fconvert.t(var_30_3)))
005994a4      sub_5e8450()
005994a9      int32_t esi_1 = *data_8bbe28
005994af      if (esi_1 != 0)
005994ba          do
005994b5              *(*esi_1 + 4)()
005994bc              esi_1 = *(esi_1 + 8)
005994bc          while (esi_1 != 0)
005994c3      sub_5fcdc0()
005994d0      sub_6105c0(fconvert.s(fconvert.t(var_30_3)))
005994d0      top = 0xfffd
005994d8      if (*data_8ef320 != 0)
005994e5          char eax_3
005994e5          int32_t ecx_9
005994e5          eax_3, ecx_9 = sub_501830(data_8ef324)
005994ed          if (eax_3 != 0)
005994f6              sub_5075e0(ecx_9, data_8ef324)
005994fe      sub_421340()
0059950c  top = top - 1
0059950c  unimplemented  {fldz }
0059950e  long double temp1 = fconvert.t(var_30_3)
0059950e  unimplemented  {fcomp dword [esp+0x8]} f- temp1
0059950e  bool c0_1 = unimplemented  {fcomp dword [esp+0x8]} f< temp1
0059950e  bool c2_1 = is_unordered.t(unimplemented  {fcomp dword [esp+0x8]}, temp1)
0059950e  bool c3_1 = unimplemented  {fcomp dword [esp+0x8]} f== temp1
0059950e  unimplemented  {fcomp dword [esp+0x8]}
00599512  (c0_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((c2_1 ? 1 : 0 << 0xa) | ((c3_1 ? 1 : 0 << 0xe) | (((top + 1) & 7) << 0xb)))):1.b & 5
00599514  bool p_1 = unimplemented  {test ah, 0x5}
00599517  if (not(p_1) || (p_1 && *data_8ef424 s> 0xa))
00599522      sub_5114a0()
00599527      int32_t esi_2 = *data_8bbdc8
0059952d      if (esi_2 != 0)
00599531          do
00599531              if (*(esi_2 + 0x25) == 0)
00599538                  sub_469dd0(esi_2)
0059953d              esi_2 = *(esi_2 + 0xc)
0059953d          while (esi_2 != 0)
00599549      *data_8ef428 = sub_599190()
00599553  unimplemented  {call sub_467f50}
00599558  if (sub_467f50().b == 0)
0059955c      int32_t ecx_11 = *data_8ef31c
00599562      if (ecx_11 != 0 && *(*ecx_11 + 0x10)(arg1, ebx, var_30_3, var_2c) != 0)
00599587          BOOL eax_9
00599587          eax_9.b = IsWindowVisible(hWnd: *(**data_8ef31c + 0x10)()) != 0
0059958e          *fsbase = var_c
0059959a          return eax_9
005995a0  int32_t eax_5
005995a0  eax_5.b = 0
005995a2  *fsbase = var_c
005995ad  return eax_5

----------------------------------------------------------

//GameLoop calls this with arg2 = 0xfe = 11111110
int32_t* __stdcall sub_5edf30(void* arg1 @ ecx, char arg2)

005edf37  int32_t* fsbase
005edf37  void* eax = *fsbase
005edf3d  void* var_c = eax
005edf3e  *fsbase = &var_c
005edf49  char ebx = arg2
005edf50  int32_t esi
005edf50  int32_t var_40 = esi
005edf50  int32_t* esp_1 = &var_40
005edf4d  int32_t var_48
005edf4d  if ((ebx & 1) != 0)                                               // false
005edf59      eax = *(arg1 + 8)
005edf5c      if (*(eax + 0x58) == 0)
005edf67          var_48 = 0x8c3ac8
005edf6e          eax = sub_4946d0(*(eax + 0x5c), data_8c3ac8, 0)
005edf6e          esp_1 = &var_40
005edf73          arg2.d = eax
005edf77          if (eax != 0)
005edf85              eax = sub_5bf9e0(eax)
005edf92  int16_t top
005edf92  if ((ebx & 4) != 0)                                               // true
005edf9f      var_48 = 0x8c3ac8
005edfa6      eax = sub_494f70(*(*(arg1 + 8) + 0x5c), data_8c3ac8, 0)
005edfa6      esp_1 = &var_40
005edfab      arg2.d = eax
005edfaf      if (eax != 0)
005edfbd          int80_t st0_1
005edfbd          st0_1, eax = sub_5d1420(eax)
005edfbd          top = 0xffff
005edfca  if ((ebx & 8) != 0)                                           // true
005edfd3      eax = *(arg1 + 0x38)
005edfd6      void* ebx_1 = arg1 + 0x34
005edfda      void* edi_1 = *eax
005edfdc      void* esi_2 = ebx_1
005edfe6      int32_t ebp
005edfe6      var_48 = ebp
005edfe6      void* esp_2 = &var_48
005edfe9      while (true)
005edfe9          int32_t ebp_1 = *(ebx_1 + 4)
005edfe7          if (esi_2 == 0 || (esi_2 != 0 && esi_2 != ebx_1))
005edff2              eax = sub_761e75()
005edff9          if (edi_1 == ebp_1)
005edff9              break
005edfff          if (esi_2 == 0)
005ee003              sub_761e75()
005ee008          if (edi_1 == *(esi_2 + 4))
005ee00d              sub_761e75()
005ee012          void* ebp_2 = *(edi_1 + 0x18)
005ee015          void* edi_2 = *(esp_2 + 0x10)
005ee01c          void* esi_3 = *(*(edi_2 + 8) + 0x64)
005ee01f          int32_t ecx_4
005ee01f          if (esi_3 != 0)
005ee023              ecx_4 = *(esi_3 + 0x30)
005ee02d              *(esi_3 + 0x3c) = *data_8ba770
005ee01f              if (ecx_4 == 0 && *(esi_3 + 0x28) != ecx_4)
005ee037                  if ((*(esi_3 + 0x38) & 0x9000) != 0)
005ee040                      *(esp_2 - 4) = 0x8951a0
005ee047                      sub_41a5e0(esi_3)
005ee047                      esp_2 = esp_2
005ee04c                  ecx_4 = *(esi_3 + 0x30)
005ee01f          if (esi_3 == 0 || ((esi_3 != 0 && ecx_4 == 0) && *(esi_3 + 0x28) == ecx_4))
005ee051              ecx_4 = 0
005ee053          *(esp_2 - 4) = 1
005ee053          esp_2 = esp_2 - 4
005ee055          *(esp_2 - 4) = ebp_2
005ee056          esp_2 = esp_2 + 4
005ee05b          if (sub_420d30(ecx_4) != 0)
005ee06b              void* eax_8 = *(**(ebp_2 + 8) + 0xc)()
005ee072              *(esp_2 - 4) = *(eax_8 + 0x14)
005ee077              sub_560bb0(esp_2 + 0x24)
005ee080              *(esp_2 - 4) = *(esp_2 + 0x24)
005ee083              *(esp_2 + 0x44) = 2
005ee08b              sub_434d10(eax_8)
005ee093              *(esp_2 - 4) = *(eax_8 + 0x14)
005ee098              sub_560bb0(esp_2 + 0x30)
005ee098              void* esp_10 = esp_2
005ee0a0              void* edi_3 = *(*(edi_2 + 8) + 0x64)
005ee0a5              *(esp_10 + 0x44) = 3
005ee0a3              int32_t ecx_12
005ee0a3              if (edi_3 != 0)
005ee0b2                  *(edi_3 + 0x3c) = *data_8ba770
005ee0b5                  ecx_12 = *(edi_3 + 0x30)
005ee0a3                  if (ecx_12 == 0 && *(edi_3 + 0x28) != ecx_12)
005ee0c1                      if ((*(edi_3 + 0x38) & 0x9000) != 0)
005ee0ca                          *(esp_10 - 4) = 0x8951a0
005ee0d1                          sub_41a5e0(edi_3)
005ee0d1                          esp_10 = esp_10
005ee0d6                      ecx_12 = *(edi_3 + 0x30)
005ee0a3              if (edi_3 == 0 || ((edi_3 != 0 && ecx_12 == 0) && *(edi_3 + 0x28) == ecx_12))
005ee0db                  ecx_12 = 0
005ee0dd              int32_t edi_4 = *(esp_10 + 0x30)
005ee0e1              *(esp_10 - 4) = 1
005ee0e3              *(esp_10 - 8) = ebp_2
005ee0e9              *(esp_10 - 4) = sub_420d30(ecx_12)
005ee0ea              *(esp_10 - 8) = edi_4
005ee0ed              sub_434d30(eax_8)
005ee0f2              unimplemented  {fldz }
005ee0f4              int32_t* ecx_15 = *(ebp_2 + 8)
005ee0f7              *(esp_10 + 0x4c) = fconvert.s(unimplemented  {fst dword [esp+0x4c]})
005ee0fb              unimplemented  {fld1 }
005ee0ff              int32_t edx_4 = *(*ecx_15 + 0x14)
005ee105              *(esp_10 - 4) = fconvert.s(unimplemented  {fstp dword [esp+0x4]})
005ee105              unimplemented  {fstp dword [esp+0x4]}
005ee10d              *(esp_10 - 8) = fconvert.s(unimplemented  {fstp dword [esp]})
005ee10d              unimplemented  {fstp dword [esp]}
005ee110              *(esp_10 - 0xc) = esp_10 + 0x4c
005ee115              *(esp_10 - 0x10) = *(esp_10 + 0x24)
005ee116              edx_4()
005ee116              void* esp_21 = esp_10
005ee118              unimplemented  {fld dword [data_7e77b8]}
005ee11e              long double temp1_1 = fconvert.t(*(esp_21 + 0x4c))
005ee11e              unimplemented  {fcomp dword [esp+0x4c]} f- temp1_1
005ee11e              bool c0_1 = unimplemented  {fcomp dword [esp+0x4c]} f< temp1_1
005ee11e              bool c2_1 = is_unordered.t(unimplemented  {fcomp dword [esp+0x4c]}, temp1_1)
005ee11e              bool c3_1 = unimplemented  {fcomp dword [esp+0x4c]} f== temp1_1
005ee11e              unimplemented  {fcomp dword [esp+0x4c]}
005ee11e              top = top
005ee122              (c0_1 ? 1 : 0 << 8) | ((0 ? 1 : 0 << 9) | ((c2_1 ? 1 : 0 << 0xa) | ((c3_1 ? 1 : 0 << 0xe) | ((top & 7) << 0xb)))):1.b & 5
005ee124              bool p_1 = unimplemented  {test ah, 0x5}
005ee127              if (not(p_1))
005ee132                  if (((*(*(ebp_2 + 8) + 0x10) u>> 0xd).b & 1) == 0)
005ee137                      int32_t edx_5 = *(esp_21 + 0x30)
005ee13b                      int32_t edi_5 = *(esp_21 + 0x24)
005ee13f                      *(esp_21 - 4) = 9
005ee143                      *(esp_21 + 0x14) = 0
005ee148                      *(esp_21 + 0x18) = edx_5
005ee14c                      int32_t eax_16 = sub_434e10(eax_8)
005ee14c                      esp_21 = esp_21
005ee151                      if (eax_16 != 0)
005ee159                          *(esp_21 - 4) = esp_21 + 0x14
005ee15a                          *(esp_21 - 8) = 0
005ee15c                          *(esp_21 - 0xc) = eax_8
005ee15d                          *(esp_21 - 0x10) = edi_5
005ee15e                          eax_16()
005ee15e                          esp_21 = esp_21
005ee151                      if (eax_16 == 0)
005ee16f                          *(esp_21 - 4) = esp_21 + 0x14
005ee170                          *(esp_21 - 8) = 0
005ee172                          *(esp_21 - 0xc) = eax_8
005ee173                          *(esp_21 - 0x10) = edi_5
005ee173                          esp_21 = esp_21 - 0x10
005ee174                          sub_435300()
005ee151                      if (eax_16 != 0 || eax_16 == 0)
005ee179                          esp_21 = esp_21 + 0x10
005ee132                  if (((*(*(ebp_2 + 8) + 0x10) u>> 0xd).b & 1) != 0 || (((*(*(ebp_2 + 8) + 0x10) u>> 0xd).b & 1) == 0 && *(esp_21 + 0x14) == 0))
005ee188                      int32_t eax_18 = *(esp_21 + 0x24)
005ee18c                      int32_t edx_7 = *(**(ebp_2 + 0xc) + 0x2c)
005ee18f                      *(esp_21 - 4) = eax_8
005ee190                      *(esp_21 - 8) = eax_18
005ee191                      edx_7()
005ee191                      esp_21 = esp_21
005ee193                      void* ebp_3 = *(ebp_2 + 8)
005ee196                      *(ebp_3 + 0x10) = *(ebp_3 + 0x10) & 0xffffdfff
005ee1a1              *(esp_21 - 4) = *(esp_21 + 0x24)
005ee1a4              sub_434d70(eax_8)
005ee1ad              *(esp_21 - 4) = *(esp_21 + 0x30)
005ee1b0              sub_434d70(eax_8)
005ee1b0              esp_2 = esp_21
005ee1b9              *(esp_2 + 0x44) = 2
005ee1be              sub_560b90(esp_2 + 0x30)
005ee1c7              *(esp_2 + 0x44) = 0xffffffff
005ee1cf              sub_560b90(esp_2 + 0x24)
005ee1d8          eax = sub_539c20(esp_2 + 0x1c)
005ee1dd          edi_1 = *(esp_2 + 0x20)
005ee1e1          esi_2 = *(esp_2 + 0x1c)
005ee1ea      *esp_2
005ee1eb      *(esp_2 + 4)
005ee1eb      esp_1 = esp_2 + 8
005ee1f0  *esp_1
005ee1f1  *(esp_1 + 4)
005ee1f2  *fsbase = *(esp_1 + 0x34)
005ee1fc  return eax
