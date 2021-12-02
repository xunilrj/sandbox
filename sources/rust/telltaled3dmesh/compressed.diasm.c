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
