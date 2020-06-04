; ModuleID = 'main.cpp'
source_filename = "main.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%"class.std::__1::basic_ostream" = type { i32 (...)**, %"class.std::__1::basic_ios.base" }
%"class.std::__1::basic_ios.base" = type <{ %"class.std::__1::ios_base", %"class.std::__1::basic_ostream"*, i32 }>
%"class.std::__1::ios_base" = type { i32 (...)**, i32, i64, i64, i32, i32, i8*, i8*, void (i32, %"class.std::__1::ios_base"*, i32)**, i32*, i64, i64, i64*, i64, i64, i8**, i64, i64 }
%"class.std::__1::locale::id" = type <{ %"struct.std::__1::once_flag", i32, [4 x i8] }>
%"struct.std::__1::once_flag" = type { i64 }
%_Z15print_and_sleepv.Frame = type { void (%_Z15print_and_sleepv.Frame*)*, void (%_Z15print_and_sleepv.Frame*)*, %"class.cppcoro::detail::task_promise", i2, %"class.std::exception_ptr", %"class.std::__1::locale", %"class.std::__1::locale" }
%"class.cppcoro::detail::task_promise" = type { %"class.cppcoro::detail::task_promise_base", %"class.std::exception_ptr" }
%"class.cppcoro::detail::task_promise_base" = type { %"class.std::experimental::coroutines_v1::coroutine_handle.0" }
%"class.std::experimental::coroutines_v1::coroutine_handle.0" = type { i8* }
%"class.std::exception_ptr" = type { i8* }
%"class.std::__1::locale" = type { %"class.std::__1::locale::__imp"* }
%"class.std::__1::locale::__imp" = type opaque
%"class.cppcoro::task" = type { %"class.std::experimental::coroutines_v1::coroutine_handle" }
%"class.std::experimental::coroutines_v1::coroutine_handle" = type { %"class.std::experimental::coroutines_v1::coroutine_handle.0" }
%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry" = type { i8, %"class.std::__1::basic_ostream"* }
%"class.std::__1::basic_streambuf" = type { i32 (...)**, %"class.std::__1::locale", i8*, i8*, i8*, i8*, i8*, i8* }
%"class.std::__1::locale::facet" = type { %"class.std::__1::__shared_count" }
%"class.std::__1::__shared_count" = type { i32 (...)**, i64 }
%"class.std::__1::ctype" = type <{ %"class.std::__1::locale::facet", i16*, i8, [7 x i8] }>
%"class.std::__1::basic_string" = type { %"class.std::__1::__compressed_pair" }
%"class.std::__1::__compressed_pair" = type { %"struct.std::__1::__compressed_pair_elem" }
%"struct.std::__1::__compressed_pair_elem" = type { %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__rep" }
%"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__rep" = type { %union.anon }
%union.anon = type { %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__long" }
%"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__long" = type { i64, i64, i8* }
%"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short" = type { %union.anon.1, [23 x i8] }
%union.anon.1 = type { i8 }

$__clang_call_terminate = comdat any

$_ZNSt3__124__put_character_sequenceIcNS_11char_traitsIcEEEERNS_13basic_ostreamIT_T0_EES7_PKS4_m = comdat any

$_ZNSt3__116__pad_and_outputIcNS_11char_traitsIcEEEENS_19ostreambuf_iteratorIT_T0_EES6_PKS4_S8_S8_RNS_8ios_baseES4_ = comdat any

@_ZNSt3__14coutE = external dso_local global %"class.std::__1::basic_ostream", align 8
@.str = private unnamed_addr constant [12 x i8] c"sleeping...\00", align 1
@.str.1 = private unnamed_addr constant [7 x i8] c" done!\00", align 1
@_ZNSt3__15ctypeIcE2idE = external dso_local global %"class.std::__1::locale::id", align 8
@_Z15print_and_sleepv.resumers = private constant [3 x void (%_Z15print_and_sleepv.Frame*)*] [void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.resume, void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.destroy, void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.cleanup]

; Function Attrs: uwtable
define dso_local void @_Z15print_and_sleepv(%"class.cppcoro::task"* noalias nocapture sret %0) #0 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = tail call i8* @_Znwm(i64 64)
  %3 = bitcast i8* %2 to void (%_Z15print_and_sleepv.Frame*)**
  store void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.resume, void (%_Z15print_and_sleepv.Frame*)** %3, align 8
  %4 = select i1 true, void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.destroy, void (%_Z15print_and_sleepv.Frame*)* @_Z15print_and_sleepv.cleanup
  %5 = getelementptr inbounds i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to void (%_Z15print_and_sleepv.Frame*)**
  store void (%_Z15print_and_sleepv.Frame*)* %4, void (%_Z15print_and_sleepv.Frame*)** %6, align 8
  %7 = getelementptr inbounds i8, i8* %2, i64 16
  tail call void @llvm.lifetime.start.p0i8(i64 16, i8* nonnull %7) #2
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 8 dereferenceable(16) %7, i8 0, i64 16, i1 false)
  %8 = getelementptr inbounds %"class.cppcoro::task", %"class.cppcoro::task"* %0, i64 0, i32 0, i32 0, i32 0
  store i8* %2, i8** %8, align 8, !alias.scope !2
  %9 = getelementptr inbounds i8, i8* %2, i64 32
  %10 = bitcast i8* %9 to i2*
  store i2 0, i2* %10, align 1
  ret void
}

; Function Attrs: argmemonly nounwind readonly
declare token @llvm.coro.id(i32, i8* readnone, i8* nocapture readonly, i8*) #1

; Function Attrs: nounwind
declare i1 @llvm.coro.alloc(token) #2

; Function Attrs: nobuiltin nofree
declare dso_local noalias nonnull i8* @_Znwm(i64) local_unnamed_addr #3

; Function Attrs: nounwind
declare i8* @llvm.coro.begin(token, i8* writeonly) #2

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #4

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #4

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #4

declare dso_local i32 @__gxx_personality_v0(...)

declare dso_local i8* @__cxa_begin_catch(i8*) local_unnamed_addr

declare dso_local void @__cxa_end_catch() local_unnamed_addr

; Function Attrs: nobuiltin nounwind
declare dso_local void @_ZdlPv(i8*) local_unnamed_addr #5

; Function Attrs: noinline noreturn nounwind
define linkonce_odr hidden void @__clang_call_terminate(i8* %0) local_unnamed_addr #6 comdat {
  %2 = tail call i8* @__cxa_begin_catch(i8* %0) #2
  tail call void @_ZSt9terminatev() #10
  unreachable
}

declare dso_local void @_ZSt9terminatev() local_unnamed_addr

; Function Attrs: nounwind
declare dso_local void @_ZSt17current_exceptionv(%"class.std::exception_ptr"* sret) local_unnamed_addr #7

; Function Attrs: nounwind
declare dso_local dereferenceable(8) %"class.std::exception_ptr"* @_ZNSt13exception_ptraSERKS_(%"class.std::exception_ptr"*, %"class.std::exception_ptr"* dereferenceable(8)) local_unnamed_addr #7

; Function Attrs: nounwind
declare dso_local void @_ZNSt13exception_ptrD1Ev(%"class.std::exception_ptr"*) unnamed_addr #7

; Function Attrs: uwtable
define linkonce_odr dso_local dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__124__put_character_sequenceIcNS_11char_traitsIcEEEERNS_13basic_ostreamIT_T0_EES7_PKS4_m(%"class.std::__1::basic_ostream"* dereferenceable(160) %0, i8* %1, i64 %2) local_unnamed_addr #8 comdat personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %4 = alloca %"class.std::__1::locale", align 8
  %5 = alloca %"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry", align 8
  %6 = getelementptr inbounds %"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry", %"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"* %5, i64 0, i32 0
  call void @llvm.lifetime.start.p0i8(i64 16, i8* nonnull %6) #2
  invoke void @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryC1ERS3_(%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"* nonnull %5, %"class.std::__1::basic_ostream"* nonnull dereferenceable(160) %0)
          to label %7 unwind label %65

7:                                                ; preds = %3
  %8 = load i8, i8* %6, align 8, !tbaa !5, !range !11
  %9 = icmp eq i8 %8, 0
  br i1 %9, label %74, label %10

10:                                               ; preds = %7
  %11 = bitcast %"class.std::__1::basic_ostream"* %0 to i8**
  %12 = load i8*, i8** %11, align 8, !tbaa !12
  %13 = getelementptr i8, i8* %12, i64 -24
  %14 = bitcast i8* %13 to i64*
  %15 = load i64, i64* %14, align 8
  %16 = bitcast %"class.std::__1::basic_ostream"* %0 to i8*
  %17 = getelementptr inbounds i8, i8* %16, i64 %15
  %18 = getelementptr inbounds i8, i8* %17, i64 40
  %19 = bitcast i8* %18 to %"class.std::__1::basic_streambuf"**
  %20 = load %"class.std::__1::basic_streambuf"*, %"class.std::__1::basic_streambuf"** %19, align 8, !tbaa !14
  %21 = bitcast i8* %17 to %"class.std::__1::ios_base"*
  %22 = getelementptr inbounds i8, i8* %17, i64 8
  %23 = bitcast i8* %22 to i32*
  %24 = load i32, i32* %23, align 8, !tbaa !18
  %25 = getelementptr inbounds i8, i8* %1, i64 %2
  %26 = getelementptr inbounds i8, i8* %17, i64 144
  %27 = bitcast i8* %26 to i32*
  %28 = load i32, i32* %27, align 8, !tbaa !19
  %29 = icmp eq i32 %28, -1
  br i1 %29, label %30, label %45

30:                                               ; preds = %10
  %31 = bitcast %"class.std::__1::locale"* %4 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %31) #2
  invoke void @_ZNKSt3__18ios_base6getlocEv(%"class.std::__1::locale"* nonnull sret %4, %"class.std::__1::ios_base"* nonnull %21)
          to label %32 unwind label %72

32:                                               ; preds = %30
  %33 = invoke %"class.std::__1::locale::facet"* @_ZNKSt3__16locale9use_facetERNS0_2idE(%"class.std::__1::locale"* nonnull %4, %"class.std::__1::locale::id"* nonnull dereferenceable(16) @_ZNSt3__15ctypeIcE2idE)
          to label %34 unwind label %41

34:                                               ; preds = %32
  %35 = bitcast %"class.std::__1::locale::facet"* %33 to %"class.std::__1::ctype"*
  %36 = bitcast %"class.std::__1::locale::facet"* %33 to i8 (%"class.std::__1::ctype"*, i8)***
  %37 = load i8 (%"class.std::__1::ctype"*, i8)**, i8 (%"class.std::__1::ctype"*, i8)*** %36, align 8, !tbaa !12
  %38 = getelementptr inbounds i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %37, i64 7
  %39 = load i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %38, align 8
  %40 = invoke signext i8 %39(%"class.std::__1::ctype"* %35, i8 signext 32)
          to label %43 unwind label %41

41:                                               ; preds = %34, %32
  %42 = landingpad { i8*, i32 }
          catch i8* null
  call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %4) #2
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %31) #2
  br label %75

43:                                               ; preds = %34
  call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %4) #2
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %31) #2
  %44 = sext i8 %40 to i32
  store i32 %44, i32* %27, align 8, !tbaa !19
  br label %45

45:                                               ; preds = %43, %10
  %46 = phi i32 [ %44, %43 ], [ %28, %10 ]
  %47 = trunc i32 %46 to i8
  %48 = and i32 %24, 176
  %49 = icmp eq i32 %48, 32
  %50 = select i1 %49, i8* %25, i8* %1
  %51 = invoke %"class.std::__1::basic_streambuf"* @_ZNSt3__116__pad_and_outputIcNS_11char_traitsIcEEEENS_19ostreambuf_iteratorIT_T0_EES6_PKS4_S8_S8_RNS_8ios_baseES4_(%"class.std::__1::basic_streambuf"* %20, i8* %1, i8* %50, i8* %25, %"class.std::__1::ios_base"* nonnull dereferenceable(136) %21, i8 signext %47)
          to label %52 unwind label %72

52:                                               ; preds = %45
  %53 = icmp eq %"class.std::__1::basic_streambuf"* %51, null
  br i1 %53, label %54, label %74

54:                                               ; preds = %52
  %55 = load i8*, i8** %11, align 8, !tbaa !12
  %56 = getelementptr i8, i8* %55, i64 -24
  %57 = bitcast i8* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = getelementptr inbounds i8, i8* %16, i64 %58
  %60 = bitcast i8* %59 to %"class.std::__1::ios_base"*
  %61 = getelementptr inbounds i8, i8* %59, i64 32
  %62 = bitcast i8* %61 to i32*
  %63 = load i32, i32* %62, align 8, !tbaa !21
  %64 = or i32 %63, 5
  invoke void @_ZNSt3__18ios_base5clearEj(%"class.std::__1::ios_base"* nonnull %60, i32 %64)
          to label %74 unwind label %70

65:                                               ; preds = %3
  %66 = landingpad { i8*, i32 }
          catch i8* null
  %67 = extractvalue { i8*, i32 } %66, 0
  %68 = bitcast %"class.std::__1::basic_ostream"* %0 to i8**
  %69 = bitcast %"class.std::__1::basic_ostream"* %0 to i8*
  br label %78

70:                                               ; preds = %54
  %71 = landingpad { i8*, i32 }
          catch i8* null
  br label %75

72:                                               ; preds = %30, %45
  %73 = landingpad { i8*, i32 }
          catch i8* null
  br label %75

74:                                               ; preds = %7, %54, %52
  call void @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryD1Ev(%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"* nonnull %5) #2
  call void @llvm.lifetime.end.p0i8(i64 16, i8* nonnull %6) #2
  br label %90

75:                                               ; preds = %72, %41, %70
  %76 = phi { i8*, i32 } [ %71, %70 ], [ %73, %72 ], [ %42, %41 ]
  %77 = extractvalue { i8*, i32 } %76, 0
  call void @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryD1Ev(%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"* nonnull %5) #2
  br label %78

78:                                               ; preds = %75, %65
  %79 = phi i8* [ %16, %75 ], [ %69, %65 ]
  %80 = phi i8** [ %11, %75 ], [ %68, %65 ]
  %81 = phi i8* [ %77, %75 ], [ %67, %65 ]
  call void @llvm.lifetime.end.p0i8(i64 16, i8* nonnull %6) #2
  %82 = call i8* @__cxa_begin_catch(i8* %81) #2
  %83 = load i8*, i8** %80, align 8, !tbaa !12
  %84 = getelementptr i8, i8* %83, i64 -24
  %85 = bitcast i8* %84 to i64*
  %86 = load i64, i64* %85, align 8
  %87 = getelementptr inbounds i8, i8* %79, i64 %86
  %88 = bitcast i8* %87 to %"class.std::__1::ios_base"*
  invoke void @_ZNSt3__18ios_base33__set_badbit_and_consider_rethrowEv(%"class.std::__1::ios_base"* nonnull %88)
          to label %89 unwind label %91

89:                                               ; preds = %78
  call void @__cxa_end_catch()
  br label %90

90:                                               ; preds = %89, %74
  ret %"class.std::__1::basic_ostream"* %0

91:                                               ; preds = %78
  %92 = landingpad { i8*, i32 }
          cleanup
  invoke void @__cxa_end_catch()
          to label %93 unwind label %94

93:                                               ; preds = %91
  resume { i8*, i32 } %92

94:                                               ; preds = %91
  %95 = landingpad { i8*, i32 }
          catch i8* null
  %96 = extractvalue { i8*, i32 } %95, 0
  call void @__clang_call_terminate(i8* %96) #10
  unreachable
}

declare dso_local void @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryC1ERS3_(%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"*, %"class.std::__1::basic_ostream"* dereferenceable(160)) unnamed_addr #9

; Function Attrs: uwtable
define linkonce_odr hidden %"class.std::__1::basic_streambuf"* @_ZNSt3__116__pad_and_outputIcNS_11char_traitsIcEEEENS_19ostreambuf_iteratorIT_T0_EES6_PKS4_S8_S8_RNS_8ios_baseES4_(%"class.std::__1::basic_streambuf"* %0, i8* %1, i8* %2, i8* %3, %"class.std::__1::ios_base"* dereferenceable(136) %4, i8 signext %5) local_unnamed_addr #8 comdat personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %7 = alloca %"class.std::__1::basic_string", align 8
  %8 = icmp eq %"class.std::__1::basic_streambuf"* %0, null
  br i1 %8, label %90, label %9

9:                                                ; preds = %6
  %10 = ptrtoint i8* %3 to i64
  %11 = ptrtoint i8* %1 to i64
  %12 = sub i64 %10, %11
  %13 = getelementptr inbounds %"class.std::__1::ios_base", %"class.std::__1::ios_base"* %4, i64 0, i32 3
  %14 = load i64, i64* %13, align 8, !tbaa !22
  %15 = icmp sgt i64 %14, %12
  %16 = sub nsw i64 %14, %12
  %17 = select i1 %15, i64 %16, i64 0
  %18 = ptrtoint i8* %2 to i64
  %19 = sub i64 %18, %11
  %20 = icmp sgt i64 %19, 0
  br i1 %20, label %21, label %28

21:                                               ; preds = %9
  %22 = bitcast %"class.std::__1::basic_streambuf"* %0 to i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)***
  %23 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)**, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*** %22, align 8, !tbaa !12
  %24 = getelementptr inbounds i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %23, i64 12
  %25 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %24, align 8
  %26 = tail call i64 %25(%"class.std::__1::basic_streambuf"* nonnull %0, i8* %1, i64 %19)
  %27 = icmp eq i64 %26, %19
  br i1 %27, label %28, label %90

28:                                               ; preds = %21, %9
  %29 = icmp sgt i64 %17, 0
  br i1 %29, label %30, label %79

30:                                               ; preds = %28
  %31 = bitcast %"class.std::__1::basic_string"* %7 to i8*
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %31) #2
  %32 = icmp ult i64 %17, 23
  br i1 %32, label %42, label %33

33:                                               ; preds = %30
  %34 = add nuw i64 %17, 16
  %35 = and i64 %34, -16
  %36 = tail call i8* @_Znwm(i64 %35) #11
  %37 = getelementptr inbounds %"class.std::__1::basic_string", %"class.std::__1::basic_string"* %7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0, i32 2
  store i8* %36, i8** %37, align 8, !tbaa !23
  %38 = or i64 %35, 1
  %39 = getelementptr inbounds %"class.std::__1::basic_string", %"class.std::__1::basic_string"* %7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0, i32 0
  store i64 %38, i64* %39, align 8, !tbaa !23
  %40 = getelementptr inbounds %"class.std::__1::basic_string", %"class.std::__1::basic_string"* %7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0, i32 1
  store i64 %17, i64* %40, align 8, !tbaa !23
  %41 = bitcast %"class.std::__1::basic_string"* %7 to %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"*
  br label %47

42:                                               ; preds = %30
  %43 = trunc i64 %17 to i8
  %44 = shl nuw nsw i8 %43, 1
  store i8 %44, i8* %31, align 8, !tbaa !23
  %45 = bitcast %"class.std::__1::basic_string"* %7 to %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"*
  %46 = getelementptr inbounds %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short", %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"* %45, i64 0, i32 1, i64 0
  br label %47

47:                                               ; preds = %33, %42
  %48 = phi %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"* [ %41, %33 ], [ %45, %42 ]
  %49 = phi i8* [ %36, %33 ], [ %46, %42 ]
  call void @llvm.memset.p0i8.i64(i8* nonnull align 1 %49, i8 %5, i64 %17, i1 false) #2
  %50 = getelementptr inbounds i8, i8* %49, i64 %17
  store i8 0, i8* %50, align 1, !tbaa !23
  %51 = load i8, i8* %31, align 8, !tbaa !23
  %52 = and i8 %51, 1
  %53 = icmp eq i8 %52, 0
  %54 = getelementptr inbounds %"class.std::__1::basic_string", %"class.std::__1::basic_string"* %7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0, i32 2
  %55 = load i8*, i8** %54, align 8
  %56 = getelementptr inbounds %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short", %"struct.std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"* %48, i64 0, i32 1, i64 0
  %57 = select i1 %53, i8* %56, i8* %55
  %58 = bitcast %"class.std::__1::basic_streambuf"* %0 to i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)***
  %59 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)**, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*** %58, align 8, !tbaa !12
  %60 = getelementptr inbounds i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %59, i64 12
  %61 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %60, align 8
  %62 = invoke i64 %61(%"class.std::__1::basic_streambuf"* nonnull %0, i8* %57, i64 %17)
          to label %63 unwind label %71

63:                                               ; preds = %47
  %64 = icmp eq i64 %62, %17
  %65 = load i8, i8* %31, align 8, !tbaa !23
  %66 = and i8 %65, 1
  %67 = icmp eq i8 %66, 0
  br i1 %67, label %70, label %68

68:                                               ; preds = %63
  %69 = load i8*, i8** %54, align 8, !tbaa !23
  call void @_ZdlPv(i8* %69) #12
  br label %70

70:                                               ; preds = %63, %68
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %31) #2
  br i1 %64, label %79, label %90

71:                                               ; preds = %47
  %72 = landingpad { i8*, i32 }
          cleanup
  %73 = load i8, i8* %31, align 8, !tbaa !23
  %74 = and i8 %73, 1
  %75 = icmp eq i8 %74, 0
  br i1 %75, label %78, label %76

76:                                               ; preds = %71
  %77 = load i8*, i8** %54, align 8, !tbaa !23
  call void @_ZdlPv(i8* %77) #12
  br label %78

78:                                               ; preds = %71, %76
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %31) #2
  resume { i8*, i32 } %72

79:                                               ; preds = %70, %28
  %80 = sub i64 %10, %18
  %81 = icmp sgt i64 %80, 0
  br i1 %81, label %82, label %89

82:                                               ; preds = %79
  %83 = bitcast %"class.std::__1::basic_streambuf"* %0 to i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)***
  %84 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)**, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*** %83, align 8, !tbaa !12
  %85 = getelementptr inbounds i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %84, i64 12
  %86 = load i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)*, i64 (%"class.std::__1::basic_streambuf"*, i8*, i64)** %85, align 8
  %87 = call i64 %86(%"class.std::__1::basic_streambuf"* nonnull %0, i8* %2, i64 %80)
  %88 = icmp eq i64 %87, %80
  br i1 %88, label %89, label %90

89:                                               ; preds = %82, %79
  store i64 0, i64* %13, align 8, !tbaa !22
  br label %90

90:                                               ; preds = %89, %70, %21, %82, %6
  %91 = phi %"class.std::__1::basic_streambuf"* [ null, %6 ], [ %0, %89 ], [ null, %70 ], [ null, %21 ], [ null, %82 ]
  ret %"class.std::__1::basic_streambuf"* %91
}

; Function Attrs: nounwind
declare dso_local void @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryD1Ev(%"class.std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"*) unnamed_addr #7

declare dso_local void @_ZNSt3__18ios_base33__set_badbit_and_consider_rethrowEv(%"class.std::__1::ios_base"*) local_unnamed_addr #9

declare dso_local void @_ZNKSt3__18ios_base6getlocEv(%"class.std::__1::locale"* sret, %"class.std::__1::ios_base"*) local_unnamed_addr #9

; Function Attrs: nounwind
declare dso_local void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"*) unnamed_addr #7

declare dso_local %"class.std::__1::locale::facet"* @_ZNKSt3__16locale9use_facetERNS0_2idE(%"class.std::__1::locale"*, %"class.std::__1::locale::id"* dereferenceable(16)) local_unnamed_addr #9

declare dso_local void @_ZNSt3__18ios_base5clearEj(%"class.std::__1::ios_base"*, i32) local_unnamed_addr #9

declare dso_local dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE3putEc(%"class.std::__1::basic_ostream"*, i8 signext) local_unnamed_addr #9

declare dso_local dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE5flushEv(%"class.std::__1::basic_ostream"*) local_unnamed_addr #9

; Function Attrs: argmemonly nounwind readonly
declare i8* @llvm.coro.subfn.addr(i8* nocapture readonly, i8) #1

; Function Attrs: uwtable
define internal fastcc void @_Z15print_and_sleepv.resume(%_Z15print_and_sleepv.Frame* noalias nonnull %0) #0 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 3
  %3 = load i2, i2* %2, align 1
  %4 = icmp eq i2 %3, 0
  br i1 %4, label %5, label %51

5:                                                ; preds = %1
  %6 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__124__put_character_sequenceIcNS_11char_traitsIcEEEERNS_13basic_ostreamIT_T0_EES7_PKS4_m(%"class.std::__1::basic_ostream"* nonnull dereferenceable(160) @_ZNSt3__14coutE, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str, i64 0, i64 0), i64 11)
          to label %7 unwind label %33

7:                                                ; preds = %5
  %8 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 5
  %9 = bitcast %"class.std::__1::basic_ostream"* %6 to i8**
  %10 = load i8*, i8** %9, align 8, !tbaa !12
  %11 = getelementptr i8, i8* %10, i64 -24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = bitcast %"class.std::__1::basic_ostream"* %6 to i8*
  %15 = getelementptr inbounds i8, i8* %14, i64 %13
  %16 = bitcast %"class.std::__1::locale"* %8 to i8*
  tail call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %16) #2
  %17 = bitcast i8* %15 to %"class.std::__1::ios_base"*
  invoke void @_ZNKSt3__18ios_base6getlocEv(%"class.std::__1::locale"* nonnull sret %8, %"class.std::__1::ios_base"* nonnull %17)
          to label %18 unwind label %33

18:                                               ; preds = %7
  %19 = invoke %"class.std::__1::locale::facet"* @_ZNKSt3__16locale9use_facetERNS0_2idE(%"class.std::__1::locale"* nonnull %8, %"class.std::__1::locale::id"* nonnull dereferenceable(16) @_ZNSt3__15ctypeIcE2idE)
          to label %20 unwind label %27

20:                                               ; preds = %18
  %21 = bitcast %"class.std::__1::locale::facet"* %19 to %"class.std::__1::ctype"*
  %22 = bitcast %"class.std::__1::locale::facet"* %19 to i8 (%"class.std::__1::ctype"*, i8)***
  %23 = load i8 (%"class.std::__1::ctype"*, i8)**, i8 (%"class.std::__1::ctype"*, i8)*** %22, align 8, !tbaa !12
  %24 = getelementptr inbounds i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %23, i64 7
  %25 = load i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %24, align 8
  %26 = invoke signext i8 %25(%"class.std::__1::ctype"* %21, i8 signext 10)
          to label %29 unwind label %27

27:                                               ; preds = %20, %18
  %28 = landingpad { i8*, i32 }
          catch i8* null
  tail call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %8) #2
  tail call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %16) #2
  br label %35

29:                                               ; preds = %20
  tail call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %8) #2
  tail call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %16) #2
  %30 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE3putEc(%"class.std::__1::basic_ostream"* nonnull %6, i8 signext %26)
          to label %31 unwind label %33

31:                                               ; preds = %29
  %32 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE5flushEv(%"class.std::__1::basic_ostream"* nonnull %6)
          to label %79 unwind label %33

33:                                               ; preds = %77, %75, %53, %51, %31, %29, %7, %5
  %34 = landingpad { i8*, i32 }
          catch i8* null
  br label %35

35:                                               ; preds = %73, %33, %27
  %36 = phi { i8*, i32 } [ %74, %73 ], [ %34, %33 ], [ %28, %27 ]
  %37 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 4
  %38 = extractvalue { i8*, i32 } %36, 0
  %39 = tail call i8* @__cxa_begin_catch(i8* %38) #2
  %40 = bitcast %"class.std::exception_ptr"* %37 to i8*
  tail call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %40) #2
  tail call void @_ZSt17current_exceptionv(%"class.std::exception_ptr"* nonnull sret %37) #2
  %41 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2, i32 1
  %42 = tail call dereferenceable(8) %"class.std::exception_ptr"* @_ZNSt13exception_ptraSERKS_(%"class.std::exception_ptr"* nonnull %41, %"class.std::exception_ptr"* nonnull dereferenceable(8) %37) #2
  tail call void @_ZNSt13exception_ptrD1Ev(%"class.std::exception_ptr"* nonnull %37) #2
  tail call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %40) #2
  tail call void @__cxa_end_catch()
  br label %43

43:                                               ; preds = %35, %77
  %44 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 0
  store void (%_Z15print_and_sleepv.Frame*)* null, void (%_Z15print_and_sleepv.Frame*)** %44, align 8
  %45 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2, i32 0, i32 0, i32 0
  %46 = load i8*, i8** %45, align 8, !tbaa.struct !24
  %47 = bitcast i8* %46 to { i8*, i8* }*
  %48 = getelementptr inbounds { i8*, i8* }, { i8*, i8* }* %47, i32 0, i32 0
  %49 = load i8*, i8** %48
  %50 = bitcast i8* %49 to void (i8*)*
  musttail call fastcc void %50(i8* %46)
  ret void

51:                                               ; preds = %1
  %52 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__124__put_character_sequenceIcNS_11char_traitsIcEEEERNS_13basic_ostreamIT_T0_EES7_PKS4_m(%"class.std::__1::basic_ostream"* nonnull dereferenceable(160) @_ZNSt3__14coutE, i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i64 0, i64 0), i64 6)
          to label %53 unwind label %33

53:                                               ; preds = %51
  %54 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 6
  %55 = bitcast %"class.std::__1::basic_ostream"* %52 to i8**
  %56 = load i8*, i8** %55, align 8, !tbaa !12
  %57 = getelementptr i8, i8* %56, i64 -24
  %58 = bitcast i8* %57 to i64*
  %59 = load i64, i64* %58, align 8
  %60 = bitcast %"class.std::__1::basic_ostream"* %52 to i8*
  %61 = getelementptr inbounds i8, i8* %60, i64 %59
  %62 = bitcast %"class.std::__1::locale"* %54 to i8*
  tail call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %62) #2
  %63 = bitcast i8* %61 to %"class.std::__1::ios_base"*
  invoke void @_ZNKSt3__18ios_base6getlocEv(%"class.std::__1::locale"* nonnull sret %54, %"class.std::__1::ios_base"* nonnull %63)
          to label %64 unwind label %33

64:                                               ; preds = %53
  %65 = invoke %"class.std::__1::locale::facet"* @_ZNKSt3__16locale9use_facetERNS0_2idE(%"class.std::__1::locale"* nonnull %54, %"class.std::__1::locale::id"* nonnull dereferenceable(16) @_ZNSt3__15ctypeIcE2idE)
          to label %66 unwind label %73

66:                                               ; preds = %64
  %67 = bitcast %"class.std::__1::locale::facet"* %65 to %"class.std::__1::ctype"*
  %68 = bitcast %"class.std::__1::locale::facet"* %65 to i8 (%"class.std::__1::ctype"*, i8)***
  %69 = load i8 (%"class.std::__1::ctype"*, i8)**, i8 (%"class.std::__1::ctype"*, i8)*** %68, align 8, !tbaa !12
  %70 = getelementptr inbounds i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %69, i64 7
  %71 = load i8 (%"class.std::__1::ctype"*, i8)*, i8 (%"class.std::__1::ctype"*, i8)** %70, align 8
  %72 = invoke signext i8 %71(%"class.std::__1::ctype"* %67, i8 signext 10)
          to label %75 unwind label %73

73:                                               ; preds = %66, %64
  %74 = landingpad { i8*, i32 }
          catch i8* null
  tail call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %54) #2
  tail call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %62) #2
  br label %35

75:                                               ; preds = %66
  tail call void @_ZNSt3__16localeD1Ev(%"class.std::__1::locale"* nonnull %54) #2
  tail call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %62) #2
  %76 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE3putEc(%"class.std::__1::basic_ostream"* nonnull %52, i8 signext %72)
          to label %77 unwind label %33

77:                                               ; preds = %75
  %78 = invoke dereferenceable(160) %"class.std::__1::basic_ostream"* @_ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE5flushEv(%"class.std::__1::basic_ostream"* nonnull %52)
          to label %43 unwind label %33

79:                                               ; preds = %31
  store i2 1, i2* %2, align 1
  ret void
}

; Function Attrs: uwtable
define internal fastcc void @_Z15print_and_sleepv.destroy(%_Z15print_and_sleepv.Frame* noalias nonnull %0) #0 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = bitcast %_Z15print_and_sleepv.Frame* %0 to i8*
  %3 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2
  %4 = bitcast %"class.cppcoro::detail::task_promise"* %3 to i8*
  %5 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2, i32 1
  tail call void @_ZNSt13exception_ptrD1Ev(%"class.std::exception_ptr"* nonnull %5) #2
  tail call void @llvm.lifetime.end.p0i8(i64 16, i8* nonnull %4) #2
  tail call void @_ZdlPv(i8* nonnull %2) #2
  ret void
}

; Function Attrs: uwtable
define internal fastcc void @_Z15print_and_sleepv.cleanup(%_Z15print_and_sleepv.Frame* noalias nonnull %0) #0 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2
  %3 = bitcast %"class.cppcoro::detail::task_promise"* %2 to i8*
  %4 = getelementptr inbounds %_Z15print_and_sleepv.Frame, %_Z15print_and_sleepv.Frame* %0, i64 0, i32 2, i32 1
  tail call void @_ZNSt13exception_ptrD1Ev(%"class.std::exception_ptr"* nonnull %4) #2
  tail call void @llvm.lifetime.end.p0i8(i64 16, i8* nonnull %3) #2
  ret void
}

attributes #0 = { uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind readonly }
attributes #2 = { nounwind }
attributes #3 = { nobuiltin nofree "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind willreturn }
attributes #5 = { nobuiltin nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { noinline noreturn nounwind }
attributes #7 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #9 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #10 = { noreturn nounwind }
attributes #11 = { builtin }
attributes #12 = { builtin nounwind }

!llvm.linker.options = !{}
!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 10.0.0-4ubuntu1 "}
!2 = !{!3}
!3 = distinct !{!3, !4, !"_ZN7cppcoro6detail12task_promiseIvE17get_return_objectEv: argument 0"}
!4 = distinct !{!4, !"_ZN7cppcoro6detail12task_promiseIvE17get_return_objectEv"}
!5 = !{!6, !7, i64 0}
!6 = !{!"_ZTSNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryE", !7, i64 0, !10, i64 8}
!7 = !{!"bool", !8, i64 0}
!8 = !{!"omnipotent char", !9, i64 0}
!9 = !{!"Simple C++ TBAA"}
!10 = !{!"any pointer", !8, i64 0}
!11 = !{i8 0, i8 2}
!12 = !{!13, !13, i64 0}
!13 = !{!"vtable pointer", !9, i64 0}
!14 = !{!15, !10, i64 40}
!15 = !{!"_ZTSNSt3__18ios_baseE", !16, i64 8, !17, i64 16, !17, i64 24, !16, i64 32, !16, i64 36, !10, i64 40, !10, i64 48, !10, i64 56, !10, i64 64, !17, i64 72, !17, i64 80, !10, i64 88, !17, i64 96, !17, i64 104, !10, i64 112, !17, i64 120, !17, i64 128}
!16 = !{!"int", !8, i64 0}
!17 = !{!"long", !8, i64 0}
!18 = !{!15, !16, i64 8}
!19 = !{!20, !16, i64 144}
!20 = !{!"_ZTSNSt3__19basic_iosIcNS_11char_traitsIcEEEE", !10, i64 136, !16, i64 144}
!21 = !{!15, !16, i64 32}
!22 = !{!15, !17, i64 24}
!23 = !{!8, !8, i64 0}
!24 = !{i64 0, i64 8, !25}
!25 = !{!10, !10, i64 0}
