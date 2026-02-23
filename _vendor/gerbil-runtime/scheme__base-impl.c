#ifdef ___LINKER_INFO
; File: "scheme__base-impl.c", produced by Gambit v4.9.7
(
409007
(C)
"scheme__base-impl"
("scheme__base-impl")
()
(("scheme__base-impl"))
( #|*/"*/"symbols|#
"scheme/base-impl#file-error?"
"scheme/base-impl#r7rs-raise"
"scheme/base-impl#r7rs-with-exception-handler"
"scheme/base-impl#raise-continuable"
"scheme/base-impl#read-error?"
"scheme__base-impl"
"scheme__base-impl#"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"scheme__base-impl#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"scheme/base-impl#file-error?"
"scheme/base-impl#r7rs-raise"
"scheme/base-impl#r7rs-with-exception-handler"
"scheme/base-impl#raise-continuable"
"scheme/base-impl#read-error?"
"scheme/base-impl::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##primordial-exception-handler"
"##thread-end-with-uncaught-exception!"
"__with-exception-handler"
"current-exception-handler"
"datum-parsing-exception?"
"error"
"file-exists-exception?"
"no-such-file-or-directory-exception?"
"permission-denied-exception?"
"raise"
"with-exception-handler"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "scheme__base-impl"
#define ___LINKER_ID ___LNK_scheme____base_2d_impl
#define ___MH_PROC ___H_scheme____base_2d_impl
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 7
#define ___GLOCOUNT 18
#define ___SUPCOUNT 7
#define ___SUBCOUNT 5
#define ___LBLCOUNT 32
#define ___MODDESCR ___REF_SUB(2)
#include "gambit.h"

___NEED_SYM(___S_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___NEED_SYM(___S_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___NEED_SYM(___S_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___NEED_SYM(___S_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___NEED_SYM(___S_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___NEED_SYM(___S_scheme____base_2d_impl)
___NEED_SYM(___S_scheme____base_2d_impl_23_)

___NEED_GLO(___G__23__23_primordial_2d_exception_2d_handler)
___NEED_GLO(___G__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_)
___NEED_GLO(___G_____with_2d_exception_2d_handler)
___NEED_GLO(___G_current_2d_exception_2d_handler)
___NEED_GLO(___G_datum_2d_parsing_2d_exception_3f_)
___NEED_GLO(___G_error)
___NEED_GLO(___G_file_2d_exists_2d_exception_3f_)
___NEED_GLO(___G_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_)
___NEED_GLO(___G_permission_2d_denied_2d_exception_3f_)
___NEED_GLO(___G_raise)
___NEED_GLO(___G_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___NEED_GLO(___G_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___NEED_GLO(___G_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___NEED_GLO(___G_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___NEED_GLO(___G_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___NEED_GLO(___G_scheme_2f_base_2d_impl_3a__3a_timestamp)
___NEED_GLO(___G_scheme____base_2d_impl_23_)
___NEED_GLO(___G_with_2d_exception_2d_handler)

___BEGIN_SYM
___DEF_SYM(0,___S_scheme_2f_base_2d_impl_23_file_2d_error_3f_,"scheme/base-impl#file-error?")
___DEF_SYM(1,___S_scheme_2f_base_2d_impl_23_r7rs_2d_raise,"scheme/base-impl#r7rs-raise")
___DEF_SYM(2,___S_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,"scheme/base-impl#r7rs-with-exception-handler")

___DEF_SYM(3,___S_scheme_2f_base_2d_impl_23_raise_2d_continuable,"scheme/base-impl#raise-continuable")

___DEF_SYM(4,___S_scheme_2f_base_2d_impl_23_read_2d_error_3f_,"scheme/base-impl#read-error?")
___DEF_SYM(5,___S_scheme____base_2d_impl,"scheme__base-impl")
___DEF_SYM(6,___S_scheme____base_2d_impl_23_,"scheme__base-impl#")
___END_SYM

#define ___SYM_scheme_2f_base_2d_impl_23_file_2d_error_3f_ ___SYM(0,___S_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
#define ___SYM_scheme_2f_base_2d_impl_23_r7rs_2d_raise ___SYM(1,___S_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
#define ___SYM_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler ___SYM(2,___S_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
#define ___SYM_scheme_2f_base_2d_impl_23_raise_2d_continuable ___SYM(3,___S_scheme_2f_base_2d_impl_23_raise_2d_continuable)
#define ___SYM_scheme_2f_base_2d_impl_23_read_2d_error_3f_ ___SYM(4,___S_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
#define ___SYM_scheme____base_2d_impl ___SYM(5,___S_scheme____base_2d_impl)
#define ___SYM_scheme____base_2d_impl_23_ ___SYM(6,___S_scheme____base_2d_impl_23_)

___BEGIN_GLO
___DEF_GLO(0,"scheme/base-impl#file-error?")
___DEF_GLO(1,"scheme/base-impl#r7rs-raise")
___DEF_GLO(2,"scheme/base-impl#r7rs-with-exception-handler")

___DEF_GLO(3,"scheme/base-impl#raise-continuable")

___DEF_GLO(4,"scheme/base-impl#read-error?")
___DEF_GLO(5,"scheme/base-impl::timestamp")
___DEF_GLO(6,"scheme__base-impl#")
___DEF_GLO(7,"##primordial-exception-handler")
___DEF_GLO(8,"##thread-end-with-uncaught-exception!")

___DEF_GLO(9,"__with-exception-handler")
___DEF_GLO(10,"current-exception-handler")
___DEF_GLO(11,"datum-parsing-exception?")
___DEF_GLO(12,"error")
___DEF_GLO(13,"file-exists-exception?")
___DEF_GLO(14,"no-such-file-or-directory-exception?")

___DEF_GLO(15,"permission-denied-exception?")
___DEF_GLO(16,"raise")
___DEF_GLO(17,"with-exception-handler")
___END_GLO

#define ___GLO_scheme_2f_base_2d_impl_23_file_2d_error_3f_ ___GLO(0,___G_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
#define ___PRM_scheme_2f_base_2d_impl_23_file_2d_error_3f_ ___PRM(0,___G_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
#define ___GLO_scheme_2f_base_2d_impl_23_r7rs_2d_raise ___GLO(1,___G_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
#define ___PRM_scheme_2f_base_2d_impl_23_r7rs_2d_raise ___PRM(1,___G_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
#define ___GLO_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler ___GLO(2,___G_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
#define ___PRM_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler ___PRM(2,___G_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
#define ___GLO_scheme_2f_base_2d_impl_23_raise_2d_continuable ___GLO(3,___G_scheme_2f_base_2d_impl_23_raise_2d_continuable)
#define ___PRM_scheme_2f_base_2d_impl_23_raise_2d_continuable ___PRM(3,___G_scheme_2f_base_2d_impl_23_raise_2d_continuable)
#define ___GLO_scheme_2f_base_2d_impl_23_read_2d_error_3f_ ___GLO(4,___G_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
#define ___PRM_scheme_2f_base_2d_impl_23_read_2d_error_3f_ ___PRM(4,___G_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
#define ___GLO_scheme_2f_base_2d_impl_3a__3a_timestamp ___GLO(5,___G_scheme_2f_base_2d_impl_3a__3a_timestamp)
#define ___PRM_scheme_2f_base_2d_impl_3a__3a_timestamp ___PRM(5,___G_scheme_2f_base_2d_impl_3a__3a_timestamp)
#define ___GLO_scheme____base_2d_impl_23_ ___GLO(6,___G_scheme____base_2d_impl_23_)
#define ___PRM_scheme____base_2d_impl_23_ ___PRM(6,___G_scheme____base_2d_impl_23_)
#define ___GLO__23__23_primordial_2d_exception_2d_handler ___GLO(7,___G__23__23_primordial_2d_exception_2d_handler)
#define ___PRM__23__23_primordial_2d_exception_2d_handler ___PRM(7,___G__23__23_primordial_2d_exception_2d_handler)
#define ___GLO__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_ ___GLO(8,___G__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_)
#define ___PRM__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_ ___PRM(8,___G__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_)
#define ___GLO_____with_2d_exception_2d_handler ___GLO(9,___G_____with_2d_exception_2d_handler)
#define ___PRM_____with_2d_exception_2d_handler ___PRM(9,___G_____with_2d_exception_2d_handler)
#define ___GLO_current_2d_exception_2d_handler ___GLO(10,___G_current_2d_exception_2d_handler)
#define ___PRM_current_2d_exception_2d_handler ___PRM(10,___G_current_2d_exception_2d_handler)
#define ___GLO_datum_2d_parsing_2d_exception_3f_ ___GLO(11,___G_datum_2d_parsing_2d_exception_3f_)
#define ___PRM_datum_2d_parsing_2d_exception_3f_ ___PRM(11,___G_datum_2d_parsing_2d_exception_3f_)
#define ___GLO_error ___GLO(12,___G_error)
#define ___PRM_error ___PRM(12,___G_error)
#define ___GLO_file_2d_exists_2d_exception_3f_ ___GLO(13,___G_file_2d_exists_2d_exception_3f_)
#define ___PRM_file_2d_exists_2d_exception_3f_ ___PRM(13,___G_file_2d_exists_2d_exception_3f_)
#define ___GLO_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_ ___GLO(14,___G_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_)
#define ___PRM_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_ ___PRM(14,___G_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_)
#define ___GLO_permission_2d_denied_2d_exception_3f_ ___GLO(15,___G_permission_2d_denied_2d_exception_3f_)
#define ___PRM_permission_2d_denied_2d_exception_3f_ ___PRM(15,___G_permission_2d_denied_2d_exception_3f_)
#define ___GLO_raise ___GLO(16,___G_raise)
#define ___PRM_raise ___PRM(16,___G_raise)
#define ___GLO_with_2d_exception_2d_handler ___GLO(17,___G_with_2d_exception_2d_handler)
#define ___PRM_with_2d_exception_2d_handler ___PRM(17,___G_with_2d_exception_2d_handler)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c80L)
___DEF_SUB_STR(___X1,55UL)
               ___STR8(69,120,99,101,112,116,105,111)
               ___STR8(110,32,104,97,110,100,108,101)
               ___STR8(114,32,114,101,116,117,114,110)
               ___STR8(101,100,32,105,110,32,110,111)
               ___STR8(110,32,99,111,110,116,105,110)
               ___STR8(117,97,98,108,101,32,101,120)
               ___STR7(99,101,112,116,105,111,110)
___DEF_SUB_VEC(___X2,6UL)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SUB(4))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,1UL)
               ___VEC1(___REF_SYM(5,___S_scheme____base_2d_impl))
               ___VEC0
___DEF_SUB_VEC(___X4,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme____base_2d_impl_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L1_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L2_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L3_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L4_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L5_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L6_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L7_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L8_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL(___L9_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___DEF_M_HLBL(___L1_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL(___L1_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL(___L2_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL(___L3_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL(___L4_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL(___L5_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_M_HLBL(___L1_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_M_HLBL(___L2_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_M_HLBL(___L3_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_M_HLBL(___L4_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___DEF_M_HLBL(___L1_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme____base_2d_impl_23_
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme____base_2d_impl_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme____base_2d_impl_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_scheme____base_2d_impl_23_)
   ___SET_GLO(5,___G_scheme_2f_base_2d_impl_3a__3a_timestamp,___BIGFIX(0,1770925184LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L1_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L2_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L3_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L4_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L5_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L6_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L7_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L8_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___DEF_P_HLBL(___L9_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(0),10,___G_current_2d_exception_2d_handler)
___DEF_SLBL(2,___L2_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___SET_STK(-4,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(-4),5)
   ___ADD_CLO_ELEM(0,___R1)
   ___ADD_CLO_ELEM(1,___STK(-6))
   ___END_SETUP_CLO(2)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-4))
   ___SET_R0(___STK(-7))
   ___ADJFP(-4)
   ___CHECK_HEAP(3,4096)
___DEF_SLBL(3,___L3_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___POLL(4)
___DEF_SLBL(4,___L4_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),17,___G_with_2d_exception_2d_handler)
___DEF_SLBL(5,___L5_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(5,1,0,0)
   ___SET_STK(1,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(1),8)
   ___ADD_CLO_ELEM(0,___R1)
   ___ADD_CLO_ELEM(1,___CLO(___R4,2))
   ___END_SETUP_CLO(2)
   ___SET_R2(___STK(1))
   ___SET_R1(___CLO(___R4,1))
   ___ADJFP(1)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___POLL(7)
___DEF_SLBL(7,___L7_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),9,___G_____with_2d_exception_2d_handler)
___DEF_SLBL(8,___L8_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(8,0,0,0)
   ___SET_R1(___CLO(___R4,1))
   ___POLL(9)
___DEF_SLBL(9,___L9_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler)
   ___JUMPGENSAFE(___SET_NARGS(1),___CLO(___R4,2))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_base_2d_impl_23_raise_2d_continuable
#undef ___PH_LBL0
#define ___PH_LBL0 14
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___DEF_P_HLBL(___L1_scheme_2f_base_2d_impl_23_raise_2d_continuable)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_base_2d_impl_23_raise_2d_continuable)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_base_2d_impl_23_raise_2d_continuable)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_base_2d_impl_23_raise_2d_continuable)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G_raise)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise
#undef ___PH_LBL0
#define ___PH_LBL0 17
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_P_HLBL(___L1_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_P_HLBL(___L2_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_P_HLBL(___L3_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_P_HLBL(___L4_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___DEF_P_HLBL(___L5_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G_raise)
___DEF_SLBL(2,___L2_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(1))
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),12,___G_error)
___DEF_SLBL(3,___L3_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),7,___G__23__23_primordial_2d_exception_2d_handler)
___DEF_SLBL(4,___L4_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(5)
___DEF_SLBL(5,___L5_scheme_2f_base_2d_impl_23_r7rs_2d_raise)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),8,___G__23__23_thread_2d_end_2d_with_2d_uncaught_2d_exception_21_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 24
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_P_HLBL(___L1_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_P_HLBL(___L2_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_P_HLBL(___L3_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___DEF_P_HLBL(___L4_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),14,___G_no_2d_such_2d_file_2d_or_2d_directory_2d_exception_3f_)
___DEF_SLBL(2,___L2_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L5_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),13,___G_file_2d_exists_2d_exception_3f_)
___DEF_SLBL(3,___L3_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L5_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),15,___G_permission_2d_denied_2d_exception_3f_)
___DEF_GLBL(___L5_scheme_2f_base_2d_impl_23_file_2d_error_3f_)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_base_2d_impl_23_read_2d_error_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 30
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___DEF_P_HLBL(___L1_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_base_2d_impl_23_read_2d_error_3f_)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),11,___G_datum_2d_parsing_2d_exception_3f_)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_scheme____base_2d_impl_23_,___REF_SYM(6,___S_scheme____base_2d_impl_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_scheme____base_2d_impl_23_,0,-1)
,___DEF_LBL_INTRO(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___REF_SYM(2,___S_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler),___REF_FAL,10,0)
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,2,-1)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,1,2)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,0,2)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_scheme_2f_base_2d_impl_23_raise_2d_continuable,___REF_SYM(3,___S_scheme_2f_base_2d_impl_23_raise_2d_continuable),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_raise_2d_continuable,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_raise_2d_continuable,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___REF_SYM(1,___S_scheme_2f_base_2d_impl_23_r7rs_2d_raise),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_r7rs_2d_raise,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,___REF_SYM(0,___S_scheme_2f_base_2d_impl_23_file_2d_error_3f_),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_file_2d_error_3f_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H_scheme_2f_base_2d_impl_23_read_2d_error_3f_,___REF_SYM(4,___S_scheme_2f_base_2d_impl_23_read_2d_error_3f_),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_scheme_2f_base_2d_impl_23_read_2d_error_3f_,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_base_2d_impl_23_read_2d_error_3f_,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(6,___G_scheme____base_2d_impl_23_,1)
___DEF_MOD_PRM(2,___G_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,3)
___DEF_MOD_PRM(3,___G_scheme_2f_base_2d_impl_23_raise_2d_continuable,14)
___DEF_MOD_PRM(1,___G_scheme_2f_base_2d_impl_23_r7rs_2d_raise,17)
___DEF_MOD_PRM(0,___G_scheme_2f_base_2d_impl_23_file_2d_error_3f_,24)
___DEF_MOD_PRM(4,___G_scheme_2f_base_2d_impl_23_read_2d_error_3f_,30)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(6,___G_scheme____base_2d_impl_23_,1)
___DEF_MOD_GLO(2,___G_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,3)
___DEF_MOD_GLO(3,___G_scheme_2f_base_2d_impl_23_raise_2d_continuable,14)
___DEF_MOD_GLO(1,___G_scheme_2f_base_2d_impl_23_r7rs_2d_raise,17)
___DEF_MOD_GLO(0,___G_scheme_2f_base_2d_impl_23_file_2d_error_3f_,24)
___DEF_MOD_GLO(4,___G_scheme_2f_base_2d_impl_23_read_2d_error_3f_,30)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_scheme_2f_base_2d_impl_23_file_2d_error_3f_,"scheme/base-impl#file-error?")
___DEF_MOD_SYM(1,___S_scheme_2f_base_2d_impl_23_r7rs_2d_raise,"scheme/base-impl#r7rs-raise")
___DEF_MOD_SYM(2,___S_scheme_2f_base_2d_impl_23_r7rs_2d_with_2d_exception_2d_handler,"scheme/base-impl#r7rs-with-exception-handler")

___DEF_MOD_SYM(3,___S_scheme_2f_base_2d_impl_23_raise_2d_continuable,"scheme/base-impl#raise-continuable")

___DEF_MOD_SYM(4,___S_scheme_2f_base_2d_impl_23_read_2d_error_3f_,"scheme/base-impl#read-error?")
___DEF_MOD_SYM(5,___S_scheme____base_2d_impl,"scheme__base-impl")
___DEF_MOD_SYM(6,___S_scheme____base_2d_impl_23_,"scheme__base-impl#")
___END_MOD_SYM_KEY

#endif
