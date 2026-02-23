#ifdef ___LINKER_INFO
; File: "scheme__load-impl.c", produced by Gambit v4.9.7
(
409007
(C)
"scheme__load-impl"
("scheme__load-impl")
()
(("scheme__load-impl"))
( #|*/"*/"symbols|#
"scheme/load-impl#r7rs-load"
"scheme/load-impl#r7rs-load__%"
"scheme/load-impl#r7rs-load__0"
"scheme__load-impl"
"scheme__load-impl#"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"scheme/load-impl#r7rs-load__%"
"scheme/load-impl#r7rs-load__0"
"scheme__load-impl#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"scheme/load-impl#r7rs-load"
"scheme/load-impl::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##length"
"##load"
"##raise-wrong-number-of-arguments-exception"
"apply"
"call-with-parameters__1"
"gx#current-expander-context"
"scheme/repl-impl#r7rs-interaction-environment"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "scheme__load-impl"
#define ___LINKER_ID ___LNK_scheme____load_2d_impl
#define ___MH_PROC ___H_scheme____load_2d_impl
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 5
#define ___GLOCOUNT 12
#define ___SUPCOUNT 5
#define ___SUBCOUNT 4
#define ___LBLCOUNT 20
#define ___MODDESCR ___REF_SUB(1)
#include "gambit.h"

___NEED_SYM(___S_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___NEED_SYM(___S_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___NEED_SYM(___S_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___NEED_SYM(___S_scheme____load_2d_impl)
___NEED_SYM(___S_scheme____load_2d_impl_23_)

___NEED_GLO(___G__23__23_length)
___NEED_GLO(___G__23__23_load)
___NEED_GLO(___G__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception)
___NEED_GLO(___G_apply)
___NEED_GLO(___G_call_2d_with_2d_parameters____1)
___NEED_GLO(___G_gx_23_current_2d_expander_2d_context)
___NEED_GLO(___G_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___NEED_GLO(___G_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___NEED_GLO(___G_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___NEED_GLO(___G_scheme_2f_load_2d_impl_3a__3a_timestamp)
___NEED_GLO(___G_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment)
___NEED_GLO(___G_scheme____load_2d_impl_23_)

___BEGIN_SYM
___DEF_SYM(0,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load,"scheme/load-impl#r7rs-load")
___DEF_SYM(1,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,"scheme/load-impl#r7rs-load__%")

___DEF_SYM(2,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,"scheme/load-impl#r7rs-load__0")

___DEF_SYM(3,___S_scheme____load_2d_impl,"scheme__load-impl")
___DEF_SYM(4,___S_scheme____load_2d_impl_23_,"scheme__load-impl#")
___END_SYM

#define ___SYM_scheme_2f_load_2d_impl_23_r7rs_2d_load ___SYM(0,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load)
#define ___SYM_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_ ___SYM(1,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
#define ___SYM_scheme_2f_load_2d_impl_23_r7rs_2d_load____0 ___SYM(2,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
#define ___SYM_scheme____load_2d_impl ___SYM(3,___S_scheme____load_2d_impl)
#define ___SYM_scheme____load_2d_impl_23_ ___SYM(4,___S_scheme____load_2d_impl_23_)

___BEGIN_GLO
___DEF_GLO(0,"scheme/load-impl#r7rs-load")
___DEF_GLO(1,"scheme/load-impl#r7rs-load__%")
___DEF_GLO(2,"scheme/load-impl#r7rs-load__0")
___DEF_GLO(3,"scheme/load-impl::timestamp")
___DEF_GLO(4,"scheme__load-impl#")
___DEF_GLO(5,"##length")
___DEF_GLO(6,"##load")
___DEF_GLO(7,"##raise-wrong-number-of-arguments-exception")

___DEF_GLO(8,"apply")
___DEF_GLO(9,"call-with-parameters__1")
___DEF_GLO(10,"gx#current-expander-context")
___DEF_GLO(11,"scheme/repl-impl#r7rs-interaction-environment")

___END_GLO

#define ___GLO_scheme_2f_load_2d_impl_23_r7rs_2d_load ___GLO(0,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load)
#define ___PRM_scheme_2f_load_2d_impl_23_r7rs_2d_load ___PRM(0,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load)
#define ___GLO_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_ ___GLO(1,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
#define ___PRM_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_ ___PRM(1,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
#define ___GLO_scheme_2f_load_2d_impl_23_r7rs_2d_load____0 ___GLO(2,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
#define ___PRM_scheme_2f_load_2d_impl_23_r7rs_2d_load____0 ___PRM(2,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
#define ___GLO_scheme_2f_load_2d_impl_3a__3a_timestamp ___GLO(3,___G_scheme_2f_load_2d_impl_3a__3a_timestamp)
#define ___PRM_scheme_2f_load_2d_impl_3a__3a_timestamp ___PRM(3,___G_scheme_2f_load_2d_impl_3a__3a_timestamp)
#define ___GLO_scheme____load_2d_impl_23_ ___GLO(4,___G_scheme____load_2d_impl_23_)
#define ___PRM_scheme____load_2d_impl_23_ ___PRM(4,___G_scheme____load_2d_impl_23_)
#define ___GLO__23__23_length ___GLO(5,___G__23__23_length)
#define ___PRM__23__23_length ___PRM(5,___G__23__23_length)
#define ___GLO__23__23_load ___GLO(6,___G__23__23_load)
#define ___PRM__23__23_load ___PRM(6,___G__23__23_load)
#define ___GLO__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception ___GLO(7,___G__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception)
#define ___PRM__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception ___PRM(7,___G__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception)
#define ___GLO_apply ___GLO(8,___G_apply)
#define ___PRM_apply ___PRM(8,___G_apply)
#define ___GLO_call_2d_with_2d_parameters____1 ___GLO(9,___G_call_2d_with_2d_parameters____1)
#define ___PRM_call_2d_with_2d_parameters____1 ___PRM(9,___G_call_2d_with_2d_parameters____1)
#define ___GLO_gx_23_current_2d_expander_2d_context ___GLO(10,___G_gx_23_current_2d_expander_2d_context)
#define ___PRM_gx_23_current_2d_expander_2d_context ___PRM(10,___G_gx_23_current_2d_expander_2d_context)
#define ___GLO_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment ___GLO(11,___G_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment)
#define ___PRM_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment ___PRM(11,___G_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c81L)
___DEF_SUB_VEC(___X1,6UL)
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X2,1UL)
               ___VEC1(___REF_SYM(3,___S_scheme____load_2d_impl))
               ___VEC0
___DEF_SUB_VEC(___X3,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme____load_2d_impl_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL(___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL(___L5_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_M_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_M_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_M_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_M_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_M_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_M_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_M_HLBL(___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme____load_2d_impl_23_
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
___DEF_P_HLBL(___L0_scheme____load_2d_impl_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme____load_2d_impl_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_scheme____load_2d_impl_23_)
   ___SET_GLO(3,___G_scheme_2f_load_2d_impl_3a__3a_timestamp,___BIGFIX(0,1770925185LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_P_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_P_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_P_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_P_HLBL(___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___DEF_P_HLBL(___L5_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),3)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R3(___R2)
   ___SET_R1(___STK(1))
   ___SET_R2(___GLO_gx_23_current_2d_expander_2d_context)
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___POLL(2)
___DEF_SLBL(2,___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),9,___G_call_2d_with_2d_parameters____1)
___DEF_SLBL(3,___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(3,0,0,0)
   ___SET_STK(1,___CLO(___R4,1))
   ___SET_STK(2,___LBL(5))
   ___SET_R3(___FAL)
   ___SET_R2(___TRU)
   ___SET_R1(___TRU)
   ___ADJFP(2)
   ___POLL(4)
___DEF_SLBL(4,___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),6,___G__23__23_load)
___DEF_SLBL(5,___L5_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(5,0,0,0)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0
#undef ___PH_LBL0
#define ___PH_LBL0 10
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_P_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_P_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___DEF_P_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),11,___G_scheme_2f_repl_2d_impl_23_r7rs_2d_interaction_2d_environment)
___DEF_SLBL(2,___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load____0)
   ___ADJFP(-8)
   ___JUMPINT(___SET_NARGS(2),___PRC(3),___L_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_load_2d_impl_23_r7rs_2d_load
#undef ___PH_LBL0
#define ___PH_LBL0 15
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_P_HLBL(___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_P_HLBL(___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_P_HLBL(___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_P_HLBL(___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(0,0,0,0)
___DEF_GLBL(___L_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(1))
   ___ADJFP(8)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_length)
___DEF_SLBL(1,___L1_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___IF(___NOT(___FIXEQ(___R1,___FIX(1L))))
   ___GOTO(___L5_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___END_IF
   ___SET_R2(___STK(-6))
   ___SET_R1(___PRC(10))
   ___SET_R0(___STK(-7))
   ___POLL(2)
___DEF_SLBL(2,___L2_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___GOTO(___L6_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_GLBL(___L5_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___IF(___NOT(___FIXEQ(___R1,___FIX(2L))))
   ___GOTO(___L7_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___END_IF
   ___SET_R2(___STK(-6))
   ___SET_R1(___PRC(3))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3_scheme_2f_load_2d_impl_23_r7rs_2d_load)
___DEF_GLBL(___L6_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___ADJFP(-8)
   ___JUMPPRM(___SET_NARGS(2),___PRM_apply)
___DEF_GLBL(___L7_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___SET_R2(___STK(-6))
   ___SET_R1(___LBL(0))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_scheme_2f_load_2d_impl_23_r7rs_2d_load)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),7,___G__23__23_raise_2d_wrong_2d_number_2d_of_2d_arguments_2d_exception)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_scheme____load_2d_impl_23_,___REF_SYM(4,___S_scheme____load_2d_impl_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_scheme____load_2d_impl_23_,0,-1)
,___DEF_LBL_INTRO(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,___REF_SYM(1,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,2,-1)
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,0,1)
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_PROC(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,1,-1)
,___DEF_LBL_INTRO(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,___REF_SYM(2,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load____0),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,___REF_SYM(0,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_scheme_2f_load_2d_impl_23_r7rs_2d_load,___IFD(___RETI,8,8,0x3f00L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(4,___G_scheme____load_2d_impl_23_,1)
___DEF_MOD_PRM(1,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,3)
___DEF_MOD_PRM(2,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,10)
___DEF_MOD_PRM(0,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load,15)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(4,___G_scheme____load_2d_impl_23_,1)
___DEF_MOD_GLO(1,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,3)
___DEF_MOD_GLO(2,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,10)
___DEF_MOD_GLO(0,___G_scheme_2f_load_2d_impl_23_r7rs_2d_load,15)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load,"scheme/load-impl#r7rs-load")
___DEF_MOD_SYM(1,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load_____25_,"scheme/load-impl#r7rs-load__%")

___DEF_MOD_SYM(2,___S_scheme_2f_load_2d_impl_23_r7rs_2d_load____0,"scheme/load-impl#r7rs-load__0")

___DEF_MOD_SYM(3,___S_scheme____load_2d_impl,"scheme__load-impl")
___DEF_MOD_SYM(4,___S_scheme____load_2d_impl_23_,"scheme__load-impl#")
___END_MOD_SYM_KEY

#endif
