#ifdef ___LINKER_INFO
; File: "scheme__eval-impl.c", produced by Gambit v4.9.7
(
409007
(C)
"scheme__eval-impl"
("scheme__eval-impl")
()
(("scheme__eval-impl"))
( #|*/"*/"symbols|#
":init!"
":scheme/r7rs"
"import"
"scheme/eval-impl#make-environment"
"scheme/eval-impl#r7rs-environment"
"scheme/eval-impl#r7rs-eval"
"scheme__eval-impl"
"scheme__eval-impl#"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"class"
"method"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"scheme/eval-impl#environments"
"scheme/eval-impl#init!"
"scheme/eval-impl#make-environment"
"scheme__eval-impl#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"scheme/eval-impl#r7rs-environment"
"scheme/eval-impl#r7rs-eval"
"scheme/eval-impl::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"__hash-get"
"__hash-put!"
"__make-promise"
"absent-value"
"call-with-parameters__1"
"direct-method-ref"
"error"
"eval"
"gerbil-load-expander!"
"gx#current-expander-context"
"gx#top-context::t"
"make-hash-table__%"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "scheme__eval-impl"
#define ___LINKER_ID ___LNK_scheme____eval_2d_impl
#define ___MH_PROC ___H_scheme____eval_2d_impl
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 8
#define ___KEYCOUNT 2
#define ___GLOCOUNT 19
#define ___SUPCOUNT 7
#define ___CNSCOUNT 2
#define ___SUBCOUNT 6
#define ___LBLCOUNT 41
#define ___MODDESCR ___REF_SUB(3)
#include "gambit.h"

___NEED_SYM(___S__3a_init_21_)
___NEED_SYM(___S__3a_scheme_2f_r7rs)
___NEED_SYM(___S_import)
___NEED_SYM(___S_scheme_2f_eval_2d_impl_23_make_2d_environment)
___NEED_SYM(___S_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___NEED_SYM(___S_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___NEED_SYM(___S_scheme____eval_2d_impl)
___NEED_SYM(___S_scheme____eval_2d_impl_23_)

___NEED_KEY(___K_class)
___NEED_KEY(___K_method)

___NEED_GLO(___G_____hash_2d_get)
___NEED_GLO(___G_____hash_2d_put_21_)
___NEED_GLO(___G_____make_2d_promise)
___NEED_GLO(___G_absent_2d_value)
___NEED_GLO(___G_call_2d_with_2d_parameters____1)
___NEED_GLO(___G_direct_2d_method_2d_ref)
___NEED_GLO(___G_error)
___NEED_GLO(___G_eval)
___NEED_GLO(___G_gerbil_2d_load_2d_expander_21_)
___NEED_GLO(___G_gx_23_current_2d_expander_2d_context)
___NEED_GLO(___G_gx_23_top_2d_context_3a__3a_t)
___NEED_GLO(___G_make_2d_hash_2d_table_____25_)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_23_environments)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_23_init_21_)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_23_make_2d_environment)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___NEED_GLO(___G_scheme_2f_eval_2d_impl_3a__3a_timestamp)
___NEED_GLO(___G_scheme____eval_2d_impl_23_)

___BEGIN_SYM
___DEF_SYM(0,___S__3a_init_21_,":init!")
___DEF_SYM(1,___S__3a_scheme_2f_r7rs,":scheme/r7rs")
___DEF_SYM(2,___S_import,"import")
___DEF_SYM(3,___S_scheme_2f_eval_2d_impl_23_make_2d_environment,"scheme/eval-impl#make-environment")

___DEF_SYM(4,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,"scheme/eval-impl#r7rs-environment")

___DEF_SYM(5,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,"scheme/eval-impl#r7rs-eval")
___DEF_SYM(6,___S_scheme____eval_2d_impl,"scheme__eval-impl")
___DEF_SYM(7,___S_scheme____eval_2d_impl_23_,"scheme__eval-impl#")
___END_SYM

#define ___SYM__3a_init_21_ ___SYM(0,___S__3a_init_21_)
#define ___SYM__3a_scheme_2f_r7rs ___SYM(1,___S__3a_scheme_2f_r7rs)
#define ___SYM_import ___SYM(2,___S_import)
#define ___SYM_scheme_2f_eval_2d_impl_23_make_2d_environment ___SYM(3,___S_scheme_2f_eval_2d_impl_23_make_2d_environment)
#define ___SYM_scheme_2f_eval_2d_impl_23_r7rs_2d_environment ___SYM(4,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
#define ___SYM_scheme_2f_eval_2d_impl_23_r7rs_2d_eval ___SYM(5,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
#define ___SYM_scheme____eval_2d_impl ___SYM(6,___S_scheme____eval_2d_impl)
#define ___SYM_scheme____eval_2d_impl_23_ ___SYM(7,___S_scheme____eval_2d_impl_23_)

___BEGIN_KEY
___DEF_KEY(0,___K_class,"class")
___DEF_KEY(1,___K_method,"method")
___END_KEY

#define ___KEY_class ___KEY(0,___K_class)
#define ___KEY_method ___KEY(1,___K_method)

___BEGIN_GLO
___DEF_GLO(0,"scheme/eval-impl#environments")
___DEF_GLO(1,"scheme/eval-impl#init!")
___DEF_GLO(2,"scheme/eval-impl#make-environment")

___DEF_GLO(3,"scheme/eval-impl#r7rs-environment")

___DEF_GLO(4,"scheme/eval-impl#r7rs-eval")
___DEF_GLO(5,"scheme/eval-impl::timestamp")
___DEF_GLO(6,"scheme__eval-impl#")
___DEF_GLO(7,"__hash-get")
___DEF_GLO(8,"__hash-put!")
___DEF_GLO(9,"__make-promise")
___DEF_GLO(10,"absent-value")
___DEF_GLO(11,"call-with-parameters__1")
___DEF_GLO(12,"direct-method-ref")
___DEF_GLO(13,"error")
___DEF_GLO(14,"eval")
___DEF_GLO(15,"gerbil-load-expander!")
___DEF_GLO(16,"gx#current-expander-context")
___DEF_GLO(17,"gx#top-context::t")
___DEF_GLO(18,"make-hash-table__%")
___END_GLO

#define ___GLO_scheme_2f_eval_2d_impl_23_environments ___GLO(0,___G_scheme_2f_eval_2d_impl_23_environments)
#define ___PRM_scheme_2f_eval_2d_impl_23_environments ___PRM(0,___G_scheme_2f_eval_2d_impl_23_environments)
#define ___GLO_scheme_2f_eval_2d_impl_23_init_21_ ___GLO(1,___G_scheme_2f_eval_2d_impl_23_init_21_)
#define ___PRM_scheme_2f_eval_2d_impl_23_init_21_ ___PRM(1,___G_scheme_2f_eval_2d_impl_23_init_21_)
#define ___GLO_scheme_2f_eval_2d_impl_23_make_2d_environment ___GLO(2,___G_scheme_2f_eval_2d_impl_23_make_2d_environment)
#define ___PRM_scheme_2f_eval_2d_impl_23_make_2d_environment ___PRM(2,___G_scheme_2f_eval_2d_impl_23_make_2d_environment)
#define ___GLO_scheme_2f_eval_2d_impl_23_r7rs_2d_environment ___GLO(3,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
#define ___PRM_scheme_2f_eval_2d_impl_23_r7rs_2d_environment ___PRM(3,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
#define ___GLO_scheme_2f_eval_2d_impl_23_r7rs_2d_eval ___GLO(4,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
#define ___PRM_scheme_2f_eval_2d_impl_23_r7rs_2d_eval ___PRM(4,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
#define ___GLO_scheme_2f_eval_2d_impl_3a__3a_timestamp ___GLO(5,___G_scheme_2f_eval_2d_impl_3a__3a_timestamp)
#define ___PRM_scheme_2f_eval_2d_impl_3a__3a_timestamp ___PRM(5,___G_scheme_2f_eval_2d_impl_3a__3a_timestamp)
#define ___GLO_scheme____eval_2d_impl_23_ ___GLO(6,___G_scheme____eval_2d_impl_23_)
#define ___PRM_scheme____eval_2d_impl_23_ ___PRM(6,___G_scheme____eval_2d_impl_23_)
#define ___GLO_____hash_2d_get ___GLO(7,___G_____hash_2d_get)
#define ___PRM_____hash_2d_get ___PRM(7,___G_____hash_2d_get)
#define ___GLO_____hash_2d_put_21_ ___GLO(8,___G_____hash_2d_put_21_)
#define ___PRM_____hash_2d_put_21_ ___PRM(8,___G_____hash_2d_put_21_)
#define ___GLO_____make_2d_promise ___GLO(9,___G_____make_2d_promise)
#define ___PRM_____make_2d_promise ___PRM(9,___G_____make_2d_promise)
#define ___GLO_absent_2d_value ___GLO(10,___G_absent_2d_value)
#define ___PRM_absent_2d_value ___PRM(10,___G_absent_2d_value)
#define ___GLO_call_2d_with_2d_parameters____1 ___GLO(11,___G_call_2d_with_2d_parameters____1)
#define ___PRM_call_2d_with_2d_parameters____1 ___PRM(11,___G_call_2d_with_2d_parameters____1)
#define ___GLO_direct_2d_method_2d_ref ___GLO(12,___G_direct_2d_method_2d_ref)
#define ___PRM_direct_2d_method_2d_ref ___PRM(12,___G_direct_2d_method_2d_ref)
#define ___GLO_error ___GLO(13,___G_error)
#define ___PRM_error ___PRM(13,___G_error)
#define ___GLO_eval ___GLO(14,___G_eval)
#define ___PRM_eval ___PRM(14,___G_eval)
#define ___GLO_gerbil_2d_load_2d_expander_21_ ___GLO(15,___G_gerbil_2d_load_2d_expander_21_)
#define ___PRM_gerbil_2d_load_2d_expander_21_ ___PRM(15,___G_gerbil_2d_load_2d_expander_21_)
#define ___GLO_gx_23_current_2d_expander_2d_context ___GLO(16,___G_gx_23_current_2d_expander_2d_context)
#define ___PRM_gx_23_current_2d_expander_2d_context ___PRM(16,___G_gx_23_current_2d_expander_2d_context)
#define ___GLO_gx_23_top_2d_context_3a__3a_t ___GLO(17,___G_gx_23_top_2d_context_3a__3a_t)
#define ___PRM_gx_23_top_2d_context_3a__3a_t ___PRM(17,___G_gx_23_top_2d_context_3a__3a_t)
#define ___GLO_make_2d_hash_2d_table_____25_ ___GLO(18,___G_make_2d_hash_2d_table_____25_)
#define ___PRM_make_2d_hash_2d_table_____25_ ___PRM(18,___G_make_2d_hash_2d_table_____25_)

___BEGIN_CNS
 ___DEF_CNS(___REF_SYM(2,___S_import),___REF_CNS(1))
,___DEF_CNS(___REF_SYM(1,___S__3a_scheme_2f_r7rs),___REF_NUL)
___END_CNS

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c81L)
___DEF_SUB_STR(___X1,41UL)
               ___STR8(109,105,115,115,105,110,103,32)
               ___STR8(99,111,110,115,116,114,117,99)
               ___STR8(116,111,114,32,109,101,116,104)
               ___STR8(111,100,32,105,109,112,108,101)
               ___STR8(109,101,110,116,97,116,105,111)
               ___STR1(110)
___DEF_SUB_STR(___X2,41UL)
               ___STR8(109,105,115,115,105,110,103,32)
               ___STR8(99,111,110,115,116,114,117,99)
               ___STR8(116,111,114,32,109,101,116,104)
               ___STR8(111,100,32,105,109,112,108,101)
               ___STR8(109,101,110,116,97,116,105,111)
               ___STR1(110)
___DEF_SUB_VEC(___X3,6UL)
               ___VEC1(___REF_SUB(4))
               ___VEC1(___REF_SUB(5))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X4,1UL)
               ___VEC1(___REF_SYM(6,___S_scheme____eval_2d_impl))
               ___VEC0
___DEF_SUB_VEC(___X5,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
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
___DEF_M_HLBL(___L0_scheme____eval_2d_impl_23_)
___DEF_M_HLBL(___L1_scheme____eval_2d_impl_23_)
___DEF_M_HLBL(___L2_scheme____eval_2d_impl_23_)
___DEF_M_HLBL(___L3_scheme____eval_2d_impl_23_)
___DEF_M_HLBL(___L4_scheme____eval_2d_impl_23_)
___DEF_M_HLBL(___L5_scheme____eval_2d_impl_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL(___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL(___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL(___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL(___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL(___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L1_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L2_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L3_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L4_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L5_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L6_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L7_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L8_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L9_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L10_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L11_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L12_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L13_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L14_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L15_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL(___L16_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L6_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_M_HLBL(___L7_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme____eval_2d_impl_23_
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme____eval_2d_impl_23_)
___DEF_P_HLBL(___L1_scheme____eval_2d_impl_23_)
___DEF_P_HLBL(___L2_scheme____eval_2d_impl_23_)
___DEF_P_HLBL(___L3_scheme____eval_2d_impl_23_)
___DEF_P_HLBL(___L4_scheme____eval_2d_impl_23_)
___DEF_P_HLBL(___L5_scheme____eval_2d_impl_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme____eval_2d_impl_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_scheme____eval_2d_impl_23_)
   ___SET_GLO(5,___G_scheme_2f_eval_2d_impl_3a__3a_timestamp,___BIGFIX(0,1770925185LL))
   ___SET_STK(1,___R0)
   ___SET_R1(___LBL(4))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme____eval_2d_impl_23_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),9,___G_____make_2d_promise)
___DEF_SLBL(2,___L2_scheme____eval_2d_impl_23_)
   ___SET_GLO(1,___G_scheme_2f_eval_2d_impl_23_init_21_,___R1)
   ___SET_STK(1,___FAL)
   ___SET_STK(2,___GLO_absent_2d_value)
   ___SET_STK(3,___GLO_absent_2d_value)
   ___SET_STK(4,___GLO_absent_2d_value)
   ___SET_STK(5,___GLO_absent_2d_value)
   ___SET_STK(6,___GLO_absent_2d_value)
   ___SET_R3(___GLO_absent_2d_value)
   ___SET_R2(___GLO_absent_2d_value)
   ___SET_R1(___GLO_absent_2d_value)
   ___SET_R0(___LBL(3))
   ___ADJFP(6)
   ___JUMPGLONOTSAFE(___SET_NARGS(9),18,___G_make_2d_hash_2d_table_____25_)
___DEF_SLBL(3,___L3_scheme____eval_2d_impl_23_)
   ___SET_GLO(0,___G_scheme_2f_eval_2d_impl_23_environments,___R1)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_SLBL(4,___L4_scheme____eval_2d_impl_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(4,0,0,0)
   ___POLL(5)
___DEF_SLBL(5,___L5_scheme____eval_2d_impl_23_)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),15,___G_gerbil_2d_load_2d_expander_21_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval
#undef ___PH_LBL0
#define ___PH_LBL0 8
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_P_HLBL(___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_P_HLBL(___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_P_HLBL(___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_P_HLBL(___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___DEF_P_HLBL(___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___FORCE1(1,___GLO_scheme_2f_eval_2d_impl_23_init_21_)
___DEF_SLBL(1,___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___FORCE2
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),4)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R3(___R2)
   ___SET_R1(___STK(1))
   ___SET_R2(___GLO_gx_23_current_2d_expander_2d_context)
   ___ADJFP(1)
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___POLL(3)
___DEF_SLBL(3,___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),11,___G_call_2d_with_2d_parameters____1)
___DEF_SLBL(4,___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(4,0,0,0)
   ___SET_R1(___CLO(___R4,1))
   ___POLL(5)
___DEF_SLBL(5,___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_eval)
   ___JUMPPRM(___SET_NARGS(1),___PRM_eval)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_eval_2d_impl_23_make_2d_environment
#undef ___PH_LBL0
#define ___PH_LBL0 15
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L1_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L2_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L3_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L4_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L5_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L6_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L7_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L8_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L9_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L10_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L11_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L12_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L13_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L14_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L15_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_P_HLBL(___L16_scheme_2f_eval_2d_impl_23_make_2d_environment)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___FORCE1(1,___GLO_scheme_2f_eval_2d_impl_23_init_21_)
___DEF_SLBL(1,___L1_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___FORCE2
   ___BEGIN_ALLOC_STRUCTURE(6UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_gx_23_top_2d_context_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___FAL)
   ___ADD_STRUCTURE_ELEM(2,___FAL)
   ___ADD_STRUCTURE_ELEM(3,___FAL)
   ___ADD_STRUCTURE_ELEM(4,___FAL)
   ___ADD_STRUCTURE_ELEM(5,___FAL)
   ___END_ALLOC_STRUCTURE(6)
   ___SET_R2(___GET_STRUCTURE(6))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R3(___SYM__3a_init_21_)
   ___SET_R1(___GLO_gx_23_top_2d_context_3a__3a_t)
   ___ADJFP(8)
   ___CHECK_HEAP(2,4096)
___DEF_SLBL(2,___L2_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___POLL(3)
___DEF_SLBL(3,___L3_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(3),12,___G_direct_2d_method_2d_ref)
___DEF_SLBL(4,___L4_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L20_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___END_IF
   ___SET_STK(1,___SUB(1))
   ___SET_STK(2,___KEY_class)
   ___SET_R3(___SYM__3a_init_21_)
   ___SET_R2(___KEY_method)
   ___SET_R1(___GLO_gx_23_top_2d_context_3a__3a_t)
   ___SET_R0(___LBL(5))
   ___ADJFP(2)
   ___JUMPGLOSAFE(___SET_NARGS(5),13,___G_error)
___DEF_SLBL(5,___L5_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_STK(-4,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(-4),8)
   ___ADD_CLO_ELEM(0,___STK(-5))
   ___ADD_CLO_ELEM(1,___STK(-6))
   ___END_SETUP_CLO(2)
   ___SET_R3(___STK(-5))
   ___SET_R1(___STK(-4))
   ___SET_R2(___GLO_gx_23_current_2d_expander_2d_context)
   ___SET_R0(___STK(-7))
   ___ADJFP(-4)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___POLL(7)
___DEF_SLBL(7,___L7_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),11,___G_call_2d_with_2d_parameters____1)
___DEF_SLBL(8,___L8_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(8,0,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R4)
   ___SET_R1(___CNS(0))
   ___SET_R0(___LBL(9))
   ___ADJFP(8)
   ___JUMPPRM(___SET_NARGS(1),___PRM_eval)
___DEF_SLBL(9,___L9_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___CLO(___STK(-6),2))
   ___SET_R0(___LBL(16))
   ___POLL(10)
___DEF_SLBL(10,___L10_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___GOTO(___L17_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_SLBL(11,___L11_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___CDR(___STK(-6)))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(12)
___DEF_SLBL(12,___L12_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_GLBL(___L17_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L19_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___END_IF
   ___SET_R2(___CAR(___R1))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R1(___CONS(___R2,___NUL))
   ___SET_R1(___CONS(___SYM_import,___R1))
   ___SET_R0(___LBL(14))
   ___ADJFP(8)
   ___CHECK_HEAP(13,4096)
___DEF_SLBL(13,___L13_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___JUMPPRM(___SET_NARGS(1),___PRM_eval)
___DEF_SLBL(14,___L14_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___CDR(___STK(-6)))
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L18_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___END_IF
   ___SET_R2(___CAR(___R1))
   ___SET_STK(-6,___R1)
   ___SET_R1(___CONS(___R2,___NUL))
   ___SET_R1(___CONS(___SYM_import,___R1))
   ___SET_R0(___LBL(11))
   ___CHECK_HEAP(15,4096)
___DEF_SLBL(15,___L15_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___JUMPPRM(___SET_NARGS(1),___PRM_eval)
___DEF_GLBL(___L18_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___VOID)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L19_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___DEF_SLBL(16,___L16_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_R1(___CLO(___STK(-6),1))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L20_scheme_2f_eval_2d_impl_23_make_2d_environment)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(5))
   ___JUMPGENSAFE(___SET_NARGS(1),___STK(-4))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment
#undef ___PH_LBL0
#define ___PH_LBL0 33
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L6_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_P_HLBL(___L7_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(0,0,0,0)
___DEF_GLBL(___L_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___R1)
   ___SET_R1(___GLO_scheme_2f_eval_2d_impl_23_environments)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),7,___G_____hash_2d_get)
___DEF_SLBL(2,___L2_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L8_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___END_IF
   ___GOTO(___L10_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
___DEF_SLBL(3,___L3_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_R1(___STK(-5))
___DEF_GLBL(___L8_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___BEGIN_ALLOC_STRUCTURE(6UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_gx_23_top_2d_context_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___FAL)
   ___ADD_STRUCTURE_ELEM(2,___FAL)
   ___ADD_STRUCTURE_ELEM(3,___FAL)
   ___ADD_STRUCTURE_ELEM(4,___FAL)
   ___ADD_STRUCTURE_ELEM(5,___FAL)
   ___END_ALLOC_STRUCTURE(6)
   ___SET_R2(___GET_STRUCTURE(6))
   ___SET_STK(-6,___R1)
   ___SET_STK(-5,___R2)
   ___SET_R3(___SYM__3a_init_21_)
   ___SET_R1(___GLO_gx_23_top_2d_context_3a__3a_t)
   ___SET_R0(___LBL(5))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___JUMPGLOSAFE(___SET_NARGS(3),12,___G_direct_2d_method_2d_ref)
___DEF_SLBL(5,___L5_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L9_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___END_IF
   ___SET_STK(1,___SUB(2))
   ___SET_STK(2,___KEY_class)
   ___SET_R3(___SYM__3a_init_21_)
   ___SET_R2(___KEY_method)
   ___SET_R1(___GLO_gx_23_top_2d_context_3a__3a_t)
   ___SET_R0(___LBL(6))
   ___ADJFP(2)
   ___JUMPGLOSAFE(___SET_NARGS(5),13,___G_error)
___DEF_SLBL(6,___L6_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_R1(___STK(-5))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L9_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_R2(___STK(-6))
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(6))
   ___JUMPGENSAFE(___SET_NARGS(2),___STK(-6))
___DEF_GLBL(___L10_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(7))
   ___JUMPINT(___SET_NARGS(1),___PRC(15),___L_scheme_2f_eval_2d_impl_23_make_2d_environment)
___DEF_SLBL(7,___L7_scheme_2f_eval_2d_impl_23_r7rs_2d_environment)
   ___SET_STK(-5,___R1)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___GLO_scheme_2f_eval_2d_impl_23_environments)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),8,___G_____hash_2d_put_21_)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_scheme____eval_2d_impl_23_,___REF_SYM(7,___S_scheme____eval_2d_impl_23_),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_scheme____eval_2d_impl_23_,0,-1)
,___DEF_LBL_RET(___H_scheme____eval_2d_impl_23_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_scheme____eval_2d_impl_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_scheme____eval_2d_impl_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_PROC(___H_scheme____eval_2d_impl_23_,0,-1)
,___DEF_LBL_RET(___H_scheme____eval_2d_impl_23_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,___REF_SYM(5,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_eval),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,2,-1)
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,0,1)
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___REF_SYM(3,___S_scheme_2f_eval_2d_impl_23_make_2d_environment),___REF_FAL,17,0)
,___DEF_LBL_PROC(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_PROC(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,0,2)
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_make_2d_environment,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_INTRO(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___REF_SYM(4,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_environment),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,___IFD(___RETN,5,0,0x3L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(6,___G_scheme____eval_2d_impl_23_,1)
___DEF_MOD_PRM(4,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,8)
___DEF_MOD_PRM(2,___G_scheme_2f_eval_2d_impl_23_make_2d_environment,15)
___DEF_MOD_PRM(3,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,33)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(6,___G_scheme____eval_2d_impl_23_,1)
___DEF_MOD_GLO(4,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,8)
___DEF_MOD_GLO(2,___G_scheme_2f_eval_2d_impl_23_make_2d_environment,15)
___DEF_MOD_GLO(3,___G_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,33)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__3a_init_21_,":init!")
___DEF_MOD_SYM(1,___S__3a_scheme_2f_r7rs,":scheme/r7rs")
___DEF_MOD_SYM(2,___S_import,"import")
___DEF_MOD_SYM(3,___S_scheme_2f_eval_2d_impl_23_make_2d_environment,"scheme/eval-impl#make-environment")

___DEF_MOD_SYM(4,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_environment,"scheme/eval-impl#r7rs-environment")

___DEF_MOD_SYM(5,___S_scheme_2f_eval_2d_impl_23_r7rs_2d_eval,"scheme/eval-impl#r7rs-eval")
___DEF_MOD_SYM(6,___S_scheme____eval_2d_impl,"scheme__eval-impl")
___DEF_MOD_SYM(7,___S_scheme____eval_2d_impl_23_,"scheme__eval-impl#")
___DEF_MOD_KEY(0,___K_class,"class")
___DEF_MOD_KEY(1,___K_method,"method")
___END_MOD_SYM_KEY

#endif
