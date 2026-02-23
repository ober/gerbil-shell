#ifdef ___LINKER_INFO
; File: "scheme__lazy-impl.c", produced by Gambit v4.9.7
(
409007
(C)
"scheme__lazy-impl"
("scheme__lazy-impl")
()
(("scheme__lazy-impl"))
( #|*/"*/"symbols|#
"scheme/lazy-impl#r7rs-make-promise"
"scheme/lazy-impl#r7rs-promise?"
"scheme__lazy-impl"
"scheme__lazy-impl#"
"std/lazy#lazy::t"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"scheme__lazy-impl#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"scheme/lazy-impl#r7rs-make-promise"
"scheme/lazy-impl#r7rs-promise?"
"scheme/lazy-impl::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"std/lazy#eager"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "scheme__lazy-impl"
#define ___LINKER_ID ___LNK_scheme____lazy_2d_impl
#define ___MH_PROC ___H_scheme____lazy_2d_impl
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 5
#define ___GLOCOUNT 5
#define ___SUPCOUNT 4
#define ___SUBCOUNT 4
#define ___LBLCOUNT 7
#define ___MODDESCR ___REF_SUB(1)
#include "gambit.h"

___NEED_SYM(___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___NEED_SYM(___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
___NEED_SYM(___S_scheme____lazy_2d_impl)
___NEED_SYM(___S_scheme____lazy_2d_impl_23_)
___NEED_SYM(___S_std_2f_lazy_23_lazy_3a__3a_t)

___NEED_GLO(___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___NEED_GLO(___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
___NEED_GLO(___G_scheme_2f_lazy_2d_impl_3a__3a_timestamp)
___NEED_GLO(___G_scheme____lazy_2d_impl_23_)
___NEED_GLO(___G_std_2f_lazy_23_eager)

___BEGIN_SYM
___DEF_SYM(0,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,"scheme/lazy-impl#r7rs-make-promise")

___DEF_SYM(1,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,"scheme/lazy-impl#r7rs-promise?")

___DEF_SYM(2,___S_scheme____lazy_2d_impl,"scheme__lazy-impl")
___DEF_SYM(3,___S_scheme____lazy_2d_impl_23_,"scheme__lazy-impl#")
___DEF_SYM(4,___S_std_2f_lazy_23_lazy_3a__3a_t,"std/lazy#lazy::t")
___END_SYM

#define ___SYM_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise ___SYM(0,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
#define ___SYM_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_ ___SYM(1,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
#define ___SYM_scheme____lazy_2d_impl ___SYM(2,___S_scheme____lazy_2d_impl)
#define ___SYM_scheme____lazy_2d_impl_23_ ___SYM(3,___S_scheme____lazy_2d_impl_23_)
#define ___SYM_std_2f_lazy_23_lazy_3a__3a_t ___SYM(4,___S_std_2f_lazy_23_lazy_3a__3a_t)

___BEGIN_GLO
___DEF_GLO(0,"scheme/lazy-impl#r7rs-make-promise")

___DEF_GLO(1,"scheme/lazy-impl#r7rs-promise?")
___DEF_GLO(2,"scheme/lazy-impl::timestamp")
___DEF_GLO(3,"scheme__lazy-impl#")
___DEF_GLO(4,"std/lazy#eager")
___END_GLO

#define ___GLO_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise ___GLO(0,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
#define ___PRM_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise ___PRM(0,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
#define ___GLO_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_ ___GLO(1,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
#define ___PRM_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_ ___PRM(1,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
#define ___GLO_scheme_2f_lazy_2d_impl_3a__3a_timestamp ___GLO(2,___G_scheme_2f_lazy_2d_impl_3a__3a_timestamp)
#define ___PRM_scheme_2f_lazy_2d_impl_3a__3a_timestamp ___PRM(2,___G_scheme_2f_lazy_2d_impl_3a__3a_timestamp)
#define ___GLO_scheme____lazy_2d_impl_23_ ___GLO(3,___G_scheme____lazy_2d_impl_23_)
#define ___PRM_scheme____lazy_2d_impl_23_ ___PRM(3,___G_scheme____lazy_2d_impl_23_)
#define ___GLO_std_2f_lazy_23_eager ___GLO(4,___G_std_2f_lazy_23_eager)
#define ___PRM_std_2f_lazy_23_eager ___PRM(4,___G_std_2f_lazy_23_eager)

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
               ___VEC1(___REF_SYM(2,___S_scheme____lazy_2d_impl))
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
#define ___MD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_R1 ___W_R2 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme____lazy_2d_impl_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___DEF_M_HLBL(___L1_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme____lazy_2d_impl_23_
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
___DEF_P_HLBL(___L0_scheme____lazy_2d_impl_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme____lazy_2d_impl_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_scheme____lazy_2d_impl_23_)
   ___SET_GLO(2,___G_scheme_2f_lazy_2d_impl_3a__3a_timestamp,___BIGFIX(0,1770925185LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___DEF_P_HLBL(___L1_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
   ___POLL(1)
___DEF_SLBL(1,___L1_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),4,___G_std_2f_lazy_23_eager)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1 ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1 ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
   ___SET_R2(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM_std_2f_lazy_23_lazy_3a__3a_t)))
   ___IF(___NOT(___NOTFALSEP(___R2)))
   ___GOTO(___L1_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
   ___END_IF
   ___SET_R1(___R2)
   ___JUMPRET(___R0)
___DEF_GLBL(___L1_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_)
   ___SET_R1(___BOOLEAN(___PROMISEP(___R1)))
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_scheme____lazy_2d_impl_23_,___REF_SYM(3,___S_scheme____lazy_2d_impl_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_scheme____lazy_2d_impl_23_,0,-1)
,___DEF_LBL_INTRO(___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,___REF_SYM(0,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,1,-1)
,___DEF_LBL_RET(___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,___REF_SYM(1,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,1,-1)
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(3,___G_scheme____lazy_2d_impl_23_,1)
___DEF_MOD_PRM(0,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,3)
___DEF_MOD_PRM(1,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,6)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(3,___G_scheme____lazy_2d_impl_23_,1)
___DEF_MOD_GLO(0,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,3)
___DEF_MOD_GLO(1,___G_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,6)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_make_2d_promise,"scheme/lazy-impl#r7rs-make-promise")

___DEF_MOD_SYM(1,___S_scheme_2f_lazy_2d_impl_23_r7rs_2d_promise_3f_,"scheme/lazy-impl#r7rs-promise?")

___DEF_MOD_SYM(2,___S_scheme____lazy_2d_impl,"scheme__lazy-impl")
___DEF_MOD_SYM(3,___S_scheme____lazy_2d_impl_23_,"scheme__lazy-impl#")
___DEF_MOD_SYM(4,___S_std_2f_lazy_23_lazy_3a__3a_t,"std/lazy#lazy::t")
___END_MOD_SYM_KEY

#endif
