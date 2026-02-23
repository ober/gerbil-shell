#ifdef ___LINKER_INFO
; File: "gerbil__core__contract~ClassMeta.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__core__contract~ClassMeta"
("gerbil__core__contract~ClassMeta")
()
(("gerbil__core__contract~ClassMeta"))
( #|*/"*/"symbols|#
":object"
":t"
"class"
"gerbil.core#class-type-info::t"
"gerbil/core/contract~ClassMeta#!class-precedence-list"
"gerbil__core__contract~ClassMeta"
"gerbil__core__contract~ClassMeta#"
"id"
"object"
"precedence-list"
"struct?"
"super"
"system?"
"t"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"eq"
"get-precedence-list"
"struct"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil__core__contract~ClassMeta#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/core/contract~ClassMeta#!class-precedence-list"
"gerbil/core/contract~ClassMeta::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##append"
"c4-linearize"
"class-slot-ref"
"class-slot-set!"
"cons"
"error"
"foldl"
"gerbil/core/mop~MOP-2#class-type-info::t"
"gx#core-quote-syntax"
"gx#free-identifier=?"
"gx#syntax-local-value"
"member"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__core__contract~ClassMeta"
#define ___LINKER_ID ___LNK_gerbil____core____contract_7e_ClassMeta
#define ___MH_PROC ___H_gerbil____core____contract_7e_ClassMeta
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 14
#define ___KEYCOUNT 3
#define ___GLOCOUNT 15
#define ___SUPCOUNT 3
#define ___CNSCOUNT 3
#define ___SUBCOUNT 5
#define ___LBLCOUNT 42
#define ___OFDCOUNT 2
#define ___MODDESCR ___REF_SUB(2)
#include "gambit.h"

___NEED_SYM(___S__3a_object)
___NEED_SYM(___S__3a_t)
___NEED_SYM(___S_class)
___NEED_SYM(___S_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)
___NEED_SYM(___S_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___NEED_SYM(___S_gerbil____core____contract_7e_ClassMeta)
___NEED_SYM(___S_gerbil____core____contract_7e_ClassMeta_23_)
___NEED_SYM(___S_id)
___NEED_SYM(___S_object)
___NEED_SYM(___S_precedence_2d_list)
___NEED_SYM(___S_struct_3f_)
___NEED_SYM(___S_super)
___NEED_SYM(___S_system_3f_)
___NEED_SYM(___S_t)

___NEED_KEY(___K_eq)
___NEED_KEY(___K_get_2d_precedence_2d_list)
___NEED_KEY(___K_struct)

___NEED_GLO(___G__23__23_append)
___NEED_GLO(___G_c4_2d_linearize)
___NEED_GLO(___G_class_2d_slot_2d_ref)
___NEED_GLO(___G_class_2d_slot_2d_set_21_)
___NEED_GLO(___G_cons)
___NEED_GLO(___G_error)
___NEED_GLO(___G_foldl)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp)
___NEED_GLO(___G_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
___NEED_GLO(___G_gerbil____core____contract_7e_ClassMeta_23_)
___NEED_GLO(___G_gx_23_core_2d_quote_2d_syntax)
___NEED_GLO(___G_gx_23_free_2d_identifier_3d__3f_)
___NEED_GLO(___G_gx_23_syntax_2d_local_2d_value)
___NEED_GLO(___G_member)

___BEGIN_SYM
___DEF_SYM(0,___S__3a_object,":object")
___DEF_SYM(1,___S__3a_t,":t")
___DEF_SYM(2,___S_class,"class")
___DEF_SYM(3,___S_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t,"gerbil.core#class-type-info::t")

___DEF_SYM(4,___S_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,"gerbil/core/contract~ClassMeta#!class-precedence-list")

___DEF_SYM(5,___S_gerbil____core____contract_7e_ClassMeta,"gerbil__core__contract~ClassMeta")

___DEF_SYM(6,___S_gerbil____core____contract_7e_ClassMeta_23_,"gerbil__core__contract~ClassMeta#")

___DEF_SYM(7,___S_id,"id")
___DEF_SYM(8,___S_object,"object")
___DEF_SYM(9,___S_precedence_2d_list,"precedence-list")
___DEF_SYM(10,___S_struct_3f_,"struct?")
___DEF_SYM(11,___S_super,"super")
___DEF_SYM(12,___S_system_3f_,"system?")
___DEF_SYM(13,___S_t,"t")
___END_SYM

#define ___SYM__3a_object ___SYM(0,___S__3a_object)
#define ___SYM__3a_t ___SYM(1,___S__3a_t)
#define ___SYM_class ___SYM(2,___S_class)
#define ___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t ___SYM(3,___S_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)
#define ___SYM_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list ___SYM(4,___S_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
#define ___SYM_gerbil____core____contract_7e_ClassMeta ___SYM(5,___S_gerbil____core____contract_7e_ClassMeta)
#define ___SYM_gerbil____core____contract_7e_ClassMeta_23_ ___SYM(6,___S_gerbil____core____contract_7e_ClassMeta_23_)
#define ___SYM_id ___SYM(7,___S_id)
#define ___SYM_object ___SYM(8,___S_object)
#define ___SYM_precedence_2d_list ___SYM(9,___S_precedence_2d_list)
#define ___SYM_struct_3f_ ___SYM(10,___S_struct_3f_)
#define ___SYM_super ___SYM(11,___S_super)
#define ___SYM_system_3f_ ___SYM(12,___S_system_3f_)
#define ___SYM_t ___SYM(13,___S_t)

___BEGIN_KEY
___DEF_KEY(0,___K_eq,"eq")
___DEF_KEY(1,___K_get_2d_precedence_2d_list,"get-precedence-list")
___DEF_KEY(2,___K_struct,"struct")
___END_KEY

#define ___KEY_eq ___KEY(0,___K_eq)
#define ___KEY_get_2d_precedence_2d_list ___KEY(1,___K_get_2d_precedence_2d_list)
#define ___KEY_struct ___KEY(2,___K_struct)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/core/contract~ClassMeta#!class-precedence-list")

___DEF_GLO(1,"gerbil/core/contract~ClassMeta::timestamp")

___DEF_GLO(2,"gerbil__core__contract~ClassMeta#")

___DEF_GLO(3,"##append")
___DEF_GLO(4,"c4-linearize")
___DEF_GLO(5,"class-slot-ref")
___DEF_GLO(6,"class-slot-set!")
___DEF_GLO(7,"cons")
___DEF_GLO(8,"error")
___DEF_GLO(9,"foldl")
___DEF_GLO(10,"gerbil/core/mop~MOP-2#class-type-info::t")

___DEF_GLO(11,"gx#core-quote-syntax")
___DEF_GLO(12,"gx#free-identifier=?")
___DEF_GLO(13,"gx#syntax-local-value")
___DEF_GLO(14,"member")
___END_GLO

#define ___GLO_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list ___GLO(0,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
#define ___PRM_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list ___PRM(0,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
#define ___GLO_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp ___GLO(1,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp)
#define ___PRM_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp ___PRM(1,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp)
#define ___GLO_gerbil____core____contract_7e_ClassMeta_23_ ___GLO(2,___G_gerbil____core____contract_7e_ClassMeta_23_)
#define ___PRM_gerbil____core____contract_7e_ClassMeta_23_ ___PRM(2,___G_gerbil____core____contract_7e_ClassMeta_23_)
#define ___GLO__23__23_append ___GLO(3,___G__23__23_append)
#define ___PRM__23__23_append ___PRM(3,___G__23__23_append)
#define ___GLO_c4_2d_linearize ___GLO(4,___G_c4_2d_linearize)
#define ___PRM_c4_2d_linearize ___PRM(4,___G_c4_2d_linearize)
#define ___GLO_class_2d_slot_2d_ref ___GLO(5,___G_class_2d_slot_2d_ref)
#define ___PRM_class_2d_slot_2d_ref ___PRM(5,___G_class_2d_slot_2d_ref)
#define ___GLO_class_2d_slot_2d_set_21_ ___GLO(6,___G_class_2d_slot_2d_set_21_)
#define ___PRM_class_2d_slot_2d_set_21_ ___PRM(6,___G_class_2d_slot_2d_set_21_)
#define ___GLO_cons ___GLO(7,___G_cons)
#define ___PRM_cons ___PRM(7,___G_cons)
#define ___GLO_error ___GLO(8,___G_error)
#define ___PRM_error ___PRM(8,___G_error)
#define ___GLO_foldl ___GLO(9,___G_foldl)
#define ___PRM_foldl ___PRM(9,___G_foldl)
#define ___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t ___GLO(10,___G_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
#define ___PRM_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t ___PRM(10,___G_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
#define ___GLO_gx_23_core_2d_quote_2d_syntax ___GLO(11,___G_gx_23_core_2d_quote_2d_syntax)
#define ___PRM_gx_23_core_2d_quote_2d_syntax ___PRM(11,___G_gx_23_core_2d_quote_2d_syntax)
#define ___GLO_gx_23_free_2d_identifier_3d__3f_ ___GLO(12,___G_gx_23_free_2d_identifier_3d__3f_)
#define ___PRM_gx_23_free_2d_identifier_3d__3f_ ___PRM(12,___G_gx_23_free_2d_identifier_3d__3f_)
#define ___GLO_gx_23_syntax_2d_local_2d_value ___GLO(13,___G_gx_23_syntax_2d_local_2d_value)
#define ___PRM_gx_23_syntax_2d_local_2d_value ___PRM(13,___G_gx_23_syntax_2d_local_2d_value)
#define ___GLO_member ___GLO(14,___G_member)
#define ___PRM_member ___PRM(14,___G_member)

___BEGIN_CNS
 ___DEF_CNS(___REF_SYM(13,___S_t),___REF_CNS(1))
,___DEF_CNS(___REF_SYM(8,___S_object),___REF_CNS(2))
,___DEF_CNS(___REF_SYM(2,___S_class),___REF_NUL)
___END_CNS

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2a4cL)
___DEF_SUB_STR(___X1,24UL)
               ___STR8(67,111,110,116,101,120,116,32)
               ___STR8(101,120,112,101,99,116,115,32)
               ___STR8(50,32,118,97,108,117,101,115)
               ___STR0
___DEF_SUB_VEC(___X2,6UL)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SUB(4))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,1UL)
               ___VEC1(___REF_SYM(5,___S_gerbil____core____contract_7e_ClassMeta))
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
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil____core____contract_7e_ClassMeta_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L2_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L3_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L4_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L5_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L6_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L7_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L8_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L9_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L10_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L11_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L12_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L13_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L14_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L15_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L16_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L17_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L18_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L19_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L20_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L21_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L22_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L23_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L24_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L25_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L26_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L27_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L28_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L29_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L30_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L31_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L32_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L33_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L34_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L35_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L36_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L37_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_M_HLBL(___L38_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____core____contract_7e_ClassMeta_23_
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
___DEF_P_HLBL(___L0_gerbil____core____contract_7e_ClassMeta_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____core____contract_7e_ClassMeta_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____core____contract_7e_ClassMeta_23_)
   ___SET_GLO(1,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_3a__3a_timestamp,___BIGFIX(0,1770924620LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L2_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L3_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L4_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L5_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L6_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L7_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L8_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L9_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L10_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L11_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L12_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L13_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L14_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L15_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L16_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L17_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L18_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L19_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L20_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L21_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L22_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L23_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L24_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L25_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L26_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L27_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L28_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L29_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L30_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L31_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L32_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L33_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L34_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L35_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L36_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L37_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_P_HLBL(___L38_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___STRUCTUREDIOP(___R1,___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t))
   ___GOTO(___L39_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___GOTO(___L64_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(36))
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)))
   ___GOTO(___L64_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L39_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(5L),___FAL,___FAL))
   ___IF(___NOTFALSEP(___R2))
   ___GOTO(___L46_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L40_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(9,___NUL)
   ___ADJFP(9)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)))
   ___GOTO(___L63_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___FAL,___FAL))
___DEF_GLBL(___L41_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___KEY_get_2d_precedence_2d_list)
   ___SET_STK(3,___LBL(32))
   ___SET_STK(4,___KEY_struct)
   ___SET_R1(___LBL(28))
   ___SET_R3(___GLO_gx_23_free_2d_identifier_3d__3f_)
   ___SET_R2(___KEY_eq)
   ___ADJFP(4)
   ___POLL(2)
___DEF_SLBL(2,___L2_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(8),4,___G_c4_2d_linearize)
___DEF_SLBL(3,___L3_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOT(___VALUESP(___R1)))
   ___GOTO(___L60_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R2(___VALUESLENGTH(___R1))
   ___IF(___FIXEQ(___R2,___FIX(2L)))
   ___GOTO(___L42_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___GOTO(___L61_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(4,___L4_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___STK(-5))
___DEF_GLBL(___L42_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___VALUESREF(___R1,___FIX(0L)))
   ___IF(___NOT(___STRUCTUREDIOP(___STK(-6),___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)))
   ___GOTO(___L59_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___STK(-6),___FIX(1L),___FAL,___FAL))
___DEF_GLBL(___L43_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(-5,___R1)
   ___SET_R1(___R2)
   ___SET_R2(___CNS(0))
   ___SET_R0(___LBL(7))
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L45_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L44_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R3(___CAR(___R2))
   ___IF(___EQP(___R1,___R3))
   ___GOTO(___L46_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R2(___CDR(___R2))
   ___POLL(5)
___DEF_SLBL(5,___L5_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L44_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L45_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___DEF_SLBL(6,___L6_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___IF(___NOT(___NOTFALSEP(___R2)))
   ___GOTO(___L40_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L46_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___R2)
   ___JUMPRET(___R0)
___DEF_SLBL(7,___L7_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L47_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___GOTO(___L51_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(8,___L8_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L50_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L47_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___STK(-5))
   ___IF(___STRUCTUREDIOP(___STK(-6),___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t))
   ___GOTO(___L49_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L48_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(-5,___R1)
   ___SET_STK(1,___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___SET_R3(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_precedence_2d_list)
   ___SET_R0(___LBL(9))
   ___ADJFP(1)
   ___JUMPGLOSAFE(___SET_NARGS(4),6,___G_class_2d_slot_2d_set_21_)
___DEF_SLBL(9,___L9_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___STK(-5))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_SLBL(10,___L10_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOT(___STRUCTUREDIOP(___STK(-6),___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)))
   ___GOTO(___L48_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L49_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___UNCHECKEDSTRUCTURESET(___STK(-6),___R1,___FIX(5L),___FAL,___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L50_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___SYM__3a_t)
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_gx_23_core_2d_quote_2d_syntax)
___DEF_SLBL(11,___L11_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(10))
   ___CHECK_HEAP(12,4096)
___DEF_SLBL(12,___L12_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___JUMPPRM(___SET_NARGS(2),___PRM__23__23_append)
___DEF_GLBL(___L51_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___STK(-5))
   ___SET_R3(___GLO_gx_23_free_2d_identifier_3d__3f_)
   ___SET_R1(___SYM__3a_object)
   ___SET_R0(___LBL(13))
   ___JUMPPRM(___SET_NARGS(3),___PRM_member)
___DEF_SLBL(13,___L13_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L47_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___IF(___NOT(___STRUCTUREDIOP(___STK(-6),___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t)))
   ___GOTO(___L58_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___IF(___NOTFALSEP(___UNCHECKEDSTRUCTUREREF(___STK(-6),___FIX(9L),___FAL,___FAL)))
   ___GOTO(___L57_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___GOTO(___L52_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(14,___L14_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L57_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L52_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___STK(-5))
   ___SET_R2(___NUL)
   ___SET_R0(___LBL(10))
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L54_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L53_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R3(___CAR(___R1))
   ___SET_R4(___CDR(___R1))
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_STK(5,___R4)
   ___SET_R1(___R3)
   ___SET_R2(___SYM__3a_t)
   ___ADJFP(8)
   ___POLL(15)
___DEF_SLBL(15,___L15_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G_gx_23_free_2d_identifier_3d__3f_)
___DEF_SLBL(16,___L16_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L56_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R2(___CONS(___STK(-4),___STK(-5)))
   ___SET_R1(___STK(-3))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___CHECK_HEAP(17,4096)
___DEF_SLBL(17,___L17_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___POLL(18)
___DEF_SLBL(18,___L18_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___PAIRP(___R1))
   ___GOTO(___L53_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L54_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R1(___SYM__3a_object)
   ___ADJFP(8)
   ___POLL(19)
___DEF_SLBL(19,___L19_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(20))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_gx_23_core_2d_quote_2d_syntax)
___DEF_SLBL(20,___L20_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(-5,___R1)
   ___SET_R1(___SYM__3a_t)
   ___SET_R0(___LBL(21))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_gx_23_core_2d_quote_2d_syntax)
___DEF_SLBL(21,___L21_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_R2(___CONS(___STK(-5),___R1))
   ___SET_R3(___STK(-6))
   ___SET_R1(___PRM_cons)
   ___SET_R0(___STK(-7))
   ___CHECK_HEAP(22,4096)
___DEF_SLBL(22,___L22_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___POLL(23)
___DEF_SLBL(23,___L23_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___GOTO(___L55_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(24,___L24_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___CONS(___R1,___STK(-6)))
   ___SET_R3(___STK(-5))
   ___SET_R1(___PRM_cons)
   ___SET_R0(___STK(-7))
   ___CHECK_HEAP(25,4096)
___DEF_SLBL(25,___L25_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___POLL(26)
___DEF_SLBL(26,___L26_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_GLBL(___L55_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(3),9,___G_foldl)
___DEF_GLBL(___L56_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___SYM__3a_object)
   ___SET_R0(___LBL(24))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_gx_23_core_2d_quote_2d_syntax)
___DEF_GLBL(___L57_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___STK(-5))
   ___SET_R3(___GLO_gx_23_free_2d_identifier_3d__3f_)
   ___SET_R1(___SYM__3a_t)
   ___SET_R0(___LBL(8))
   ___JUMPPRM(___SET_NARGS(3),___PRM_member)
___DEF_GLBL(___L58_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___STK(-6))
   ___SET_R3(___SYM_system_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(3),5,___G_class_2d_slot_2d_ref)
___DEF_GLBL(___L59_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(-5,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R3(___SYM_id)
   ___SET_R1(___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___SET_R0(___LBL(27))
   ___JUMPGLOSAFE(___SET_NARGS(3),5,___G_class_2d_slot_2d_ref)
___DEF_SLBL(27,___L27_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___GOTO(___L43_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_GLBL(___L60_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___FIX(1L))
   ___IF(___FIXEQ(___R2,___FIX(2L)))
   ___GOTO(___L42_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
___DEF_GLBL(___L61_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(-5,___R1)
   ___SET_R1(___SUB(1))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(2),8,___G_error)
___DEF_SLBL(28,___L28_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(28,1,0,0)
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(29)
___DEF_SLBL(29,___L29_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(30))
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_gx_23_syntax_2d_local_2d_value)
___DEF_SLBL(30,___L30_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF(___STRUCTUREDIOP(___R1,___SYM_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t))
   ___GOTO(___L62_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___END_IF
   ___SET_R2(___R1)
   ___SET_R3(___SYM_struct_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___SET_R0(___STK(-3))
   ___POLL(31)
___DEF_SLBL(31,___L31_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(3),5,___G_class_2d_slot_2d_ref)
___DEF_GLBL(___L62_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(7L),___FAL,___FAL))
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___DEF_SLBL(32,___L32_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(32,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(33)
___DEF_SLBL(33,___L33_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(1))
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_gx_23_syntax_2d_local_2d_value)
___DEF_GLBL(___L63_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R2(___R1)
   ___SET_R3(___SYM_super)
   ___SET_R1(___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___ADJFP(3)
   ___POLL(34)
___DEF_SLBL(34,___L34_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(35))
   ___JUMPGLOSAFE(___SET_NARGS(3),5,___G_class_2d_slot_2d_ref)
___DEF_SLBL(35,___L35_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___ADJFP(-3)
   ___GOTO(___L41_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
___DEF_SLBL(36,___L36_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R1(___CONS(___STK(-6),___R1))
   ___ADJFP(-7)
   ___CHECK_HEAP(37,4096)
___DEF_SLBL(37,___L37_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L64_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___R1)
   ___SET_R3(___SYM_precedence_2d_list)
   ___SET_R1(___GLO_gerbil_2f_core_2f_mop_7e_MOP_2d_2_23_class_2d_type_2d_info_3a__3a_t)
   ___ADJFP(8)
   ___POLL(38)
___DEF_SLBL(38,___L38_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(3),5,___G_class_2d_slot_2d_ref)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____core____contract_7e_ClassMeta_23_,___REF_SYM(6,___S_gerbil____core____contract_7e_ClassMeta_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_gerbil____core____contract_7e_ClassMeta_23_,0,-1)
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___REF_SYM(4,___S_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list),___REF_FAL,39,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___OFD(___RETI,13,0,0x3f1f03L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,0,0x3f1fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___OFD(___RETI,12,0,0x3f103L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,9,0,0x103L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,___IFD(___RETI,8,0,0x3f03L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,13,0)
               ___GCMAP1(0x3f1f03L)
,___DEF_OFD(___RETI,12,0)
               ___GCMAP1(0x3f103L)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(2,___G_gerbil____core____contract_7e_ClassMeta_23_,1)
___DEF_MOD_PRM(0,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,3)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(2,___G_gerbil____core____contract_7e_ClassMeta_23_,1)
___DEF_MOD_GLO(0,___G_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,3)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__3a_object,":object")
___DEF_MOD_SYM(1,___S__3a_t,":t")
___DEF_MOD_SYM(2,___S_class,"class")
___DEF_MOD_SYM(3,___S_gerbil_2e_core_23_class_2d_type_2d_info_3a__3a_t,"gerbil.core#class-type-info::t")

___DEF_MOD_SYM(4,___S_gerbil_2f_core_2f_contract_7e_ClassMeta_23__21_class_2d_precedence_2d_list,"gerbil/core/contract~ClassMeta#!class-precedence-list")

___DEF_MOD_SYM(5,___S_gerbil____core____contract_7e_ClassMeta,"gerbil__core__contract~ClassMeta")

___DEF_MOD_SYM(6,___S_gerbil____core____contract_7e_ClassMeta_23_,"gerbil__core__contract~ClassMeta#")

___DEF_MOD_SYM(7,___S_id,"id")
___DEF_MOD_SYM(8,___S_object,"object")
___DEF_MOD_SYM(9,___S_precedence_2d_list,"precedence-list")
___DEF_MOD_SYM(10,___S_struct_3f_,"struct?")
___DEF_MOD_SYM(11,___S_super,"super")
___DEF_MOD_SYM(12,___S_system_3f_,"system?")
___DEF_MOD_SYM(13,___S_t,"t")
___DEF_MOD_KEY(0,___K_eq,"eq")
___DEF_MOD_KEY(1,___K_get_2d_precedence_2d_list,"get-precedence-list")
___DEF_MOD_KEY(2,___K_struct,"struct")
___END_MOD_SYM_KEY

#endif
