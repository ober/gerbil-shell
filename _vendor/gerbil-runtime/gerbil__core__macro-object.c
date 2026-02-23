#ifdef ___LINKER_INFO
; File: "gerbil__core__macro-object.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__core__macro-object"
("gerbil__core__macro-object")
()
(("gerbil__core__macro-object"))
( #|*/"*/"symbols|#
"apply-macro-expander"
"gerbil.core#macro-object::t"
"gerbil/core/macro-object#macro-object::apply-macro-expander"
"gerbil/core/macro-object#macro-object::apply-macro-expander::specialize"
"gerbil/core/macro-object#make-macro-object"
"gerbil__core__macro-object"
"gerbil__core__macro-object#"
"macro"
"macro-object"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil/core/macro-object#macro-object::apply-macro-expander"
"gerbil/core/macro-object#macro-object::apply-macro-expander::specialize"
"gerbil/core/macro-object#macro-object::t"
"gerbil__core__macro-object#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/core/macro-object#&macro-object-macro"
"gerbil/core/macro-object#&macro-object-macro-set!"
"gerbil/core/macro-object#macro-object-macro"
"gerbil/core/macro-object#macro-object-macro-set!"
"gerbil/core/macro-object#macro-object?"
"gerbil/core/macro-object#make-macro-object"
"gerbil/core/macro-object::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"apply"
"bind-method!"
"bind-specializer!"
"class-slot-offset"
"class-slot-ref"
"error"
"gx#core-apply-expander"
"make-class-predicate"
"make-class-slot-accessor"
"make-class-slot-mutator"
"make-class-slot-unchecked-accessor"
"make-class-slot-unchecked-mutator"
"make-class-type"
"make-instance"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__core__macro-object"
#define ___LINKER_ID ___LNK_gerbil____core____macro_2d_object
#define ___MH_PROC ___H_gerbil____core____macro_2d_object
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 9
#define ___GLOCOUNT 25
#define ___SUPCOUNT 11
#define ___CNSCOUNT 1
#define ___SUBCOUNT 5
#define ___LBLCOUNT 28
#define ___MODDESCR ___REF_SUB(2)
#include "gambit.h"

___NEED_SYM(___S_apply_2d_macro_2d_expander)
___NEED_SYM(___S_gerbil_2e_core_23_macro_2d_object_3a__3a_t)
___NEED_SYM(___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___NEED_SYM(___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___NEED_SYM(___S_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___NEED_SYM(___S_gerbil____core____macro_2d_object)
___NEED_SYM(___S_gerbil____core____macro_2d_object_23_)
___NEED_SYM(___S_macro)
___NEED_SYM(___S_macro_2d_object)

___NEED_GLO(___G_apply)
___NEED_GLO(___G_bind_2d_method_21_)
___NEED_GLO(___G_bind_2d_specializer_21_)
___NEED_GLO(___G_class_2d_slot_2d_offset)
___NEED_GLO(___G_class_2d_slot_2d_ref)
___NEED_GLO(___G_error)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___NEED_GLO(___G_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp)
___NEED_GLO(___G_gerbil____core____macro_2d_object_23_)
___NEED_GLO(___G_gx_23_core_2d_apply_2d_expander)
___NEED_GLO(___G_make_2d_class_2d_predicate)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_accessor)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_mutator)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___NEED_GLO(___G_make_2d_class_2d_type)
___NEED_GLO(___G_make_2d_instance)

___BEGIN_SYM
___DEF_SYM(0,___S_apply_2d_macro_2d_expander,"apply-macro-expander")
___DEF_SYM(1,___S_gerbil_2e_core_23_macro_2d_object_3a__3a_t,"gerbil.core#macro-object::t")
___DEF_SYM(2,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,"gerbil/core/macro-object#macro-object::apply-macro-expander")

___DEF_SYM(3,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,"gerbil/core/macro-object#macro-object::apply-macro-expander::specialize")

___DEF_SYM(4,___S_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,"gerbil/core/macro-object#make-macro-object")

___DEF_SYM(5,___S_gerbil____core____macro_2d_object,"gerbil__core__macro-object")
___DEF_SYM(6,___S_gerbil____core____macro_2d_object_23_,"gerbil__core__macro-object#")
___DEF_SYM(7,___S_macro,"macro")
___DEF_SYM(8,___S_macro_2d_object,"macro-object")
___END_SYM

#define ___SYM_apply_2d_macro_2d_expander ___SYM(0,___S_apply_2d_macro_2d_expander)
#define ___SYM_gerbil_2e_core_23_macro_2d_object_3a__3a_t ___SYM(1,___S_gerbil_2e_core_23_macro_2d_object_3a__3a_t)
#define ___SYM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander ___SYM(2,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
#define ___SYM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize ___SYM(3,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
#define ___SYM_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object ___SYM(4,___S_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
#define ___SYM_gerbil____core____macro_2d_object ___SYM(5,___S_gerbil____core____macro_2d_object)
#define ___SYM_gerbil____core____macro_2d_object_23_ ___SYM(6,___S_gerbil____core____macro_2d_object_23_)
#define ___SYM_macro ___SYM(7,___S_macro)
#define ___SYM_macro_2d_object ___SYM(8,___S_macro_2d_object)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/core/macro-object#&macro-object-macro")

___DEF_GLO(1,"gerbil/core/macro-object#&macro-object-macro-set!")

___DEF_GLO(2,"gerbil/core/macro-object#macro-object-macro")

___DEF_GLO(3,"gerbil/core/macro-object#macro-object-macro-set!")

___DEF_GLO(4,"gerbil/core/macro-object#macro-object::apply-macro-expander")

___DEF_GLO(5,"gerbil/core/macro-object#macro-object::apply-macro-expander::specialize")

___DEF_GLO(6,"gerbil/core/macro-object#macro-object::t")

___DEF_GLO(7,"gerbil/core/macro-object#macro-object?")

___DEF_GLO(8,"gerbil/core/macro-object#make-macro-object")

___DEF_GLO(9,"gerbil/core/macro-object::timestamp")

___DEF_GLO(10,"gerbil__core__macro-object#")
___DEF_GLO(11,"apply")
___DEF_GLO(12,"bind-method!")
___DEF_GLO(13,"bind-specializer!")
___DEF_GLO(14,"class-slot-offset")
___DEF_GLO(15,"class-slot-ref")
___DEF_GLO(16,"error")
___DEF_GLO(17,"gx#core-apply-expander")
___DEF_GLO(18,"make-class-predicate")
___DEF_GLO(19,"make-class-slot-accessor")
___DEF_GLO(20,"make-class-slot-mutator")
___DEF_GLO(21,"make-class-slot-unchecked-accessor")

___DEF_GLO(22,"make-class-slot-unchecked-mutator")

___DEF_GLO(23,"make-class-type")
___DEF_GLO(24,"make-instance")
___END_GLO

#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro ___GLO(0,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro ___PRM(0,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_ ___GLO(1,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_ ___PRM(1,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro ___GLO(2,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro ___PRM(2,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_ ___GLO(3,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_ ___PRM(3,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander ___GLO(4,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander ___PRM(4,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize ___GLO(5,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize ___PRM(5,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t ___GLO(6,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t ___PRM(6,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_ ___GLO(7,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_ ___PRM(7,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object ___GLO(8,___G_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object ___PRM(8,___G_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
#define ___GLO_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp ___GLO(9,___G_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp)
#define ___PRM_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp ___PRM(9,___G_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp)
#define ___GLO_gerbil____core____macro_2d_object_23_ ___GLO(10,___G_gerbil____core____macro_2d_object_23_)
#define ___PRM_gerbil____core____macro_2d_object_23_ ___PRM(10,___G_gerbil____core____macro_2d_object_23_)
#define ___GLO_apply ___GLO(11,___G_apply)
#define ___PRM_apply ___PRM(11,___G_apply)
#define ___GLO_bind_2d_method_21_ ___GLO(12,___G_bind_2d_method_21_)
#define ___PRM_bind_2d_method_21_ ___PRM(12,___G_bind_2d_method_21_)
#define ___GLO_bind_2d_specializer_21_ ___GLO(13,___G_bind_2d_specializer_21_)
#define ___PRM_bind_2d_specializer_21_ ___PRM(13,___G_bind_2d_specializer_21_)
#define ___GLO_class_2d_slot_2d_offset ___GLO(14,___G_class_2d_slot_2d_offset)
#define ___PRM_class_2d_slot_2d_offset ___PRM(14,___G_class_2d_slot_2d_offset)
#define ___GLO_class_2d_slot_2d_ref ___GLO(15,___G_class_2d_slot_2d_ref)
#define ___PRM_class_2d_slot_2d_ref ___PRM(15,___G_class_2d_slot_2d_ref)
#define ___GLO_error ___GLO(16,___G_error)
#define ___PRM_error ___PRM(16,___G_error)
#define ___GLO_gx_23_core_2d_apply_2d_expander ___GLO(17,___G_gx_23_core_2d_apply_2d_expander)
#define ___PRM_gx_23_core_2d_apply_2d_expander ___PRM(17,___G_gx_23_core_2d_apply_2d_expander)
#define ___GLO_make_2d_class_2d_predicate ___GLO(18,___G_make_2d_class_2d_predicate)
#define ___PRM_make_2d_class_2d_predicate ___PRM(18,___G_make_2d_class_2d_predicate)
#define ___GLO_make_2d_class_2d_slot_2d_accessor ___GLO(19,___G_make_2d_class_2d_slot_2d_accessor)
#define ___PRM_make_2d_class_2d_slot_2d_accessor ___PRM(19,___G_make_2d_class_2d_slot_2d_accessor)
#define ___GLO_make_2d_class_2d_slot_2d_mutator ___GLO(20,___G_make_2d_class_2d_slot_2d_mutator)
#define ___PRM_make_2d_class_2d_slot_2d_mutator ___PRM(20,___G_make_2d_class_2d_slot_2d_mutator)
#define ___GLO_make_2d_class_2d_slot_2d_unchecked_2d_accessor ___GLO(21,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
#define ___PRM_make_2d_class_2d_slot_2d_unchecked_2d_accessor ___PRM(21,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
#define ___GLO_make_2d_class_2d_slot_2d_unchecked_2d_mutator ___GLO(22,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
#define ___PRM_make_2d_class_2d_slot_2d_unchecked_2d_mutator ___PRM(22,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
#define ___GLO_make_2d_class_2d_type ___GLO(23,___G_make_2d_class_2d_type)
#define ___PRM_make_2d_class_2d_type ___PRM(23,___G_make_2d_class_2d_type)
#define ___GLO_make_2d_instance ___GLO(24,___G_make_2d_instance)
#define ___PRM_make_2d_instance ___PRM(24,___G_make_2d_instance)

___BEGIN_CNS
 ___DEF_CNS(___REF_SYM(7,___S_macro),___REF_NUL)
___END_CNS

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2a4aL)
___DEF_SUB_STR(___X1,12UL)
               ___STR8(85,110,107,110,111,119,110,32)
               ___STR4(115,108,111,116)
___DEF_SUB_VEC(___X2,6UL)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SUB(4))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,1UL)
               ___VEC1(___REF_SYM(5,___S_gerbil____core____macro_2d_object))
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
___DEF_M_HLBL(___L0_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L1_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L2_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L3_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L4_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L5_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L6_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L7_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L8_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL(___L9_gerbil____core____macro_2d_object_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_M_HLBL(___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_M_HLBL(___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_M_HLBL(___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L5_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_M_HLBL(___L6_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____core____macro_2d_object_23_
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
___DEF_P_HLBL(___L0_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L1_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L2_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L3_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L4_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L5_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L6_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L7_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L8_gerbil____core____macro_2d_object_23_)
___DEF_P_HLBL(___L9_gerbil____core____macro_2d_object_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____core____macro_2d_object_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(9,___G_gerbil_2f_core_2f_macro_2d_object_3a__3a_timestamp,___BIGFIX(0,1770924618LL))
   ___SET_STK(1,___R0)
   ___SET_STK(5,___SYM_gerbil_2e_core_23_macro_2d_object_3a__3a_t)
   ___SET_STK(6,___SYM_macro_2d_object)
   ___SET_STK(7,___NUL)
   ___SET_R3(___FAL)
   ___SET_R2(___NUL)
   ___SET_R1(___CNS(0))
   ___ADJFP(7)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil____core____macro_2d_object_23_)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(6),23,___G_make_2d_class_2d_type)
___DEF_SLBL(2,___L2_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(6,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t,___R1)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),18,___G_make_2d_class_2d_predicate)
___DEF_SLBL(3,___L3_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(7,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3f_,___R1)
   ___SET_R2(___SYM_macro)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(2),19,___G_make_2d_class_2d_slot_2d_accessor)
___DEF_SLBL(4,___L4_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(2,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro,___R1)
   ___SET_R2(___SYM_macro)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(2),20,___G_make_2d_class_2d_slot_2d_mutator)
___DEF_SLBL(5,___L5_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(3,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_2d_macro_2d_set_21_,___R1)
   ___SET_R2(___SYM_macro)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(2),21,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___DEF_SLBL(6,___L6_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(0,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro,___R1)
   ___SET_R2(___SYM_macro)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(2),22,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___DEF_SLBL(7,___L7_gerbil____core____macro_2d_object_23_)
   ___SET_GLO(1,___G_gerbil_2f_core_2f_macro_2d_object_23__26_macro_2d_object_2d_macro_2d_set_21_,___R1)
   ___SET_R2(___PRC(21))
   ___SET_R1(___PRC(15))
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(2),13,___G_bind_2d_specializer_21_)
___DEF_SLBL(8,___L8_gerbil____core____macro_2d_object_23_)
   ___SET_STK(-2,___STK(-3))
   ___SET_STK(-3,___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R3(___FAL)
   ___SET_R2(___PRC(15))
   ___SET_R1(___SYM_apply_2d_macro_2d_expander)
   ___SET_R0(___STK(-2))
   ___POLL(9)
___DEF_SLBL(9,___L9_gerbil____core____macro_2d_object_23_)
   ___ADJFP(-3)
   ___JUMPGLOSAFE(___SET_NARGS(4),12,___G_bind_2d_method_21_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object
#undef ___PH_LBL0
#define ___PH_LBL0 12
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(0,0,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
   ___SET_R3(___R1)
   ___SET_R2(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___SET_R1(___GLO_make_2d_instance)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object)
   ___JUMPPRM(___SET_NARGS(3),___PRM_apply)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander
#undef ___PH_LBL0
#define ___PH_LBL0 15
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_P_HLBL(___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_P_HLBL(___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_P_HLBL(___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___IF(___NOT(___STRUCTUREDIOP(___R1,___SYM_gerbil_2e_core_23_macro_2d_object_3a__3a_t)))
   ___GOTO(___L6_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___FAL,___FAL))
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___GOTO(___L5_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_SLBL(2,___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(3)
___DEF_SLBL(3,___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
___DEF_GLBL(___L5_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___JUMPGLOSAFE(___SET_NARGS(2),17,___G_gx_23_core_2d_apply_2d_expander)
___DEF_GLBL(___L6_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R2(___R1)
   ___SET_R3(___SYM_macro)
   ___SET_R1(___GLO_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_t)
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(3),15,___G_class_2d_slot_2d_ref)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize
#undef ___PH_LBL0
#define ___PH_LBL0 21
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L5_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_P_HLBL(___L6_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___SET_STK(1,___R0)
   ___SET_R2(___SYM_macro)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(2),14,___G_class_2d_slot_2d_offset)
___DEF_SLBL(2,___L2_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L7_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___END_IF
   ___SET_R2(___SYM_macro)
   ___SET_R1(___SUB(1))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(2),16,___G_error)
___DEF_SLBL(3,___L3_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
___DEF_GLBL(___L7_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___SET_STK(-2,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(-2),5)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R1(___STK(-2))
   ___ADJFP(-3)
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___ADJFP(-1)
   ___JUMPRET(___STK(1))
___DEF_SLBL(5,___L5_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(5,2,0,0)
   ___SET_R4(___CLO(___R4,1))
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___R4,___FAL,___FAL))
   ___POLL(6)
___DEF_SLBL(6,___L6_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize)
   ___JUMPGLOSAFE(___SET_NARGS(2),17,___G_gx_23_core_2d_apply_2d_expander)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____core____macro_2d_object_23_,___REF_SYM(6,___S_gerbil____core____macro_2d_object_23_),___REF_FAL,10,0)
,___DEF_LBL_PROC(___H_gerbil____core____macro_2d_object_23_,0,-1)
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETI,7,0,0x3f71L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____macro_2d_object_23_,___IFD(___RETI,4,4,0x3f1L))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,___REF_SYM(4,___S_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,___REF_SYM(2,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander),___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,2,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___REF_SYM(3,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize),___REF_FAL,7,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,2,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___IFD(___RETI,1,0,0x3f1L))
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,2,1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(10,___G_gerbil____core____macro_2d_object_23_,1)
___DEF_MOD_PRM(8,___G_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,12)
___DEF_MOD_PRM(4,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,15)
___DEF_MOD_PRM(5,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,21)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(10,___G_gerbil____core____macro_2d_object_23_,1)
___DEF_MOD_GLO(8,___G_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,12)
___DEF_MOD_GLO(4,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,15)
___DEF_MOD_GLO(5,___G_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,21)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_apply_2d_macro_2d_expander,"apply-macro-expander")
___DEF_MOD_SYM(1,___S_gerbil_2e_core_23_macro_2d_object_3a__3a_t,"gerbil.core#macro-object::t")
___DEF_MOD_SYM(2,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander,"gerbil/core/macro-object#macro-object::apply-macro-expander")

___DEF_MOD_SYM(3,___S_gerbil_2f_core_2f_macro_2d_object_23_macro_2d_object_3a__3a_apply_2d_macro_2d_expander_3a__3a_specialize,"gerbil/core/macro-object#macro-object::apply-macro-expander::specialize")

___DEF_MOD_SYM(4,___S_gerbil_2f_core_2f_macro_2d_object_23_make_2d_macro_2d_object,"gerbil/core/macro-object#make-macro-object")

___DEF_MOD_SYM(5,___S_gerbil____core____macro_2d_object,"gerbil__core__macro-object")
___DEF_MOD_SYM(6,___S_gerbil____core____macro_2d_object_23_,"gerbil__core__macro-object#")
___DEF_MOD_SYM(7,___S_macro,"macro")
___DEF_MOD_SYM(8,___S_macro_2d_object,"macro-object")
___END_MOD_SYM_KEY

#endif
