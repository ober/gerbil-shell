#ifdef ___LINKER_INFO
; File: "gerbil__core__contract~TypeEnv.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__core__contract~TypeEnv"
("gerbil__core__contract~TypeEnv")
()
(("gerbil__core__contract~TypeEnv"))
( #|*/"*/"symbols|#
"@@type"
"checked?"
"gerbil/core/contract~TypeEnv#current-type-env"
"gerbil/core/contract~TypeEnv#make-type-env"
"gerbil/core/contract~TypeEnv#type-env-lookup"
"gerbil/core/contract~TypeEnv#type-env::t"
"gerbil__core__contract~TypeEnv"
"gerbil__core__contract~TypeEnv#"
"super"
"type"
"type-env"
"var"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"final"
"struct"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil/core/contract~TypeEnv#type-env::t"
"gerbil__core__contract~TypeEnv#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/core/contract~TypeEnv#&type-env-checked?"
"gerbil/core/contract~TypeEnv#&type-env-checked?-set!"
"gerbil/core/contract~TypeEnv#&type-env-super"
"gerbil/core/contract~TypeEnv#&type-env-super-set!"
"gerbil/core/contract~TypeEnv#&type-env-type"
"gerbil/core/contract~TypeEnv#&type-env-type-set!"
"gerbil/core/contract~TypeEnv#&type-env-var"
"gerbil/core/contract~TypeEnv#&type-env-var-set!"
"gerbil/core/contract~TypeEnv#current-type-env"
"gerbil/core/contract~TypeEnv#make-type-env"
"gerbil/core/contract~TypeEnv#type-env-checked?"
"gerbil/core/contract~TypeEnv#type-env-checked?-set!"
"gerbil/core/contract~TypeEnv#type-env-lookup"
"gerbil/core/contract~TypeEnv#type-env-super"
"gerbil/core/contract~TypeEnv#type-env-super-set!"
"gerbil/core/contract~TypeEnv#type-env-type"
"gerbil/core/contract~TypeEnv#type-env-type-set!"
"gerbil/core/contract~TypeEnv#type-env-var"
"gerbil/core/contract~TypeEnv#type-env-var-set!"
"gerbil/core/contract~TypeEnv#type-env?"
"gerbil/core/contract~TypeEnv::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##dead-end"
"##direct-structure-ref"
"apply"
"false"
"gx#free-identifier=?"
"gx#syntax-local-introduce"
"gx#syntax-local-value"
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
#define ___MODULE_NAME "gerbil__core__contract~TypeEnv"
#define ___LINKER_ID ___LNK_gerbil____core____contract_7e_TypeEnv
#define ___MH_PROC ___H_gerbil____core____contract_7e_TypeEnv
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 12
#define ___KEYCOUNT 2
#define ___GLOCOUNT 37
#define ___SUPCOUNT 23
#define ___CNSCOUNT 4
#define ___SUBCOUNT 4
#define ___LBLCOUNT 42
#define ___MODDESCR ___REF_SUB(1)
#include "gambit.h"

___NEED_SYM(___S__40__40_type)
___NEED_SYM(___S_checked_3f_)
___NEED_SYM(___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___NEED_SYM(___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___NEED_SYM(___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___NEED_SYM(___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
___NEED_SYM(___S_gerbil____core____contract_7e_TypeEnv)
___NEED_SYM(___S_gerbil____core____contract_7e_TypeEnv_23_)
___NEED_SYM(___S_super)
___NEED_SYM(___S_type)
___NEED_SYM(___S_type_2d_env)
___NEED_SYM(___S_var)

___NEED_KEY(___K_final)
___NEED_KEY(___K_struct)

___NEED_GLO(___G__23__23_dead_2d_end)
___NEED_GLO(___G__23__23_direct_2d_structure_2d_ref)
___NEED_GLO(___G_apply)
___NEED_GLO(___G_false)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_)
___NEED_GLO(___G_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp)
___NEED_GLO(___G_gerbil____core____contract_7e_TypeEnv_23_)
___NEED_GLO(___G_gx_23_free_2d_identifier_3d__3f_)
___NEED_GLO(___G_gx_23_syntax_2d_local_2d_introduce)
___NEED_GLO(___G_gx_23_syntax_2d_local_2d_value)
___NEED_GLO(___G_make_2d_class_2d_predicate)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_accessor)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_mutator)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___NEED_GLO(___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___NEED_GLO(___G_make_2d_class_2d_type)
___NEED_GLO(___G_make_2d_instance)

___BEGIN_SYM
___DEF_SYM(0,___S__40__40_type,"@@type")
___DEF_SYM(1,___S_checked_3f_,"checked?")
___DEF_SYM(2,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,"gerbil/core/contract~TypeEnv#current-type-env")

___DEF_SYM(3,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,"gerbil/core/contract~TypeEnv#make-type-env")

___DEF_SYM(4,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,"gerbil/core/contract~TypeEnv#type-env-lookup")

___DEF_SYM(5,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,"gerbil/core/contract~TypeEnv#type-env::t")

___DEF_SYM(6,___S_gerbil____core____contract_7e_TypeEnv,"gerbil__core__contract~TypeEnv")

___DEF_SYM(7,___S_gerbil____core____contract_7e_TypeEnv_23_,"gerbil__core__contract~TypeEnv#")

___DEF_SYM(8,___S_super,"super")
___DEF_SYM(9,___S_type,"type")
___DEF_SYM(10,___S_type_2d_env,"type-env")
___DEF_SYM(11,___S_var,"var")
___END_SYM

#define ___SYM__40__40_type ___SYM(0,___S__40__40_type)
#define ___SYM_checked_3f_ ___SYM(1,___S_checked_3f_)
#define ___SYM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env ___SYM(2,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
#define ___SYM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env ___SYM(3,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
#define ___SYM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup ___SYM(4,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
#define ___SYM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t ___SYM(5,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
#define ___SYM_gerbil____core____contract_7e_TypeEnv ___SYM(6,___S_gerbil____core____contract_7e_TypeEnv)
#define ___SYM_gerbil____core____contract_7e_TypeEnv_23_ ___SYM(7,___S_gerbil____core____contract_7e_TypeEnv_23_)
#define ___SYM_super ___SYM(8,___S_super)
#define ___SYM_type ___SYM(9,___S_type)
#define ___SYM_type_2d_env ___SYM(10,___S_type_2d_env)
#define ___SYM_var ___SYM(11,___S_var)

___BEGIN_KEY
___DEF_KEY(0,___K_final,"final")
___DEF_KEY(1,___K_struct,"struct")
___END_KEY

#define ___KEY_final ___KEY(0,___K_final)
#define ___KEY_struct ___KEY(1,___K_struct)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/core/contract~TypeEnv#&type-env-checked?")

___DEF_GLO(1,"gerbil/core/contract~TypeEnv#&type-env-checked?-set!")

___DEF_GLO(2,"gerbil/core/contract~TypeEnv#&type-env-super")

___DEF_GLO(3,"gerbil/core/contract~TypeEnv#&type-env-super-set!")

___DEF_GLO(4,"gerbil/core/contract~TypeEnv#&type-env-type")

___DEF_GLO(5,"gerbil/core/contract~TypeEnv#&type-env-type-set!")

___DEF_GLO(6,"gerbil/core/contract~TypeEnv#&type-env-var")

___DEF_GLO(7,"gerbil/core/contract~TypeEnv#&type-env-var-set!")

___DEF_GLO(8,"gerbil/core/contract~TypeEnv#current-type-env")

___DEF_GLO(9,"gerbil/core/contract~TypeEnv#make-type-env")

___DEF_GLO(10,"gerbil/core/contract~TypeEnv#type-env-checked?")

___DEF_GLO(11,"gerbil/core/contract~TypeEnv#type-env-checked?-set!")

___DEF_GLO(12,"gerbil/core/contract~TypeEnv#type-env-lookup")

___DEF_GLO(13,"gerbil/core/contract~TypeEnv#type-env-super")

___DEF_GLO(14,"gerbil/core/contract~TypeEnv#type-env-super-set!")

___DEF_GLO(15,"gerbil/core/contract~TypeEnv#type-env-type")

___DEF_GLO(16,"gerbil/core/contract~TypeEnv#type-env-type-set!")

___DEF_GLO(17,"gerbil/core/contract~TypeEnv#type-env-var")

___DEF_GLO(18,"gerbil/core/contract~TypeEnv#type-env-var-set!")

___DEF_GLO(19,"gerbil/core/contract~TypeEnv#type-env::t")

___DEF_GLO(20,"gerbil/core/contract~TypeEnv#type-env?")

___DEF_GLO(21,"gerbil/core/contract~TypeEnv::timestamp")

___DEF_GLO(22,"gerbil__core__contract~TypeEnv#")
___DEF_GLO(23,"##dead-end")
___DEF_GLO(24,"##direct-structure-ref")
___DEF_GLO(25,"apply")
___DEF_GLO(26,"false")
___DEF_GLO(27,"gx#free-identifier=?")
___DEF_GLO(28,"gx#syntax-local-introduce")
___DEF_GLO(29,"gx#syntax-local-value")
___DEF_GLO(30,"make-class-predicate")
___DEF_GLO(31,"make-class-slot-accessor")
___DEF_GLO(32,"make-class-slot-mutator")
___DEF_GLO(33,"make-class-slot-unchecked-accessor")

___DEF_GLO(34,"make-class-slot-unchecked-mutator")

___DEF_GLO(35,"make-class-type")
___DEF_GLO(36,"make-instance")
___END_GLO

#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_ ___GLO(0,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_ ___PRM(0,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_ ___GLO(1,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_ ___PRM(1,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super ___GLO(2,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super ___PRM(2,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_ ___GLO(3,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_ ___PRM(3,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type ___GLO(4,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type ___PRM(4,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_ ___GLO(5,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_ ___PRM(5,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var ___GLO(6,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var ___PRM(6,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_ ___GLO(7,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_ ___PRM(7,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env ___GLO(8,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env ___PRM(8,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env ___GLO(9,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env ___PRM(9,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_ ___GLO(10,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_ ___PRM(10,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_ ___GLO(11,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_ ___PRM(11,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup ___GLO(12,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup ___PRM(12,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super ___GLO(13,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super ___PRM(13,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_ ___GLO(14,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_ ___PRM(14,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type ___GLO(15,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type ___PRM(15,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_ ___GLO(16,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_ ___PRM(16,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var ___GLO(17,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var ___PRM(17,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_ ___GLO(18,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_ ___PRM(18,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t ___GLO(19,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t ___PRM(19,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_ ___GLO(20,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_ ___PRM(20,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_)
#define ___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp ___GLO(21,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp)
#define ___PRM_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp ___PRM(21,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp)
#define ___GLO_gerbil____core____contract_7e_TypeEnv_23_ ___GLO(22,___G_gerbil____core____contract_7e_TypeEnv_23_)
#define ___PRM_gerbil____core____contract_7e_TypeEnv_23_ ___PRM(22,___G_gerbil____core____contract_7e_TypeEnv_23_)
#define ___GLO__23__23_dead_2d_end ___GLO(23,___G__23__23_dead_2d_end)
#define ___PRM__23__23_dead_2d_end ___PRM(23,___G__23__23_dead_2d_end)
#define ___GLO__23__23_direct_2d_structure_2d_ref ___GLO(24,___G__23__23_direct_2d_structure_2d_ref)
#define ___PRM__23__23_direct_2d_structure_2d_ref ___PRM(24,___G__23__23_direct_2d_structure_2d_ref)
#define ___GLO_apply ___GLO(25,___G_apply)
#define ___PRM_apply ___PRM(25,___G_apply)
#define ___GLO_false ___GLO(26,___G_false)
#define ___PRM_false ___PRM(26,___G_false)
#define ___GLO_gx_23_free_2d_identifier_3d__3f_ ___GLO(27,___G_gx_23_free_2d_identifier_3d__3f_)
#define ___PRM_gx_23_free_2d_identifier_3d__3f_ ___PRM(27,___G_gx_23_free_2d_identifier_3d__3f_)
#define ___GLO_gx_23_syntax_2d_local_2d_introduce ___GLO(28,___G_gx_23_syntax_2d_local_2d_introduce)
#define ___PRM_gx_23_syntax_2d_local_2d_introduce ___PRM(28,___G_gx_23_syntax_2d_local_2d_introduce)
#define ___GLO_gx_23_syntax_2d_local_2d_value ___GLO(29,___G_gx_23_syntax_2d_local_2d_value)
#define ___PRM_gx_23_syntax_2d_local_2d_value ___PRM(29,___G_gx_23_syntax_2d_local_2d_value)
#define ___GLO_make_2d_class_2d_predicate ___GLO(30,___G_make_2d_class_2d_predicate)
#define ___PRM_make_2d_class_2d_predicate ___PRM(30,___G_make_2d_class_2d_predicate)
#define ___GLO_make_2d_class_2d_slot_2d_accessor ___GLO(31,___G_make_2d_class_2d_slot_2d_accessor)
#define ___PRM_make_2d_class_2d_slot_2d_accessor ___PRM(31,___G_make_2d_class_2d_slot_2d_accessor)
#define ___GLO_make_2d_class_2d_slot_2d_mutator ___GLO(32,___G_make_2d_class_2d_slot_2d_mutator)
#define ___PRM_make_2d_class_2d_slot_2d_mutator ___PRM(32,___G_make_2d_class_2d_slot_2d_mutator)
#define ___GLO_make_2d_class_2d_slot_2d_unchecked_2d_accessor ___GLO(33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
#define ___PRM_make_2d_class_2d_slot_2d_unchecked_2d_accessor ___PRM(33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
#define ___GLO_make_2d_class_2d_slot_2d_unchecked_2d_mutator ___GLO(34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
#define ___PRM_make_2d_class_2d_slot_2d_unchecked_2d_mutator ___PRM(34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
#define ___GLO_make_2d_class_2d_type ___GLO(35,___G_make_2d_class_2d_type)
#define ___PRM_make_2d_class_2d_type ___PRM(35,___G_make_2d_class_2d_type)
#define ___GLO_make_2d_instance ___GLO(36,___G_make_2d_instance)
#define ___PRM_make_2d_instance ___PRM(36,___G_make_2d_instance)

___BEGIN_CNS
 ___DEF_CNS(___REF_SYM(11,___S_var),___REF_CNS(1))
,___DEF_CNS(___REF_SYM(9,___S_type),___REF_CNS(2))
,___DEF_CNS(___REF_SYM(1,___S_checked_3f_),___REF_CNS(3))
,___DEF_CNS(___REF_SYM(8,___S_super),___REF_NUL)
___END_CNS

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2a4cL)
___DEF_SUB_VEC(___X1,6UL)
               ___VEC1(___REF_SUB(2))
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X2,1UL)
               ___VEC1(___REF_SYM(6,___S_gerbil____core____contract_7e_TypeEnv))
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
___DEF_M_HLBL(___L0_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L1_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L2_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L3_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L4_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L5_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L6_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L7_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L8_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L9_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L10_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L11_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L12_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L13_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L14_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L15_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L16_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L17_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L18_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L19_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL(___L20_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_M_HLBL(___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_M_HLBL(___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L4_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L5_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L6_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L7_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L8_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L9_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_M_HLBL(___L10_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____core____contract_7e_TypeEnv_23_
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L1_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L2_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L3_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L4_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L5_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L6_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L7_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L8_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L9_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L10_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L11_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L12_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L13_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L14_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L15_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L16_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L17_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L18_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L19_gerbil____core____contract_7e_TypeEnv_23_)
___DEF_P_HLBL(___L20_gerbil____core____contract_7e_TypeEnv_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____core____contract_7e_TypeEnv_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(21,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_3a__3a_timestamp,___BIGFIX(0,1770924620LL))
   ___SET_STK(1,___R0)
   ___SET_STK(5,___SYM_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_STK(6,___SYM_type_2d_env)
   ___SET_STK(7,___NUL)
   ___SET_R3(___FAL)
   ___SET_R1(___CONS(___KEY_final,___TRU))
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_R2(___CONS(___KEY_struct,___TRU))
   ___SET_R2(___CONS(___R2,___R1))
   ___SET_R1(___CNS(0))
   ___ADJFP(7)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_gerbil____core____contract_7e_TypeEnv_23_)
   ___POLL(2)
___DEF_SLBL(2,___L2_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(6),35,___G_make_2d_class_2d_type)
___DEF_SLBL(3,___L3_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(19,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,___R1)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),30,___G_make_2d_class_2d_predicate)
___DEF_SLBL(4,___L4_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(20,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3f_,___R1)
   ___SET_R2(___SYM_var)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(2),31,___G_make_2d_class_2d_slot_2d_accessor)
___DEF_SLBL(5,___L5_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(17,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var,___R1)
   ___SET_R2(___SYM_type)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(2),31,___G_make_2d_class_2d_slot_2d_accessor)
___DEF_SLBL(6,___L6_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(15,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type,___R1)
   ___SET_R2(___SYM_checked_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(2),31,___G_make_2d_class_2d_slot_2d_accessor)
___DEF_SLBL(7,___L7_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(10,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f_,___R1)
   ___SET_R2(___SYM_super)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(2),31,___G_make_2d_class_2d_slot_2d_accessor)
___DEF_SLBL(8,___L8_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(13,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super,___R1)
   ___SET_R2(___SYM_var)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(2),32,___G_make_2d_class_2d_slot_2d_mutator)
___DEF_SLBL(9,___L9_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(18,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_var_2d_set_21_,___R1)
   ___SET_R2(___SYM_type)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(2),32,___G_make_2d_class_2d_slot_2d_mutator)
___DEF_SLBL(10,___L10_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(16,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_type_2d_set_21_,___R1)
   ___SET_R2(___SYM_checked_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(2),32,___G_make_2d_class_2d_slot_2d_mutator)
___DEF_SLBL(11,___L11_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(11,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_checked_3f__2d_set_21_,___R1)
   ___SET_R2(___SYM_super)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(2),32,___G_make_2d_class_2d_slot_2d_mutator)
___DEF_SLBL(12,___L12_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(14,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_super_2d_set_21_,___R1)
   ___SET_R2(___SYM_var)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(2),33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___DEF_SLBL(13,___L13_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(6,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var,___R1)
   ___SET_R2(___SYM_type)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(2),33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___DEF_SLBL(14,___L14_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(4,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type,___R1)
   ___SET_R2(___SYM_checked_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(2),33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___DEF_SLBL(15,___L15_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(0,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f_,___R1)
   ___SET_R2(___SYM_super)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(2),33,___G_make_2d_class_2d_slot_2d_unchecked_2d_accessor)
___DEF_SLBL(16,___L16_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(2,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super,___R1)
   ___SET_R2(___SYM_var)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(17))
   ___JUMPGLOSAFE(___SET_NARGS(2),34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___DEF_SLBL(17,___L17_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(7,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_var_2d_set_21_,___R1)
   ___SET_R2(___SYM_type)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(18))
   ___JUMPGLOSAFE(___SET_NARGS(2),34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___DEF_SLBL(18,___L18_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(5,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_type_2d_set_21_,___R1)
   ___SET_R2(___SYM_checked_3f_)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(19))
   ___JUMPGLOSAFE(___SET_NARGS(2),34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___DEF_SLBL(19,___L19_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(1,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_checked_3f__2d_set_21_,___R1)
   ___SET_R2(___SYM_super)
   ___SET_R1(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R0(___LBL(20))
   ___JUMPGLOSAFE(___SET_NARGS(2),34,___G_make_2d_class_2d_slot_2d_unchecked_2d_mutator)
___DEF_SLBL(20,___L20_gerbil____core____contract_7e_TypeEnv_23_)
   ___SET_GLO(3,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23__26_type_2d_env_2d_super_2d_set_21_,___R1)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env
#undef ___PH_LBL0
#define ___PH_LBL0 23
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(0,0,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
   ___SET_R3(___R1)
   ___SET_R2(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R1(___GLO_make_2d_instance)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env)
   ___JUMPPRM(___SET_NARGS(3),___PRM_apply)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env
#undef ___PH_LBL0
#define ___PH_LBL0 26
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_P_HLBL(___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___DEF_P_HLBL(___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
   ___SET_STK(1,___R0)
   ___SET_R1(___SYM__40__40_type)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(1),28,___G_gx_23_syntax_2d_local_2d_introduce)
___DEF_SLBL(2,___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
   ___SET_R2(___GLO_false)
   ___SET_R0(___STK(-3))
   ___POLL(3)
___DEF_SLBL(3,___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),29,___G_gx_23_syntax_2d_local_2d_value)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup
#undef ___PH_LBL0
#define ___PH_LBL0 31
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L4_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L5_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L6_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L7_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L8_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L9_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_P_HLBL(___L10_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R1(___SYM__40__40_type)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(1),28,___G_gx_23_syntax_2d_local_2d_introduce)
___DEF_SLBL(2,___L2_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R2(___GLO_false)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(2),29,___G_gx_23_syntax_2d_local_2d_value)
___DEF_SLBL(3,___L3_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___GOTO(___L11_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_SLBL(5,___L5_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L15_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_R1(___TYPEID(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t))
   ___IF(___NOT(___STRUCTUREDIOP(___STK(-5),___R1)))
   ___GOTO(___L14_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(-5),___FIX(4L),___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,___FAL))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(6)
___DEF_SLBL(6,___L6_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
___DEF_GLBL(___L11_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___IF(___NOT(___NOTFALSEP(___R2)))
   ___GOTO(___L17_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___TYPEID(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t))
   ___ADJFP(3)
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___R1)))
   ___GOTO(___L16_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(1L),___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,___FAL))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-1))
   ___ADJFP(5)
   ___POLL(7)
___DEF_SLBL(7,___L7_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(2),27,___G_gx_23_free_2d_identifier_3d__3f_)
___DEF_SLBL(8,___L8_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L15_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_R1(___TYPEID(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t))
   ___IF(___NOT(___STRUCTUREDIOP(___STK(-5),___R1)))
   ___GOTO(___L14_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(-5),___FIX(4L),___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,___FAL))
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L13_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_STK(-5,___R1)
   ___SET_R1(___TYPEID(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t))
   ___IF(___STRUCTUREDIOP(___STK(-5),___R1))
   ___GOTO(___L12_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___END_IF
   ___SET_STK(1,___STK(-5))
   ___SET_R3(___FAL)
   ___SET_R2(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R1(___FIX(1L))
   ___SET_R0(___LBL(9))
   ___ADJFP(1)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_direct_2d_structure_2d_ref)
___DEF_SLBL(9,___L9_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R0(___LBL(9))
   ___JUMPPRM(___SET_NARGS(0),___PRM__23__23_dead_2d_end)
___DEF_GLBL(___L12_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(-5),___FIX(1L),___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,___FAL))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(2),27,___G_gx_23_free_2d_identifier_3d__3f_)
___DEF_GLBL(___L13_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R1(___FAL)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L14_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_STK(1,___STK(-5))
   ___SET_R3(___FAL)
   ___SET_R2(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R1(___FIX(4L))
   ___SET_R0(___LBL(10))
   ___ADJFP(1)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_direct_2d_structure_2d_ref)
___DEF_SLBL(10,___L10_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R0(___LBL(10))
   ___JUMPPRM(___SET_NARGS(0),___PRM__23__23_dead_2d_end)
___DEF_GLBL(___L15_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R1(___STK(-5))
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L16_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_STK(6,___R2)
   ___SET_R3(___FAL)
   ___SET_R2(___GLO_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t)
   ___SET_R1(___FIX(1L))
   ___SET_R0(___LBL(9))
   ___ADJFP(6)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_direct_2d_structure_2d_ref)
___DEF_GLBL(___L17_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup)
   ___SET_R1(___FAL)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____core____contract_7e_TypeEnv_23_,___REF_SYM(7,___S_gerbil____core____contract_7e_TypeEnv_23_),___REF_FAL,21,0)
,___DEF_LBL_PROC(___H_gerbil____core____contract_7e_TypeEnv_23_,0,-1)
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETI,7,0,0x3f71L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETI,7,0,0x3f71L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____core____contract_7e_TypeEnv_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,___REF_SYM(3,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,___REF_SYM(2,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,0,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___REF_SYM(4,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup),___REF_FAL,11,0)
,___DEF_LBL_PROC(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,___IFD(___RETN,5,0,0x3L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(22,___G_gerbil____core____contract_7e_TypeEnv_23_,1)
___DEF_MOD_PRM(9,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,23)
___DEF_MOD_PRM(8,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,26)
___DEF_MOD_PRM(12,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,31)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(22,___G_gerbil____core____contract_7e_TypeEnv_23_,1)
___DEF_MOD_GLO(9,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,23)
___DEF_MOD_GLO(8,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,26)
___DEF_MOD_GLO(12,___G_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,31)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S__40__40_type,"@@type")
___DEF_MOD_SYM(1,___S_checked_3f_,"checked?")
___DEF_MOD_SYM(2,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_current_2d_type_2d_env,"gerbil/core/contract~TypeEnv#current-type-env")

___DEF_MOD_SYM(3,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_make_2d_type_2d_env,"gerbil/core/contract~TypeEnv#make-type-env")

___DEF_MOD_SYM(4,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_2d_lookup,"gerbil/core/contract~TypeEnv#type-env-lookup")

___DEF_MOD_SYM(5,___S_gerbil_2f_core_2f_contract_7e_TypeEnv_23_type_2d_env_3a__3a_t,"gerbil/core/contract~TypeEnv#type-env::t")

___DEF_MOD_SYM(6,___S_gerbil____core____contract_7e_TypeEnv,"gerbil__core__contract~TypeEnv")

___DEF_MOD_SYM(7,___S_gerbil____core____contract_7e_TypeEnv_23_,"gerbil__core__contract~TypeEnv#")

___DEF_MOD_SYM(8,___S_super,"super")
___DEF_MOD_SYM(9,___S_type,"type")
___DEF_MOD_SYM(10,___S_type_2d_env,"type-env")
___DEF_MOD_SYM(11,___S_var,"var")
___DEF_MOD_KEY(0,___K_final,"final")
___DEF_MOD_KEY(1,___K_struct,"struct")
___END_MOD_SYM_KEY

#endif
