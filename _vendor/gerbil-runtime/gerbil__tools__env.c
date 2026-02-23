#ifdef ___LINKER_INFO
; File: "gerbil__tools__env.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__tools__env"
("gerbil__tools__env")
()
(("gerbil__tools__env"))
( #|*/"*/"symbols|#
"gerbil-path"
"gerbil/tools/env#setup-local-env!"
"gerbil/tools/env#setup-local-pkg-env!"
"gerbil__tools__env"
"gerbil__tools__env#"
"global-env"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil/tools/env#setup-local-pkg-env!"
"gerbil__tools__env#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/tools/env#gerbil-path-option"
"gerbil/tools/env#global-env-flag"
"gerbil/tools/env#setup-local-env!"
"gerbil/tools/env::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##file-exists?"
"##getenv"
"##setenv"
"absent-value"
"add-load-path!"
"create-directory"
"create-directory*__0"
"current-directory"
"hash-get"
"path-expand"
"path-normalize"
"std/cli/getopt#flag__%__%"
"std/cli/getopt#option__%__%"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__tools__env"
#define ___LINKER_ID ___LNK_gerbil____tools____env
#define ___MH_PROC ___H_gerbil____tools____env
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 6
#define ___GLOCOUNT 19
#define ___SUPCOUNT 6
#define ___SUBCOUNT 17
#define ___LBLCOUNT 31
#define ___MODDESCR ___REF_SUB(14)
#include "gambit.h"

___NEED_SYM(___S_gerbil_2d_path)
___NEED_SYM(___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___NEED_SYM(___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___NEED_SYM(___S_gerbil____tools____env)
___NEED_SYM(___S_gerbil____tools____env_23_)
___NEED_SYM(___S_global_2d_env)

___NEED_GLO(___G__23__23_file_2d_exists_3f_)
___NEED_GLO(___G__23__23_getenv)
___NEED_GLO(___G__23__23_setenv)
___NEED_GLO(___G_absent_2d_value)
___NEED_GLO(___G_add_2d_load_2d_path_21_)
___NEED_GLO(___G_create_2d_directory)
___NEED_GLO(___G_create_2d_directory_2a_____0)
___NEED_GLO(___G_current_2d_directory)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_3a__3a_timestamp)
___NEED_GLO(___G_gerbil____tools____env_23_)
___NEED_GLO(___G_hash_2d_get)
___NEED_GLO(___G_path_2d_expand)
___NEED_GLO(___G_path_2d_normalize)
___NEED_GLO(___G_std_2f_cli_2f_getopt_23_flag_____25______25_)
___NEED_GLO(___G_std_2f_cli_2f_getopt_23_option_____25______25_)

___BEGIN_SYM
___DEF_SYM(0,___S_gerbil_2d_path,"gerbil-path")
___DEF_SYM(1,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,"gerbil/tools/env#setup-local-env!")

___DEF_SYM(2,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,"gerbil/tools/env#setup-local-pkg-env!")

___DEF_SYM(3,___S_gerbil____tools____env,"gerbil__tools__env")
___DEF_SYM(4,___S_gerbil____tools____env_23_,"gerbil__tools__env#")
___DEF_SYM(5,___S_global_2d_env,"global-env")
___END_SYM

#define ___SYM_gerbil_2d_path ___SYM(0,___S_gerbil_2d_path)
#define ___SYM_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_ ___SYM(1,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
#define ___SYM_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_ ___SYM(2,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
#define ___SYM_gerbil____tools____env ___SYM(3,___S_gerbil____tools____env)
#define ___SYM_gerbil____tools____env_23_ ___SYM(4,___S_gerbil____tools____env_23_)
#define ___SYM_global_2d_env ___SYM(5,___S_global_2d_env)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/tools/env#gerbil-path-option")

___DEF_GLO(1,"gerbil/tools/env#global-env-flag")
___DEF_GLO(2,"gerbil/tools/env#setup-local-env!")

___DEF_GLO(3,"gerbil/tools/env#setup-local-pkg-env!")

___DEF_GLO(4,"gerbil/tools/env::timestamp")
___DEF_GLO(5,"gerbil__tools__env#")
___DEF_GLO(6,"##file-exists?")
___DEF_GLO(7,"##getenv")
___DEF_GLO(8,"##setenv")
___DEF_GLO(9,"absent-value")
___DEF_GLO(10,"add-load-path!")
___DEF_GLO(11,"create-directory")
___DEF_GLO(12,"create-directory*__0")
___DEF_GLO(13,"current-directory")
___DEF_GLO(14,"hash-get")
___DEF_GLO(15,"path-expand")
___DEF_GLO(16,"path-normalize")
___DEF_GLO(17,"std/cli/getopt#flag__%__%")
___DEF_GLO(18,"std/cli/getopt#option__%__%")
___END_GLO

#define ___GLO_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option ___GLO(0,___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
#define ___PRM_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option ___PRM(0,___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
#define ___GLO_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag ___GLO(1,___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
#define ___PRM_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag ___PRM(1,___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
#define ___GLO_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_ ___GLO(2,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
#define ___PRM_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_ ___PRM(2,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
#define ___GLO_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_ ___GLO(3,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
#define ___PRM_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_ ___PRM(3,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
#define ___GLO_gerbil_2f_tools_2f_env_3a__3a_timestamp ___GLO(4,___G_gerbil_2f_tools_2f_env_3a__3a_timestamp)
#define ___PRM_gerbil_2f_tools_2f_env_3a__3a_timestamp ___PRM(4,___G_gerbil_2f_tools_2f_env_3a__3a_timestamp)
#define ___GLO_gerbil____tools____env_23_ ___GLO(5,___G_gerbil____tools____env_23_)
#define ___PRM_gerbil____tools____env_23_ ___PRM(5,___G_gerbil____tools____env_23_)
#define ___GLO__23__23_file_2d_exists_3f_ ___GLO(6,___G__23__23_file_2d_exists_3f_)
#define ___PRM__23__23_file_2d_exists_3f_ ___PRM(6,___G__23__23_file_2d_exists_3f_)
#define ___GLO__23__23_getenv ___GLO(7,___G__23__23_getenv)
#define ___PRM__23__23_getenv ___PRM(7,___G__23__23_getenv)
#define ___GLO__23__23_setenv ___GLO(8,___G__23__23_setenv)
#define ___PRM__23__23_setenv ___PRM(8,___G__23__23_setenv)
#define ___GLO_absent_2d_value ___GLO(9,___G_absent_2d_value)
#define ___PRM_absent_2d_value ___PRM(9,___G_absent_2d_value)
#define ___GLO_add_2d_load_2d_path_21_ ___GLO(10,___G_add_2d_load_2d_path_21_)
#define ___PRM_add_2d_load_2d_path_21_ ___PRM(10,___G_add_2d_load_2d_path_21_)
#define ___GLO_create_2d_directory ___GLO(11,___G_create_2d_directory)
#define ___PRM_create_2d_directory ___PRM(11,___G_create_2d_directory)
#define ___GLO_create_2d_directory_2a_____0 ___GLO(12,___G_create_2d_directory_2a_____0)
#define ___PRM_create_2d_directory_2a_____0 ___PRM(12,___G_create_2d_directory_2a_____0)
#define ___GLO_current_2d_directory ___GLO(13,___G_current_2d_directory)
#define ___PRM_current_2d_directory ___PRM(13,___G_current_2d_directory)
#define ___GLO_hash_2d_get ___GLO(14,___G_hash_2d_get)
#define ___PRM_hash_2d_get ___PRM(14,___G_hash_2d_get)
#define ___GLO_path_2d_expand ___GLO(15,___G_path_2d_expand)
#define ___PRM_path_2d_expand ___PRM(15,___G_path_2d_expand)
#define ___GLO_path_2d_normalize ___GLO(16,___G_path_2d_normalize)
#define ___PRM_path_2d_normalize ___PRM(16,___G_path_2d_normalize)
#define ___GLO_std_2f_cli_2f_getopt_23_flag_____25______25_ ___GLO(17,___G_std_2f_cli_2f_getopt_23_flag_____25______25_)
#define ___PRM_std_2f_cli_2f_getopt_23_flag_____25______25_ ___PRM(17,___G_std_2f_cli_2f_getopt_23_flag_____25______25_)
#define ___GLO_std_2f_cli_2f_getopt_23_option_____25______25_ ___GLO(18,___G_std_2f_cli_2f_getopt_23_option_____25______25_)
#define ___PRM_std_2f_cli_2f_getopt_23_option_____25______25_ ___PRM(18,___G_std_2f_cli_2f_getopt_23_option_____25______25_)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c8cL)
___DEF_SUB_STR(___X1,53UL)
               ___STR8(117,115,101,32,116,104,101,32)
               ___STR8(117,115,101,114,32,103,108,111)
               ___STR8(98,97,108,32,101,110,118,32)
               ___STR8(101,118,101,110,32,105,110,32)
               ___STR8(108,111,99,97,108,32,112,97)
               ___STR8(99,107,97,103,101,32,99,111)
               ___STR5(110,116,101,120,116)
___DEF_SUB_STR(___X2,12UL)
               ___STR8(45,45,103,108,111,98,97,108)
               ___STR4(45,101,110,118)
___DEF_SUB_STR(___X3,2UL)
               ___STR2(45,103)
___DEF_SUB_STR(___X4,49UL)
               ___STR8(115,112,101,99,105,102,105,101)
               ___STR8(115,32,116,104,101,32,71,69)
               ___STR8(82,66,73,76,95,80,65,84)
               ___STR8(72,32,102,111,114,32,101,110)
               ___STR8(115,101,109,98,108,101,32,111)
               ___STR8(112,101,114,97,116,105,111,110)
               ___STR1(115)
___DEF_SUB_STR(___X5,13UL)
               ___STR8(45,45,103,101,114,98,105,108)
               ___STR5(45,112,97,116,104)
___DEF_SUB_STR(___X6,2UL)
               ___STR2(45,71)
___DEF_SUB_STR(___X7,11UL)
               ___STR8(71,69,82,66,73,76,95,80)
               ___STR3(65,84,72)
___DEF_SUB_STR(___X8,3UL)
               ___STR3(108,105,98)
___DEF_SUB_STR(___X9,11UL)
               ___STR8(71,69,82,66,73,76,95,80)
               ___STR3(65,84,72)
___DEF_SUB_STR(___X10,10UL)
               ___STR8(103,101,114,98,105,108,46,112)
               ___STR2(107,103)
___DEF_SUB_STR(___X11,7UL)
               ___STR7(46,103,101,114,98,105,108)
___DEF_SUB_STR(___X12,11UL)
               ___STR8(71,69,82,66,73,76,95,80)
               ___STR3(65,84,72)
___DEF_SUB_STR(___X13,3UL)
               ___STR3(108,105,98)
___DEF_SUB_VEC(___X14,6UL)
               ___VEC1(___REF_SUB(15))
               ___VEC1(___REF_SUB(16))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X15,1UL)
               ___VEC1(___REF_SYM(3,___S_gerbil____tools____env))
               ___VEC0
___DEF_SUB_VEC(___X16,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
,___DEF_SUB(___X7)
,___DEF_SUB(___X8)
,___DEF_SUB(___X9)
,___DEF_SUB(___X10)
,___DEF_SUB(___X11)
,___DEF_SUB(___X12)
,___DEF_SUB(___X13)
,___DEF_SUB(___X14)
,___DEF_SUB(___X15)
,___DEF_SUB(___X16)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil____tools____env_23_)
___DEF_M_HLBL(___L1_gerbil____tools____env_23_)
___DEF_M_HLBL(___L2_gerbil____tools____env_23_)
___DEF_M_HLBL(___L3_gerbil____tools____env_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____tools____env_23_
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
___DEF_P_HLBL(___L0_gerbil____tools____env_23_)
___DEF_P_HLBL(___L1_gerbil____tools____env_23_)
___DEF_P_HLBL(___L2_gerbil____tools____env_23_)
___DEF_P_HLBL(___L3_gerbil____tools____env_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____tools____env_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____tools____env_23_)
   ___SET_GLO(4,___G_gerbil_2f_tools_2f_env_3a__3a_timestamp,___BIGFIX(0,1770925196LL))
   ___SET_STK(1,___R0)
   ___SET_STK(5,___FAL)
   ___SET_STK(6,___SUB(1))
   ___SET_R3(___SUB(2))
   ___SET_R2(___SUB(3))
   ___SET_R1(___SYM_global_2d_env)
   ___ADJFP(6)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil____tools____env_23_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(5),17,___G_std_2f_cli_2f_getopt_23_flag_____25______25_)
___DEF_SLBL(2,___L2_gerbil____tools____env_23_)
   ___SET_GLO(1,___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag,___R1)
   ___SET_STK(1,___FAL)
   ___SET_STK(2,___SUB(4))
   ___SET_STK(3,___GLO_absent_2d_value)
   ___SET_STK(4,___GLO_absent_2d_value)
   ___SET_R3(___SUB(5))
   ___SET_R2(___SUB(6))
   ___SET_R1(___SYM_gerbil_2d_path)
   ___SET_R0(___LBL(3))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(7),18,___G_std_2f_cli_2f_getopt_23_option_____25______25_)
___DEF_SLBL(3,___L3_gerbil____tools____env_23_)
   ___SET_GLO(0,___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option,___R1)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___SYM_gerbil_2d_path)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_get)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L13_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___END_IF
   ___SET_R0(___LBL(3))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G_path_2d_expand)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_STK(-2,___R1)
   ___SET_R0(___LBL(4))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),6,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L11_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___END_IF
   ___GOTO(___L12_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_GLBL(___L11_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(7))
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),8,___G__23__23_setenv)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(8))
   ___SET_R0(___LBL(7))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_path_2d_expand)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R0(___STK(-3))
   ___POLL(8)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),10,___G_add_2d_load_2d_path_21_)
___DEF_GLBL(___L12_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),12,___G_create_2d_directory_2a_____0)
___DEF_GLBL(___L13_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_global_2d_env)
   ___SET_R0(___LBL(9))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_get)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L14_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___END_IF
   ___SET_R1(___FAL)
   ___SET_R0(___STK(-3))
   ___POLL(10)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___ADJFP(-4)
   ___JUMPINT(___SET_NARGS(1),___PRC(18),___L_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_GLBL(___L14_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
   ___SET_R1(___VOID)
   ___ADJFP(-4)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_
#undef ___PH_LBL0
#define ___PH_LBL0 18
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___FAL)
   ___SET_R1(___SUB(9))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),7,___G__23__23_getenv)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L15_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___END_IF
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(0),13,___G_current_2d_directory)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),16,___G_path_2d_normalize)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_STK(-5,___R1)
   ___SET_R2(___R1)
   ___SET_R1(___SUB(10))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_path_2d_expand)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_STK(-4,___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___SUB(11))
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_path_2d_expand)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),6,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L15_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___END_IF
   ___IF(___NOT(___NOTFALSEP(___STK(-6))))
   ___GOTO(___L13_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(8))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),6,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L13_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___END_IF
   ___GOTO(___L14_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
___DEF_GLBL(___L13_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R2(___STK(-5))
   ___SET_R1(___SUB(12))
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),8,___G__23__23_setenv)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R2(___STK(-5))
   ___SET_R1(___SUB(13))
   ___SET_R0(___LBL(11))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_path_2d_expand)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R0(___STK(-3))
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),10,___G_add_2d_load_2d_path_21_)
___DEF_GLBL(___L14_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_create_2d_directory)
___DEF_GLBL(___L15_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_)
   ___SET_R1(___VOID)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____tools____env_23_,___REF_SYM(4,___S_gerbil____tools____env_23_),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_gerbil____tools____env_23_,0,-1)
,___DEF_LBL_RET(___H_gerbil____tools____env_23_,___IFD(___RETI,6,0,0x3f31L))
,___DEF_LBL_RET(___H_gerbil____tools____env_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil____tools____env_23_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___REF_SYM(1,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_),___REF_FAL,11,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___REF_SYM(2,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_),___REF_FAL,13,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0xbL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,___IFD(___RETI,4,4,0x3f0L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(5,___G_gerbil____tools____env_23_,1)
___DEF_MOD_PRM(2,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,6)
___DEF_MOD_PRM(3,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,18)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(5,___G_gerbil____tools____env_23_,1)
___DEF_MOD_GLO(2,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,6)
___DEF_MOD_GLO(3,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,18)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_gerbil_2d_path,"gerbil-path")
___DEF_MOD_SYM(1,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_,"gerbil/tools/env#setup-local-env!")

___DEF_MOD_SYM(2,___S_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_pkg_2d_env_21_,"gerbil/tools/env#setup-local-pkg-env!")

___DEF_MOD_SYM(3,___S_gerbil____tools____env,"gerbil__tools__env")
___DEF_MOD_SYM(4,___S_gerbil____tools____env_23_,"gerbil__tools__env#")
___DEF_MOD_SYM(5,___S_global_2d_env,"global-env")
___END_MOD_SYM_KEY

#endif
