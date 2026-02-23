#ifdef ___LINKER_INFO
; File: "gerbil__tools__gxhttpd.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__tools__gxhttpd"
("gerbil__tools__gxhttpd")
()
(("gerbil__tools__gxhttpd"))
( #|*/"*/"symbols|#
"config"
"ensemble"
"gerbil/tools/gxhttpd#gxhttpd-main"
"gerbil/tools/gxhttpd#main"
"gerbil/tools/gxhttpd#prepare-ensemble-filesystem!"
"gerbil/tools/gxhttpd#run-ensemble-server!"
"gerbil__tools__gxhttpd"
"gerbil__tools__gxhttpd#"
"httpd"
"server"
"server-id"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
"application"
"roles"
"root"
"server-config"
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil/tools/gxhttpd#gxhttpd-main"
"gerbil/tools/gxhttpd#prepare-ensemble-filesystem!"
"gerbil/tools/gxhttpd#run-ensemble-server!"
"gerbil__tools__gxhttpd#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/tools/gxhttpd#main"
"gerbil/tools/gxhttpd::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##file-exists?"
"HashTable::interface"
"HashTable::t"
"__hash-get"
"absent-value"
"agetq__0"
"cast"
"create-directory*__0"
"create-symbolic-link"
"current-directory"
"error"
"file-exists?"
"gerbil/tools/env#gerbil-path-option"
"gerbil/tools/env#global-env-flag"
"gerbil/tools/env#setup-local-env!"
"gerbil/tools/gxhttpd/config#do-config"
"gerbil/tools/gxhttpd/config#get-ensemble-config"
"gerbil/tools/gxhttpd/config#get-httpd-config"
"gerbil/tools/gxhttpd/opt#config-cmd"
"gerbil/tools/gxhttpd/opt#ensemble-cmd"
"gerbil/tools/gxhttpd/opt#server-cmd"
"gerbil/tools/gxhttpd/server#run-server!"
"hash-ref__0"
"path-directory"
"path-expand"
"path-normalize"
"std/actor-v18/ensemble-config#load-ensemble-server-config__0"
"std/actor-v18/ensemble-server#become-ensemble-server!"
"std/actor-v18/ensemble-supervisor#become-ensemble-supervisor!__0"
"std/cli/getopt#call-with-getopt__%"
"std/config#config-get!"
"std/config#config-get__%"
"string-prefix?"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__tools__gxhttpd"
#define ___LINKER_ID ___LNK_gerbil____tools____gxhttpd
#define ___MH_PROC ___H_gerbil____tools____gxhttpd
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 11
#define ___KEYCOUNT 4
#define ___GLOCOUNT 39
#define ___SUPCOUNT 6
#define ___SUBCOUNT 10
#define ___LBLCOUNT 51
#define ___MODDESCR ___REF_SUB(7)
#include "gambit.h"

___NEED_SYM(___S_config)
___NEED_SYM(___S_ensemble)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxhttpd_23_main)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___NEED_SYM(___S_gerbil____tools____gxhttpd)
___NEED_SYM(___S_gerbil____tools____gxhttpd_23_)
___NEED_SYM(___S_httpd)
___NEED_SYM(___S_server)
___NEED_SYM(___S_server_2d_id)

___NEED_KEY(___K_application)
___NEED_KEY(___K_roles)
___NEED_KEY(___K_root)
___NEED_KEY(___K_server_2d_config)

___NEED_GLO(___G__23__23_file_2d_exists_3f_)
___NEED_GLO(___G_HashTable_3a__3a_interface)
___NEED_GLO(___G_HashTable_3a__3a_t)
___NEED_GLO(___G_____hash_2d_get)
___NEED_GLO(___G_absent_2d_value)
___NEED_GLO(___G_agetq____0)
___NEED_GLO(___G_cast)
___NEED_GLO(___G_create_2d_directory_2a_____0)
___NEED_GLO(___G_create_2d_symbolic_2d_link)
___NEED_GLO(___G_current_2d_directory)
___NEED_GLO(___G_error)
___NEED_GLO(___G_file_2d_exists_3f_)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
___NEED_GLO(___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_23_main)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp)
___NEED_GLO(___G_gerbil____tools____gxhttpd_23_)
___NEED_GLO(___G_hash_2d_ref____0)
___NEED_GLO(___G_path_2d_directory)
___NEED_GLO(___G_path_2d_expand)
___NEED_GLO(___G_path_2d_normalize)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
___NEED_GLO(___G_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_)
___NEED_GLO(___G_std_2f_config_23_config_2d_get_21_)
___NEED_GLO(___G_std_2f_config_23_config_2d_get_____25_)
___NEED_GLO(___G_string_2d_prefix_3f_)

___BEGIN_SYM
___DEF_SYM(0,___S_config,"config")
___DEF_SYM(1,___S_ensemble,"ensemble")
___DEF_SYM(2,___S_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,"gerbil/tools/gxhttpd#gxhttpd-main")

___DEF_SYM(3,___S_gerbil_2f_tools_2f_gxhttpd_23_main,"gerbil/tools/gxhttpd#main")
___DEF_SYM(4,___S_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,"gerbil/tools/gxhttpd#prepare-ensemble-filesystem!")

___DEF_SYM(5,___S_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,"gerbil/tools/gxhttpd#run-ensemble-server!")

___DEF_SYM(6,___S_gerbil____tools____gxhttpd,"gerbil__tools__gxhttpd")
___DEF_SYM(7,___S_gerbil____tools____gxhttpd_23_,"gerbil__tools__gxhttpd#")
___DEF_SYM(8,___S_httpd,"httpd")
___DEF_SYM(9,___S_server,"server")
___DEF_SYM(10,___S_server_2d_id,"server-id")
___END_SYM

#define ___SYM_config ___SYM(0,___S_config)
#define ___SYM_ensemble ___SYM(1,___S_ensemble)
#define ___SYM_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main ___SYM(2,___S_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
#define ___SYM_gerbil_2f_tools_2f_gxhttpd_23_main ___SYM(3,___S_gerbil_2f_tools_2f_gxhttpd_23_main)
#define ___SYM_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_ ___SYM(4,___S_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
#define ___SYM_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_ ___SYM(5,___S_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
#define ___SYM_gerbil____tools____gxhttpd ___SYM(6,___S_gerbil____tools____gxhttpd)
#define ___SYM_gerbil____tools____gxhttpd_23_ ___SYM(7,___S_gerbil____tools____gxhttpd_23_)
#define ___SYM_httpd ___SYM(8,___S_httpd)
#define ___SYM_server ___SYM(9,___S_server)
#define ___SYM_server_2d_id ___SYM(10,___S_server_2d_id)

___BEGIN_KEY
___DEF_KEY(0,___K_application,"application")
___DEF_KEY(1,___K_roles,"roles")
___DEF_KEY(2,___K_root,"root")
___DEF_KEY(3,___K_server_2d_config,"server-config")
___END_KEY

#define ___KEY_application ___KEY(0,___K_application)
#define ___KEY_roles ___KEY(1,___K_roles)
#define ___KEY_root ___KEY(2,___K_root)
#define ___KEY_server_2d_config ___KEY(3,___K_server_2d_config)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/tools/gxhttpd#gxhttpd-main")

___DEF_GLO(1,"gerbil/tools/gxhttpd#main")
___DEF_GLO(2,"gerbil/tools/gxhttpd#prepare-ensemble-filesystem!")

___DEF_GLO(3,"gerbil/tools/gxhttpd#run-ensemble-server!")

___DEF_GLO(4,"gerbil/tools/gxhttpd::timestamp")
___DEF_GLO(5,"gerbil__tools__gxhttpd#")
___DEF_GLO(6,"##file-exists?")
___DEF_GLO(7,"HashTable::interface")
___DEF_GLO(8,"HashTable::t")
___DEF_GLO(9,"__hash-get")
___DEF_GLO(10,"absent-value")
___DEF_GLO(11,"agetq__0")
___DEF_GLO(12,"cast")
___DEF_GLO(13,"create-directory*__0")
___DEF_GLO(14,"create-symbolic-link")
___DEF_GLO(15,"current-directory")
___DEF_GLO(16,"error")
___DEF_GLO(17,"file-exists?")
___DEF_GLO(18,"gerbil/tools/env#gerbil-path-option")

___DEF_GLO(19,"gerbil/tools/env#global-env-flag")
___DEF_GLO(20,"gerbil/tools/env#setup-local-env!")

___DEF_GLO(21,"gerbil/tools/gxhttpd/config#do-config")

___DEF_GLO(22,"gerbil/tools/gxhttpd/config#get-ensemble-config")

___DEF_GLO(23,"gerbil/tools/gxhttpd/config#get-httpd-config")

___DEF_GLO(24,"gerbil/tools/gxhttpd/opt#config-cmd")

___DEF_GLO(25,"gerbil/tools/gxhttpd/opt#ensemble-cmd")

___DEF_GLO(26,"gerbil/tools/gxhttpd/opt#server-cmd")

___DEF_GLO(27,"gerbil/tools/gxhttpd/server#run-server!")

___DEF_GLO(28,"hash-ref__0")
___DEF_GLO(29,"path-directory")
___DEF_GLO(30,"path-expand")
___DEF_GLO(31,"path-normalize")
___DEF_GLO(32,"std/actor-v18/ensemble-config#load-ensemble-server-config__0")

___DEF_GLO(33,"std/actor-v18/ensemble-server#become-ensemble-server!")

___DEF_GLO(34,"std/actor-v18/ensemble-supervisor#become-ensemble-supervisor!__0")

___DEF_GLO(35,"std/cli/getopt#call-with-getopt__%")

___DEF_GLO(36,"std/config#config-get!")
___DEF_GLO(37,"std/config#config-get__%")
___DEF_GLO(38,"string-prefix?")
___END_GLO

#define ___GLO_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main ___GLO(0,___G_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main ___PRM(0,___G_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_23_main ___GLO(1,___G_gerbil_2f_tools_2f_gxhttpd_23_main)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_23_main ___PRM(1,___G_gerbil_2f_tools_2f_gxhttpd_23_main)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_ ___GLO(2,___G_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_ ___PRM(2,___G_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_ ___GLO(3,___G_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_ ___PRM(3,___G_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp ___GLO(4,___G_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp ___PRM(4,___G_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp)
#define ___GLO_gerbil____tools____gxhttpd_23_ ___GLO(5,___G_gerbil____tools____gxhttpd_23_)
#define ___PRM_gerbil____tools____gxhttpd_23_ ___PRM(5,___G_gerbil____tools____gxhttpd_23_)
#define ___GLO__23__23_file_2d_exists_3f_ ___GLO(6,___G__23__23_file_2d_exists_3f_)
#define ___PRM__23__23_file_2d_exists_3f_ ___PRM(6,___G__23__23_file_2d_exists_3f_)
#define ___GLO_HashTable_3a__3a_interface ___GLO(7,___G_HashTable_3a__3a_interface)
#define ___PRM_HashTable_3a__3a_interface ___PRM(7,___G_HashTable_3a__3a_interface)
#define ___GLO_HashTable_3a__3a_t ___GLO(8,___G_HashTable_3a__3a_t)
#define ___PRM_HashTable_3a__3a_t ___PRM(8,___G_HashTable_3a__3a_t)
#define ___GLO_____hash_2d_get ___GLO(9,___G_____hash_2d_get)
#define ___PRM_____hash_2d_get ___PRM(9,___G_____hash_2d_get)
#define ___GLO_absent_2d_value ___GLO(10,___G_absent_2d_value)
#define ___PRM_absent_2d_value ___PRM(10,___G_absent_2d_value)
#define ___GLO_agetq____0 ___GLO(11,___G_agetq____0)
#define ___PRM_agetq____0 ___PRM(11,___G_agetq____0)
#define ___GLO_cast ___GLO(12,___G_cast)
#define ___PRM_cast ___PRM(12,___G_cast)
#define ___GLO_create_2d_directory_2a_____0 ___GLO(13,___G_create_2d_directory_2a_____0)
#define ___PRM_create_2d_directory_2a_____0 ___PRM(13,___G_create_2d_directory_2a_____0)
#define ___GLO_create_2d_symbolic_2d_link ___GLO(14,___G_create_2d_symbolic_2d_link)
#define ___PRM_create_2d_symbolic_2d_link ___PRM(14,___G_create_2d_symbolic_2d_link)
#define ___GLO_current_2d_directory ___GLO(15,___G_current_2d_directory)
#define ___PRM_current_2d_directory ___PRM(15,___G_current_2d_directory)
#define ___GLO_error ___GLO(16,___G_error)
#define ___PRM_error ___PRM(16,___G_error)
#define ___GLO_file_2d_exists_3f_ ___GLO(17,___G_file_2d_exists_3f_)
#define ___PRM_file_2d_exists_3f_ ___PRM(17,___G_file_2d_exists_3f_)
#define ___GLO_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option ___GLO(18,___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
#define ___PRM_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option ___PRM(18,___G_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
#define ___GLO_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag ___GLO(19,___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
#define ___PRM_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag ___PRM(19,___G_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
#define ___GLO_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_ ___GLO(20,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
#define ___PRM_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_ ___PRM(20,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config ___GLO(21,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config ___PRM(21,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config ___GLO(22,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config ___PRM(22,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config ___GLO(23,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config ___PRM(23,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd ___GLO(24,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd ___PRM(24,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd ___GLO(25,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd ___PRM(25,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd ___GLO(26,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd ___PRM(26,___G_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd)
#define ___GLO_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_ ___GLO(27,___G_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_)
#define ___PRM_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_ ___PRM(27,___G_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_)
#define ___GLO_hash_2d_ref____0 ___GLO(28,___G_hash_2d_ref____0)
#define ___PRM_hash_2d_ref____0 ___PRM(28,___G_hash_2d_ref____0)
#define ___GLO_path_2d_directory ___GLO(29,___G_path_2d_directory)
#define ___PRM_path_2d_directory ___PRM(29,___G_path_2d_directory)
#define ___GLO_path_2d_expand ___GLO(30,___G_path_2d_expand)
#define ___PRM_path_2d_expand ___PRM(30,___G_path_2d_expand)
#define ___GLO_path_2d_normalize ___GLO(31,___G_path_2d_normalize)
#define ___PRM_path_2d_normalize ___PRM(31,___G_path_2d_normalize)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0 ___GLO(32,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0 ___PRM(32,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_ ___GLO(33,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_ ___PRM(33,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0 ___GLO(34,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0 ___PRM(34,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
#define ___GLO_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_ ___GLO(35,___G_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_)
#define ___PRM_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_ ___PRM(35,___G_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_)
#define ___GLO_std_2f_config_23_config_2d_get_21_ ___GLO(36,___G_std_2f_config_23_config_2d_get_21_)
#define ___PRM_std_2f_config_23_config_2d_get_21_ ___PRM(36,___G_std_2f_config_23_config_2d_get_21_)
#define ___GLO_std_2f_config_23_config_2d_get_____25_ ___GLO(37,___G_std_2f_config_23_config_2d_get_____25_)
#define ___PRM_std_2f_config_23_config_2d_get_____25_ ___PRM(37,___G_std_2f_config_23_config_2d_get_____25_)
#define ___GLO_string_2d_prefix_3f_ ___GLO(38,___G_string_2d_prefix_3f_)
#define ___PRM_string_2d_prefix_3f_ ___PRM(38,___G_string_2d_prefix_3f_)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c8fL)
___DEF_SUB_STR(___X1,7UL)
               ___STR7(103,120,104,116,116,112,100)
___DEF_SUB_STR(___X2,22UL)
               ___STR8(84,104,101,32,71,101,114,98)
               ___STR8(105,108,32,72,84,84,80,32)
               ___STR6(68,97,101,109,111,110)
___DEF_SUB_STR(___X3,2UL)
               ___STR2(102,115)
___DEF_SUB_STR(___X4,1UL)
               ___STR1(47)
___DEF_SUB_STR(___X5,42UL)
               ___STR8(104,116,116,112,100,32,99,111)
               ___STR8(110,116,101,110,116,32,114,111)
               ___STR8(111,116,32,100,105,114,101,99)
               ___STR8(116,111,114,121,32,100,111,101)
               ___STR8(115,110,39,116,32,101,120,105)
               ___STR2(115,116)
___DEF_SUB_STR(___X6,27UL)
               ___STR8(109,105,115,115,105,110,103,32)
               ___STR8(104,116,116,112,100,32,99,111)
               ___STR8(110,102,105,103,117,114,97,116)
               ___STR3(105,111,110)
___DEF_SUB_VEC(___X7,6UL)
               ___VEC1(___REF_SUB(8))
               ___VEC1(___REF_SUB(9))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X8,1UL)
               ___VEC1(___REF_SYM(6,___S_gerbil____tools____gxhttpd))
               ___VEC0
___DEF_SUB_VEC(___X9,0UL)
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
___DEF_M_HLBL(___L0_gerbil____tools____gxhttpd_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_main)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_main)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL(___L16_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L16_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L17_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L18_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL(___L19_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____tools____gxhttpd_23_
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
___DEF_P_HLBL(___L0_gerbil____tools____gxhttpd_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____tools____gxhttpd_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____tools____gxhttpd_23_)
   ___SET_GLO(4,___G_gerbil_2f_tools_2f_gxhttpd_3a__3a_timestamp,___BIGFIX(0,1770925199LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxhttpd_23_main
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_main)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_main)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxhttpd_23_main)
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
   ___GET_REST(0,0,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxhttpd_23_main)
   ___SET_STK(1,___FAL)
   ___SET_STK(2,___SUB(1))
   ___SET_STK(3,___SUB(2))
   ___SET_STK(4,___GLO_absent_2d_value)
   ___SET_STK(5,___PRC(6))
   ___SET_STK(6,___R1)
   ___SET_STK(7,___GLO_gerbil_2f_tools_2f_env_23_global_2d_env_2d_flag)
   ___SET_STK(8,___GLO_gerbil_2f_tools_2f_env_23_gerbil_2d_path_2d_option)
   ___SET_R3(___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_config_2d_cmd)
   ___SET_R2(___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_ensemble_2d_cmd)
   ___SET_R1(___GLO_gerbil_2f_tools_2f_gxhttpd_2f_opt_23_server_2d_cmd)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxhttpd_23_main)
   ___JUMPGLONOTSAFE(___SET_NARGS(11),35,___G_std_2f_cli_2f_getopt_23_call_2d_with_2d_getopt_____25_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_P_HLBL(___L16_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___R2)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),20,___G_gerbil_2f_tools_2f_env_23_setup_2d_local_2d_env_21_)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF(___NOT(___STRUCTUREP(___STK(-5))))
   ___GOTO(___L22_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___SET_R1(___STRUCTURETYPE(___STK(-5)))
   ___IF(___NOT(___EQP(___GLO_HashTable_3a__3a_t,___R1)))
   ___GOTO(___L22_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___IF(___EQP(___SYM_server,___STK(-6)))
   ___GOTO(___L20_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___GOTO(___L17_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF(___EQP(___SYM_server,___STK(-6)))
   ___GOTO(___L20_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
___DEF_GLBL(___L17_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF(___EQP(___SYM_ensemble,___STK(-6)))
   ___GOTO(___L19_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___IF(___NOT(___EQP(___SYM_config,___STK(-6))))
   ___GOTO(___L18_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),21,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_do_2d_config)
___DEF_GLBL(___L18_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R1(___VOID)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L19_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(5))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),22,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_ensemble_2d_config)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_STK(-2,___R1)
   ___SET_R0(___LBL(6))
   ___ADJFP(4)
   ___JUMPINT(___SET_NARGS(1),___PRC(24),___L_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(7)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),34,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
___DEF_GLBL(___L20_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_STK(-6,___R1)
   ___SET_R2(___SYM_server_2d_id)
   ___SET_R0(___LBL(8))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),9,___G_____hash_2d_get)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L21_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_server_2d_id)
   ___SET_R0(___LBL(9))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),28,___G_hash_2d_ref____0)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),32,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_STK(-2,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(-2),13)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(-2))
   ___SET_R0(___STK(-3))
   ___ADJFP(-2)
   ___CHECK_HEAP(11,4096)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),33,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(13,0,0,0)
   ___SET_R1(___CLO(___R4,1))
   ___POLL(14)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___JUMPINT(___SET_NARGS(1),___PRC(45),___L_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_GLBL(___L21_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(15))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),23,___G_gerbil_2f_tools_2f_gxhttpd_2f_config_23_get_2d_httpd_2d_config)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R0(___STK(-3))
   ___POLL(16)
___DEF_SLBL(16,___L16_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),27,___G_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_)
___DEF_GLBL(___L22_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main)
   ___SET_R2(___STK(-5))
   ___SET_R1(___GLO_HashTable_3a__3a_interface)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),12,___G_cast)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_
#undef ___PH_LBL0
#define ___PH_LBL0 24
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L16_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L17_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L18_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_P_HLBL(___L19_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(0),15,___G_current_2d_directory)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R3(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___KEY_root)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),37,___G_std_2f_config_23_config_2d_get_____25_)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),31,___G_path_2d_normalize)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___R1)
   ___SET_R1(___SUB(3))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(2),30,___G_path_2d_expand)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___KEY_roles)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),36,___G_std_2f_config_23_config_2d_get_21_)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___R1)
   ___SET_R1(___SYM_httpd)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),11,___G_agetq____0)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___KEY_server_2d_config)
   ___SET_R0(___LBL(8))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),36,___G_std_2f_config_23_config_2d_get_21_)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___KEY_application)
   ___SET_R0(___LBL(9))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),36,___G_std_2f_config_23_config_2d_get_21_)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___R1)
   ___SET_R1(___SYM_httpd)
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),11,___G_agetq____0)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___KEY_root)
   ___SET_R0(___LBL(11))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),36,___G_std_2f_config_23_config_2d_get_21_)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_STK(-6,___R1)
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),17,___G_file_2d_exists_3f_)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L20_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___END_IF
   ___GOTO(___L22_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
___DEF_GLBL(___L20_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(4))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(2),38,___G_string_2d_prefix_3f_)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L21_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___END_IF
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(2),30,___G_path_2d_expand)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),29,___G_path_2d_directory)
___DEF_SLBL(16,___L16_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R0(___LBL(17))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),13,___G_create_2d_directory_2a_____0)
___DEF_SLBL(17,___L17_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(18))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),6,___G__23__23_file_2d_exists_3f_)
___DEF_SLBL(18,___L18_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L21_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___END_IF
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(19)
___DEF_SLBL(19,___L19_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),14,___G_create_2d_symbolic_2d_link)
___DEF_GLBL(___L21_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R1(___VOID)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L22_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(5))
   ___SET_R0(___LBL(13))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),16,___G_error)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_
#undef ___PH_LBL0
#define ___PH_LBL0 45
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___KEY_application)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),36,___G_std_2f_config_23_config_2d_get_21_)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___SET_R2(___R1)
   ___SET_R1(___SYM_httpd)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),11,___G_agetq____0)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L6_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),27,___G_gerbil_2f_tools_2f_gxhttpd_2f_server_23_run_2d_server_21_)
___DEF_GLBL(___L6_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(6))
   ___SET_R0(___STK(-7))
   ___POLL(5)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),16,___G_error)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____tools____gxhttpd_23_,___REF_SYM(7,___S_gerbil____tools____gxhttpd_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_gerbil____tools____gxhttpd_23_,0,-1)
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxhttpd_23_main,___REF_SYM(3,___S_gerbil_2f_tools_2f_gxhttpd_23_main),___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxhttpd_23_main,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_main,___IFD(___RETI,8,8,0x3fffL))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___REF_SYM(2,___S_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main),___REF_FAL,17,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,2,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,0,1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___REF_SYM(4,___S_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_),___REF_FAL,20,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___REF_SYM(5,___S_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_),___REF_FAL,6,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,___IFD(___RETI,8,8,0x3f00L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(5,___G_gerbil____tools____gxhttpd_23_,1)
___DEF_MOD_PRM(1,___G_gerbil_2f_tools_2f_gxhttpd_23_main,3)
___DEF_MOD_PRM(0,___G_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,6)
___DEF_MOD_PRM(2,___G_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,24)
___DEF_MOD_PRM(3,___G_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,45)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(5,___G_gerbil____tools____gxhttpd_23_,1)
___DEF_MOD_GLO(1,___G_gerbil_2f_tools_2f_gxhttpd_23_main,3)
___DEF_MOD_GLO(0,___G_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,6)
___DEF_MOD_GLO(2,___G_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,24)
___DEF_MOD_GLO(3,___G_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,45)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_config,"config")
___DEF_MOD_SYM(1,___S_ensemble,"ensemble")
___DEF_MOD_SYM(2,___S_gerbil_2f_tools_2f_gxhttpd_23_gxhttpd_2d_main,"gerbil/tools/gxhttpd#gxhttpd-main")

___DEF_MOD_SYM(3,___S_gerbil_2f_tools_2f_gxhttpd_23_main,"gerbil/tools/gxhttpd#main")
___DEF_MOD_SYM(4,___S_gerbil_2f_tools_2f_gxhttpd_23_prepare_2d_ensemble_2d_filesystem_21_,"gerbil/tools/gxhttpd#prepare-ensemble-filesystem!")

___DEF_MOD_SYM(5,___S_gerbil_2f_tools_2f_gxhttpd_23_run_2d_ensemble_2d_server_21_,"gerbil/tools/gxhttpd#run-ensemble-server!")

___DEF_MOD_SYM(6,___S_gerbil____tools____gxhttpd,"gerbil__tools__gxhttpd")
___DEF_MOD_SYM(7,___S_gerbil____tools____gxhttpd_23_,"gerbil__tools__gxhttpd#")
___DEF_MOD_SYM(8,___S_httpd,"httpd")
___DEF_MOD_SYM(9,___S_server,"server")
___DEF_MOD_SYM(10,___S_server_2d_id,"server-id")
___DEF_MOD_KEY(0,___K_application,"application")
___DEF_MOD_KEY(1,___K_roles,"roles")
___DEF_MOD_KEY(2,___K_root,"root")
___DEF_MOD_KEY(3,___K_server_2d_config,"server-config")
___END_MOD_SYM_KEY

#endif
