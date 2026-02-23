#ifdef ___LINKER_INFO
; File: "gerbil__tools__gxensemble__srv.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__tools__gxensemble__srv"
("gerbil__tools__gxensemble__srv")
()
(("gerbil__tools__gxensemble__srv"))
( #|*/"*/"symbols|#
"announce"
"ensemble-domain"
"gerbil/tools/gxensemble/srv#do-registry"
"gerbil/tools/gxensemble/srv#do-run"
"gerbil/tools/gxensemble/srv#do-supervisor"
"gerbil/tools/gxensemble/srv#get-module-main"
"gerbil__tools__gxensemble__srv"
"gerbil__tools__gxensemble__srv#"
"listen"
"logging"
"logging-file"
"main"
"main-args"
"module-id"
"registry"
"roles"
"server-id"
"supervised"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil/tools/gxensemble/srv#get-module-main"
"gerbil__tools__gxensemble__srv#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/tools/gxensemble/srv#do-registry"
"gerbil/tools/gxensemble/srv#do-run"
"gerbil/tools/gxensemble/srv#do-supervisor"
"gerbil/tools/gxensemble/srv::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"##call-with-current-continuation"
"##dead-end"
"##direct-structure-ref"
"##structure-ref"
"="
"absent-value"
"apply"
"error"
"eval"
"gerbil-load-expander!"
"gerbil/tools/gxensemble/util#get-ensemble-domain"
"gx#binding::t"
"gx#core-resolve-module-export"
"gx#import-module__%"
"gx#module-context::t"
"gx#module-export::t"
"hash-get"
"hash-ref__0"
"std/actor-v18/cookie#get-actor-server-cookie__0"
"std/actor-v18/ensemble-config#load-ensemble-config__0"
"std/actor-v18/ensemble-config#load-ensemble-server-config__0"
"std/actor-v18/ensemble-config#load-ensemble-server-config__1"
"std/actor-v18/ensemble-server#become-ensemble-server!"
"std/actor-v18/ensemble-server#call-with-ensemble-server__%"
"std/actor-v18/ensemble-supervisor#become-ensemble-supervisor!__0"
"std/actor-v18/registry#start-ensemble-registry!__0"
"std/actor-v18/server-identifier#server-identifier-at-domain"
"std/iter#iter"
"std/iter#iter-end"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__tools__gxensemble__srv"
#define ___LINKER_ID ___LNK_gerbil____tools____gxensemble____srv
#define ___MH_PROC ___H_gerbil____tools____gxensemble____srv
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 18
#define ___GLOCOUNT 35
#define ___SUPCOUNT 6
#define ___SUBCOUNT 5
#define ___LBLCOUNT 79
#define ___OFDCOUNT 3
#define ___MODDESCR ___REF_SUB(2)
#include "gambit.h"

___NEED_SYM(___S_announce)
___NEED_SYM(___S_ensemble_2d_domain)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___NEED_SYM(___S_gerbil____tools____gxensemble____srv)
___NEED_SYM(___S_gerbil____tools____gxensemble____srv_23_)
___NEED_SYM(___S_listen)
___NEED_SYM(___S_logging)
___NEED_SYM(___S_logging_2d_file)
___NEED_SYM(___S_main)
___NEED_SYM(___S_main_2d_args)
___NEED_SYM(___S_module_2d_id)
___NEED_SYM(___S_registry)
___NEED_SYM(___S_roles)
___NEED_SYM(___S_server_2d_id)
___NEED_SYM(___S_supervised)

___NEED_GLO(___G__23__23_call_2d_with_2d_current_2d_continuation)
___NEED_GLO(___G__23__23_dead_2d_end)
___NEED_GLO(___G__23__23_direct_2d_structure_2d_ref)
___NEED_GLO(___G__23__23_structure_2d_ref)
___NEED_GLO(___G__3d_)
___NEED_GLO(___G_absent_2d_value)
___NEED_GLO(___G_apply)
___NEED_GLO(___G_error)
___NEED_GLO(___G_eval)
___NEED_GLO(___G_gerbil_2d_load_2d_expander_21_)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain)
___NEED_GLO(___G_gerbil____tools____gxensemble____srv_23_)
___NEED_GLO(___G_gx_23_binding_3a__3a_t)
___NEED_GLO(___G_gx_23_core_2d_resolve_2d_module_2d_export)
___NEED_GLO(___G_gx_23_import_2d_module_____25_)
___NEED_GLO(___G_gx_23_module_2d_context_3a__3a_t)
___NEED_GLO(___G_gx_23_module_2d_export_3a__3a_t)
___NEED_GLO(___G_hash_2d_get)
___NEED_GLO(___G_hash_2d_ref____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain)
___NEED_GLO(___G_std_2f_iter_23_iter)
___NEED_GLO(___G_std_2f_iter_23_iter_2d_end)

___BEGIN_SYM
___DEF_SYM(0,___S_announce,"announce")
___DEF_SYM(1,___S_ensemble_2d_domain,"ensemble-domain")
___DEF_SYM(2,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,"gerbil/tools/gxensemble/srv#do-registry")

___DEF_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,"gerbil/tools/gxensemble/srv#do-run")

___DEF_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,"gerbil/tools/gxensemble/srv#do-supervisor")

___DEF_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,"gerbil/tools/gxensemble/srv#get-module-main")

___DEF_SYM(6,___S_gerbil____tools____gxensemble____srv,"gerbil__tools__gxensemble__srv")

___DEF_SYM(7,___S_gerbil____tools____gxensemble____srv_23_,"gerbil__tools__gxensemble__srv#")

___DEF_SYM(8,___S_listen,"listen")
___DEF_SYM(9,___S_logging,"logging")
___DEF_SYM(10,___S_logging_2d_file,"logging-file")
___DEF_SYM(11,___S_main,"main")
___DEF_SYM(12,___S_main_2d_args,"main-args")
___DEF_SYM(13,___S_module_2d_id,"module-id")
___DEF_SYM(14,___S_registry,"registry")
___DEF_SYM(15,___S_roles,"roles")
___DEF_SYM(16,___S_server_2d_id,"server-id")
___DEF_SYM(17,___S_supervised,"supervised")
___END_SYM

#define ___SYM_announce ___SYM(0,___S_announce)
#define ___SYM_ensemble_2d_domain ___SYM(1,___S_ensemble_2d_domain)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry ___SYM(2,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run ___SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor ___SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main ___SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
#define ___SYM_gerbil____tools____gxensemble____srv ___SYM(6,___S_gerbil____tools____gxensemble____srv)
#define ___SYM_gerbil____tools____gxensemble____srv_23_ ___SYM(7,___S_gerbil____tools____gxensemble____srv_23_)
#define ___SYM_listen ___SYM(8,___S_listen)
#define ___SYM_logging ___SYM(9,___S_logging)
#define ___SYM_logging_2d_file ___SYM(10,___S_logging_2d_file)
#define ___SYM_main ___SYM(11,___S_main)
#define ___SYM_main_2d_args ___SYM(12,___S_main_2d_args)
#define ___SYM_module_2d_id ___SYM(13,___S_module_2d_id)
#define ___SYM_registry ___SYM(14,___S_registry)
#define ___SYM_roles ___SYM(15,___S_roles)
#define ___SYM_server_2d_id ___SYM(16,___S_server_2d_id)
#define ___SYM_supervised ___SYM(17,___S_supervised)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/tools/gxensemble/srv#do-registry")

___DEF_GLO(1,"gerbil/tools/gxensemble/srv#do-run")

___DEF_GLO(2,"gerbil/tools/gxensemble/srv#do-supervisor")

___DEF_GLO(3,"gerbil/tools/gxensemble/srv#get-module-main")

___DEF_GLO(4,"gerbil/tools/gxensemble/srv::timestamp")

___DEF_GLO(5,"gerbil__tools__gxensemble__srv#")
___DEF_GLO(6,"##call-with-current-continuation")
___DEF_GLO(7,"##dead-end")
___DEF_GLO(8,"##direct-structure-ref")
___DEF_GLO(9,"##structure-ref")
___DEF_GLO(10,"=")
___DEF_GLO(11,"absent-value")
___DEF_GLO(12,"apply")
___DEF_GLO(13,"error")
___DEF_GLO(14,"eval")
___DEF_GLO(15,"gerbil-load-expander!")
___DEF_GLO(16,"gerbil/tools/gxensemble/util#get-ensemble-domain")

___DEF_GLO(17,"gx#binding::t")
___DEF_GLO(18,"gx#core-resolve-module-export")
___DEF_GLO(19,"gx#import-module__%")
___DEF_GLO(20,"gx#module-context::t")
___DEF_GLO(21,"gx#module-export::t")
___DEF_GLO(22,"hash-get")
___DEF_GLO(23,"hash-ref__0")
___DEF_GLO(24,"std/actor-v18/cookie#get-actor-server-cookie__0")

___DEF_GLO(25,"std/actor-v18/ensemble-config#load-ensemble-config__0")

___DEF_GLO(26,"std/actor-v18/ensemble-config#load-ensemble-server-config__0")

___DEF_GLO(27,"std/actor-v18/ensemble-config#load-ensemble-server-config__1")

___DEF_GLO(28,"std/actor-v18/ensemble-server#become-ensemble-server!")

___DEF_GLO(29,"std/actor-v18/ensemble-server#call-with-ensemble-server__%")

___DEF_GLO(30,"std/actor-v18/ensemble-supervisor#become-ensemble-supervisor!__0")

___DEF_GLO(31,"std/actor-v18/registry#start-ensemble-registry!__0")

___DEF_GLO(32,"std/actor-v18/server-identifier#server-identifier-at-domain")

___DEF_GLO(33,"std/iter#iter")
___DEF_GLO(34,"std/iter#iter-end")
___END_GLO

#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry ___GLO(0,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry ___PRM(0,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run ___GLO(1,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run ___PRM(1,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor ___GLO(2,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor ___PRM(2,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main ___GLO(3,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main ___PRM(3,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp ___GLO(4,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp ___PRM(4,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp)
#define ___GLO_gerbil____tools____gxensemble____srv_23_ ___GLO(5,___G_gerbil____tools____gxensemble____srv_23_)
#define ___PRM_gerbil____tools____gxensemble____srv_23_ ___PRM(5,___G_gerbil____tools____gxensemble____srv_23_)
#define ___GLO__23__23_call_2d_with_2d_current_2d_continuation ___GLO(6,___G__23__23_call_2d_with_2d_current_2d_continuation)
#define ___PRM__23__23_call_2d_with_2d_current_2d_continuation ___PRM(6,___G__23__23_call_2d_with_2d_current_2d_continuation)
#define ___GLO__23__23_dead_2d_end ___GLO(7,___G__23__23_dead_2d_end)
#define ___PRM__23__23_dead_2d_end ___PRM(7,___G__23__23_dead_2d_end)
#define ___GLO__23__23_direct_2d_structure_2d_ref ___GLO(8,___G__23__23_direct_2d_structure_2d_ref)
#define ___PRM__23__23_direct_2d_structure_2d_ref ___PRM(8,___G__23__23_direct_2d_structure_2d_ref)
#define ___GLO__23__23_structure_2d_ref ___GLO(9,___G__23__23_structure_2d_ref)
#define ___PRM__23__23_structure_2d_ref ___PRM(9,___G__23__23_structure_2d_ref)
#define ___GLO__3d_ ___GLO(10,___G__3d_)
#define ___PRM__3d_ ___PRM(10,___G__3d_)
#define ___GLO_absent_2d_value ___GLO(11,___G_absent_2d_value)
#define ___PRM_absent_2d_value ___PRM(11,___G_absent_2d_value)
#define ___GLO_apply ___GLO(12,___G_apply)
#define ___PRM_apply ___PRM(12,___G_apply)
#define ___GLO_error ___GLO(13,___G_error)
#define ___PRM_error ___PRM(13,___G_error)
#define ___GLO_eval ___GLO(14,___G_eval)
#define ___PRM_eval ___PRM(14,___G_eval)
#define ___GLO_gerbil_2d_load_2d_expander_21_ ___GLO(15,___G_gerbil_2d_load_2d_expander_21_)
#define ___PRM_gerbil_2d_load_2d_expander_21_ ___PRM(15,___G_gerbil_2d_load_2d_expander_21_)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain ___GLO(16,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain ___PRM(16,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain)
#define ___GLO_gx_23_binding_3a__3a_t ___GLO(17,___G_gx_23_binding_3a__3a_t)
#define ___PRM_gx_23_binding_3a__3a_t ___PRM(17,___G_gx_23_binding_3a__3a_t)
#define ___GLO_gx_23_core_2d_resolve_2d_module_2d_export ___GLO(18,___G_gx_23_core_2d_resolve_2d_module_2d_export)
#define ___PRM_gx_23_core_2d_resolve_2d_module_2d_export ___PRM(18,___G_gx_23_core_2d_resolve_2d_module_2d_export)
#define ___GLO_gx_23_import_2d_module_____25_ ___GLO(19,___G_gx_23_import_2d_module_____25_)
#define ___PRM_gx_23_import_2d_module_____25_ ___PRM(19,___G_gx_23_import_2d_module_____25_)
#define ___GLO_gx_23_module_2d_context_3a__3a_t ___GLO(20,___G_gx_23_module_2d_context_3a__3a_t)
#define ___PRM_gx_23_module_2d_context_3a__3a_t ___PRM(20,___G_gx_23_module_2d_context_3a__3a_t)
#define ___GLO_gx_23_module_2d_export_3a__3a_t ___GLO(21,___G_gx_23_module_2d_export_3a__3a_t)
#define ___PRM_gx_23_module_2d_export_3a__3a_t ___PRM(21,___G_gx_23_module_2d_export_3a__3a_t)
#define ___GLO_hash_2d_get ___GLO(22,___G_hash_2d_get)
#define ___PRM_hash_2d_get ___PRM(22,___G_hash_2d_get)
#define ___GLO_hash_2d_ref____0 ___GLO(23,___G_hash_2d_ref____0)
#define ___PRM_hash_2d_ref____0 ___PRM(23,___G_hash_2d_ref____0)
#define ___GLO_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0 ___GLO(24,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
#define ___PRM_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0 ___PRM(24,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0 ___GLO(25,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0 ___PRM(25,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0 ___GLO(26,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0 ___PRM(26,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1 ___GLO(27,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1 ___PRM(27,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_ ___GLO(28,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_ ___PRM(28,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_ ___GLO(29,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_ ___PRM(29,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0 ___GLO(30,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0 ___PRM(30,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
#define ___GLO_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0 ___GLO(31,___G_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0 ___PRM(31,___G_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0)
#define ___GLO_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain ___GLO(32,___G_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain)
#define ___PRM_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain ___PRM(32,___G_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain)
#define ___GLO_std_2f_iter_23_iter ___GLO(33,___G_std_2f_iter_23_iter)
#define ___PRM_std_2f_iter_23_iter ___PRM(33,___G_std_2f_iter_23_iter)
#define ___GLO_std_2f_iter_23_iter_2d_end ___GLO(34,___G_std_2f_iter_23_iter_2d_end)
#define ___PRM_std_2f_iter_23_iter_2d_end ___PRM(34,___G_std_2f_iter_23_iter_2d_end)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c8eL)
___DEF_SUB_STR(___X1,27UL)
               ___STR8(109,111,100,117,108,101,32,100)
               ___STR8(111,101,115,32,110,111,116,32)
               ___STR8(101,120,112,111,114,116,32,109)
               ___STR3(97,105,110)
___DEF_SUB_VEC(___X2,6UL)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SUB(4))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,1UL)
               ___VEC1(___REF_SYM(6,___S_gerbil____tools____gxensemble____srv))
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
___DEF_M_HLBL(___L0_gerbil____tools____gxensemble____srv_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL(___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L24_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L25_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L26_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L27_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_M_HLBL(___L28_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____tools____gxensemble____srv_23_
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
___DEF_P_HLBL(___L0_gerbil____tools____gxensemble____srv_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____tools____gxensemble____srv_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____tools____gxensemble____srv_23_)
   ___SET_GLO(4,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_3a__3a_timestamp,___BIGFIX(0,1770925198LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry
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
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_server_2d_id)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),22,___G_hash_2d_get)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___END_IF
   ___SET_R1(___CONS(___SYM_registry,___STK(-5)))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
___DEF_GLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_supervised)
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),22,___G_hash_2d_get)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_logging)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_logging_2d_file)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-2,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_listen)
   ___SET_R0(___LBL(8))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-10,___R1)
   ___SET_R0(___LBL(9))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_STK(-5,___STK(-11))
   ___SET_STK(-11,___FAL)
   ___SET_STK(-4,___STK(-10))
   ___SET_STK(-10,___STK(-7))
   ___SET_STK(-7,___STK(-9))
   ___SET_STK(-9,___STK(-6))
   ___SET_STK(-6,___STK(-8))
   ___SET_STK(-8,___STK(-4))
   ___SET_STK(-4,___STK(-7))
   ___SET_STK(-7,___GLO_absent_2d_value)
   ___SET_STK(-3,___STK(-6))
   ___SET_STK(-6,___FAL)
   ___SET_STK(-2,___STK(-5))
   ___SET_STK(-5,___STK(-4))
   ___SET_STK(-4,___GLO_absent_2d_value)
   ___SET_STK(-1,___STK(-3))
   ___SET_STK(-3,___R1)
   ___SET_STK(0,___STK(-2))
   ___SET_STK(-2,___GLO_absent_2d_value)
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___GLO_absent_2d_value)
   ___SET_STK(2,___STK(0))
   ___SET_STK(0,___GLO_absent_2d_value)
   ___SET_STK(3,___STK(1))
   ___SET_STK(1,___GLO_absent_2d_value)
   ___SET_STK(4,___STK(2))
   ___SET_STK(2,___FAL)
   ___SET_R3(___LBL(11))
   ___SET_R2(___STK(3))
   ___SET_R1(___GLO_absent_2d_value)
   ___SET_R0(___STK(4))
   ___ADJFP(4)
   ___POLL(10)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___ADJFP(-2)
   ___JUMPGLONOTSAFE(___SET_NARGS(17),29,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(11,0,0,0)
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),31,___G_std_2f_actor_2d_v18_2f_registry_23_start_2d_ensemble_2d_registry_21_____0)
___DEF_GLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(13))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),32,___G_std_2f_actor_2d_v18_2f_server_2d_identifier_23_server_2d_identifier_2d_at_2d_domain)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_R0(___LBL(14))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),26,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____0)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___SET_R2(___LBL(11))
   ___SET_R0(___STK(-3))
   ___POLL(15)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),28,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor
#undef ___PH_LBL0
#define ___PH_LBL0 20
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
   ___SET_STK(1,___R0)
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),25,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_config____0)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
   ___SET_R0(___STK(-3))
   ___POLL(3)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),30,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_become_2d_ensemble_2d_supervisor_21_____0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run
#undef ___PH_LBL0
#define ___PH_LBL0 25
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___DEF_P_HLBL(___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___SYM_module_2d_id)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_R0(___LBL(3))
   ___JUMPINT(___SET_NARGS(1),___PRC(50),___L_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_main_2d_args)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_supervised)
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),22,___G_hash_2d_get)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L24_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),16,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_ensemble_2d_domain)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_server_2d_id)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_R2(___STK(-3))
   ___SET_R0(___LBL(8))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),27,___G_std_2f_actor_2d_v18_2f_ensemble_2d_config_23_load_2d_ensemble_2d_server_2d_config____1)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-3,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(-3),11)
   ___ADD_CLO_ELEM(0,___STK(-4))
   ___ADD_CLO_ELEM(1,___STK(-5))
   ___END_SETUP_CLO(2)
   ___SET_R2(___STK(-3))
   ___SET_R0(___STK(-7))
   ___ADJFP(-3)
   ___CHECK_HEAP(9,4096)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___POLL(10)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___ADJFP(-5)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),28,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_become_2d_ensemble_2d_server_21_)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(11,0,0,0)
   ___SET_R2(___CLO(___R4,1))
   ___SET_R1(___CLO(___R4,2))
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___JUMPPRM(___SET_NARGS(2),___PRM_apply)
___DEF_GLBL(___L24_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_logging)
   ___SET_R0(___LBL(13))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_logging_2d_file)
   ___SET_R0(___LBL(14))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-2,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_listen)
   ___SET_R0(___LBL(15))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-10))
   ___SET_R2(___SYM_announce)
   ___SET_R0(___LBL(16))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(16,___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-10))
   ___SET_R2(___SYM_roles)
   ___SET_R0(___LBL(17))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(17,___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-10))
   ___SET_R2(___SYM_ensemble_2d_domain)
   ___SET_R0(___LBL(18))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(18,___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-2,___R1)
   ___SET_R0(___LBL(19))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
___DEF_SLBL(19,___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-14))
   ___SET_R2(___SYM_registry)
   ___SET_R0(___LBL(20))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(20,___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-14))
   ___SET_R2(___SYM_server_2d_id)
   ___SET_R0(___LBL(21))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),23,___G_hash_2d_ref____0)
___DEF_SLBL(21,___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___SET_STK(-3,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(-3),11)
   ___ADD_CLO_ELEM(0,___STK(-12))
   ___ADD_CLO_ELEM(1,___STK(-13))
   ___END_SETUP_CLO(2)
   ___SET_STK(-14,___STK(-15))
   ___SET_STK(-15,___FAL)
   ___SET_STK(-13,___STK(-14))
   ___SET_STK(-14,___STK(-11))
   ___SET_STK(-12,___STK(-13))
   ___SET_STK(-13,___STK(-10))
   ___SET_STK(-11,___STK(-12))
   ___SET_STK(-12,___STK(-9))
   ___SET_STK(-10,___STK(-11))
   ___SET_STK(-11,___STK(-8))
   ___SET_STK(-9,___STK(-10))
   ___SET_STK(-10,___STK(-7))
   ___SET_STK(-8,___STK(-9))
   ___SET_STK(-9,___STK(-6))
   ___SET_STK(-7,___STK(-8))
   ___SET_STK(-8,___GLO_absent_2d_value)
   ___SET_STK(-6,___STK(-7))
   ___SET_STK(-7,___STK(-5))
   ___SET_STK(-5,___STK(-6))
   ___SET_STK(-6,___GLO_absent_2d_value)
   ___SET_STK(-2,___STK(-5))
   ___SET_STK(-5,___GLO_absent_2d_value)
   ___SET_STK(-1,___STK(-4))
   ___SET_STK(-4,___GLO_absent_2d_value)
   ___SET_STK(0,___STK(-3))
   ___SET_STK(-3,___GLO_absent_2d_value)
   ___SET_STK(1,___STK(-2))
   ___SET_STK(-2,___STK(-1))
   ___SET_R3(___STK(0))
   ___SET_R2(___R1)
   ___SET_R1(___GLO_absent_2d_value)
   ___SET_R0(___STK(1))
   ___ADJFP(1)
   ___CHECK_HEAP(22,4096)
___DEF_SLBL(22,___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___POLL(23)
___DEF_SLBL(23,___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run)
   ___ADJFP(-3)
   ___JUMPGLONOTSAFE(___SET_NARGS(17),29,___G_std_2f_actor_2d_v18_2f_ensemble_2d_server_23_call_2d_with_2d_ensemble_2d_server_____25_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main
#undef ___PH_LBL0
#define ___PH_LBL0 50
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L24_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L25_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L26_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L27_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_P_HLBL(___L28_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),15,___G_gerbil_2d_load_2d_expander_21_)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___STK(-6))
   ___SET_R3(___TRU)
   ___SET_R2(___FAL)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),19,___G_gx_23_import_2d_module_____25_)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(-5,___ALLOC_CLO(2UL))
   ___BEGIN_SETUP_CLO(2,___STK(-5),6)
   ___ADD_CLO_ELEM(0,___R1)
   ___ADD_CLO_ELEM(1,___STK(-6))
   ___END_SETUP_CLO(2)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-7))
   ___ADJFP(-5)
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___POLL(5)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___ADJFP(-3)
   ___JUMPPRM(___SET_NARGS(1),___PRM__23__23_call_2d_with_2d_current_2d_continuation)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(6,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___SET_STK(9,___CLO(___R4,1))
   ___SET_R3(___FAL)
   ___SET_R2(___GLO_gx_23_module_2d_context_3a__3a_t)
   ___SET_R1(___FIX(9L))
   ___SET_R0(___LBL(7))
   ___ADJFP(9)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_structure_2d_ref)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L43_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(26))
   ___POLL(8)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___GOTO(___L29_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(10)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_GLBL(___L29_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L42_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___CAR(___R2))
   ___SET_R0(___LBL(19))
   ___ADJFP(8)
   ___POLL(11)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_GLBL(___L30_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___GLO_gx_23_module_2d_export_3a__3a_t)
   ___SET_R3(___TYPEID(___STK(1)))
   ___ADJFP(1)
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___R3)))
   ___GOTO(___L36_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(3L),___STK(0),___FAL))
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L35_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___IF(___NOT(___FIXEQ(___R3,___FIX(0L))))
   ___GOTO(___L32_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
___DEF_GLBL(___L31_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(0,___GLO_gx_23_module_2d_export_3a__3a_t)
   ___SET_R3(___TYPEID(___STK(0)))
   ___IF(___NOT(___STRUCTUREDIOP(___R2,___R3)))
   ___GOTO(___L34_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_R3(___UNCHECKEDSTRUCTUREREF(___R2,___FIX(4L),___STK(0),___FAL))
   ___IF(___NOT(___EQP(___R3,___SYM_main)))
   ___GOTO(___L32_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R1)
   ___SET_R1(___R2)
   ___ADJFP(7)
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(13))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),18,___G_gx_23_core_2d_resolve_2d_module_2d_export)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___R1)
   ___SET_R3(___FAL)
   ___SET_R2(___GLO_gx_23_binding_3a__3a_t)
   ___SET_R1(___FIX(1L))
   ___SET_R0(___LBL(14))
   ___ADJFP(1)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_structure_2d_ref)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(15))
   ___JUMPPRM(___SET_NARGS(1),___PRM_eval)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___STK(-7))
   ___POLL(16)
___DEF_SLBL(16,___L16_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___ADJFP(-8)
   ___JUMPGENSAFE(___SET_NARGS(1),___STK(2))
___DEF_SLBL(17,___L17_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L33_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___ADJFP(-7)
___DEF_GLBL(___L32_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___VOID)
   ___ADJFP(-1)
   ___JUMPRET(___R0)
___DEF_GLBL(___L33_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-7)
   ___GOTO(___L31_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_GLBL(___L34_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(8,___R2)
   ___SET_R2(___STK(0))
   ___SET_R3(___FAL)
   ___SET_R1(___FIX(4L))
   ___SET_R0(___LBL(18))
   ___ADJFP(8)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_direct_2d_structure_2d_ref)
___DEF_SLBL(18,___L18_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(18))
   ___JUMPPRM(___SET_NARGS(0),___PRM__23__23_dead_2d_end)
___DEF_GLBL(___L35_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(0,___R0)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R2)
   ___SET_R1(___R3)
   ___SET_R2(___FIX(0L))
   ___SET_R0(___LBL(17))
   ___ADJFP(7)
   ___JUMPPRM(___SET_NARGS(2),___PRM__3d_)
___DEF_GLBL(___L36_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(8,___R2)
   ___SET_R2(___STK(0))
   ___SET_R3(___FAL)
   ___SET_R1(___FIX(3L))
   ___SET_R0(___LBL(18))
   ___ADJFP(8)
   ___JUMPPRM(___SET_NARGS(4),___PRM__23__23_direct_2d_structure_2d_ref)
___DEF_SLBL(19,___L19_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___CDR(___STK(-5)))
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L37_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___SET_STK(-5,___R1)
   ___SET_R2(___CAR(___R1))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(9))
   ___GOTO(___L30_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(20,___L20_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF(___NOT(___EQP(___GLO_std_2f_iter_23_iter_2d_end,___R1)))
   ___GOTO(___L38_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
___DEF_GLBL(___L37_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___VOID)
   ___ADJFP(-8)
   ___JUMPRET(___STK(1))
___DEF_GLBL(___L38_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(21))
   ___GOTO(___L30_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(21,___L21_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(22)
___DEF_SLBL(22,___L22_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___GOTO(___L39_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(23,___L23_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R2(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___FAL,___FAL))
   ___SET_STK(-4,___R1)
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(25))
___DEF_GLBL(___L39_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R1(___R2)
   ___ADJFP(8)
   ___POLL(24)
___DEF_SLBL(24,___L24_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(20))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___R3)
___DEF_SLBL(25,___L25_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___STK(-4),___FIX(3L),___FAL,___FAL))
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L41_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___GOTO(___L40_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_SLBL(26,___L26_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
___DEF_GLBL(___L40_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R2(___CLO(___STK(-5),2))
   ___SET_R1(___SUB(1))
   ___SET_R0(___STK(-7))
   ___POLL(27)
___DEF_SLBL(27,___L27_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_error)
___DEF_GLBL(___L41_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(26))
   ___JUMPGENNOTSAFE(___SET_NARGS(1),___STK(-6))
___DEF_GLBL(___L42_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___DEF_GLBL(___L43_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___IF(___NULLP(___R1))
   ___GOTO(___L40_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___END_IF
   ___POLL(28)
___DEF_SLBL(28,___L28_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main)
   ___SET_R0(___LBL(23))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),33,___G_std_2f_iter_23_iter)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____tools____gxensemble____srv_23_,___REF_SYM(7,___S_gerbil____tools____gxensemble____srv_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_gerbil____tools____gxensemble____srv_23_,0,-1)
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___REF_SYM(2,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry),___REF_FAL,16,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,9,0,0x3dL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___OFD(___RETI,16,16,0x3f3fffL))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,0,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,___REF_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor),___REF_FAL,4,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___REF_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run),___REF_FAL,24,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0x1dL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETI,5,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETI,5,8,0x3f00L))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,0,2)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,9,0,0x7fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,9,0,0xffL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,9,0,0x1ffL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,13,0,0x3ffL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,13,0,0x7ffL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___IFD(___RETN,13,0,0xffdL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___OFD(___RETI,17,20,0x3f03fffL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,___OFD(___RETI,17,20,0x3f03fffL))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___REF_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main),___REF_FAL,29,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,1,2)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f05L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,8,0x3f02L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,1,0xeL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,___IFD(___RETI,8,0,0x3f07L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,16,16)
               ___GCMAP1(0x3f3fffL)
,___DEF_OFD(___RETI,17,20)
               ___GCMAP1(0x3f03fffL)
,___DEF_OFD(___RETI,17,20)
               ___GCMAP1(0x3f03fffL)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(5,___G_gerbil____tools____gxensemble____srv_23_,1)
___DEF_MOD_PRM(0,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,3)
___DEF_MOD_PRM(2,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,20)
___DEF_MOD_PRM(1,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,25)
___DEF_MOD_PRM(3,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,50)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(5,___G_gerbil____tools____gxensemble____srv_23_,1)
___DEF_MOD_GLO(0,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,3)
___DEF_MOD_GLO(2,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,20)
___DEF_MOD_GLO(1,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,25)
___DEF_MOD_GLO(3,___G_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,50)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_announce,"announce")
___DEF_MOD_SYM(1,___S_ensemble_2d_domain,"ensemble-domain")
___DEF_MOD_SYM(2,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_registry,"gerbil/tools/gxensemble/srv#do-registry")

___DEF_MOD_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_run,"gerbil/tools/gxensemble/srv#do-run")

___DEF_MOD_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_do_2d_supervisor,"gerbil/tools/gxensemble/srv#do-supervisor")

___DEF_MOD_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_srv_23_get_2d_module_2d_main,"gerbil/tools/gxensemble/srv#get-module-main")

___DEF_MOD_SYM(6,___S_gerbil____tools____gxensemble____srv,"gerbil__tools__gxensemble__srv")

___DEF_MOD_SYM(7,___S_gerbil____tools____gxensemble____srv_23_,"gerbil__tools__gxensemble__srv#")

___DEF_MOD_SYM(8,___S_listen,"listen")
___DEF_MOD_SYM(9,___S_logging,"logging")
___DEF_MOD_SYM(10,___S_logging_2d_file,"logging-file")
___DEF_MOD_SYM(11,___S_main,"main")
___DEF_MOD_SYM(12,___S_main_2d_args,"main-args")
___DEF_MOD_SYM(13,___S_module_2d_id,"module-id")
___DEF_MOD_SYM(14,___S_registry,"registry")
___DEF_MOD_SYM(15,___S_roles,"roles")
___DEF_MOD_SYM(16,___S_server_2d_id,"server-id")
___DEF_MOD_SYM(17,___S_supervised,"supervised")
___END_MOD_SYM_KEY

#endif
