#ifdef ___LINKER_INFO
; File: "gerbil__tools__gxensemble__admin.c", produced by Gambit v4.9.7
(
409007
(C)
"gerbil__tools__gxensemble__admin"
("gerbil__tools__gxensemble__admin")
()
(("gerbil__tools__gxensemble__admin"))
( #|*/"*/"symbols|#
"authorized-server-id"
"capabilities"
"force"
"gerbil/tools/gxensemble/admin#do-admin-authorize"
"gerbil/tools/gxensemble/admin#do-admin-cookie"
"gerbil/tools/gxensemble/admin#do-admin-creds"
"gerbil/tools/gxensemble/admin#do-admin-retract"
"gerbil__tools__gxensemble__admin"
"gerbil__tools__gxensemble__admin#"
"server-id"
"supervised"
"supervisor"
"view"
) #|*/"*/"symbols|#
( #|*/"*/"keywords|#
) #|*/"*/"keywords|#
( #|*/"*/"globals-s-d|#
"gerbil__tools__gxensemble__admin#"
) #|*/"*/"globals-s-d|#
( #|*/"*/"globals-s-nd|#
"gerbil/tools/gxensemble/admin#do-admin-authorize"
"gerbil/tools/gxensemble/admin#do-admin-cookie"
"gerbil/tools/gxensemble/admin#do-admin-creds"
"gerbil/tools/gxensemble/admin#do-admin-retract"
"gerbil/tools/gxensemble/admin::timestamp"
) #|*/"*/"globals-s-nd|#
( #|*/"*/"globals-ns|#
"absent-value"
"displayln"
"equal?"
"error"
"gerbil/tools/gxensemble/util#call-with-console-server"
"gerbil/tools/gxensemble/util#get-privkey"
"gerbil/tools/gxensemble/util#maybe-authorize!"
"hash-get"
"hash-ref__0"
"std/actor-v18/admin#ensemble-admin-pubkey-path"
"std/actor-v18/admin#generate-admin-keypair!__%__0"
"std/actor-v18/cookie#generate-ensemble-cookie!__%__0"
"std/actor-v18/cookie#get-actor-server-cookie__0"
"std/actor-v18/ensemble#admin-authorize__%__%"
"std/actor-v18/ensemble#admin-retract__%"
"std/actor-v18/ensemble-supervisor#ensemble-supervisor-invoke!__%"
"std/actor-v18/message#reference:::init!__0"
"std/actor-v18/message#reference::t"
"std/actor-v18/path#ensemble-domain-supervisor__0"
"std/actor-v18/proto#!admin-auth::t"
"std/actor-v18/proto#!admin-retract::t"
"std/misc/ports#read-file-u8vector__%"
"std/misc/ports#read-password__%__0"
"std/text/hex#hex-encode__0"
) #|*/"*/"globals-ns|#
( #|*/"*/"meta-info|#
) #|*/"*/"meta-info|#
)
#else
#define ___VERSION 409007
#define ___MODULE_NAME "gerbil__tools__gxensemble__admin"
#define ___LINKER_ID ___LNK_gerbil____tools____gxensemble____admin
#define ___MH_PROC ___H_gerbil____tools____gxensemble____admin
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 13
#define ___GLOCOUNT 30
#define ___SUPCOUNT 6
#define ___SUBCOUNT 7
#define ___LBLCOUNT 61
#define ___OFDCOUNT 7
#define ___MODDESCR ___REF_SUB(4)
#include "gambit.h"

___NEED_SYM(___S_authorized_2d_server_2d_id)
___NEED_SYM(___S_capabilities)
___NEED_SYM(___S_force)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___NEED_SYM(___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___NEED_SYM(___S_gerbil____tools____gxensemble____admin)
___NEED_SYM(___S_gerbil____tools____gxensemble____admin_23_)
___NEED_SYM(___S_server_2d_id)
___NEED_SYM(___S_supervised)
___NEED_SYM(___S_supervisor)
___NEED_SYM(___S_view)

___NEED_GLO(___G_absent_2d_value)
___NEED_GLO(___G_displayln)
___NEED_GLO(___G_equal_3f_)
___NEED_GLO(___G_error)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey)
___NEED_GLO(___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_)
___NEED_GLO(___G_gerbil____tools____gxensemble____admin_23_)
___NEED_GLO(___G_hash_2d_get)
___NEED_GLO(___G_hash_2d_ref____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t)
___NEED_GLO(___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t)
___NEED_GLO(___G_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_)
___NEED_GLO(___G_std_2f_misc_2f_ports_23_read_2d_password_____25_____0)
___NEED_GLO(___G_std_2f_text_2f_hex_23_hex_2d_encode____0)

___BEGIN_SYM
___DEF_SYM(0,___S_authorized_2d_server_2d_id,"authorized-server-id")
___DEF_SYM(1,___S_capabilities,"capabilities")
___DEF_SYM(2,___S_force,"force")
___DEF_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,"gerbil/tools/gxensemble/admin#do-admin-authorize")

___DEF_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,"gerbil/tools/gxensemble/admin#do-admin-cookie")

___DEF_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,"gerbil/tools/gxensemble/admin#do-admin-creds")

___DEF_SYM(6,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,"gerbil/tools/gxensemble/admin#do-admin-retract")

___DEF_SYM(7,___S_gerbil____tools____gxensemble____admin,"gerbil__tools__gxensemble__admin")

___DEF_SYM(8,___S_gerbil____tools____gxensemble____admin_23_,"gerbil__tools__gxensemble__admin#")

___DEF_SYM(9,___S_server_2d_id,"server-id")
___DEF_SYM(10,___S_supervised,"supervised")
___DEF_SYM(11,___S_supervisor,"supervisor")
___DEF_SYM(12,___S_view,"view")
___END_SYM

#define ___SYM_authorized_2d_server_2d_id ___SYM(0,___S_authorized_2d_server_2d_id)
#define ___SYM_capabilities ___SYM(1,___S_capabilities)
#define ___SYM_force ___SYM(2,___S_force)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize ___SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie ___SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds ___SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
#define ___SYM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract ___SYM(6,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
#define ___SYM_gerbil____tools____gxensemble____admin ___SYM(7,___S_gerbil____tools____gxensemble____admin)
#define ___SYM_gerbil____tools____gxensemble____admin_23_ ___SYM(8,___S_gerbil____tools____gxensemble____admin_23_)
#define ___SYM_server_2d_id ___SYM(9,___S_server_2d_id)
#define ___SYM_supervised ___SYM(10,___S_supervised)
#define ___SYM_supervisor ___SYM(11,___S_supervisor)
#define ___SYM_view ___SYM(12,___S_view)

___BEGIN_GLO
___DEF_GLO(0,"gerbil/tools/gxensemble/admin#do-admin-authorize")

___DEF_GLO(1,"gerbil/tools/gxensemble/admin#do-admin-cookie")

___DEF_GLO(2,"gerbil/tools/gxensemble/admin#do-admin-creds")

___DEF_GLO(3,"gerbil/tools/gxensemble/admin#do-admin-retract")

___DEF_GLO(4,"gerbil/tools/gxensemble/admin::timestamp")

___DEF_GLO(5,"gerbil__tools__gxensemble__admin#")

___DEF_GLO(6,"absent-value")
___DEF_GLO(7,"displayln")
___DEF_GLO(8,"equal?")
___DEF_GLO(9,"error")
___DEF_GLO(10,"gerbil/tools/gxensemble/util#call-with-console-server")

___DEF_GLO(11,"gerbil/tools/gxensemble/util#get-privkey")

___DEF_GLO(12,"gerbil/tools/gxensemble/util#maybe-authorize!")

___DEF_GLO(13,"hash-get")
___DEF_GLO(14,"hash-ref__0")
___DEF_GLO(15,"std/actor-v18/admin#ensemble-admin-pubkey-path")

___DEF_GLO(16,"std/actor-v18/admin#generate-admin-keypair!__%__0")

___DEF_GLO(17,"std/actor-v18/cookie#generate-ensemble-cookie!__%__0")

___DEF_GLO(18,"std/actor-v18/cookie#get-actor-server-cookie__0")

___DEF_GLO(19,"std/actor-v18/ensemble#admin-authorize__%__%")

___DEF_GLO(20,"std/actor-v18/ensemble#admin-retract__%")

___DEF_GLO(21,"std/actor-v18/ensemble-supervisor#ensemble-supervisor-invoke!__%")

___DEF_GLO(22,"std/actor-v18/message#reference:::init!__0")

___DEF_GLO(23,"std/actor-v18/message#reference::t")

___DEF_GLO(24,"std/actor-v18/path#ensemble-domain-supervisor__0")

___DEF_GLO(25,"std/actor-v18/proto#!admin-auth::t")

___DEF_GLO(26,"std/actor-v18/proto#!admin-retract::t")

___DEF_GLO(27,"std/misc/ports#read-file-u8vector__%")

___DEF_GLO(28,"std/misc/ports#read-password__%__0")

___DEF_GLO(29,"std/text/hex#hex-encode__0")
___END_GLO

#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize ___GLO(0,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize ___PRM(0,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie ___GLO(1,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie ___PRM(1,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds ___GLO(2,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds ___PRM(2,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract ___GLO(3,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract ___PRM(3,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp ___GLO(4,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp ___PRM(4,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp)
#define ___GLO_gerbil____tools____gxensemble____admin_23_ ___GLO(5,___G_gerbil____tools____gxensemble____admin_23_)
#define ___PRM_gerbil____tools____gxensemble____admin_23_ ___PRM(5,___G_gerbil____tools____gxensemble____admin_23_)
#define ___GLO_absent_2d_value ___GLO(6,___G_absent_2d_value)
#define ___PRM_absent_2d_value ___PRM(6,___G_absent_2d_value)
#define ___GLO_displayln ___GLO(7,___G_displayln)
#define ___PRM_displayln ___PRM(7,___G_displayln)
#define ___GLO_equal_3f_ ___GLO(8,___G_equal_3f_)
#define ___PRM_equal_3f_ ___PRM(8,___G_equal_3f_)
#define ___GLO_error ___GLO(9,___G_error)
#define ___PRM_error ___PRM(9,___G_error)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server ___GLO(10,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server ___PRM(10,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey ___GLO(11,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey ___PRM(11,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey)
#define ___GLO_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_ ___GLO(12,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_)
#define ___PRM_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_ ___PRM(12,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_)
#define ___GLO_hash_2d_get ___GLO(13,___G_hash_2d_get)
#define ___PRM_hash_2d_get ___PRM(13,___G_hash_2d_get)
#define ___GLO_hash_2d_ref____0 ___GLO(14,___G_hash_2d_ref____0)
#define ___PRM_hash_2d_ref____0 ___PRM(14,___G_hash_2d_ref____0)
#define ___GLO_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path ___GLO(15,___G_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path)
#define ___PRM_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path ___PRM(15,___G_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path)
#define ___GLO_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0 ___GLO(16,___G_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0 ___PRM(16,___G_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0)
#define ___GLO_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0 ___GLO(17,___G_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0 ___PRM(17,___G_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0)
#define ___GLO_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0 ___GLO(18,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
#define ___PRM_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0 ___PRM(18,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_ ___GLO(19,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_ ___PRM(19,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_ ___GLO(20,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_ ___PRM(20,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_)
#define ___GLO_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_ ___GLO(21,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_)
#define ___PRM_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_ ___PRM(21,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_)
#define ___GLO_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0 ___GLO(22,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0)
#define ___PRM_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0 ___PRM(22,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0)
#define ___GLO_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t ___GLO(23,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t)
#define ___PRM_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t ___PRM(23,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t)
#define ___GLO_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0 ___GLO(24,___G_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0)
#define ___PRM_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0 ___PRM(24,___G_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0)
#define ___GLO_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t ___GLO(25,___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t)
#define ___PRM_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t ___PRM(25,___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t)
#define ___GLO_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t ___GLO(26,___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t)
#define ___PRM_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t ___PRM(26,___G_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t)
#define ___GLO_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_ ___GLO(27,___G_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_)
#define ___PRM_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_ ___PRM(27,___G_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_)
#define ___GLO_std_2f_misc_2f_ports_23_read_2d_password_____25_____0 ___GLO(28,___G_std_2f_misc_2f_ports_23_read_2d_password_____25_____0)
#define ___PRM_std_2f_misc_2f_ports_23_read_2d_password_____25_____0 ___PRM(28,___G_std_2f_misc_2f_ports_23_read_2d_password_____25_____0)
#define ___GLO_std_2f_text_2f_hex_23_hex_2d_encode____0 ___GLO(29,___G_std_2f_text_2f_hex_23_hex_2d_encode____0)
#define ___PRM_std_2f_text_2f_hex_23_hex_2d_encode____0 ___PRM(29,___G_std_2f_text_2f_hex_23_hex_2d_encode____0)

___DEF_SUB_BIGFIX(___X0,1UL)
               ___BIGFIX1(0x698e2c8eL)
___DEF_SUB_STR(___X1,18UL)
               ___STR8(69,110,116,101,114,32,112,97)
               ___STR8(115,115,112,104,114,97,115,101)
               ___STR2(58,32)
___DEF_SUB_STR(___X2,21UL)
               ___STR8(82,101,45,101,110,116,101,114)
               ___STR8(32,112,97,115,115,112,104,114)
               ___STR5(97,115,101,58,32)
___DEF_SUB_STR(___X3,38UL)
               ___STR8(97,100,109,105,110,105,115,116)
               ___STR8(114,97,116,105,118,101,32,112)
               ___STR8(97,115,115,112,104,114,97,115)
               ___STR8(101,115,32,100,111,110,39,116)
               ___STR6(32,109,97,116,99,104)
___DEF_SUB_VEC(___X4,6UL)
               ___VEC1(___REF_SUB(5))
               ___VEC1(___REF_SUB(6))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X5,1UL)
               ___VEC1(___REF_SYM(7,___S_gerbil____tools____gxensemble____admin))
               ___VEC0
___DEF_SUB_VEC(___X6,0UL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
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
___DEF_M_HLBL(___L0_gerbil____tools____gxensemble____admin_23_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_M_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil____tools____gxensemble____admin_23_
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
___DEF_P_HLBL(___L0_gerbil____tools____gxensemble____admin_23_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil____tools____gxensemble____admin_23_)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L_gerbil____tools____gxensemble____admin_23_)
   ___SET_GLO(4,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_3a__3a_timestamp,___BIGFIX(0,1770925198LL))
   ___SET_R1(___VOID)
   ___JUMPRET(___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___SYM_view)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___END_IF
   ___SET_R0(___LBL(3))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),18,___G_std_2f_actor_2d_v18_2f_cookie_23_get_2d_actor_2d_server_2d_cookie____0)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),29,___G_std_2f_text_2f_hex_23_hex_2d_encode____0)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_R0(___STK(-3))
   ___POLL(5)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),7,___G_displayln)
___DEF_GLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_force)
   ___SET_R0(___LBL(6))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___SET_R2(___R1)
   ___SET_R1(___FAL)
   ___SET_R0(___STK(-3))
   ___POLL(7)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),17,___G_std_2f_actor_2d_v18_2f_cookie_23_generate_2d_ensemble_2d_cookie_21______25_____0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds
#undef ___PH_LBL0
#define ___PH_LBL0 12
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R2(___SYM_view)
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R0(___LBL(2))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___END_IF
   ___SET_R2(___SUB(1))
   ___SET_R1(___FAL)
   ___SET_R0(___LBL(3))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),28,___G_std_2f_misc_2f_ports_23_read_2d_password_____25_____0)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_STK(-5,___R1)
   ___SET_R2(___SUB(2))
   ___SET_R1(___FAL)
   ___SET_R0(___LBL(4))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),28,___G_std_2f_misc_2f_ports_23_read_2d_password_____25_____0)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___IF(___EQP(___STK(-5),___R1))
   ___GOTO(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___END_IF
   ___IF(___NOT(___POSSIBLYEQUALP(___STK(-5),___R1)))
   ___GOTO(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___END_IF
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(5))
   ___JUMPPRM(___SET_NARGS(2),___PRM_equal_3f_)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___END_IF
   ___GOTO(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
___DEF_GLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R1(___STK(-6))
   ___SET_R2(___SYM_force)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R3(___STK(-5))
   ___SET_R2(___R1)
   ___SET_R1(___FAL)
   ___SET_R0(___STK(-7))
   ___POLL(8)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),16,___G_std_2f_actor_2d_v18_2f_admin_23_generate_2d_admin_2d_keypair_21______25_____0)
___DEF_GLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R1(___SUB(3))
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),9,___G_error)
___DEF_GLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R0(___LBL(9))
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(0),15,___G_std_2f_actor_2d_v18_2f_admin_23_ensemble_2d_admin_2d_pubkey_2d_path)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R3(___R1)
   ___SET_R2(___GLO_absent_2d_value)
   ___SET_R1(___FAL)
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(3),27,___G_std_2f_misc_2f_ports_23_read_2d_file_2d_u8vector_____25_)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R0(___LBL(11))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),29,___G_std_2f_text_2f_hex_23_hex_2d_encode____0)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___SET_R0(___STK(-3))
   ___POLL(12)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds)
   ___ADJFP(-4)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),7,___G_displayln)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize
#undef ___PH_LBL0
#define ___PH_LBL0 26
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_P_HLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),3)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(1))
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___POLL(2)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),10,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(3,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___SET_R1(___CLO(___R4,1))
   ___SET_R2(___SYM_server_2d_id)
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_ref____0)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_STK(-4,___R1)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_authorized_2d_server_2d_id)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_ref____0)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_STK(-3,___R1)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_capabilities)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_ref____0)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_STK(-2,___R1)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_supervised)
   ___SET_R0(___LBL(8))
   ___ADJFP(4)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L20_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___END_IF
   ___SET_R1(___CLO(___STK(-9),1))
   ___SET_R2(___SYM_supervisor)
   ___SET_R0(___LBL(9))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L18_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___END_IF
   ___GOTO(___L19_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
___DEF_GLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___BEGIN_ALLOC_STRUCTURE(3UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___FAL)
   ___ADD_STRUCTURE_ELEM(2,___FAL)
   ___END_ALLOC_STRUCTURE(3)
   ___SET_R2(___GET_STRUCTURE(3))
   ___SET_STK(-9,___R1)
   ___SET_STK(-5,___R2)
   ___SET_R2(___STK(-8))
   ___SET_R1(___STK(-5))
   ___SET_R3(___FIX(0L))
   ___SET_R0(___LBL(12))
   ___CHECK_HEAP(11,4096)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),22,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___BEGIN_ALLOC_STRUCTURE(3UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_auth_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___STK(-7))
   ___ADD_STRUCTURE_ELEM(2,___STK(-6))
   ___END_ALLOC_STRUCTURE(3)
   ___SET_R1(___GET_STRUCTURE(3))
   ___SET_STK(-8,___STK(-11))
   ___SET_STK(-11,___FAL)
   ___SET_STK(-7,___STK(-10))
   ___SET_STK(-10,___STK(-9))
   ___SET_R3(___STK(-7))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-8))
   ___CHECK_HEAP(13,4096)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___POLL(14)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___ADJFP(-10)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),21,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_)
___DEF_GLBL(___L19_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_R0(___LBL(10))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0)
___DEF_GLBL(___L20_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_R1(___STK(-8))
   ___SET_R0(___LBL(15))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),12,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_R0(___LBL(16))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),11,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_get_2d_privkey)
___DEF_SLBL(16,___L16_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___SET_STK(-9,___STK(-11))
   ___SET_STK(-11,___FAL)
   ___SET_STK(-5,___STK(-10))
   ___SET_STK(-10,___STK(-6))
   ___SET_STK(-6,___STK(-9))
   ___SET_STK(-9,___R1)
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-7))
   ___SET_R1(___STK(-8))
   ___SET_R0(___STK(-6))
   ___POLL(17)
___DEF_SLBL(17,___L17_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize)
   ___ADJFP(-9)
   ___JUMPGLONOTSAFE(___SET_NARGS(6),19,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_authorize_____25______25_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract
#undef ___PH_LBL0
#define ___PH_LBL0 45
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_P_HLBL(___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_STK(1,___ALLOC_CLO(1UL))
   ___BEGIN_SETUP_CLO(1,___STK(1),3)
   ___ADD_CLO_ELEM(0,___R1)
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(1))
   ___ADJFP(1)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___POLL(2)
___DEF_SLBL(2,___L2_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___ADJFP(-1)
   ___JUMPGLONOTSAFE(___SET_NARGS(2),10,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_call_2d_with_2d_console_2d_server)
___DEF_SLBL(3,___L3_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(3,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R4)
   ___SET_R1(___CLO(___R4,1))
   ___SET_R2(___SYM_server_2d_id)
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_R0(___LBL(5))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_ref____0)
___DEF_SLBL(5,___L5_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_STK(-4,___R1)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_authorized_2d_server_2d_id)
   ___SET_R0(___LBL(6))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),14,___G_hash_2d_ref____0)
___DEF_SLBL(6,___L6_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_STK(-3,___R1)
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_supervised)
   ___SET_R0(___LBL(7))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(7,___L7_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___IF(___NOT(___NOTFALSEP(___R1)))
   ___GOTO(___L18_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___END_IF
   ___SET_R1(___CLO(___STK(-5),1))
   ___SET_R2(___SYM_supervisor)
   ___SET_R0(___LBL(8))
   ___JUMPGLONOTSAFE(___SET_NARGS(2),13,___G_hash_2d_get)
___DEF_SLBL(8,___L8_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___IF(___NOTFALSEP(___R1))
   ___GOTO(___L16_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___END_IF
   ___GOTO(___L17_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_SLBL(9,___L9_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
___DEF_GLBL(___L16_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___BEGIN_ALLOC_STRUCTURE(3UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___FAL)
   ___ADD_STRUCTURE_ELEM(2,___FAL)
   ___END_ALLOC_STRUCTURE(3)
   ___SET_R2(___GET_STRUCTURE(3))
   ___SET_STK(-5,___R1)
   ___SET_STK(-2,___R2)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-2))
   ___SET_R3(___FIX(0L))
   ___SET_R0(___LBL(11))
   ___ADJFP(4)
   ___CHECK_HEAP(10,4096)
___DEF_SLBL(10,___L10_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),22,___G_std_2f_actor_2d_v18_2f_message_23_reference_3a__3a__3a_init_21_____0)
___DEF_SLBL(11,___L11_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___BEGIN_ALLOC_STRUCTURE(2UL)
   ___ADD_STRUCTURE_ELEM(0,___GLO_std_2f_actor_2d_v18_2f_proto_23__21_admin_2d_retract_3a__3a_t)
   ___ADD_STRUCTURE_ELEM(1,___STK(-7))
   ___END_ALLOC_STRUCTURE(2)
   ___SET_R1(___GET_STRUCTURE(2))
   ___SET_STK(-8,___STK(-11))
   ___SET_STK(-11,___FAL)
   ___SET_STK(-7,___STK(-10))
   ___SET_STK(-10,___STK(-9))
   ___SET_R3(___STK(-7))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-8))
   ___CHECK_HEAP(12,4096)
___DEF_SLBL(12,___L12_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___POLL(13)
___DEF_SLBL(13,___L13_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___ADJFP(-10)
   ___JUMPGLONOTSAFE(___SET_NARGS(5),21,___G_std_2f_actor_2d_v18_2f_ensemble_2d_supervisor_23_ensemble_2d_supervisor_2d_invoke_21______25_)
___DEF_GLBL(___L17_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_R0(___LBL(9))
   ___JUMPGLONOTSAFE(___SET_NARGS(0),24,___G_std_2f_actor_2d_v18_2f_path_23_ensemble_2d_domain_2d_supervisor____0)
___DEF_GLBL(___L18_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(14))
   ___JUMPGLONOTSAFE(___SET_NARGS(1),12,___G_gerbil_2f_tools_2f_gxensemble_2f_util_23_maybe_2d_authorize_21_)
___DEF_SLBL(14,___L14_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___SET_R3(___STK(-6))
   ___SET_R2(___STK(-3))
   ___SET_R1(___STK(-4))
   ___SET_R0(___STK(-7))
   ___POLL(15)
___DEF_SLBL(15,___L15_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract)
   ___ADJFP(-8)
   ___JUMPGLONOTSAFE(___SET_NARGS(3),20,___G_std_2f_actor_2d_v18_2f_ensemble_23_admin_2d_retract_____25_)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H_gerbil____tools____gxensemble____admin_23_,___REF_SYM(8,___S_gerbil____tools____gxensemble____admin_23_),___REF_FAL,1,0)
,___DEF_LBL_PROC(___H_gerbil____tools____gxensemble____admin_23_,0,-1)
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___REF_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie),___REF_FAL,8,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___REF_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds),___REF_FAL,13,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___REF_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize),___REF_FAL,18,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,1,1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___OFD(___RETI,12,0,0x3f077L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x77L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___OFD(___RETI,12,12,0x3f003L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___OFD(___RETI,12,12,0x3f003L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,___OFD(___RETI,12,12,0x3f007L))
,___DEF_LBL_INTRO(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___REF_SYM(6,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract),___REF_FAL,16,0)
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,1,-1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_PROC(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,1,1)
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0x1bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0x1bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___OFD(___RETI,12,0,0x3f037L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,9,0,0x37L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___OFD(___RETI,12,12,0x3f003L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___OFD(___RETI,12,12,0x3f003L))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETN,5,0,0x1bL))
,___DEF_LBL_RET(___H_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,___IFD(___RETI,8,8,0x3f00L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,12,0)
               ___GCMAP1(0x3f077L)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f003L)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f003L)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f007L)
,___DEF_OFD(___RETI,12,0)
               ___GCMAP1(0x3f037L)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f003L)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f003L)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(5,___G_gerbil____tools____gxensemble____admin_23_,1)
___DEF_MOD_PRM(1,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,3)
___DEF_MOD_PRM(2,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,12)
___DEF_MOD_PRM(0,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,26)
___DEF_MOD_PRM(3,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,45)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(5,___G_gerbil____tools____gxensemble____admin_23_,1)
___DEF_MOD_GLO(1,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,3)
___DEF_MOD_GLO(2,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,12)
___DEF_MOD_GLO(0,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,26)
___DEF_MOD_GLO(3,___G_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,45)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_authorized_2d_server_2d_id,"authorized-server-id")
___DEF_MOD_SYM(1,___S_capabilities,"capabilities")
___DEF_MOD_SYM(2,___S_force,"force")
___DEF_MOD_SYM(3,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_authorize,"gerbil/tools/gxensemble/admin#do-admin-authorize")

___DEF_MOD_SYM(4,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_cookie,"gerbil/tools/gxensemble/admin#do-admin-cookie")

___DEF_MOD_SYM(5,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_creds,"gerbil/tools/gxensemble/admin#do-admin-creds")

___DEF_MOD_SYM(6,___S_gerbil_2f_tools_2f_gxensemble_2f_admin_23_do_2d_admin_2d_retract,"gerbil/tools/gxensemble/admin#do-admin-retract")

___DEF_MOD_SYM(7,___S_gerbil____tools____gxensemble____admin,"gerbil__tools__gxensemble__admin")

___DEF_MOD_SYM(8,___S_gerbil____tools____gxensemble____admin_23_,"gerbil__tools__gxensemble__admin#")

___DEF_MOD_SYM(9,___S_server_2d_id,"server-id")
___DEF_MOD_SYM(10,___S_supervised,"supervised")
___DEF_MOD_SYM(11,___S_supervisor,"supervisor")
___DEF_MOD_SYM(12,___S_view,"view")
___END_MOD_SYM_KEY

#endif
