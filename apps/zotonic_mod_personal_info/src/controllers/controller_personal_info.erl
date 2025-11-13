-module(controller_personal_info).

-export([
    service_available/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc 个人信息页面服务可用性检查
service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

%% @doc 个人信息页面权限校验，仅允许本人或管理员访问
is_authorized(Context) ->
    z_controller_helper:is_authorized([{use, z_context:get(acl_module, Context, mod_admin)}], Context).

%% @doc 渲染个人信息管理页面
process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    UserId = z_acl:user(Context),
    Vars = [
        {user_id, UserId}
    ],
    Html = z_template:render("admin_personal_info.tpl", Vars, Context),
    z_context:output(Html, Context).

