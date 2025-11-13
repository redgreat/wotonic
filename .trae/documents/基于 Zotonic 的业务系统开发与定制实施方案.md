## 一、框架适用性评估

核心架构：OTP 应用与模块化

* 主体分层：`apps/zotonic_core`（核心模型/支持库/数据库）、`apps/zotonic_listen_http`（HTTP/HTTPS监听）、`apps/zotonic_launcher`（启动/生命周期）、大量 `zotonic_mod_*` 业务模块。

模块管理：`z_module_manager` 负责启停与依赖（apps/zotonic\_core/src/support/z\_module\_manager.erl）。可按站点启用/禁用模块，支持依赖扫描与优先级。

* 扩展机制：模型/控制器/模板

  * 模型接口：`zotonic_model` 行为与 `m_*` 模型（如 `m_rsc`、`m_config`、`m_identity`）。模块通过模板和控制器扩展 UI 与功能。

  * 路由机制：各模块 `priv/dispatch/dispatch` 定义路由，如后台 `apps/zotonic_mod_admin/priv/dispatch/dispatch` 映射到控制器。

* 后台管理（admin）支持

  * 控制器：`controller_admin` 进行权限校验并渲染模板（apps/zotonic\_mod\_admin/src/controllers/controller\_admin.erl:43–54）。

  * 菜单与页面：`admin_base.tpl` 等提供完整后台 UI（apps/zotonic\_mod\_admin/priv/templates/admin\_base.tpl）。

  * 配置维护：`controller_admin_config` 列出与编辑配置键值（apps/zotonic\_mod\_admin\_config/src/controllers/controller\_admin\_config.erl:49–56）。

  * 用户与身份：`zotonic_mod_admin_identity` 提供用户搜索、账号、邮箱、密码设置等（priv/templates/*、src/actions/*）。

* 模块化与可剥离性

  * 模块启停：`m_modules`/`z_module_manager` 支持查询已安装/启用模块与启停操作（apps/zotonic\_core/src/models/m\_modules.erl:56–88）。

  * 可禁用发布相关 UI：发布控件主要在 `mod_admin` 模板（如 `*_publish*.tpl`）。通过模块启停或条件配置可隐藏/禁用发布功能。

  * 核心保留：保留 `zotonic_core`、`mod_base` 等核心模块，按需禁用 CMS 特性模块（如 SEO、菜单、评论等）。

结论：现有框架高度模块化，后台与权限齐备，适合作为业务系统基座。可在不改动核心的前提下，通过启停模块与新增自定义模块实现“剥离 CMS、保留核心”的目标。

## 二、数据库支持方案

* 现状与限制

  * 默认数据库：PostgreSQL（`z_db_pgsql` 使用 epgsql 驱动，apps/zotonic\_core/src/db/z\_db\_pgsql.erl）。

  * 抽象层接口：`z_db.erl` 封装查询/事务/表操作（apps/zotonic\_core/src/db/z\_db.erl），但类型与行为与 epgsql 强绑定（`database_server() :: postgresql`）。

  * 可扩展接口：存在 `z_db_worker` 行为，但回调类型依赖 epgsql（apps/zotonic\_core/src/db/z\_db\_worker.erl:22–35）。

* 目标：支持 MySQL / PostgreSQL / MongoDB

* 设计：统一数据库访问抽象层

  * 新增通用适配行为 `z_db_adapter`（提案）：定义 `start_link/Args`、`test_connection/Args`、`squery/3`、`equery/4`、批量执行、事务等与驱动无关的返回结构。

  * 适配器实现：

    * `z_db_pgsql_adapter`（基于现有 `z_db_pgsql` 重构，仍用 epgsql）。

    * `z_db_mysql_adapter`（选型 Erlang MySQL 驱动，如 `mysql-otp`），映射参数与结果到统一结构。

    * `z_db_mongo_adapter`（选型 `mongodb-erlang`），提供 CRUD 与聚合封装；对 `m_rsc` 等依赖 SQL 的模型需提供“等价接口”或限定子集。

  * `z_db.erl` 改造：以站点配置选择适配器，统一出参（避免直接暴露 epgsql 类型），维持现有 `q/equery/transaction` API 不变。

  * 模型兼容：为 `m_rsc`/`m_edge` 等 SQL 依赖模型定义“兼容层”，MySQL 全量兼容，Mongo 以文档型映射实现资源/边关系（必要时限制某些查询语义）。

* 迁移与验证

  * 配置扩展：站点 `config` 增加 `db.engine = pgsql|mysql|mongo`、连接参数与 `dbschema` 兼容方案。

  * 连接健康检查：实现 `test_connection` 按引擎检测/建库/建表或集合存在性。

  * 基准测试与一致性测试：对典型模型与查询路径做跨引擎一致性校验。

## 三、个人信息系统模块

* 目标：用户个人信息展示/编辑/权限控制，支持可扩展字段。

* 复用与扩展

  * 复用 `zotonic_mod_admin_identity` 的用户与身份管理（模板与动作齐备）。

  * 新增模块 `mod_personal_info`：

    * 控制器：个人信息首页/编辑页（复用 ACL 校验 `z_acl:is_allowed`，参考 controller\_admin.erl）。

    * 模型：抽象用户扩展字段（存储于 `props` 或独立表/集合，兼容多数据库）。

    * 模板：前台或后台视图 `personal_info.tpl`/`_edit.tpl`，遵循现有 Admin UI 风格（如 `admin_base.tpl`）。

  * 字段扩展机制：通过配置驱动字段集（如 `m_config` 中 `personal_info.fields`），模板动态渲染与验证（validators）。

* 权限控制

  * 规则：仅本人与管理员可编辑，公开字段可展示。

  * ACL 集成：操作级校验（view/update），参考 `z_acl.erl` 接口（apps/zotonic\_core/src/support/z\_acl.erl:90–110）。

## 四、后台管理功能

* 后台 UI 框架：直接复用 `mod_admin`，统一导航/样式与组件。

* 配置信息维护：复用 `mod_admin_config`（controller\_admin\_config.erl），可在模块中加扩展配置页。

* 内容管理与发布控制：

  * 保留内容检索/媒体管理等通用能力。

  * 发布控制定制：通过配置开关隐藏发布相关模板块（如 `*_publish*.tpl`），或在 ACL 中禁止普通用户发布操作。

## 五、部署与访问方案

* 服务监听：`zotonic_listen_http` 使用 Cowboy 启动 HTTP/HTTPS，端口/IP 来自 `z_config`（apps/zotonic\_listen\_http/src/zotonic\_listen\_http.erl:110–121, 129–186）。

* 自动化部署：

  * Windows 11 环境：提供 PowerShell 脚本（根目录 `script/`）用于编译、配置、启动、停止、查看状态。

  * 容器化选项：复用 `docker/` 与 `docker-compose.yml`，按需生成站点配置与数据库连接。

* 自动生成可访问 URL

  * 启动时读取 `hostname` 与 `listen_port`，输出 `http(s)://<hostname>:<port>/` 到日志与控制台（参考 `zotonic_listen_http` 已记录 ip/port）。

  * 在后台状态页显示当前访问 URL（扩展 `admin_status.tpl` 或新增状态组件）。

* HTTP/HTTPS 协议：复用现有 HTTPS 支持与 SNI（`z_ssl_certs.erl`），证书按站点配置。

## 六、定制化调整

* 移除/禁用不需要的 CMS 发布功能：

  * 通过模块管理禁用相关模块（如 `mod_seo`、`mod_menu` 等非必需）。

  * 在 Admin 模板层隐藏发布控件；在 ACL 层禁止发布/删除等操作。

* 保留核心能力：

  * 站点管理/路由/模板/媒体/配置/权限等核心模块与支持库。

* 是否需要二次开发核心：

  * 为实现多数据库与统一抽象层，需要对 `z_db.erl`/`z_db_worker.erl` 进行有限重构，使其与驱动无关（不破坏上层 API）。

  * 其他核心保持不变，功能通过模块扩展实现。

## 七、实施步骤与里程碑

* 阶段 A：技术验证（1–2 周）

  * 跨模块梳理与 PoC：以 `mod_personal_info` 原型验证模型/模板/ACL 与 PostgreSQL。

  * 适配层 PoC：实现 `z_db_mysql_adapter` 最小查询/事务，验证 `z_db.erl` 适配改造的可行性。

* 阶段 B：原型开发（2–3 周）

  * 完成统一数据库抽象初版（PG/MySQL）。

  * 完成个人信息模块基本界面与字段扩展机制。

  * 后台状态页展示访问 URL。

* 阶段 C：功能实现（3–4 周）

  * 扩展内容管理所需子集，完善权限策略。

  * 发布控制开关与模板隐藏上线。

  * Mongo 适配（如需），实现资源/关系的文档映射与受限查询子集。

* 阶段 D：测试与部署（1–2 周）

  * 单元与集成测试（跨引擎一致性）。

  * PowerShell/容器化部署脚本与环境配置。

  * 验证 HTTPS 与 URL 输出/展示。

## 八、风险与应对

* 多数据库一致性风险：

  * 采用统一出参结构与行为测试，限定 Mongo 的查询语义，必要时对复杂查询走 PG/MySQL。

* 对核心的改动风险：

  * 以最小重构实现适配层；保持上层 API 与模块不变，回撤策略明确。

* 发布功能剥离风险：

  * 通过模板开关与 ACL 双重控制，确保业务流程不受影响。

## 九、交付物

* 统一数据库适配层（PG/MySQL，Mongo 可选）。

* `mod_personal_info` 模块与后台集成。

* 后台访问 URL 展示组件。

* Windows 11 下的 `script/` 部署/启动/停止/状态脚本。

* 文档：配置说明与部署指南（随代码注释与示例配置）。

