# 站点创建问题解决方案

## 🔍 问题分析

你遇到的问题是：

### 症状
- 之前创建过站点，但后台管理页面看不到
- 再次尝试创建时报错，提示站点已存在
- 日志显示：`Creating site wangcw`

### 根本原因
1. **`apps_user` 目录丢失** - 站点文件不存在
2. **数据库中有旧数据** - `wangcw` schema 已存在
3. **hosts 文件未配置** - `wangcw.local` 未添加

这导致了一个矛盾的状态：
- ✅ 数据库认为站点存在（有 schema）
- ❌ 文件系统中站点不存在（没有目录）
- ❌ Zotonic 无法启动站点（文件缺失）

## ✅ 已执行的修复

### 1. 重新创建 apps_user 目录
```bash
mkdir -p apps_user
touch apps_user/.gitkeep
```

### 2. 设置正确的权限
```bash
docker compose exec zotonic chown -R zotonic:zotonic /opt/zotonic/apps_user
```

### 3. 清理数据库中的旧数据
```bash
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"
```

删除了 37 个数据库对象，包括：
- config, module, rsc, protect, edge, medium 等表
- 各种日志表和函数

## 🚀 完整解决方案

### 方法 1：使用自动修复脚本（强烈推荐）

```bash
./script/fix-site-creation.sh
```

这个脚本会自动：
1. ✅ 添加 `wangcw.local` 到 `/etc/hosts`（需要 sudo 密码）
2. ✅ 刷新 DNS 缓存
3. ✅ 确保 `apps_user` 目录存在
4. ✅ 检查并清理数据库中的旧数据
5. ✅ 使用正确的参数创建站点
6. ✅ 启动站点

### 方法 2：手动执行步骤

```bash
# 1. 添加 hosts
echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts

# 2. 刷新 DNS
sudo dscacheutil -flushcache
sudo killall -HUP mDNSResponder

# 3. 确保目录存在
mkdir -p apps_user
docker compose exec zotonic chown -R zotonic:zotonic /opt/zotonic/apps_user

# 4. 清理旧数据（如果需要）
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"

# 5. 创建站点
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H wangcw.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  wangcw

# 6. 启动站点
docker compose exec zotonic bin/zotonic startsite wangcw
```

## 📋 为什么后台看不到站点？

Zotonic 后台管理界面显示的站点列表来自：
1. **文件系统** - `apps_user/` 目录中的站点
2. **运行状态** - 当前启动的站点

如果站点文件不存在，即使数据库中有数据，后台也不会显示该站点。

## ⚠️ 避免此问题的建议

### 1. 不要在创建站点时重启 Docker
- 等待站点完全创建完成（30-60秒）
- 创建过程中不要刷新页面
- 不要重启容器

### 2. 确保 apps_user 目录持久化
- 该目录应该在项目根目录
- 已添加到 Git（通过 .gitkeep）
- 不要手动删除

### 3. 使用命令行创建更可靠
- Web 界面创建可能因网络中断失败
- 命令行可以看到详细的错误信息
- 使用自动化脚本避免配置错误

## 🔍 验证站点是否创建成功

### 检查文件系统
```bash
# 本地
ls -la apps_user/

# 容器内
docker compose exec zotonic ls -la /opt/zotonic/apps_user/
```

应该看到 `wangcw` 目录。

### 检查数据库
```bash
docker compose exec postgres psql -U zotonic -d zotonic -c "\dn"
```

应该看到 `wangcw` schema。

### 检查站点状态
```bash
docker compose exec zotonic bin/zotonic status
```

应该显示站点正在运行。

### 访问站点
- 前台: http://wangcw.local:8000
- 管理后台: http://wangcw.local:8000/admin

## 📝 访问信息

### 站点管理后台
- URL: http://wangcw.local:8000/admin
- 用户名: `admin`
- 密码: `admin123`

### Zotonic 状态页面
- URL: https://localhost:8443
- 用户名: `wwwadmin`
- 密码: `qV1o1VUA5mkg6jNm`

## 🐛 如果还是失败

### 查看实时日志
```bash
docker compose logs -f zotonic
```

### 检查容器状态
```bash
docker compose ps
```

### 重启服务
```bash
docker compose restart zotonic
```

### 完全重建
```bash
# 停止服务
docker compose down

# 清理旧数据
rm -rf apps_user/wangcw
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"

# 重新启动
docker compose up -d

# 运行修复脚本
./script/fix-site-creation.sh
```

---

**现在请运行修复脚本：**
```bash
./script/fix-site-creation.sh
```

它会提示你输入 sudo 密码，然后自动完成所有步骤。

创建时间: 2025-12-23
