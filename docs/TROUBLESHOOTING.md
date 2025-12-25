# 站点创建失败问题解决方案

## 🔍 问题分析

根据日志分析，站点创建失败的原因有两个：

### 1. 数据库主机配置错误
在前两次尝试中，数据库主机设置为 `localhost`，但在 Docker 环境中应该是 `postgres`：

```
ERROR: Cannot create database because user cannot connect to the 'postgres' database
dbhost,<<"localhost">>
reason=econnrefused
```

### 2. 主机名无法解析
即使后来改成了 `postgres`，创建仍然失败，因为 Zotonic 要求主机名必须可以解析：

```
Error: The hostname is unknown, check your DNS or /etc/hosts file: wangcw.local
```

## ✅ 解决方案

### 方法 1：使用自动修复脚本（推荐）

```bash
./script/fix-site-creation.sh
```

这个脚本会自动：
1. 添加 `wangcw.local` 到 `/etc/hosts`
2. 刷新 DNS 缓存
3. 使用正确的参数创建站点
4. 启动站点

### 方法 2：手动修复

#### 步骤 1：添加 hosts 条目

```bash
# 编辑 hosts 文件
sudo nano /etc/hosts

# 添加以下行
127.0.0.1    wangcw.local

# 保存并退出（Ctrl+O, Enter, Ctrl+X）

# 刷新 DNS 缓存（macOS）
sudo dscacheutil -flushcache
sudo killall -HUP mDNSResponder
```

#### 步骤 2：创建站点

```bash
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H wangcw.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  wangcw
```

#### 步骤 3：启动站点

```bash
docker compose exec zotonic bin/zotonic startsite wangcw
```

## 📋 在后台界面创建站点的正确配置

如果你想通过 Web 界面创建站点，请确保填写以下配置：

| 字段 | 值 | 说明 |
|------|-----|------|
| Site name | wangcw | 站点名称 |
| Hostname | wangcw.local | **必须先添加到 /etc/hosts** |
| Skeleton | blog | 站点模板 |
| Database host | **postgres** | ⚠️ 不是 localhost！ |
| Database port | 5432 | 默认端口 |
| Database name | zotonic | 默认数据库 |
| Database schema | public | 默认 schema |
| Database user | zotonic | 默认用户 |
| Database password | zotonic | 默认密码 |
| Admin password | admin123 | 你的管理员密码 |

## ⚠️ 重要提示

### 1. 数据库主机必须是 `postgres`
在 Docker 环境中，容器之间通过服务名通信，所以：
- ✅ 正确：`postgres`
- ❌ 错误：`localhost` 或 `127.0.0.1`

### 2. 主机名必须可以解析
在创建站点之前，必须先将主机名添加到 `/etc/hosts`：

```bash
# 先添加 hosts
echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts

# 然后再创建站点
```

### 3. 权限问题已解决
`apps_user` 目录的权限已经正确设置为 `zotonic:zotonic`，不会有权限问题。

## 🔍 验证站点是否创建成功

### 检查站点目录
```bash
# 在本地
ls -la apps_user/

# 应该看到 wangcw 目录
```

### 检查站点状态
```bash
docker compose exec zotonic bin/zotonic status
```

### 查看日志
```bash
docker compose logs -f zotonic | grep wangcw
```

## 🌐 访问站点

创建成功后，访问：

- **前台**: http://wangcw.local:8000
- **管理后台**: http://wangcw.local:8000/admin
  - 用户名: `admin`
  - 密码: `admin123`（你设置的密码）

## 🐛 如果还是失败

### 1. 查看完整日志
```bash
docker compose logs zotonic | grep -A 20 "Creating site"
```

### 2. 检查数据库连接
```bash
docker compose exec postgres psql -U zotonic -d zotonic -c "SELECT 1;"
```

### 3. 检查 hosts 文件
```bash
cat /etc/hosts | grep wangcw
```

### 4. 重启服务
```bash
docker compose restart zotonic
```

## 📝 总结

站点创建失败的根本原因是：
1. ❌ 数据库主机配置错误（`localhost` 应该是 `postgres`）
2. ❌ 主机名未添加到 `/etc/hosts`

解决方法：
1. ✅ 先添加主机名到 `/etc/hosts`
2. ✅ 使用正确的数据库主机 `postgres`
3. ✅ 运行修复脚本或手动创建

---

创建时间: 2025-12-20
