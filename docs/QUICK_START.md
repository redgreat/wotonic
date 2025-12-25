# 手动创建 wangcw 站点 - 快速指南

## 🚀 快速创建（3 步完成）

### 步骤 1: 添加 hosts（只需执行一次）

```bash
echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts
```

### 步骤 2: 清理旧数据（如果之前创建过）

```bash
# 删除旧的数据库 schema
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"

# 删除旧的站点目录（如果存在）
rm -rf apps_user/wangcw
```

### 步骤 3: 创建并启动站点

```bash
# 创建站点
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H wangcw.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  wangcw

# 启动站点
docker compose exec zotonic bin/zotonic startsite wangcw
```

## ✅ 完成！

访问站点：
- **前台**: http://wangcw.local:8000
- **管理后台**: http://wangcw.local:8000/admin
  - 用户名: `admin`
  - 密码: `admin123`

---

## 🔧 或者使用自动化脚本

```bash
./script/create-wangcw-site.sh
```

这个脚本会：
1. 检查 hosts 配置（如果没有会提示你手动添加）
2. 检查并清理旧数据
3. 创建站点
4. 启动站点

---

## 📋 验证站点是否创建成功

### 检查站点目录
```bash
ls -la apps_user/
```
应该看到 `wangcw` 目录

### 检查站点状态
```bash
docker compose exec zotonic bin/zotonic status
```

### 查看日志
```bash
docker compose logs -f zotonic | grep wangcw
```

---

## 🐛 如果遇到问题

### 问题 1: 主机名无法解析
```
Error: The hostname is unknown
```

**解决**：确保已添加到 hosts
```bash
echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts
```

### 问题 2: Schema 已存在
```
ERROR: schema "wangcw" already exists
```

**解决**：删除旧 schema
```bash
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"
```

### 问题 3: 目录权限错误
```
Permission denied
```

**解决**：设置权限
```bash
docker compose exec zotonic chown -R zotonic:zotonic /opt/zotonic/apps_user
```

### 问题 4: 无法连接数据库
```
econnrefused
```

**解决**：检查数据库主机配置，必须是 `postgres` 不是 `localhost`

---

## 💡 提示

1. **创建过程需要 30-60 秒**，请耐心等待
2. **不要在创建过程中重启 Docker**
3. **如果失败，查看日志**: `docker compose logs -f zotonic`
4. **完全重新开始**:
   ```bash
   # 清理所有
   docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;"
   rm -rf apps_user/wangcw
   
   # 重新创建
   docker compose exec zotonic bin/zotonic addsite -s blog -H wangcw.local -h postgres -u zotonic -P zotonic -d zotonic -a admin123 wangcw
   docker compose exec zotonic bin/zotonic startsite wangcw
   ```

---

创建时间: 2025-12-23
