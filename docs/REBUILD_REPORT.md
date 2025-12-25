# Docker 重新构建完成报告

## ✅ 执行的操作

### 1. 停止并删除容器
```bash
docker compose down
```
- ✅ 停止了 wotonic-zotonic-1 容器
- ✅ 停止了 wotonic-postgres-1 容器
- ✅ 删除了网络 wotonic_default

### 2. 删除旧镜像
```bash
docker rmi wotonic-zotonic:latest
```
- ✅ 删除了旧的 Zotonic 镜像（3.5GB）
- ✅ 释放了磁盘空间

### 3. 重新构建镜像（无缓存）
```bash
docker compose build --no-cache
```
- ✅ 使用 `--no-cache` 参数完全重新构建
- ✅ 包含了新添加的 `apps_user` 目录
- ✅ 构建时间：约 147 秒
- ✅ 新镜像大小：约 3.5GB

### 4. 启动服务
```bash
docker compose up -d
```
- ✅ PostgreSQL 容器已启动并健康
- ✅ Zotonic 容器已启动
- ✅ 所有服务正常运行

## 📊 当前状态

### 容器状态
| 容器名 | 状态 | 端口映射 |
|--------|------|----------|
| wotonic-postgres-1 | 运行中 (健康) | 5432:5432 |
| wotonic-zotonic-1 | 运行中 | 8000:8000, 8443:8443 |

### 服务端口
- ✅ HTTP: 8000
- ✅ HTTPS: 8443
- ✅ MQTT: 1883, 8883
- ✅ SMTP: 2525
- ✅ PostgreSQL: 5432

### 目录验证
- ✅ `/opt/zotonic/apps_user/` 存在
- ✅ 权限正确：`zotonic:zotonic`
- ✅ `.gitkeep` 文件已包含

## 🎯 下一步操作

现在你可以：

### 1. 访问 Zotonic 状态页面
```
https://localhost:8443
```
- 用户名: `wwwadmin`
- 密码: `qV1o1VUA5mkg6jNm`

### 2. 创建站点

#### 方法 A：使用修复脚本（推荐）
```bash
./script/fix-site-creation.sh
```

#### 方法 B：手动创建
```bash
# 1. 添加 hosts（如果还没添加）
echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts

# 2. 创建站点
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H wangcw.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  wangcw

# 3. 启动站点
docker compose exec zotonic bin/zotonic startsite wangcw
```

#### 方法 C：通过 Web 界面创建
1. 访问 https://localhost:8443
2. 登录后台
3. 创建新站点，**确保**：
   - Database host: `postgres`（不是 localhost）
   - Hostname: 先添加到 /etc/hosts

## 📝 重要提示

### ⚠️ 创建站点前必须做的事

1. **添加主机名到 /etc/hosts**
   ```bash
   echo "127.0.0.1    <你的站点>.local" | sudo tee -a /etc/hosts
   ```

2. **使用正确的数据库主机**
   - ✅ 正确：`postgres`
   - ❌ 错误：`localhost` 或 `127.0.0.1`

### 📚 相关文档

- `SITE_SETUP_GUIDE.md` - 站点创建完整指南
- `TROUBLESHOOTING.md` - 故障排除文档
- `script/create-site.sh` - 自动化创建脚本
- `script/fix-site-creation.sh` - 修复脚本

## 🔍 验证命令

```bash
# 查看容器状态
docker compose ps

# 查看日志
docker compose logs -f zotonic

# 检查 apps_user 目录
docker compose exec zotonic ls -la /opt/zotonic/apps_user/

# 测试数据库连接
docker compose exec postgres psql -U zotonic -d zotonic -c "SELECT 1;"
```

## 📅 构建信息

- **构建时间**: 2025-12-20 20:07
- **构建方式**: 无缓存完全重建
- **Erlang 版本**: 26
- **PostgreSQL 版本**: 16-alpine
- **Zotonic 版本**: 1.0.0-rc.15

---

✅ Docker 重新构建完成！所有服务正常运行。
