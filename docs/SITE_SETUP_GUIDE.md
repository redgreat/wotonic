# Zotonic 站点创建和主机名配置指南

## 📋 目录
1. [快速开始](#快速开始)
2. [手动创建站点](#手动创建站点)
3. [配置 hosts 文件](#配置-hosts-文件)
4. [常见问题](#常见问题)

---

## 🚀 快速开始

使用提供的自动化脚本快速创建站点：

```bash
# 基本用法
./script/create-site.sh <站点名> <主机名> <管理员密码> [骨架类型]

# 示例：创建一个博客站点
./script/create-site.sh myblog myblog.local admin123 blog

# 示例：创建一个空站点
./script/create-site.sh mysite mysite.local mypass123 empty
```

---

## 🛠️ 手动创建站点

### 1. 创建站点命令

```bash
docker compose exec zotonic bin/zotonic addsite [选项] <站点名称>
```

### 2. 重要参数

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `-H <host>` | 站点主机名 | `<站点名>.test` |
| `-s <skel>` | 骨架类型 (blog/empty/nodb) | `blog` |
| `-h <host>` | 数据库主机 | `localhost` ⚠️ Docker 中应用 `postgres` |
| `-p <port>` | 数据库端口 | `5432` |
| `-u <user>` | 数据库用户 | `zotonic` |
| `-P <pass>` | 数据库密码 | `zotonic` |
| `-d <name>` | 数据库名 | `zotonic` |
| `-a <pass>` | 管理员密码 | 无 |

### 3. 创建示例

#### 创建博客站点
```bash
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H myblog.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  myblog
```

#### 创建空站点
```bash
docker compose exec zotonic bin/zotonic addsite \
  -s empty \
  -H mysite.local \
  -h postgres \
  -a mypassword \
  mysite
```

#### 创建无数据库站点
```bash
docker compose exec zotonic bin/zotonic addsite \
  -s nodb \
  -H static.local \
  -a admin123 \
  staticsite
```

---

## 🌐 配置 hosts 文件

### macOS / Linux

#### 方法 1：使用命令行

```bash
# 编辑 hosts 文件
sudo nano /etc/hosts

# 添加以下行（根据你的站点修改）
127.0.0.1    myblog.local
127.0.0.1    mysite.local

# 保存并退出
# Ctrl+O 保存，Ctrl+X 退出

# 刷新 DNS 缓存（macOS）
sudo dscacheutil -flushcache
sudo killall -HUP mDNSResponder
```

#### 方法 2：使用一行命令

```bash
# 添加单个域名
echo "127.0.0.1    myblog.local" | sudo tee -a /etc/hosts

# 添加多个域名
sudo bash -c 'cat >> /etc/hosts << EOF
127.0.0.1    myblog.local
127.0.0.1    mysite.local
127.0.0.1    shop.local
EOF'
```

### Windows

1. 以管理员身份运行记事本
2. 打开文件：`C:\Windows\System32\drivers\etc\hosts`
3. 在文件末尾添加：
   ```
   127.0.0.1    myblog.local
   127.0.0.1    mysite.local
   ```
4. 保存文件
5. 刷新 DNS 缓存：
   ```cmd
   ipconfig /flushdns
   ```

---

## 🔧 站点管理命令

### 启动站点
```bash
docker compose exec zotonic bin/zotonic startsite <站点名>
```

### 停止站点
```bash
docker compose exec zotonic bin/zotonic stopsite <站点名>
```

### 重启站点
```bash
docker compose exec zotonic bin/zotonic restartsite <站点名>
```

### 查看站点状态
```bash
docker compose exec zotonic bin/zotonic status
```

### 查看站点配置
```bash
docker compose exec zotonic bin/zotonic siteconfig <站点名>
```

---

## 🎯 访问站点

创建站点后，可以通过以下地址访问：

- **前台**: `http://<主机名>:8000` 或 `https://<主机名>:8443`
- **管理后台**: `http://<主机名>:8000/admin`

### 登录信息
- **用户名**: `admin`
- **密码**: 创建站点时使用 `-a` 参数设置的密码

---

## ❓ 常见问题

### Q1: 为什么数据库主机要用 `postgres` 而不是 `localhost`？

**A**: 在 Docker 环境中，容器之间通过服务名通信。PostgreSQL 容器的服务名是 `postgres`，所以 Zotonic 容器需要使用 `postgres` 作为数据库主机名。

### Q2: 站点创建后无法访问怎么办？

**A**: 检查以下几点：
1. 确认 hosts 文件已正确配置
2. 确认站点已启动：`docker compose exec zotonic bin/zotonic status`
3. 检查 Zotonic 日志：`docker compose logs -f zotonic`
4. 确认使用了正确的端口（8000 或 8443）

### Q3: 如何修改站点的主机名？

**A**: 
```bash
# 进入容器
docker compose exec zotonic bash

# 编辑站点配置
# 配置文件位于: apps_user/<站点名>/priv/zotonic_site.config
nano apps_user/<站点名>/priv/zotonic_site.config

# 修改 hostname 配置，然后重启站点
bin/zotonic restartsite <站点名>
```

### Q4: 如何删除站点？

**A**:
```bash
# 停止站点
docker compose exec zotonic bin/zotonic stopsite <站点名>

# 删除站点目录
docker compose exec zotonic rm -rf apps_user/<站点名>

# 删除数据库（如果需要）
docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA <站点名> CASCADE;"
```

### Q5: 站点骨架类型有什么区别？

**A**:
- **blog**: 包含完整的博客功能，适合内容管理
- **empty**: 空站点，适合从零开始自定义
- **nodb**: 无数据库站点，适合静态内容

---

## 📚 相关文档

- [Zotonic 官方文档](https://zotonic.com/docs)
- [Docker 配置文档](https://zotonic.com/docs/1411/docker)
- [站点管理指南](https://zotonic.com/id/doc_developerguide_sites)

---

## 💡 提示

1. **开发环境建议**: 使用 `.local` 或 `.test` 作为域名后缀
2. **生产环境**: 使用真实域名并配置 DNS
3. **安全性**: 不要在生产环境使用简单的管理员密码
4. **备份**: 定期备份站点数据和数据库

---

创建时间: 2025-12-20
