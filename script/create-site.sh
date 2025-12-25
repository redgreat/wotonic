#!/bin/bash

# Zotonic 新站点创建脚本
# 使用方法: ./create-site.sh <站点名> <主机名> <管理员密码>

set -e

# 检查参数
if [ $# -lt 3 ]; then
    echo "使用方法: $0 <站点名> <主机名> <管理员密码> [骨架类型]"
    echo ""
    echo "示例: $0 myblog myblog.local admin123 blog"
    echo ""
    echo "骨架类型可选: blog, empty, nodb (默认: blog)"
    exit 1
fi

SITE_NAME=$1
HOSTNAME=$2
ADMIN_PASS=$3
SKELETON=${4:-blog}

echo "========================================="
echo "创建 Zotonic 站点"
echo "========================================="
echo "站点名称: $SITE_NAME"
echo "主机名: $HOSTNAME"
echo "骨架类型: $SKELETON"
echo "========================================="

# 1. 创建站点
echo ""
echo "步骤 1: 在容器中创建站点..."
docker compose exec zotonic bin/zotonic addsite \
  -s "$SKELETON" \
  -H "$HOSTNAME" \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a "$ADMIN_PASS" \
  "$SITE_NAME"

# 2. 配置 hosts 文件
echo ""
echo "步骤 2: 配置 hosts 文件..."
echo "需要 sudo 权限来修改 /etc/hosts"

# 检查是否已存在该条目
if grep -q "$HOSTNAME" /etc/hosts; then
    echo "⚠️  /etc/hosts 中已存在 $HOSTNAME 的配置"
else
    echo "127.0.0.1    $HOSTNAME" | sudo tee -a /etc/hosts > /dev/null
    echo "✅ 已添加 $HOSTNAME 到 /etc/hosts"
fi

# 3. 刷新 DNS 缓存（macOS）
echo ""
echo "步骤 3: 刷新 DNS 缓存..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
    echo "✅ DNS 缓存已刷新"
fi

# 4. 启动站点
echo ""
echo "步骤 4: 启动站点..."
docker compose exec zotonic bin/zotonic startsite "$SITE_NAME"

echo ""
echo "========================================="
echo "✅ 站点创建完成！"
echo "========================================="
echo ""
echo "访问信息："
echo "  HTTP:  http://$HOSTNAME:8000"
echo "  HTTPS: https://$HOSTNAME:8443"
echo ""
echo "登录信息："
echo "  用户名: admin"
echo "  密码: $ADMIN_PASS"
echo ""
echo "管理后台："
echo "  http://$HOSTNAME:8000/admin"
echo ""
echo "========================================="
