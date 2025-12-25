#!/bin/bash

# 完整修复并创建 wangcw 站点的脚本

echo "========================================="
echo "修复并创建 Zotonic 站点"
echo "========================================="
echo ""

# 1. 添加 hosts 条目
echo "步骤 1: 添加 wangcw.local 到 /etc/hosts"
if grep -q "wangcw.local" /etc/hosts 2>/dev/null; then
    echo "✅ wangcw.local 已存在于 /etc/hosts"
else
    echo "需要 sudo 权限添加 hosts..."
    echo "127.0.0.1    wangcw.local" | sudo tee -a /etc/hosts
    if [ $? -eq 0 ]; then
        echo "✅ 已添加 wangcw.local 到 /etc/hosts"
    else
        echo "❌ 添加 hosts 失败"
        exit 1
    fi
fi

# 2. 刷新 DNS 缓存（macOS）
echo ""
echo "步骤 2: 刷新 DNS 缓存..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    sudo dscacheutil -flushcache 2>/dev/null
    sudo killall -HUP mDNSResponder 2>/dev/null
    echo "✅ DNS 缓存已刷新"
fi

# 3. 确保 apps_user 目录存在
echo ""
echo "步骤 3: 检查 apps_user 目录..."
if [ ! -d "apps_user" ]; then
    echo "创建 apps_user 目录..."
    mkdir -p apps_user
    touch apps_user/.gitkeep
fi

# 设置权限
docker compose exec zotonic chown -R zotonic:zotonic /opt/zotonic/apps_user 2>/dev/null
echo "✅ apps_user 目录准备完成"

# 4. 检查并清理数据库中的旧 schema
echo ""
echo "步骤 4: 检查数据库..."
SCHEMA_EXISTS=$(docker compose exec postgres psql -U zotonic -d zotonic -t -c "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'wangcw';" 2>/dev/null | tr -d ' \n')

if [ "$SCHEMA_EXISTS" = "wangcw" ]; then
    echo "⚠️  发现旧的 wangcw schema，正在删除..."
    docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;" >/dev/null 2>&1
    echo "✅ 旧数据已清理"
else
    echo "✅ 数据库干净"
fi

# 5. 创建站点
echo ""
echo "步骤 5: 创建 Zotonic 站点..."
docker compose exec zotonic bin/zotonic addsite \
  -s blog \
  -H wangcw.local \
  -h postgres \
  -u zotonic \
  -P zotonic \
  -d zotonic \
  -a admin123 \
  wangcw

if [ $? -eq 0 ]; then
    echo "✅ 站点创建成功！"
    
    # 6. 启动站点
    echo ""
    echo "步骤 6: 启动站点..."
    sleep 2
    docker compose exec zotonic bin/zotonic startsite wangcw
    
    echo ""
    echo "========================================="
    echo "✅ 完成！"
    echo "========================================="
    echo ""
    echo "访问信息："
    echo "  HTTP:  http://wangcw.local:8000"
    echo "  HTTPS: https://wangcw.local:8443"
    echo ""
    echo "管理后台："
    echo "  http://wangcw.local:8000/admin"
    echo "  用户名: admin"
    echo "  密码: admin123"
    echo ""
    echo "Zotonic 状态页面："
    echo "  https://localhost:8443"
    echo "  用户名: wwwadmin"
    echo "  密码: qV1o1VUA5mkg6jNm"
    echo ""
else
    echo "❌ 站点创建失败"
    echo ""
    echo "请检查："
    echo "1. Docker 容器是否正常运行: docker compose ps"
    echo "2. 查看日志: docker compose logs -f zotonic"
    echo "3. 检查 apps_user 目录权限"
    exit 1
fi
