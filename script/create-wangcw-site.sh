#!/bin/bash

# 简化版站点创建脚本 - 不需要 sudo

echo "========================================="
echo "创建 Zotonic 站点: wangcw"
echo "========================================="
echo ""

# 1. 检查 hosts 文件
echo "步骤 1: 检查 hosts 配置..."
if grep -q "wangcw.local" /etc/hosts 2>/dev/null; then
    echo "✅ wangcw.local 已在 /etc/hosts 中"
else
    echo "⚠️  wangcw.local 未在 /etc/hosts 中"
    echo ""
    echo "请手动执行以下命令（需要输入密码）："
    echo "  echo \"127.0.0.1    wangcw.local\" | sudo tee -a /etc/hosts"
    echo ""
    read -p "完成后按 Enter 继续，或按 Ctrl+C 取消..."
fi

# 2. 确保 apps_user 目录存在
echo ""
echo "步骤 2: 检查 apps_user 目录..."
if [ ! -d "apps_user" ]; then
    echo "创建 apps_user 目录..."
    mkdir -p apps_user
    touch apps_user/.gitkeep
fi
echo "✅ apps_user 目录存在"

# 3. 设置容器内权限
echo ""
echo "步骤 3: 设置目录权限..."
docker compose exec zotonic chown -R zotonic:zotonic /opt/zotonic/apps_user 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✅ 权限设置完成"
else
    echo "⚠️  权限设置失败，但可能不影响创建"
fi

# 4. 检查数据库
echo ""
echo "步骤 4: 检查数据库..."
SCHEMA_EXISTS=$(docker compose exec postgres psql -U zotonic -d zotonic -t -c "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'wangcw';" 2>/dev/null | tr -d ' \n')

if [ "$SCHEMA_EXISTS" = "wangcw" ]; then
    echo "⚠️  发现旧的 wangcw schema"
    read -p "是否删除旧数据并重新创建？(y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "正在删除旧数据..."
        docker compose exec postgres psql -U zotonic -d zotonic -c "DROP SCHEMA IF EXISTS wangcw CASCADE;" >/dev/null 2>&1
        echo "✅ 旧数据已清理"
    else
        echo "❌ 取消操作"
        exit 1
    fi
else
    echo "✅ 数据库干净，可以创建新站点"
fi

# 5. 创建站点
echo ""
echo "步骤 5: 创建站点..."
echo "这可能需要 30-60 秒，请耐心等待..."
echo ""

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
    echo ""
    echo "✅ 站点创建成功！"
    
    # 6. 启动站点
    echo ""
    echo "步骤 6: 启动站点..."
    sleep 2
    docker compose exec zotonic bin/zotonic startsite wangcw
    
    if [ $? -eq 0 ]; then
        echo ""
        echo "========================================="
        echo "✅ 完成！站点已成功创建并启动"
        echo "========================================="
        echo ""
        echo "📝 访问信息："
        echo ""
        echo "前台:"
        echo "  http://wangcw.local:8000"
        echo "  https://wangcw.local:8443"
        echo ""
        echo "管理后台:"
        echo "  http://wangcw.local:8000/admin"
        echo "  用户名: admin"
        echo "  密码: admin123"
        echo ""
        echo "Zotonic 状态页面:"
        echo "  https://localhost:8443"
        echo "  用户名: wwwadmin"
        echo "  密码: qV1o1VUA5mkg6jNm"
        echo ""
        echo "========================================="
    else
        echo "⚠️  站点启动可能失败，请检查日志"
        echo "运行: docker compose logs -f zotonic"
    fi
else
    echo ""
    echo "❌ 站点创建失败"
    echo ""
    echo "常见问题排查："
    echo "1. 检查容器状态: docker compose ps"
    echo "2. 查看日志: docker compose logs -f zotonic"
    echo "3. 检查 hosts 文件: cat /etc/hosts | grep wangcw"
    echo "4. 检查数据库连接: docker compose exec postgres psql -U zotonic -d zotonic -c 'SELECT 1;'"
    echo ""
    exit 1
fi
