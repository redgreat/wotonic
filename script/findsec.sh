
docker compose exec zotonic bin/zotonic status

docker compose exec zotonic ls -la docker-data/config/

docker compose exec zotonic cat docker-data/config/zotonic.config


# 方法1：从宿主机查看
docker compose exec zotonic cat docker-data/config/zotonic.config | grep password

# 方法2：进入容器查看
docker compose exec zotonic bash
cat docker-data/config/zotonic.config | grep password



# 查看所有配置文件
docker compose exec zotonic ls -la docker-data/config/

# 查看 Zotonic 日志
docker compose logs -f zotonic

# 进入容器
docker compose exec zotonic bash

