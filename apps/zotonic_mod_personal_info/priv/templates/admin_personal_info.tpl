{% extends "admin_base.tpl" %}

{% block title %}{_ 个人信息 _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ 个人信息管理 _}</h2>
    <p>{_ 查看与编辑个人基本信息 _}</p>
</div>

<div class="widget">
    <div class="widget-content">
        <dl class="dl-horizontal">
            <dt>{_ 用户ID _}</dt>
            <dd>{{ user_id }}</dd>
        </dl>
        {# 可扩展字段：按配置动态渲染 #}
    </div>
</div>
{% endblock %}

