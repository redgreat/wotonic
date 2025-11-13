param(
    [string]$Hostname,
    [int]$Port
)

Write-Host "准备部署..." -ForegroundColor Cyan
rebar3 as prod release | Out-Host

$Port = if ($Port) { $Port } else { 8000 }
$Hostname = if ($Hostname) { $Hostname } else { [System.Net.Dns]::GetHostName() }

Write-Host "部署完成。访问地址：http://$Hostname:$Port/" -ForegroundColor Green

