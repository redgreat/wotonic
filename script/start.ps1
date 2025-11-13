param(
    [string]$Hostname,
    [int]$Port,
    [string]$IconvInclude,
    [string]$IconvLib
)

function Invoke-VsDevEnv {
    param([string]$Arch)
    $candidates = @(
        "C:\Program Files\Microsoft Visual Studio\2022\BuildTools\Common7\Tools\VsDevCmd.bat",
        "C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat",
        "C:\Program Files\Microsoft Visual Studio\2022\Professional\Common7\Tools\VsDevCmd.bat",
        "C:\Program Files\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\VsDevCmd.bat"
    )
    $vs = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1
    if ($vs) {
        $cmd = "`"$vs`" -arch=$Arch && set"
        $vars = cmd /c $cmd
        foreach ($line in $vars) {
            if ($line -match '^(.*?)=(.*)$') { Set-Item -Path "Env:$($matches[1])" -Value $matches[2] }
        }
        return $true
    }
    return $false
}

function Get-ListenPort {
    $defaultPort = 8000
    $yamlPath = Join-Path $env:USERPROFILE ".zotonic/1/config.d/ports.yml"
    if (Test-Path $yamlPath) {
        $line = Select-String -Path $yamlPath -Pattern "listen_port:\s*(\d+)" | Select-Object -First 1
        if ($line -and $line.Matches.Count -gt 0) {
            return [int]$line.Matches[0].Groups[1].Value
        }
    }
    return $defaultPort
}

function Get-Hostname {
    param([string]$Override)
    if ($Override) { return $Override }
    return [System.Net.Dns]::GetHostName()
}

function Get-CodePaths {
    $paths = @()
    $lib = Join-Path (Get-Location) "_build/default/lib"
    if (Test-Path $lib) {
        Get-ChildItem -Path $lib -Directory | ForEach-Object {
            $ebin = Join-Path $_.FullName "ebin"
            if (Test-Path $ebin) { $paths += $ebin }
        }
    }
    $chk = Join-Path (Get-Location) "_build/default/checkouts"
    if (Test-Path $chk) {
        Get-ChildItem -Path $chk -Directory | ForEach-Object {
            $ebin = Join-Path $_.FullName "ebin"
            if (Test-Path $ebin) { $paths += $ebin }
        }
    }
    return ($paths | ForEach-Object { "-pa `"$_`"" }) -join " "
}

$Port = if ($Port) { $Port } else { Get-ListenPort }
$Hostname = Get-Hostname -Override $Hostname

Write-Host "编译并启动服务..." -ForegroundColor Cyan

try { chcp 65001 | Out-Null } catch {}
try { [Console]::OutputEncoding = [System.Text.Encoding]::UTF8 } catch {}
try { [Console]::InputEncoding = [System.Text.Encoding]::UTF8 } catch {}

$ok = Invoke-VsDevEnv -Arch "x64"
if (-not $ok -and -not (Get-Command cl.exe -ErrorAction SilentlyContinue)) {
    Write-Host "未检测到cl.exe，请安装并加载VS 2022 C++构建工具后重试。" -ForegroundColor Red
    exit 1
}

# vcpkg libiconv include/lib 环境注入（若已安装）
if (-not $env:VCPKG_ROOT -and (Test-Path "C:\vcpkg")) { $env:VCPKG_ROOT = "C:\vcpkg" }
if (-not $env:VCPKG_ROOT -and (Test-Path "D:\github\vcpkg")) { $env:VCPKG_ROOT = "D:\github\vcpkg" }
if (-not $env:VCPKG_ROOT) {
    $vc = Get-Command vcpkg -ErrorAction SilentlyContinue
    if ($vc) { $env:VCPKG_ROOT = Split-Path $vc.Source -Parent }
}
if ($IconvInclude) { if (Test-Path $IconvInclude) { $env:INCLUDE = "$($env:INCLUDE);$IconvInclude" } }
if ($IconvLib) { if (Test-Path $IconvLib) { $env:LIB = "$($env:LIB);$IconvLib" } }
if ($env:VCPKG_ROOT) {
    $inc = Join-Path $env:VCPKG_ROOT "installed/x64-windows/include"
    $lib = Join-Path $env:VCPKG_ROOT "installed/x64-windows/lib"
    if ((Test-Path $inc) -and (-not $IconvInclude)) { $env:INCLUDE = "$($env:INCLUDE);$inc" }
    if ((Test-Path $lib) -and (-not $IconvLib)) { $env:LIB = "$($env:LIB);$lib" }
}

$incEcho = if ($env:INCLUDE) { $env:INCLUDE } else { "" }
$libEcho = if ($env:LIB) { $env:LIB } else { "" }
Write-Host ("INCLUDE="+$incEcho) -ForegroundColor DarkGray
Write-Host ("LIB="+$libEcho) -ForegroundColor DarkGray

$iconvHeaderFound = $false
if ($env:VCPKG_ROOT) {
    $inc1 = Join-Path $env:VCPKG_ROOT "installed/x64-windows/include/iconv.h"
    $inc2 = Join-Path $env:VCPKG_ROOT "packages/libiconv_x64-windows/include/iconv.h"
    if (Test-Path $inc1) { $iconvHeaderFound = $true }
    elseif (Test-Path $inc2) { $iconvHeaderFound = $true; $env:INCLUDE = "$($env:INCLUDE);$(Split-Path $inc2 -Parent)" }
}
if (-not $iconvHeaderFound -and $IconvInclude) {
    $h3 = Join-Path $IconvInclude "iconv.h"
    if (Test-Path $h3) { $iconvHeaderFound = $true }
}
if (-not $iconvHeaderFound) {
    $cand = @(
        "C:\vcpkg\installed\x64-windows\include\iconv.h",
        "C:\vcpkg\packages\libiconv_x64-windows\include\iconv.h",
        "${env:LOCALAPPDATA}\vcpkg\installed\x64-windows\include\iconv.h",
        "${env:ProgramFiles}\vcpkg\installed\x64-windows\include\iconv.h"
    ) | Where-Object { Test-Path $_ } | Select-Object -First 1
    if ($cand) {
        $iconvHeaderFound = $true
        $env:INCLUDE = "$($env:INCLUDE);$(Split-Path $cand -Parent)"
    }
}

$iconvHeaderFound2 = $false
if ($env:INCLUDE) {
    foreach ($p in $env:INCLUDE.Split(';')) {
        if ([string]::IsNullOrWhiteSpace($p)) { continue }
        $h = Join-Path $p "iconv.h"
        if (Test-Path $h) { $iconvHeaderFound2 = $true; break }
    }
}
if (-not $iconvHeaderFound2) {
    Write-Host "未找到 iconv.h，继续尝试编译。" -ForegroundColor Yellow
}

rebar3 compile | Out-Host

Write-Host "启动 Zotonic 节点..." -ForegroundColor Cyan
Write-Host "URL: http://${Hostname}:${Port}/" -ForegroundColor Green
Write-Host "如需停止，请按 Ctrl+C 结束当前会话。" -ForegroundColor Yellow

$env:ZOTONIC_LISTEN_PORT = "$Port"
$env:ZOTONIC_IP = "$Hostname"
$env:ZOTONIC_SSL_LISTEN_PORT = "none"

$codePaths = Get-CodePaths
$erlArgs = "$codePaths -boot start_sasl -noshell -eval 'zotonic:start(), zotonic:await_startup(), timer:sleep(infinity).'"
Write-Host "erl $erlArgs" -ForegroundColor DarkGray
& erl $codePaths -boot start_sasl -noshell -s zotonic start
