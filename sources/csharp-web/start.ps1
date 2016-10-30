[cmdletbinding()]
param($Port = 0, [Switch]$Recompile, [Switch]$OpenBrowser, [Switch]$DontWait)

Push-Location
try
{
    cd (Split-Path $PSCommandPath -Parent)
    
    $currentDir = gl
    $publishedWebSiteFolder = [System.IO.Path]::Combine($currentDir, ".builds\_PublishedWebsites\Interview.WebApplication")
    
    Write-Verbose "Testing if the application is already compiled"
    $publishedWebSiteFolderExists = Test-Path $publishedWebSiteFolder
    
    if($publishedWebSiteFolderExists -eq $false){
        Write-Verbose "No. So starting the compilation"    
        .\build.ps1 *>&1 | % {Write-Verbose $_}
        Write-Verbose "Compilation finished"
    }
    else{
        Write-Verbose "Yes. Use -Recompile to force recompilation"
        if($Recompile.IsPresent){
            Write-Verbose "Forced recompilation started"    
            .\build.ps1 *>&1 | % {Write-Verbose $_}
            Write-Verbose "Compilation finished"
        }
    }

    function Find-FreeTcpPort{
        $listener = [System.Net.Sockets.TcpListener]::new([System.Net.IPAddress]::Loopback, $Port)
        $listener.Start()
        $port = $listener.LocalEndpoint.Port;
        $listener.Stop()
        $port
    }
    
    function Start-IISExpress($Path, [Switch]$Detached){
        $port = Find-FreeTcpPort
        $processName = 'C:\Program Files\IIS Express\iisexpress.exe'

        $result = Test-Path $processName
        if($result -eq $false){
            Write-Error "IISExpress not found"
        }

        $arguments = "/path:$Path /port:$port"
        if($Detached){
            Write-Verbose "Starting $processName detached"
            Write-Verbose "with arguments [$arguments]"
            Write-Verbose "the IISExpress window is open so one can easily finish the process"            
            Start-Process $processName -ArgumentList $arguments -PassThru
        }
        $port
    }
    
    Write-Verbose "Starting application at $publishedWebSiteFolder"
    
    $result = Start-IISExpress $publishedWebSiteFolder -Detached
    $process = $result[0]
    $port = $result[1]
    
    if($OpenBrowser){
        Write-Verbose "Just giving some time so the server can start"
        Start-Sleep -Seconds 1
        Write-Verbose "1 more second..."
        Start-Sleep -Seconds 1
        Write-Verbose "Automatically opening your browser to http://localhost:$port"        
        
        start "http://localhost:$port"
    }
    else {
        Write-Verbose "Please, manually open your browser to http://localhost:$port"        
    }
    
    Write-Verbose "Waiting IISExpress proccess to finish"
    
    if($DontWait.IsPresent -eq $false){
        $process.WaitForExit()
    }
    
    Write-Verbose "Finished"
}
catch
{
}
finally
{
    Pop-Location
}

 Add-Type @"
        using System;
        using System.Runtime.InteropServices;
        public class SFW {
            [DllImport("user32.dll")]
            [return: MarshalAs(UnmanagedType.Bool)]
            public static extern bool SetForegroundWindow(IntPtr hWnd);
        }
"@

function Stop-IISExpress{
    iex '[SFW]::SetForegroundWindow($process.MainWindowHandle)'
    [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Drawing") 
    [void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") 
    [System.Windows.Forms.SendKeys]::SendWait("Q")
}