function Start-Process
{
    [CmdletBinding()]
    param([Parameter(Position=0)]$Path,[Parameter(Position=1)]$Parameters,[switch]$Attach)

    $commandPath = command $path

    $pinfo = New-Object System.Diagnostics.ProcessStartInfo
    $pinfo.WorkingDirectory = (gl).Path
    $pinfo.FileName = $commandPath.Source
    $pinfo.RedirectStandardError = $true
    $pinfo.RedirectStandardOutput = $true
    $pinfo.UseShellExecute = $false
    $pinfo.Arguments = $Parameters

    $p = New-Object System.Diagnostics.Process
    $p.StartInfo = $pinfo

    try
    {
        $p.Start() | Out-Null

        if($Attach.IsPresent)
        {
            $p.WaitForExit()
        }

        $p.StandardOutput.ReadToEnd()
        #$p.StandardError.ReadToEnd() | % {Write-Verbose $_}
    }
    catch
    {
        Write-Error $_
    }
}