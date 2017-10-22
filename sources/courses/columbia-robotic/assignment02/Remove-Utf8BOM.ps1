function Remove-Utf8BOM
{
    [CmdletBinding(SupportsShouldProcess = $true)]
    PARAM(
        [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
        $File
    )
    BEGIN
    {
        $byteBuffer = New-Object System.Byte[] 3
    }
    PROCESS
    {
        $File = [System.IO.FileInfo]::new($File)
        $reader = $File.OpenRead()
        $bytesRead = $reader.Read($byteBuffer, 0, 3)
        if ($bytesRead -eq 3 -and
            $byteBuffer[0] -eq 239 -and
            $byteBuffer[1] -eq 187 -and
            $byteBuffer[2] -eq 191)
        {
            if ($PSCmdlet.ShouldProcess($File.FullName, 'Removing UTF8 BOM'))
            {
                $tempFile = [System.IO.Path]::GetTempFileName()
                $writer = [System.IO.File]::OpenWrite($tempFile)
                $reader.CopyTo($writer)
                $writer.Dispose()
                $reader.Dispose()
                Move-Item -Path $tempFile -Destination $file.FullName -Force
            }
        }
        else
        {
            $reader.Dispose()
        }
    }
}