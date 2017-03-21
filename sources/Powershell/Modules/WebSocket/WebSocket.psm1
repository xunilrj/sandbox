$Sessions = [System.Collections.Generic.Dictionary[Guid,object]]::new()
$SessionStack = [System.Collections.Stack]::new()

function Start-Session
{
    [CmdletBinding()]
    param([Parameter(Position=0)]$Uri, [switch]$Stack)

    try
    {
        $client = [System.Net.WebSockets.ClientWebSocket]::new();
        $client.ConnectAsync($Uri, [System.Threading.CancellationToken]::None).Wait()

        $SessionID = [System.Guid]::NewGuid()
        $Sessions.Add($SessionID, $client) | Out-Null

        if($Stack.IsPresent){
            $SessionStack.Push($SessionID)  | Out-Null
        }

        $SessionID
    }
    catch
    {
        Write-Error $_.Exception.ToString()
    }
}

function Send-Session
{
    [CmdletBinding()]
    param([Parameter(Position = 0, Mandatory = $true)]$Text, $Session)

    if($Session -eq $null)
    {
        $Session = $SessionStack.Peek()
    }

    $client = [System.Net.WebSockets.ClientWebSocket]$Sessions[$Session]

    $bytes = [System.Text.Encoding]::UTF8.GetBytes($Text)
    $arseg = [System.ArraySegment[byte]]::new($bytes)
    try
    {
        $client.SendAsync($arseg, "Text", $true, [System.Threading.CancellationToken]::None).Wait()
    }
    catch
    {
        Write-Error $_.Exception.ToString()
    }
}

function Receive-Session
{
    [CmdletBinding()]
    param($Session)

    if($Session -eq $null)
    {
        $Session = $SessionStack.Peek()
    }

    $client = [System.Net.WebSockets.ClientWebSocket]$Sessions[$Session]

    $PS

    $bytes = new-object byte[] 1024
    $arseg = [System.ArraySegment[byte]]::new($bytes) 
    $result = $client.ReceiveAsync($arseg, [System.Threading.CancellationToken]::None)
    $result.Wait()

    [System.Text.Encoding]::UTF8.GetString($arseg.Array, 0, $result.Result)
}

function Read-Session
{
    [CmdletBinding()]
    param($Session)

    try
    {
        
        if($Session -eq $null)
        {
            $Session = $SessionStack.Peek()
        }

        $client = [System.Net.WebSockets.ClientWebSocket]$Sessions[$Session]

        $bytes = new-object byte[] 1024
        $arseg = [System.ArraySegment[byte]]::new($bytes) 
        $builder = [System.Text.StringBuilder]::new()
        while($true)
        {
            if($client.State -eq "Open")
            {
                $result = $client.ReceiveAsync($arseg, [System.Threading.CancellationToken]::None)
                $result.Wait()

                $builder.Append([System.Text.Encoding]::UTF8.GetString($arseg.Array, 0, $result.Result.Count)) | Out-Null

                if($result.Result.EndOfMessage -and $builder.ToString().EndsWith("}"))
                {
                    $str = $builder.ToString().Replace(([char]10).ToString(),'').Replace(([char]10).ToString(),'')
                    Write-Verbose  $str
                    $str
                    $builder.Clear() | Out-Null
                }
            }
            else
            {
                break;
            }
        }
    }
    catch
    {
        Write-Error $_.Exception.ToString()
    }
}