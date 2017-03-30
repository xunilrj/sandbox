filter preparetojson
{
    param([Parameter(Mandatory=$true, ValueFromPipeline=$true)]$Objects)

    $Objects | % {
        $Object = $_
        if($Object -is [System.Collections.Hashtable]){
            preparetojson (New-Object PSCustomObject -Property $Object )
        }
        else {
            if($Object -is [System.String]){
                return $Object
            }

            $newObjectProperties = @{}

            foreach ($property in $Object.psobject.properties)
            {
                $value = $property.Value

                if($property.TypeNameOfValue -eq "System.Collections.Hashtable")
                {
                    $value = preparetojson -Object $property.Value
                }
                elseif ($property.TypeNameOfValue -eq "System.Management.Automation.PSCustomObject")
                {
                    $value = preparetojson -Object $property.Value
                }
                elseif ($property.TypeNameOfValue -eq "System.DateTime")
                {
                    $value = $property.Value.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss.fffffffK")
                }
                elseif($property.TypeNameOfValue -eq "System.String"){
                    $value = $property.Value
                }
                elseif ($property.Value -is [System.Collections.IEnumerable])
                {
                    $l = [System.Collections.Generic.List[System.Object]]::new()
                    $property.Value | % {$l.Add((preparetojson -Object $_))}

                    $value = [System.Linq.Enumerable]::ToArray($l)
                }

                $newObjectProperties[$property.Name] = $value
            }

            New-Object -TypeName PSObject -Property $newObjectProperties
        }
    }
}

function asjson
{
    process{
        $_ | preparetojson | ConvertTo-Json -Compress -Depth 10
    }
}

function get($Url){
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($BaseUri), $Url);
    }

    $result = Invoke-RestMethod -Method Get -Uri $uri -Headers @{Authorization=$Global:authToken}

     if($result -is [System.Collections.IEnumerable]){
        foreach($item in $result) { $item }
    }else{
        $result
    }
}

function post($Url, $Body){
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($BaseUri), $Url);
    }

    $asJson = ConvertTo-Json $Body -Depth 10 -Compress
    $result = Invoke-RestMethod -Method Post -Uri $uri -Headers @{Authorization=$Global:authToken} -Body $asJson -ContentType 'application/json'

    if($result -is [System.Collections.IEnumerable]){
        foreach($item in $result) { $item }
    }else{
        $result
    }
}

function postMultipart($Url, $Body){
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($BaseUri), $Url);
    }

    $result = Invoke-RestMethod -Method Post -Uri $uri -Headers @{Authorization=$Global:authToken} -Body "------------287032381131322
Content-Disposition: form-data; name=""simplefile""; filename=""simplefile.txt""
Content-Type: text/plain

$Body
------------287032381131322--" -ContentType 'multipart/form-data; boundary=----------287032381131322' 

    if($result -is [System.Collections.IEnumerable]){
        foreach($item in $result) { $item }
    }else{
        $result
    }
}

function _post($Url, $Body){
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($BaseUri), $Url);
    }

    $asJson = ConvertTo-Json $Body -Depth 10 -Compress
    Invoke-WebRequest -Method Post -Uri $uri -Headers @{Authorization=$Global:authToken} -Body $asJson -ContentType 'application/json' -UseBasicParsing
}

function _delete($Url){
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    try
    {
        Invoke-WebRequest -Method Delete -Uri $uri -Headers @{Authorization=$Global:authToken} -UseBasicParsing
    }
    catch
    {
        $_.Exception.Response
    }
}

function Invoke-Get
{
    [CmdletBinding()]
    param([Parameter(Position = 0)]$Url,[Parameter(Position = 1)][Hashtable]$Headers)

    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("GET", $uri)
    if([System.String]::IsNullOrEmpty($Global:authToken) -eq $false){
        $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    }

    if($Headers -ne $null){
        $Headers.Keys |% {
            $val = $Headers[$_]
            $request.Headers.TryAddWithoutValidation($_, $val)
        }
    }

    $request.Headers.Host = $request.RequestUri.Host
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function Invoke-Post
{
    [CmdletBinding()]
    param($Url, [Hashtable]$Headers, $Body)

    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    if($Body -is [string]){
        $asJson = $Body
    }else{
        $asJson = $Body | asjson
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("POST", $uri)
    if($Global:authToken -ne $null){
        $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    }
    $request.Content = [System.Net.Http.StringContent]::new($asJson)
    $request.Content.Headers.ContentType = [System.Net.Http.Headers.MediaTypeHeaderValue]::new("application/json")
    $request.Content.Headers.ContentLength = $asJson.Length

    if($Headers -ne $null){
        $Headers.Keys |% {
            $val = $Headers[$_]
            $request.Headers.TryAddWithoutValidation($_, $val)
        }
    }

    $request.Headers.Host = $request.RequestUri.Host
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function Invoke-PostFile
{
    [CmdletBinding()]
    param($Url, [Hashtable]$Headers, $Path)

    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("POST", $uri)
    if($Global:authToken -ne $null){
        $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    }

    $pathUri = [System.Uri]::new($Path, [System.UriKind]::RelativeOrAbsolute)
    if($pathUri.IsAbsoluteUri -eq $false){        
        $pathLocationBase = (Get-Location).Path
        $pathUri = [System.IO.Path]::Combine($pathLocationBase, $Path)
        Write-Verbose "[$Path] is a relative path. Searching file at $($pathUri)"
    }

    $pathName = [System.IO.FileInfo]::new($pathUri)

    $stream = [System.IO.MemoryStream]::new()
    $writer = [System.IO.StreamWriter]::new($stream)
    $writer.WriteLine("--INVOKEPOSTFILEBOUNDARY")
    $writer.WriteLine("Content-Disposition: form-data; name=""file""; filename=""$($pathName.Name)""")
    $writer.WriteLine("Content-Type: application/pdf")
    $writer.WriteLine("Content-Length: $([System.IO.FileInfo]::new($pathUri).Length)")    
    $writer.WriteLine()
    $writer.Flush()
    [System.IO.File]::OpenRead($pathUri).CopyTo($stream)
    $writer.WriteLine()
    $writer.Flush()
    $writer.WriteLine("--INVOKEPOSTFILEBOUNDARY--")
    $writer.Flush()

    $request.Content = [System.Net.Http.StreamContent]::new($stream)
    $request.Content.Headers.TryAddWithoutValidation("Content-Type", "multipart/form-data; boundary=INVOKEPOSTFILEBOUNDARY")
    $request.Content.Headers.ContentLength = $stream.Position
    $stream.Position = 0

    if($Headers -ne $null){
        $Headers.Keys |% {
            $val = $Headers[$_]
            $request.Headers.TryAddWithoutValidation($_, $val)
        }
    }

    $request.Headers.Host = $request.RequestUri.Host
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function Invoke-Put
{
    [CmdletBinding()]
    param($Url, [Hashtable]$Headers, $Body)
    
    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    if($Body -is [string]){
        $asJson = $Body
    }else{
        $asJson = $Body | asjson
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("PUT", $uri)
    if($Global:authToken -ne $null){
        $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    }
    $request.Content = [System.Net.Http.StringContent]::new($asJson)
    $request.Content.Headers.ContentType = [System.Net.Http.Headers.MediaTypeHeaderValue]::new("application/json")
    $request.Content.Headers.ContentLength = $asJson.Length

    if($Headers -ne $null){
        $Headers.Keys |% {
            $val = $Headers[$_]
            $request.Headers.TryAddWithoutValidation($_, $val)
        }
    }

    $request.Headers.Host = $request.RequestUri.Host
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function Invoke-Delete
{
    [CmdletBinding()]
    param($Url, [Hashtable]$Headers)

    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("DELETE", $uri)
    if($Global:authToken -ne $null){
        $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    }

    if($Headers -ne $null){
        $Headers.Keys |% {
            $val = $Headers[$_]
            $request.Headers.TryAddWithoutValidation($_, $val)
        }
    }

    $request.Content = [System.Net.Http.ByteArrayContent]::new(@())
    $request.Content.Headers.ContentLength = 0

    $request.Headers.Host = $request.RequestUri.Host
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function Invoke-Patch
{
    [CmdletBinding()]
    param($Url, $Body)

    $Verbose = [switch]$PSBoundParameters['Verbose']

    Write-Verbose "Sending HTTP Request..."
    $uri = [System.Uri]::new($Url, [System.UriKind]::RelativeOrAbsolute)

    if($uri.IsAbsoluteUri -eq $false){
        $uri = [System.Uri]::new([System.Uri]::new($Global:BaseUri), $Url);
    }

    if($Body -is [string]){
        $asJson = $Body.ToString()
    }
    else{
        $asJson = $Body | asjson
    }

    Add-Type -Assembly System.Net.Http        
    $request = [System.Net.Http.HttpRequestMessage]::new("PATCH", $uri)
    $request.Headers.Authorization = [System.Net.Http.Headers.AuthenticationHeaderValue]::Parse($Global:authToken)
    $request.Headers.Connection.Add("Keep-Alive")
    $request.Headers.Host = $request.RequestUri.Host
    $request.Content = [System.Net.Http.StringContent]::new($asJson)
    $request.Content.Headers.ContentType = [System.Net.Http.Headers.MediaTypeHeaderValue]::new("application/json")
    $request.Content.Headers.ContentLength = $asJson.Length
    
    $request | Format-Message -Stream Verbose
    
    $clientHandler = [System.Net.Http.HttpClientHandler]::new()
    $clientHandler.UseCookies = $false
    $client = [System.Net.Http.HttpClient]::new($clientHandler)
    $response = $client.SendAsync($request).Result

    $response.RequestMessage = $request
    $response | Format-Message -Stream Verbose
    $response
}

function ConvertTo-Object
{
     [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Item)
    process{
        if($_ -is [System.Net.Http.HttpResponseMessage]){
            $r = [System.Net.Http.HttpResponseMessage]$_
            (ConvertFrom-Json $r.Content.ReadAsStringAsync().Result) | preparetojson
        }
    }
}

function Format-Message{
    [CmdletBinding()]
    param([ValidateSet("Host",“Output”,”Verbose”)]$Stream = "Output",[Parameter(ValueFromPipeline=$true)]$Item)
    process{
        if($Stream -eq "Host") { $output = {param($_) Write-Host $_} }
        if($Stream -eq "Output") { $output = {param($_) Write-Output $_} }
        if($Stream -eq "Verbose") { $output = {param($_) Write-Verbose $_} }
        if($_ -is [System.Net.Http.HttpResponseMessage]){
            $current = [System.Net.Http.HttpResponseMessage]$_
            $output.Invoke("HTTP/1.1 $([int]$current.StatusCode) $($current.ReasonPhrase)")
            $_.Headers | %{
                $output.Invoke("$($_.Key): $($_.Value)")
            }
            if($current.Content -ne $null){
                $current.Content.Headers | %{
                    $output.Invoke("$($_.Key): $($_.Value)")
                }
            }
            $output.Invoke("")
            $output.Invoke($current.Content.ReadAsStringAsync().Result)
        }
        elseif($_ -is [System.Net.Http.HttpRequestMessage]){
            $current = [System.Net.Http.HttpRequestMessage]$_
            $output.Invoke("$($current.Method.ToString()) $($current.RequestUri.ToString()) HTTP/1.1")
            $current.Headers | %{
                $output.Invoke("$($_.Key): $($_.Value)")
            }
            if($current.Content -ne $null){
                $current.Content.Headers | %{
                    $output.Invoke("$($_.Key): $($_.Value)")
                }
            }
            $output.Invoke("")
            if($current.Content -ne $null){
                $output.Invoke($current.Content.ReadAsStringAsync().Result)
            }
        }
    }
}

function asUTC($date){
    if($date -is [System.String]){
        $date = [System.Xml.XmlConvert]::ToDateTime($date, [System.Xml.XmlDateTimeSerializationMode]::RoundtripKind)
    }
    
    if($date -is [System.DateTime]){
        if($date.Kind -eq [System.DateTimeKind]::Unspecified){
            [System.Xml.XmlConvert]::ToDateTime($date.ToString("o"), [System.Xml.XmlDateTimeSerializationMode]::Utc)
        }
        else{
            $date.ToUniversalTime()
        }
    }
}

function Enable-Alias()
{
    [CmdletBinding()]
    param($Scope="Global")

    Set-Alias get "Invoke-$($PSCmdlet.MyInvocation.MyCommand.Module.Prefix)Get" -Scope $Scope -Force
    Set-Alias patch "Invoke-$($PSCmdlet.MyInvocation.MyCommand.Module.Prefix)Patch" -Scope $Scope -Force
    Set-Alias fmsg "Format-$($PSCmdlet.MyInvocation.MyCommand.Module.Prefix)Message" -Scope $Scope -Force
    Set-Alias asUTC "$($PSCmdlet.MyInvocation.MyCommand.Module.Prefix)asUTC" -Scope $Scope -Force
}

Export-ModuleMember -Function Invoke-Get, Invoke-Post, Invoke-Put, Invoke-PostFile, Invoke-Delete, Invoke-Patch, ConvertTo-Object, Format-Message, Enable-Alias, asUtc