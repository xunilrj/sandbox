function ConvertFrom-Xml
{
    param([Parameter(ValueFromPipeline=$true)]$Item)
    process{
        if($Item -is [System.IO.FileInfo]){
            [xml][System.IO.File]::ReadAllText($Item.FullName)
        }else{
            [xml]$Item 
        }
    }
}

Set-Alias xml ConvertFrom-xml

function %%% {
    param([Parameter(Position=0)]$Path,[Parameter(Position=1)][string[]]$Name,[Parameter(ValueFromPipeline=$true)]$Item)
    begin{
        $splittedProperties = [System.Collections.ArrayList]::new()
        if($Path -is [string]){
            $properties = $Path.Split('.')
            $properties | % {Write-Verbose $_}
            $splittedProperties.Add($properties) | Out-Null
        }else
        {
            $Path | % {
                $properties = $_.Split('.')
                $properties | % {Write-Verbose $_}
                $splittedProperties.Add($properties) | Out-Null
            }
        }
        
    }
    process{
        $i=0  
        $currentPipeValue = $_
        $result = New-Object PSCustomObject
        $splittedProperties | % {
            
            $currentSplitProperties = $_
            $currentValue = $currentPipeValue
            $_ | % { $currentValue = ($currentValue | % $_)} | Out-Null
            #$currentValue

            if($Name -eq $null){
                $currentValue
            }
            else
            {
               $result = $result | Add-Member -Name ($Name[$i]) -MemberType NoteProperty -Value $currentValue -PassThru
            }
            $i++
        }

        if($Name -ne $null){
            Write-Output $result -NoEnumerate
        }
    }
}

function ??? {
    [CmdletBinding()]
    param([Parameter(Mandatory=$true,Position=0)][Alias("e")]$regexp, [Parameter()][Alias("v")][switch]$NotMatch, [Parameter(ValueFromPipeline = $true)]$PSItem)
    begin{
        $Regex = [System.Text.RegularExpressions.Regex]::new($regexp,"ExplicitCapture")
    }
    process{
        $result = $Regex.Match($Regex)
        if($result.Success -eq $true){
            $newobj = New-Object PSCustomObject
            $Regex.GetGroupNames() | ? {$_ -notmatch "\d+"} | % { 
                $newobj = $newobj | Add-Member -Name $_ -Value ($result[$_].Value) -MemberType NoteProperty
            }
            $newobj 
        }
    }
}

function ...
{
    param([Parameter(Position=0)]$Expression, [Parameter(Position = 1)]$Count = 999, [Parameter(Position = 2)]$SleepMs = 1000)
    do
    {
        try{
            $result = $Expression.Invoke()
            $result = $result[0]
        }catch
        {
            $result = $null
        }

        if($result -eq $null)
        {
            Start-Sleep -Milliseconds $SleepMs
        }
        --$Count
    }while($result -eq $null -and $Count -gt 0)
    $result
}

function union {
    [CmdletBinding()]
    param([Parameter(Position=0)]$List1, [Parameter(Position=1)]$List2, [Parameter(ValueFromPipeline=$true)]$Item)
    process{
        $_
    }
    end{
        if($List1 -ne $null){
            Write-Output $List1
        }
        if($List2 -ne $null){
            Write-Output $List2
        }
    }
}

function trim
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$PSItem)
    process{
        if([System.String]::IsNullOrEmpty($PSItem) -eq $false){
            $PSItem.Trim()
        }else{
            $PSItem
        }
    }
}

function Out-TxtEditor
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Item)
    begin{
        $tempFile = [System.IO.Path]::GetTempFileName() + ".txt"
        $stream = [System.IO.StreamWriter][System.IO.File]::CreateText($tempFile)
    }
    process{
        $asstring = $_
        if($asstring -isnot [string]){
            $asstring = $_.ToString()
        }
        $stream.Write($asstring)        
    }
    end{
        $stream.Dispose()
        start $tempFile
    }
}

function Enable-Streams([switch]$Verbose)
{
	$VerbosePreference = "SilentlyContinue";
	if($Verbose.IsPresent){
		$VerbosePreference = "Continue"
	}

}

function echo
{
    [CmdletBinding(DefaultParameterSetName="Normal")]
    param([Parameter(Position = 0, ParameterSetName="Normal")]$Text, [Parameter(ParameterSetName="Normal")][switch]$Eval,[Parameter(Position=0, ParameterSetName="Interactive")]$End = [System.String]::Empty,[Parameter(ParameterSetName="Interactive")][Alias("i")][switch]$Interactive,[switch]$NoEval)
    process{
        if($Eval.IsPresent){
            $Text = try{iex $Text}catch{$Text}
        }
        Write-Output $Text
    }
    end{
        if($Interactive.IsPresent){
            $eval = $NoEval.ToBool() -eq $false
            while($true){
                $str = Read-Host
                if($eval){
                    $str = try{iex $str}catch{$str}
                }

                if($str -eq $End){
                    break;
                }
                
                Write-Output ($str) -NoEnumerate
            }
        }
    }
}
Remove-Item Alias:\echo -EA SilentlyContinue

function split
{
    param([Parameter(Position=0)]$Separator,[Parameter(ValueFromPipeline=$true)]$Item)
    process{
        Write-Output ($Item -split $Separator)
    }
}