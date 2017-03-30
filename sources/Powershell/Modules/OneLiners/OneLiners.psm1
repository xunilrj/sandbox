filter ConvertFrom-Xml{
    process{
        [xml]$_
    }
}

function %% {
    param([Parameter(Position=0)]$Name,[Parameter(ValueFromPipeline=$true)]$Item)
    begin{
        $properties = $Name.Split('.')
        $properties | % {Write-Verbose $_}
    }
    process{
        $currentValue = $_
        $properties | % {
            $currentValue = $currentValue | % $_
        } | Out-Null
        $currentValue
    }
}