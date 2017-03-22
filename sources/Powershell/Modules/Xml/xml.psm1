function ConvertFrom-XmlPart($xml)
{
    $hash = @{}
    $xml | Get-Member -MemberType Property | % {
            $name = $_.Name
            if ($_.Definition.StartsWith("string ")) {
                $hash.($Name) = $xml.$($Name)
            } elseif ($_.Definition.StartsWith("System.Object[] ")) {
                $obj = $xml.$($Name)
                $hash.($Name) = $($obj | %{ $_.tag }) -join "; "
            } elseif ($_.Definition.StartsWith("System.Xml")) {
                $obj = $xml.$($Name)
                $hash.($Name) = @{}
                if ($obj.HasAttributes) {
                    $attrName = $obj.Attributes | Select-Object -First 1 | % { $_.Name }
                    if ($attrName -eq "tag") {
                        $hash.($Name) = $($obj | % { $_.tag }) -join "; "
                    } else {
                        $hash.($Name) = ConvertFrom-XmlPart $obj
                    }
                }
                if ($obj.HasChildNodes) {
                    $obj.ChildNodes | % { $hash.($Name).($_.Name) = ConvertFrom-XmlPart $($obj.$($_.Name)) }
                }
            }
        }
    return $hash
}
 
function ConvertFrom-Xml($xml) 
{
    if($xml -is [string]){
        $doc = [System.Xml.XmlDocument]::new()
        $doc.LoadXml($xml)
        $xml = $doc.FirstChild
    }
    $hash = @{}
    $hash = ConvertFrom-XmlPart($xml)
    $obj = New-Object PSCustomObject -Property $hash
    $obj.PSObject.TypeNames.Insert(0, $xml.LocalName)
    $obj
}

function ConvertTo-Xml
{
    [CmdletBinding()]
    param($RootName = "root", [switch]$AsString,[switch]$Compress, [switch]$NoRoot, [Parameter(Mandatory = $true, Position = 0, ValueFromPipeline = $true)]$PSItem)

    process{

        $currentRootName = $null
        if($_ -is [PSObject]){
            $currentRootName = $_.psobject.TypeNames[0]
        }

        $doc = [System.Xml.XmlDocument]::new()
        $root = $doc.CreateElement($RootName)
        $doc.AppendChild($root) | Out-Null

        $stack = [System.Collections.Stack]::new()
        $stack.Push(@{Root = $root;Item = $_;Name=$currentRootName})

        while($stack.Count -gt 0){  
            $current = $stack.Pop()
            $currentElem = $doc.CreateElement($current.Name) 
            $current.Root.AppendChild($currentElem) | Out-Null       
            
            if($current.Item -is [System.Collections.Hashtable]){
                $hash = [hashtable]$current.Item
                $hash.Keys | % {
                    Write-Verbose $_
                    $value = $hash[$_]
                    if($value -is [System.Collections.Hashtable]){ $stack.Push(@{Root = $currentElem;Item=$value;Name=$_}) }
                    elseif($value -is [PSObject]){ $stack.Push(@{Root = $current.Root;Item=$value;Name=$_}) }
                    else{
                        $att = $doc.CreateAttribute($_)
                        $att.Value = $value
                        $currentElem.Attributes.Append($att) | Out-Null
                    }
                }
            }elseif($current.Item -is [PSObject]){
                $psobj = [PSObject]$current.Item
                $psobj | Get-Member -MemberType *Property | %{
                    Write-Verbose $_.Name
                    $value = $psobj.($_.Name)
                    if($value -is [System.Collections.Hashtable]){ $stack.Push(@{Root = $currentElem;Item=$value;Name=$_.Name}) }
                    elseif($value -is [PSObject]){ $stack.Push(@{Root = $currentElem;Item=$value;Name=$_.Name}) }
                    else{
                        $att = $doc.CreateAttribute($_.Name)
                        $att.Value =  $value
                        $currentElem.Attributes.Append($att) | Out-Null
                    }
                }
            }
        }


        if($AsString.IsPresent){
            $settings = [System.Xml.XmlWriterSettings]::new();
            $settings.Indent = $true;
            $settings.IndentChars = ("    ");
            $settings.OmitXmlDeclaration = $true;

            $strWriter = [System.IO.StringWriter]::new()
            $xmlWriter = [System.Xml.XmlWriter]::Create($strWriter, $settings)

            $elem = $doc

            if($NoRoot.IsPresent){
                $elem = $doc.FirstChild                
            }

            $elem.WriteContentTo($xmlWriter)
            $xmlWriter.Flush()
            $strWriter.Flush()
            $strWriter.GetStringBuilder().ToString()
            $xmlWriter.Close()
            $strWriter.Close()
        }else{
            $doc
        }
    }
}

Export-ModuleMember ConvertFrom-Xml, ConvertTo-Xml