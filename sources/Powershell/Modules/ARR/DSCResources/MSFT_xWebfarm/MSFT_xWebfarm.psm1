data LocalizedData
{   
}

$_xWebfarm_DefaultLoadBalancingAlgorithm = "WeightedRoundRobin"
$_xWebfarm_DefaultApplicationHostConfig = "%windir%\system32\inetsrv\config\applicationhost.config"

function Get-TargetResource 
{
    [OutputType([System.Collections.Hashtable])]
    param 
    (   
        [Parameter(Mandatory)]
        [ValidateSet("Present", "Absent")]
        [string]$Ensure,

        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [string]$Name,
                
        [string]$ConfigPath
    )        

    Write-Verbose "xWebfarm/Get-TargetResource"
    Write-Verbose "Name: $Name"    
    Write-Verbose "ConfigPath: $ConfigPath"
    
    $config = GetApplicationHostConfig $ConfigPath
    $webFarm = GetWebfarm $Name $config
    GetTargetResourceFromConfigElement $webFarm    
}

function Set-TargetResource 
{
    [CmdletBinding(SupportsShouldProcess=$true)]
    param 
    (       
        [Parameter(Mandatory)]
        [ValidateSet("Present", "Absent")]
        [string]$Ensure,

        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [string]$Name,

        [bool]$Enabled = $true,

        [string]$Algorithm,
        [string]$QueryString,
        [string]$ServerVariable,
        [string]$Servers,

        [string]$ConfigPath
    )

    #region

    Write-Verbose "xWebfarm/Set-TargetResource"
    Write-Verbose "Ensure: $Ensure"
    Write-Verbose "Name: $Name"
    Write-Verbose "Enabled: $Enabled"
    Write-Verbose "Algorithm: $Algorithm"
    Write-Verbose "QueryString: $QueryString"
    Write-Verbose "ServerVariable: $ServerVariable"
    Write-Verbose "ConfigPath: $ConfigPath"
    Write-Verbose "Servers: $Servers"
    
    Write-Verbose "Get current webfarm state"

    $config = GetApplicationHostConfig $ConfigPath
    $webFarm = GetWebfarm $Name $config
    $resource = GetTargetResourceFromConfigElement $webFarm

    Write-Verbose "Webfarm presence. From [$($resource.Ensure )] to [$Ensure]"

    if(($Ensure -eq "present") -and ($resource.Ensure -eq "absent")){
        $webFarmElement = $config.CreateElement("webFarm")
        $webFarmElement.SetAttribute("name", $Name)        
        $config.configuration.webFarms.AppendChild($webFarmElement)

        Write-Verbose "Webfarm created: Name = $Name"
        
        $resource = GetTargetResourceFromConfigElement $webFarmElement
        $webFarm = GetWebfarm $Name $config
    }elseif(($Ensure -eq "absent") -and ($resource.Ensure -eq "present")){
        $webFarmElement = $config.configuration.webFarms.webFarm | Where-Object Name -eq $Name
        $config.configuration.webFarms.RemoveChild($webFarmElement)

        Write-Verbose "Webfarm deleted: Name = $Name"

        $resource = GetTargetResourceFromConfigElement $null
        $webFarm = $null
    }
    else {
    }
    
    if (($Ensure -eq "present") -and ($resource.Ensure -eq "present")){
        Write-Verbose "Webfarm configured: Enabled from [$($resource.Enabled)] to [$Enabled]"
        $webFarm.SetAttribute("enabled", $Enabled)
                
        if($Algorithm -eq $null){
            Write-Verbose "Webfarm configured: LoadBalancing from [$($resource.Algorithm)] to []"
            if($null -ne $webFarm.applicationRequestRouting){
                $webFarm.RemoveChild($webFarm.applicationRequestRouting)
            }
        }else{
            Write-Verbose "Webfarm configured: LoadBalancing from [$($resource.Algorithm)] to [$Algorithm]"

            $applicationRequestRoutingElement = $webFarm.applicationRequestRouting
            $loadBalancingElement = $webFarm.applicationRequestRouting.loadBalancing

            if($null -eq $webFarm.applicationRequestRouting){
                $applicationRequestRoutingElement = $config.CreateElement("applicationRequestRouting")
                $webFarm.AppendChild($applicationRequestRoutingElement)
            }

            if($null -eq $webFarm.applicationRequestRouting.loadBalancing){
                $loadBalancingElement = $config.CreateElement("loadBalancing")
                $loadBalancingElement.SetAttribute("algorithm", $_xWebfarm_DefaultLoadBalancingAlgorithm)
                $applicationRequestRoutingElement.AppendChild($loadBalancingElement)
            }

            if($Algorithm -eq "weightedroundrobin"){
                $loadBalancingElement.SetAttribute("algorithm", "WeightedRoundRobin")
                $loadBalancingElement.RemoveAttribute("hashServerVariable")
                $loadBalancingElement.RemoveAttribute("queryStringNames")
            }
            elseif($Algorithm -eq "querystring"){
                $loadBalancingElement.SetAttribute("algorithm", "RequestHash")
                $loadBalancingElement.SetAttribute("hashServerVariable", "query_string")
                $loadBalancingElement.SetAttribute("queryStringNames", [System.String]::Join(",", $QueryString))
            }
            elseif($Algorithm -eq "servervariable"){
                $loadBalancingElement.SetAttribute("algorithm", "RequestHash")
                $loadBalancingElement.SetAttribute("hashServerVariable", $ServerVariable)
                $loadBalancingElement.RemoveAttribute("queryStringNames")
            }
            elseif($Algorithm -eq "requesthash"){
                $loadBalancingElement.SetAttribute("algorithm", "RequestHash")
                $loadBalancingElement.RemoveAttribute("hashServerVariable")
                $loadBalancingElement.RemoveAttribute("queryStringNames")
            }
        }


        $requestedServers = ConvertFrom-Json $Servers
        $presentServers = ConvertFrom-Json $resource.Servers

        $needChange = $false

        Write-Verbose "Setting Servers xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

        $requestedServers | % {
            
            if($_.HttpPort -eq $null){$_ | Add-Member NoteProperty HttpPort 80}
            if($_.HttpsPort -eq $null){$_ | Add-Member NoteProperty HttpsPort 443}
            if($_.Weight -eq $null){$_ | Add-Member NoteProperty Weight 100}
            
            $requestedAddress = $_.Address
            $targetServer = $presentServers | ? Address -eq $requestedAddress | Select -First 1
            
            if($targetServer -eq $null){
                Write-Verbose "Address: $($_.Address) Adding..."
                $serverElement = $config.CreateElement("server")
                $serverElement.SetAttribute("address", $_.Address.ToString())
                $serverElement.SetAttribute("enabled", $_.Enabled.ToString().ToLower())

                $applicationRequestRoutingElement = $config.CreateElement("applicationRequestRouting")
                $applicationRequestRoutingElement.SetAttribute("httpPort", $_.HttpPort.ToString())
                $applicationRequestRoutingElement.SetAttribute("httpsPort", $_.HttpsPort.ToString())
                $applicationRequestRoutingElement.SetAttribute("weight", $_.Weight.ToString())
                $serverElement.AppendChild($applicationRequestRoutingElement)

                $webFarm.AppendChild($serverElement)
            }else
            {
                Write-Verbose "Address: $($_.Address) Updating..."
                $serverelement = $webFarm.server | ? {$_.address -eq $requestedAddress} | Select -First 1
                $serverElement.SetAttribute("enabled", $_.Enabled.ToString().ToLower())

                $applicationRequestRoutingElement = $serverElement.applicationRequestRouting
                if($applicationRequestRoutingElement -eq $null)
                {
                    $applicationRequestRoutingElement = $config.CreateElement("applicationRequestRouting")
                    $serverElement.AppendChild($applicationRequestRoutingElement)
                }

                $applicationRequestRoutingElement.SetAttribute("httpPort", $_.HttpPort.ToString())
                $applicationRequestRoutingElement.SetAttribute("httpsPort", $_.HttpsPort.ToString())
                $applicationRequestRoutingElement.SetAttribute("weight", $_.Weight.ToString())
            }
        }

        $presentServers | % {
           
            $presentServerAddress = $_.Address
            $notinlist = $requestedServers | ? Address -eq $presentServerAddress | Select -First 1

            if($notinlist -eq $null)
            {
                Write-Verbose "Address: $($_.Address) Removing..."
                $serverelement = $webFarm.server | ? {$_.address -eq $presentServerAddress} | Select -First 1
                $webFarm.RemoveChild($serverelement)
            }            
        }

         Write-Verbose "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    }

    if($null -ne $config ){
        Write-Verbose "Finished configuration."

        if($pscmdlet.ShouldProcess($computername)){
            Write-Verbose "Should process: true"
            SetApplicationHostConfig $ConfigPath $config
        }else{
            Write-Verbose "Should process: false"
        }        
    }

    #endregion
}

function Test-TargetResource 
{
    [OutputType([System.Boolean])]
    param 
    (     
        [Parameter(Mandatory)]        
        [ValidateSet("Present", "Absent")]
        [string]$Ensure,

        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [string]$Name,       
                
        [bool]$Enabled = $true,

        [string]$Algorithm,
        [string]$QueryString,
        [string]$ServerVariable,
        [string]$Servers,

        [string]$ConfigPath
    )

    #region

    Write-Verbose "xWebfarm/Test-TargetResource"
    Write-Verbose "Name: $Name"
    Write-Verbose "ConfigPath: $ConfigPath"
    Write-Verbose "Servers: $Servers"

    if([System.String]::IsNullOrEmpty($Algorithm)){
        $Algorithm = $_xWebfarm_DefaultLoadBalancingAlgorithm
    }
    
    $config = GetApplicationHostConfig $ConfigPath
    $webFarm = GetWebfarm $Name $config
    $resource = GetTargetResourceFromConfigElement $webFarm
    
    Write-Verbose "Testing Ensures: Requested [$Ensure] Resource [$($resource.Ensure)]"
    if($resource.Ensure -eq "absent"){
        if($Ensure -eq "absent"){            
            return $true
        }else{
            return $false
        }

    }elseif($resource.Ensure -eq "present"){
        if($Ensure -eq "absent"){
            return $false
        }

        Write-Verbose "Testing Enabled: Requested [$Enabled] Resource [$($resource.Enabled)]"

        if($resource.Enabled -ne $Enabled){
            return $false
        }

        if($Algorithm -ne $resource.Algorithm){
            return $false
        }

        if($Algorithm -eq "querystring"){
            if([System.String]::IsNullOrEmpty($QueryString) -eq $false){
                $queryStringList1 = [System.String]::Join(",", ($QueryString.Split(",") | Sort-Object))
                $queryStringList2 = [System.String]::Join(",", ($resource.QueryString | Sort-Object))
            
                return $queryStringList1 -eq $queryStringList2
            }
        }elseif($Algorithm -eq "servervariable"){
            if([System.String]::IsNullOrEmpty($ServerVariable) -eq $false){
                $serverVariableList1 = [System.String]::Join(",", ($ServerVariable.Split(",") | Sort-Object))
                $serverVariableList2 = [System.String]::Join(",", ($resource.ServerVariable | Sort-Object))
            
                return $serverVariableList1 -eq $serverVariableList2
            }
        }

        $requestedServers = ConvertFrom-Json $Servers
        $presentServers = ConvertFrom-Json $resource.Servers

        $needChange = $false

        Write-Verbose "Testing Servers xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

        $requestedServers | % {
            Write-Verbose "Address: $($_.Address) ------------------------"
            if($_.HttpPort -eq $null){$_ | Add-Member NoteProperty HttpPort 80}
            if($_.HttpsPort -eq $null){$_ | Add-Member NoteProperty HttpsPort 443}
            if($_.Weight -eq $null){$_ | Add-Member NoteProperty Weight 100}
            #Write-Verbose "Enabled: $($_.Enabled), HttpPort: $($_.HttpPort), HttpsPort: $($_.HttpsPort), Weight: $($_.Weight)"

            $requestedAddress = $_.Address
            $targetServer = $presentServers | ? Address -eq $requestedAddress | Select -First 1
            
            if($targetServer -eq $null){
                Write-Verbose "This would be added."
                $needChange = $true
            }else
            {
                Write-Verbose "Enabled: Requested [$($_.Enabled)] Resource [$($targetServer.Enabled)]"
                Write-Verbose "HttpPort: Requested [$($_.HttpPort)] Resource [$($targetServer.HttpPort)]"
                Write-Verbose "HttpsPort: Requested [$($_.HttpsPort)] Resource [$($targetServer.HttpsPort)]"
                Write-Verbose "Weight: Requested [$($_.Weight)] Resource [$($targetServer.Weight)]"

                $thisneedchange = $_.Enabled.ToString().ToLower() -ne $targetServer.Enabled.ToString().ToLower() -or $_.HttpPort -ne $targetServer.HttpPort -or $_.HttpsPort -ne $targetServer.HttpsPort -or $_.Weight -ne $targetServer.Weight
                $needChange = $needChange -or $thisneedchange
                Write-Verbose "Needs change: $thisneedchange"
            }
        }

        $presentServers | % {
           
            $presentServerAddress = $_.Address
            $notinlist = $requestedServers | ? Address -eq $presentServerAddress | Select -First 1

            if($notinlist -eq $null)
            {
                Write-Verbose "Address: $($_.Address) ------------------------"
                Write-Verbose "This would be deleted."

                $needChange = $true
            }            
        }

         Write-Verbose "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

         !$needChange
    }   

    #endregion
}

#region private methods

function GetWebfarm{
    param 
    (       
        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [string]$Name,
        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [xml]$Config
    )
    $farms = $Config.configuration.webFarms.webFarm | Where-Object name -eq $Name
    $measure = $farms | measure-object
    
    if($measure.Count -gt 1){
        Write-Error "More than one webfarm found! The config must be corrupted"
    }elseif($measure.Count -eq 0){
        $null
    }else{
        $farms
    }
}

function GetTargetResourceFromConfigElement($webFarm){
    $resource = @{
        Ensure = "Absent"
    }

    if($null -ne $webFarm){
        $resource.Ensure = "Present"

        if([System.String]::IsNullOrEmpty($webFarm.enabled)){
            $resource.Enabled = $false
        }else{
            $resource.Enabled = [System.Boolean]::Parse($webFarm.enabled)
        }

        #dows this farm have the specific request routing element
        if($null -ne $webFarm.applicationRequestRouting){
            $resource.Algorithm = $webFarm.applicationRequestRouting.loadBalancing.algorithm
            
            if([System.String]::IsNullOrEmpty($resource.Algorithm)){
                $resource.Algorithm = $_xWebfarm_DefaultLoadBalancingAlgorithm
            }

            if($null -ne $webFarm.applicationRequestRouting.loadBalancing){
                if($null -ne $webFarm.applicationRequestRouting.loadBalancing.hashServerVariable){
                    if($webFarm.applicationRequestRouting.loadBalancing.hashServerVariable -eq "query_string"){
                        $resource.Algorithm = "QueryString"
                        $resource.QueryString = $webFarm.applicationRequestRouting.loadBalancing.queryStringNames.Split(",")                
                    }else{
                        $resource.Algorithm = "ServerVariable"
                        $resource.ServerVariable = $webFarm.applicationRequestRouting.loadBalancing.hashServerVariable.Split(",")
                    }
                }
            }
        }else{
            $resource.Algorithm = $_xWebfarm_DefaultLoadBalancingAlgorithm            
        }
    }

    $serversList = New-Object System.Collections.ArrayList
    $webFarm.Server | % {
        $httpsPort = $_.applicationRequestRouting.httpsPort;
        if($httpsPort -eq $null){$httpsPort = 443}

        $httpPort = $_.applicationRequestRouting.httpPort;
        if($httpPort -eq $null){$httpPort = 80}

        $weight = $_.applicationRequestRouting.weight;
        if($weight -eq $null){$weight = 100}

        $item = New-Object PSCustomObject -Property @{
            Enabled = $_.enabled;
            Address = $_.address;
            HttpPort = $httpPort;
            HttpsPort = $httpsPort;
            Weight = $weight;
        }
        $serversList.Add($item)
    }
    
    $resource.Servers = ConvertTo-Json -InputObject $serversList 

    $resource 
}

function GetApplicationHostConfig($ConfigPath){
    
    if([System.String]::IsNullOrEmpty($ConfigPath)){
        $ConfigPath = [System.Environment]::ExpandEnvironmentVariables($_xWebfarm_DefaultApplicationHostConfig)
    }

    Write-Verbose "GetApplicationHostConfig $ConfigPath"

    [xml](Get-Content $ConfigPath)
}

function SetApplicationHostConfig{
    param([string]$ConfigPath, [xml]$xml)

    if([System.String]::IsNullOrEmpty($ConfigPath)){
        $ConfigPath = [System.Environment]::ExpandEnvironmentVariables($_xWebfarm_DefaultApplicationHostConfig)
    }

    Write-Verbose "SetApplicationHostConfig $ConfigPath"

    $xml.Save($ConfigPath)
}

#endregion