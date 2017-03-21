function Enable-Access
{
    param($Thumbprint, $Account, $AccessType)

    $objUser = New-Object System.Security.Principal.NTAccount($Account)
    $strSID = $objUser.Translate([System.Security.Principal.SecurityIdentifier])
    $Account = $strSID.Value

    Write-Verbose "SID: $Account"

    $certs = dir CERT:\LocalMachine\My | ? Thumbprint -like "$Thumbprint*"

    if(($certs | Measure-Object).Count -ne 1)
    {
        Write-Error "Certnot found"
    }

    $cert = $certs[0]
    Write-Verbose "Thumbprint: $($cert.Thumbprint)"

    #Grant Full Control to account listed in $serviceAccount
    $keyPath = $env:ProgramData + "\Microsoft\Crypto\RSA\MachineKeys\"; 
    $keyName = $cert.PrivateKey.CspKeyContainerInfo.UniqueKeyContainerName;
    $keyFullPath = $keyPath + $keyName;
    
    Write-Verbose "keyPath: $keyPath"
    Write-Verbose "keyName: $keyName"

    $acl = (Get-Item $keyFullPath).GetAccessControl('Access') #Get Current Access
    $buildAcl = New-Object  System.Security.AccessControl.FileSystemAccessRule($objUser,$AccessType,"Allow") #Build Access Rule
    $acl.SetAccessRule($buildAcl) #Add Access Rule
    Set-Acl $keyFullPath $acl #Save Access Rules
}