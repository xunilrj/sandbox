function get
{
    [CmdletBinding()]
    param($Path)

    $uri = [System.Uri]::new($path)
    iex "Get-$($uri.Scheme)"
}
