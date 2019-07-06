function Restart-WSL {
    net stop LxssManager
    net start LxssManager;
}

function send2bash ($name, $real)
{
    if($real -eq $null) {
        $real = $name
    }
    Remove-Item "function:\$name" -EA SilentlyContinue
    Remove-Item "function:\global:$name" -EA SilentlyContinue
    $f = New-Item -path function:\ -name global:$name -value {
        $a = ($PsBoundParameters.Values + $args) -join " ";
        if($a.EndsWith("-NewWindow")) {
            $a = $a.Replace("-NewWindow", "")
            start "bash" -ArgumentList "-i -c `"$real $a`""; 
        } else {
            bash -i -c "$real $a"; 
        }
    }.GetNewClosure() -Options AllScope
}

# classic linux

send2bash sed
send2bash make
send2bash tar

# node commands
send2bash npm
send2bash node
send2bash parcel

# others
send2bash octave
send2bash youtubedl youtube-dl