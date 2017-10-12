param($view = $false)
gci *.asy | % {
    if($view){
        asy $_ -f png
    }
    asy $_ -noView
}