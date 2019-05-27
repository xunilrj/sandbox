Import-Module ./blocks.psm1 -Force
$blocks = Parse "./dev/ComputerScience/Computer Graphics/int13H.editor"
$lines = Generate $blocks
Save "./dev/ComputerScience/Computer Graphics/int13H.html" $lines
Save "..\..\xunilrj.github.io\articles\int13H.html" $lines