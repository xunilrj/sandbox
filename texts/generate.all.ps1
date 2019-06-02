Import-Module ./blocks.psm1 -Force
cd (Split-Path $PSCommandPath -Parent)

ParseGenerateSave "./dev/ComputerScience/Computer Graphics/int13H.editor" @(
    "./dev/ComputerScience/Computer Graphics/int13H.html",
    "..\..\xunilrj.github.io\articles\int13H.html"
)

ParseGenerateSave "..\..\xunilrj.github.io\guides\livrosdedireita\livrosdedireita.editor" @(
    "..\..\..\xunilrj.github.io\guides\livrosdedireita\livrosdedireita.html"
)