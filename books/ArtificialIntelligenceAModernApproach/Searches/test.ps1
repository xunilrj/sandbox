gci *.json |% {
    gc $_ | .\searches\Debug\searches.exe
}