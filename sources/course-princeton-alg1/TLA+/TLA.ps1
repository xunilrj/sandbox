param([switch]$RunTLC)

if($RunTLC.IsPresent)
{
    Write-Host "Removing log..."
    ri tlc.result.txt -Force -EA SilentlyContinue

    Write-Host "Transpiling..."
    java -cp C:/Users/xunil/Downloads/tla pcal.trans ".\quickfind.tla"
    copy quickfind.tla ./QuickFind.toolbox/Model_1/quickfind.tla -Force
    cd ./QuickFind.toolbox/Model_1

    Write-Host "Checking..."
    java -cp C:/Users/xunil/Downloads/tla tlc2.TLC ".\MC.tla" "-config" ".\MC.cfg" > tlc.result.txt
    
    Write-Host "Done!"
}