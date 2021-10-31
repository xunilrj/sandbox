
#$file = 'C:\Program Files (x86)\Telltale Games\Tales of Monkey Island\Launch of the Screaming Narwhal\Pack\obj_ballcannon.d3dmesh' 
#rm $file
#ni -ItemType SymbolicLink -Path $file -Target "C:\Users\xunil\Downloads\ttarctext\mi101\sk13_scorpionmonkey.d3dmesh"

$file = "sk20_guybrush.skl"
$source = 'C:\Users\xunil\Downloads\ttarctext\mi101'
$target = 'C:\Program Files (x86)\Telltale Games\Tales of Monkey Island\Launch of the Screaming Narwhal\Pack'
cp "$source/$file" "$target/$file"

c:\github\milkdbg\target\debug\milkdbg.exe --path "all.createfilea.js" > a.txt
#rm $file
#ni -ItemType SymbolicLink -Path $file -Target "C:\Users\xunil\Downloads\ttarctext\mi101\sk13_scorpionmonkey.d3dmesh"