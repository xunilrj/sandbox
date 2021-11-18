
#$file = 'C:\Program Files (x86)\Telltale Games\Tales of Monkey Island\Launch of the Screaming Narwhal\Pack\obj_ballcannon.d3dmesh' 
#rm $file
#ni -ItemType SymbolicLink -Path $file -Target "C:\Users\xunil\Downloads\ttarctext\mi101\sk13_scorpionmonkey.d3dmesh"

$file = "sk20_move_guybrushwalkdeterminedship.anm"
$source = 'C:\temp\mi101'
$target = 'C:\Program Files (x86)\Telltale Games\Tales of Monkey Island\Launch of the Screaming Narwhal\Pack'
rm "$target/$file"
cp "$source/$file" "$target/$file"

# $fake_file = "sk20_guybrush_poxhandpunchface.anm"
# pushd $target
#     rm $fake_file
#     ni -ItemType SymbolicLink -Path $fake_file -Target "C:\Users\xunil\Downloads\ttarctext\mi101\$file"
# popd

rm env:/RUST_LOG
c:\temp\milkdbg\target\release\milkdbg.exe --path "all_readfile_of.js"
