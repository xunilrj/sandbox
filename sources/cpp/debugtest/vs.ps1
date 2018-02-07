pushd 'c:\Program Files (x86)\Microsoft Visual Studio 14.0\VC'
cmd /c "vcvarsall.bat x64&set" |
foreach {
  if ($_ -match "=") {
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
popd

function compile($file)
{
    cl $file /link /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.16299.0\um\x64" /DYNAMICBASE /MANIFEST /NXCOMPAT /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.16299.0\ucrt\x64" /DEBUG:FULL /PDB:main.pdb /PGD:main.pgd /NOLOGO
}

#/OUT:"C:\github\sandbox\sources\cpp\debugtest\ConsoleApplication1\Debug\ConsoleApplication1.exe" /MANIFEST /NXCOMPAT 
#/PDB:"C:\github\sandbox\sources\cpp\debugtest\ConsoleApplication1\Debug\ConsoleApplication1.pdb" 
#/DYNAMICBASE "kernel32.lib" "user32.lib" "gdi32.lib" "winspool.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "odbc32.lib" "odbccp32.lib" 
#/DEBUG /MACHINE:X86 /INCREMENTAL /PGD:"C:\github\sandbox\sources\cpp\debugtest\ConsoleApplication1\Debug\ConsoleApplication1.pgd"
# /SUBSYSTEM:CONSOLE /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /ManifestFile:"Debug\ConsoleApplication1.exe.intermediate.manifest" /ERRORREPORT:PROMPT /NOLOGO /TLBID:1 


cl /c /I"C:\github\vcpkg\installed\x86-windows\include" /ZI /nologo /W3 /WX- /diagnostics:classic /sdl /Od /Oy- /D WIN32 /D _DEBUG /D _CONSOLE /D _UNICODE /D UNICODE /Gm /EHsc /RTC1 /MDd /GS /fp:precise /permissive- /Zc:wchar_t /Zc:forScope /Zc:inline /Fp"ConsoleApplication1.pch" /Fo"" /Fd"vc141.pdb" /Gd /TP /analyze- /errorReport:prompt main.cpp /link

link main.obj /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.16299.0\um\x64" /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.16299.0\ucrt\x64" /DEBUG:FULL