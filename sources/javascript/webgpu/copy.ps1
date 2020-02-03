cp D:/OnlineDrives/Daniel/OneDrive/Desktop/models/ty.walking.001/models/* ./common/models -Verbose
cp D:/OnlineDrives/Daniel/OneDrive/Desktop/models/ty.walking.001/textures/* ./common/textures -Verbose
cp D:/OnlineDrives/Daniel/OneDrive/Desktop/models/ty.walking.001/shaders/* ./common/shaders -Verbose
cp D:/OnlineDrives/Daniel/OneDrive/Desktop/models/ty.walking.001/materials/* ./common/materials -Verbose

gci *.frag -Recurse|%{glslangValidator.exe -V $_.Fullname -o "$($_.Fullname).spv"}; 
gci *.vert -Recurse|%{glslangValidator.exe -V $_.Fullname -o "$($_.Fullname).spv"};