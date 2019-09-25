param($command, $config)
if($command -eq "generate"){
    mkdir ./bin/vs2017.32 -Force; pushd ./bin/vs2017.32
    cmake ../.. -G "Visual Studio 15 2017"
    popd

    mkdir ./bin/vs2017.64  -Force; pushd ./bin/vs2017.64
    cmake ../.. -G "Visual Studio 15 2017 Win64"
    popd
}
if($command -eq "build"){
    if($config -eq "vs.32.debug"){
        pushd ./bin/vs2017.32
        cmake --build . --config debug
    } elseif($config -eq "vs.32.release"){
        pushd ./bin/vs2017.32
        cmake --build . --config release
    } elseif($config -eq "vs.64.debug"){
        pushd ./bin/vs2017.64
        cmake --build . --config debug
    } elseif($config -eq "vs.64.release"){
        pushd ./bin/vs2017.64
        cmake --build . --config release
    }
    popd
}
if(($command -eq "run") -and ($LASTEXITCODE -eq 0)){
    if($config -eq "vs.64.debug"){
        ./bin/vs2017.64/Debug/3_0_DeviceHandshake.exe
    } 
}