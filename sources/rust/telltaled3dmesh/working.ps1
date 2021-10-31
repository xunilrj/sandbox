cargo build --release




ls C:\Users\xunil\Downloads\ttarctext\mi101\*.d3dmesh | % {
    $_.Name
} | % {
    $output = ($_.Replace(".d3dmesh", ".json"))
    Write-Host "$_ to $output"
    ./target/release/telltaled3dmesh.exe  mesh-convert --path "C:\Users\xunil\Downloads\ttarctext\mi101\$_" -o "C:\Users\xunil\Downloads\ttarctext\mi101\$output"

    $output = ($_.Replace(".d3dmesh", ".obj"))
    Write-Host "$_ to $output"
    ./target/release/telltaled3dmesh.exe  mesh-convert --path "C:\Users\xunil\Downloads\ttarctext\mi101\$_" -o "C:\Users\xunil\Downloads\ttarctext\mi101\$output"
}


