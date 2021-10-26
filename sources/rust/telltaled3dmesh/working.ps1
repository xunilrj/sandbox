cargo build --release

# @(
#     # "obj_cardclub41.d3dmesh",
#     # "obj_banana.d3dmesh",
#     # "obj_bombpirate.d3dmesh",
#     # "obj_breathmints.d3dmesh",
#     # "obj_bucketcoals.d3dmesh",
#     # "obj_chestvoodoo.d3dmesh",
#     # "obj_islandrockofgelato.d3dmesh",
#     # #"obj_coffinmonkey.d3dmesh",
#     # "obj_shippirate.d3dmesh",
#     # "obj_shipguybrush.d3dmesh",
#     # "obj_flotsamjungledayvoodooladyexteriora.d3dmesh"
#     # #"obj_flotsamjungledayvoodooladyexteriorb.d3dmesh"
#     "obj_flotsamjungledayupperla.d3dmesh"
#     # "obj_flotsamjungledayupperlc.d3dmesh"
#     #"obj_flotsamjungledaymerfolkmachine.d3dmesh"
# )
#ls C:\Users\xunil\Downloads\ttarctext\mi101\*.d3dmesh | % {
#$_.Name
#} | % {
@(
    "obj_barometermarquislarge.d3dmesh"
) | % {
    $output = ($_.Replace(".d3dmesh", ".json"))
    Write-Host "$_ to $output"
    ./target/release/telltaled3dmesh.exe  convert --path "C:\Users\xunil\Downloads\ttarctext\mi101\$_" -o "C:\Users\xunil\Downloads\ttarctext\mi101\$output"

    $output = ($_.Replace(".d3dmesh", ".obj"))
    Write-Host "$_ to $output"
    ./target/release/telltaled3dmesh.exe  convert --path "C:\Users\xunil\Downloads\ttarctext\mi101\$_" -o "C:\Users\xunil\Downloads\ttarctext\mi101\$output"
}


