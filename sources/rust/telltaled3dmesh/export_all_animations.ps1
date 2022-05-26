param($character)

if ($character.ToLower() -eq "guybrush") {
    cargo run --release -- anm-convert --path C:\Users\xunil\Downloads\ttarctext\mi101\sk20_move_guybrushwalk.anm --output ./viewer/models/guybrush_walk.gltf
    cargo run --release -- anm-convert --path C:\Users\xunil\Downloads\ttarctext\mi101\sk20_move_guybrushrun.anm --output ./viewer/models/guybrush_run.gltf
}