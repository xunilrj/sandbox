pushd .\.build
    ls *.sln|%{start ($_.Fullname)}
popd