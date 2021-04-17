#! /bin/bash

capitalize_words() {
    for i in $1; do
        B=`echo -n "${i:0:1}" | tr "[:lower:]" "[:upper:]"`
        echo -n "${B}${i:1} "
    done
}

rm runall
echo "#! /bin/bash" >> runall
chmod +x runall

find . -type f -iname "_coursera" -print0 | while IFS= read -r -d $'\0' file; do
    FOLDER=$(dirname -- "$file")
    pushd "$FOLDER"
    cargo -q init . 2>&1 > /dev/null
    popd

    CARGOTOML="$FOLDER/Cargo.toml"

    tail -n +3 "$file" | while IFS= read -r -d $'\n' problem; do
        PROBNAME=$(echo "$problem" | cut -d"," -f4)
        PROBNAME=$(capitalize_words "$PROBNAME")
        PROBNAME=$(echo "$PROBNAME"| sed -e "s/ //g")

        s=$(printf "[[bin]]\nname = \"%s\"\npath = \"src/%s.rs\"" $PROBNAME $PROBNAME)
        grep -Fxqe "$s" < "$CARGOTOML" || printf "%s\n" "$s" >> "$CARGOTOML"

        pushd "$FOLDER"
        touch "src/$PROBNAME.rs"
        popd

        echo "pushd \"$FOLDER\"" >> runall
        echo "cargo run --bin $PROBNAME" >> runall
        echo "popd" >> runall
    done
done
