param ($Call)
rm log.txt -EA SilentlyContinue
"`$`$>a<debug.txt" | cdb .\.build\Debug\la.exe
# > cdb.out.txt
cat log.txt -EA SilentlyContinue
