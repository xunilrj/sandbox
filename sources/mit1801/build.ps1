#set-alias pandoc C:\Users\xunil\AppData\Local\Pandoc\pandoc.exe
mkdir .\builds -EA SilentlyContinue
pandoc .\solution001.tex -t latex -o .\builds\solutions001.pdf
pandoc .\solution001.tex -t docx -o .\builds\solutions001.docx