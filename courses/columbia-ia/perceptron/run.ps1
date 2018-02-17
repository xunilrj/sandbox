cd C:\github\xunilrj-sandbox\sources\courses\columbia-ia\perceptron
cls
gci . -Filter *.png | ri -Force
gci . -Filter *.mp4 | ri -Force
ri output.csv -EA SilentlyContinue
python problem1_3.py input1.csv output1.csv
ffmpeg -i figure%04d.png -framerate 2 -y video.mp4
start video.mp4