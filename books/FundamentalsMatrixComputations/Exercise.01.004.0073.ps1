octave .\generate.test.data.m 6
cp .\data.csv .\source\data.csv
rm .\data.csv
cd source    
try 
{
    .\test.ps1 -Test matrix.factor.cholesky.compare.octave
}catch{}
cd ..
