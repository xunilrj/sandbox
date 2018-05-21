#incomplete.
#banded cholesky should be much better in performance.

octave .\generate.test.data.m 6
cp .\data.csv .\source\data.csv
rm .\data.csv
cd source    
try 
{
    .\test.ps1 -Test matrix.factor.cholesky.envelope.compare.octave
}catch{}
cd ..
