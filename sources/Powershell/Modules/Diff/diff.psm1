Class DiffData
{
    [int]$Length;
    [int[]]$data;
    [bool[]]$modified;
    DiffData([int[]]$initData) {
      {$this.data = $initData;}
      {$this.Length = $initData.Length}
      {$this.modified = New-Object bool[] ($this.Length + 2)}
    }
}

class Item
{
    [int]$StartA
    [int]$StartB
    [int]$deletedA
    [int]$insertedB
} 

 function CreateDiffs($DataA, $DataB) {
    $result
    .{
        $a = [System.Collections.ArrayList]::new()
        $aItem;
        $result;

        $StartA
        $StartB;
        $LineA
        $LineB;

        $LineA = 0;
        $LineB = 0;
        while ($LineA -lt $DataA.Length -or $LineB -lt $DataB.Length) {
        if (($LineA -lt $DataA.Length) -and (!$DataA.modified[$LineA]) -and ($LineB -lt $DataB.Length) -and (!$DataB.modified[$LineB])) {
            $LineA++;
            $LineB++;
        } else {
            $StartA = LineA;
            $StartB = LineB;

            while ($LineA -lt $DataA.Length -and ($LineB -ge $DataB.Length -or $DataA.modified[$LineA])) { $LineA++; }
            while ($LineB -lt $DataB.Length -and ($LineA -ge $DataA.Length -or $DataB.modified[$LineB])) { $LineB++; }

            if (($StartA -lt $LineA) -or ($StartB -lt $LineB)) {
            $aItem = New-Object Item
            $aItem.StartA = $StartA;
            $aItem.StartB = $StartB;
            $aItem.deletedA = $LineA - $StartA;
            $aItem.insertedB = $LineB - $StartB;
            $a.Add($aItem);
            }
        }
        }

        $result = New-Object Item[] $a.Count;
        $a.CopyTo($result);
    } | Out-Null
    $result;
}

function Optimize($Data) {
    $StartPos, $EndPos;

    $StartPos = 0;
    while ($StartPos -lt $Data.Length) {
        while (($StartPos -lt $Data.Length) -and ($Data.modified[$StartPos] -eq $false)){
            $StartPos++;
        }
        $EndPos = $StartPos;
        while (($EndPos -lt $Data.Length) -and ($Data.modified[$EndPos] -eq $true)){
            $EndPos++;
        }

        if (($EndPos -lt $Data.Length) -and ($Data.data[$StartPos] -eq $Data.data[$EndPos])) {
            $Data.modified[$StartPos] = false;
            $Data.modified[$EndPos] = true;
        } else {
            $StartPos = $EndPos;
        }
    }
} 

function SMS($DataA, $LowerA, $UpperA, $DataB, $LowerB, $UpperB, $DownVector, $UpVector) {
      $ret;
      .{
          $MAX = $DataA.Length + $DataB.Length + 1;

          $DownK = $LowerA - $LowerB; #// the k-line to start the forward search
          $UpK = $UpperA - $UpperB; #// the k-line to start the reverse search

          $Delta = ($UpperA - $LowerA) - ($UpperB - $LowerB);
          $oddDelta = $($Delta -band 1) -ne 0;

          $DownOffset = $MAX - $DownK;
          $UpOffset = $MAX - $UpK;

          $MaxD = (($UpperA - $LowerA + $UpperB - $LowerB) / 2) + 1;

          $DownVector[$DownOffset + $DownK + 1] = $LowerA;
          $UpVector[$UpOffset + $UpK - 1] = $UpperA;

          for ($D = 0; $D -le $MaxD; $D++) {
            for ($k = $DownK - $D; $k -le $DownK + $D; $k += 2) {
              $x
              $y;
              if ($k -eq $DownK - $D) {
                $x = $DownVector[$DownOffset + $k + $1];
              } else {
                $x = $DownVector[$DownOffset + $k - 1] + 1;
                if (($k -lt $DownK + $D) -and ($DownVector[$DownOffset + $k + 1] -ge $x)){
                  $x = $DownVector[$DownOffset + $k + 1];
                }
              }
              $y = $x - $k;

              while (($x -lt $UpperA) -and ($y -lt $UpperB) -and ($DataA.$data[$x] -eq $DataB.data[$y])) {
                $x++; $y++;
              }
              $DownVector[$DownOffset + $k] = $x;

              if ($oddDelta -and ($UpK - $D -lt $k) -and ($k -lt $UpK + $D)) {
                if ($UpVector[$UpOffset + $k] -le $DownVector[$DownOffset + $k]) {
                  $ret.x = $DownVector[$DownOffset + $k];
                  $ret.y = $DownVector[$DownOffset + $k] - $k;
                  return ($ret);
                }
              }
            } 

            for ($k = $UpK - $D; $k -le $UpK + $D; $k += 2) {
              $x
              $y;
              if ($k -eq $UpK + $D) {
                $x = $UpVector[$UpOffset + $k - 1];
              } else {
                $x = $UpVector[$UpOffset + $k + 1] - 1;
                if (($k -gt $UpK - $D) -and ($UpVector[$UpOffset + $k - 1] -lt $x)){
                  $x = $UpVector[$UpOffset + $k - 1];
                }
              }
              $y = $x - $k;

              while (($x > $LowerA) -and ($y > $LowerB) -and ($DataA.data[$x - 1] -eq $DataB.data[$y - 1])) {
                $x--; $y--;
              }
              $UpVector[$UpOffset + $k] = $x;

              if (!$oddDelta -and ($DownK - $D -le $k) -and ($k -le $DownK + $D)) {
                if ($UpVector[$UpOffset + $k] -le $DownVector[$DownOffset + $k]) {
                  $ret.x = $DownVector[$DownOffset + $k];
                  $ret.y = $DownVector[$DownOffset + $k] - $k;
                  return ($ret);
                }
              }
            }
          }
        } | Out-Null
            
    $ret
      #throw new ApplicationException("the algorithm should never come here.");
} 

function DiffCodes($aText, $h, $trimSpace, $ignoreSpace, $ignoreCase) {
    $codes
    .{
        $Lines;
        $Codes;
        $lastUsedCode = $h.Count;
        $aCode;
        $s;

        $aText = $aText.Replace("\r", "");
        $Lines = $aText.Split('\n');

        $Codes = New-Object int[] $Lines.Length;

        for ($i = 0; $i -lt $Lines.Length; ++$i) {
        $s = $Lines[$i];
        if ($trimSpace){
            $s = $s.Trim();
        }

        if ($ignoreSpace) {
            $s = [System.Text.Regex]::Replace($s, "\\s+", " ");
        }

        if ($ignoreCase){
            $s = $s.ToLower();
        }

        $aCode = $h[$s];
        if ($aCode -eq $null) {
            $lastUsedCode++;
            $h[$s] = $lastUsedCode;
            $Codes[$i] = $lastUsedCode;
        } else {
            $Codes[$i] = [int]$aCode;
        }
        }
        return $Codes;
    } | Out-Null
    $Codes
}

function LCS($DataA, $LowerA, $UpperA, $DataB, $LowerB, $UpperB, $DownVector, $UpVector) {
    .{
      while ($LowerA -lt $UpperA -and $LowerB -lt $UpperB -and $DataA.data[$LowerA] -eq $DataB.data[$LowerB]) {
        $LowerA++; $LowerB++;
      }

      while ($LowerA -lt $UpperA -and $LowerB -lt $UpperB -and $DataA.data[$UpperA - 1] -eq $DataB.data[$UpperB - 1]) {
        --$UpperA; --$UpperB;
      }

      if ($LowerA -eq $UpperA) {
        while ($LowerB -lt $UpperB)
        {
          $DataB.modified[$LowerB++] = $true;
        }
      } elseif ($LowerB -eq $UpperB) {
        
        while ($LowerA -lt $UpperA){
          $DataA.modified[$LowerA++] = $true;
        }

      } else {
        $smsrd = SMS $DataA $LowerA $UpperA $DataB$LowerB $UpperB $DownVector $UpVector
        LCS $DataA $LowerA $smsrd.x $DataB $LowerB $smsrd.y $DownVector $UpVector
        LCS $DataA $smsrd.x $UpperA $DataB $smsrd.y $UpperB $DownVector $UpVector
      }
    } | Out-Null
} 

function DiffText($TextA, $TextB, $trimSpace, $ignoreSpace, $ignoreCase) {
    
    .{
      $h = [System.Collections.Hashtable]::new($TextA.Length + $TextB.Length);

      $diffCodes = DiffCodes $TextA $h $trimSpace $ignoreSpace $ignoreCase
      $DataA = New-Object DiffData @(,$diffCodes)

      $diffCodes = DiffCodes $TextB $h $trimSpace $ignoreSpace $ignoreCase
      $DataB = New-Object DiffData @(,$diffCodes)

      $h = $null;
      $MAX = $DataA.Length + $DataB.Length + 1
      $DownVector = New-Object int[] (2 * $MAX + 2)
      $UpVector = New-Object int[] (2 * $MAX + 2)

      LCS $DataA 0 $DataA.Length $DataB 0 $DataB.Length $DownVector $UpVector

      Optimize $DataA
      Optimize $DataB
      
      $r = CreateDiffs $DataA $DataB
    } | Out-Null
    $r
}

function WriteLine([int]$nr, $typ, $aText) {    
  if ($nr -ge 0)
  {
    Write-Host "$(($nr+1).ToString("D4")) " -NoNewline
  }
  else
  {
    Write-Host "     " -NoNewline
  }

  if($typ -eq "d")
  {
    Write-Host $aText -ForegroundColor Red -NoNewline
  }
  elseif($typ -eq "i")
  {
    Write-Host $aText -ForegroundColor Green -NoNewline
  }
  else
  { 
    Write-Host $aText -ForegroundColor White -NoNewline
  }
  Write-Host ""
}

function Diff-Strings([string]$A,[string]$B)
{
    #DiffText $A $B $false $false $false
    $Assem = ( 
    “mscorlib” , 
    “System” 
    ) 
    $Source = cat "$(Split-Path $PSCommandPath -Parent)/diff.cs" -Raw
    Add-Type -ReferencedAssemblies $Assem -TypeDefinition $Source -Language CSharp | Out-Null
    $f = [my.utils.Diff]::DiffText($A,$B,$false,$false,$false)

    $aLines = $A.Split(@([System.Environment]::NewLine), "RemoveEmptyEntries");
    $bLines = $B.Split(@([System.Environment]::NewLine), "RemoveEmptyEntries");

    $n = 0;
    for ($fdx = 0; $fdx -lt $f.Length; $fdx = $fdx + 1) {
        $aItem = $f[$fdx];

        while (($n -lt $aItem.StartB) -and ($n -lt $bLines.Length)) {
            WriteLine $n $null $bLines[$n]
            $n = $n+1;
        } #// while

        #// write deleted lines
        for ($m = 0; $m -lt $aItem.deletedA; $m=$m+1) {
          WriteLine -1 "d" $aLines[$aItem.StartA + $m]
        } #// for

        #// write inserted lines
        while ($n -lt $aItem.StartB + $aItem.insertedB) {
          WriteLine $n "i" $bLines[$n]
          $n = $n + 1;
        } #// while
    } #// for
  
    #// write rest of unchanged lines
    while ($n -lt $bLines.Length) {
        WriteLine $n $null $bLines[$n]
        $n = $n+ 1;
    } #// while
}