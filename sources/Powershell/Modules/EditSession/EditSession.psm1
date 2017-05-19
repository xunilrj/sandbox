

function appendchar($char)
{
    $global:nextInsert[$global:currentMode] = [System.Management.Automation.Host.Coordinates]::new(($global:nextInsert[$global:currentMode].X+1),$global:bufferSize.Height-1)
    $currentCell = $Host.UI.RawUI.NewBufferCellArray(1, 1, [System.Management.Automation.Host.BufferCell]::new($char, [System.ConsoleColor]::White, [System.ConsoleColor]::Black, [System.Management.Automation.Host.BufferCellType]::Complete))
    $Host.UI.RawUI.SetBufferContents($global:nextInsert[$global:currentMode], $currentCell) 
}

function clearline($line)
{
    $currentCell = $Host.UI.RawUI.NewBufferCellArray($global:bufferSize.Width, 1, [System.Management.Automation.Host.BufferCell]::new(" ", [System.ConsoleColor]::White, [System.ConsoleColor]::Black, [System.Management.Automation.Host.BufferCellType]::Complete))
    $Host.UI.RawUI.SetBufferContents([System.Management.Automation.Host.Coordinates]::new(0,$line), $currentCell) 
}

function Out-EditSession
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Input)
    process{
        $Input |% {appendchar $_}
    }
}

set-alias oes Out-EditSession

function Enter-EditSession
{
     $isInteractive = [Environment]::UserInteractive
     if($isInteractive){

        $global:bufferSize = $Host.UI.RawUI.BufferSize
        $global:currentBuffer = $Host.UI.RawUI.GetBufferContents([System.Management.Automation.Host.Rectangle]::new(0,0,$bufferSize.Width, $bufferSize.Height))
        $global:currentMode = "normal"
        $global:nextInsert = @{
            "normal"=[System.Management.Automation.Host.Coordinates]::new(0,0);
            "insert"=[System.Management.Automation.Host.Coordinates]::new(0,0);
            "command"=[System.Management.Automation.Host.Coordinates]::new(0,$bufferSize.Height-1);
        }
        
        $editBuffer = $Host.UI.RawUI.NewBufferCellArray($bufferSize.Width, $bufferSize.Height, [System.Management.Automation.Host.BufferCell]::new(" ", [System.ConsoleColor]::White, [System.ConsoleColor]::Black, [System.Management.Automation.Host.BufferCellType]::Complete))
        $Host.UI.RawUI.SetBufferContents([System.Management.Automation.Host.Coordinates]::new(0,0), $editBuffer)
                
        $builder = [System.Text.StringBuilder]::new()
        while($true){
            if($Host.UI.RawUI.KeyAvailable)
            {
                $Host.UI.RawUI.FlushInputBuffer();
                $k = $Host.UI.RawUI.ReadKey("AllowCtrlC,IncludeKeyDown,IncludeKeyUp,NoEcho").Character
                if($k -eq 27){
                    break
                }elseif($k -eq '>'){
                    clearline ($global:bufferSize.Height-1)
                    $global:currentMode = "command"
                    $global:nextInsert[$global:currentMode] = [System.Management.Automation.Host.Coordinates]::new(0,$global:bufferSize.Height-1);
                    appendchar ">"
                }elseif(($global:currentMode -eq "command") -and ($k -eq 13)){
                    clearline ($global:bufferSize.Height-1)
                    $result = iex ($builder.ToString())
                    $builder.Clear()|Out-Null
                    $currentMode = "normal"

                    if($result -is [System.IO.FileInfo]){
                        $result = iex "cat $($result.FullName)"
                        $currentCell = $Host.UI.RawUI.NewBufferCellArray($result,"White","Black")
                        $Host.UI.RawUI.SetBufferContents($global:nextInsert[$global:currentMode], $currentCell)
                    }
                }
                elseif(($global:currentMode -eq "command") -and [System.Char]::IsControl($k) -eq $false){
                    $builder.Append($k) | Out-Null
                    appendchar $k 
                }
            }
        }

        $Host.UI.RawUI.SetBufferContents([System.Management.Automation.Host.Coordinates]::new(0,0), $currentBuffer)
     }
     else{
        Write-Warning "EditSession only works on Interactive sessions"
     }
}