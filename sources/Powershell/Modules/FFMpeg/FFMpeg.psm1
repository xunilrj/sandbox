function Out-Gif
{
    [CmdletBinding()]
    param([Parameter(ValueFromPipeline=$true)]$Item)

    process{
        if($Item -is [System.IO.FileInfo]){
            $Item = $Item.ToString()
        }

        $palleteTemp = "$([System.IO.Path]::GetTempFileName()).png"
        $info = [System.IO.FileInfo]::new($Item)
        $gifPath = [System.Text.RegularExpressions.Regex]::Replace($Item, "$($info.Extension)$", ".gif")

        Write-Verbose "ffmpeg -y -i ""$Item"" -vf fps=10,scale=320:-1:flags=lanczos,palettegen ""$palleteTemp"""
        ffmpeg -y -i ""$Item"" -vf fps=10,scale=320:-1:flags=lanczos,palettegen ""$palleteTemp"" *>&1 |% {}
        Write-Verbose "ffmpeg -i ""$Item"" -i ""$palleteTemp"" -filter_complex ""fps=10,scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"" ""$gifPath"""

        $duration = $null
        $current = $null
        ffmpeg -y -i ""$Item"" -i ""$palleteTemp"" -filter_complex "fps=10,scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse" ""$gifPath"" *>&1 | % {
            if($_ -match "Duration: (?<DURATION>\d\d:\d\d:\d\d.\d\d)"){
                $duration = [TimeSpan]::Parse($Matches.DURATION)
            }

            if(($_ -match "^frame=") -and ($_ -match "time=(?<CURRENT>\d\d:\d\d:\d\d.\d\d)")){
                $current = [TimeSpan]::Parse($Matches.CURRENT)
                $percentage = (($current.TotalMilliseconds/$duration.TotalMilliseconds)*100.0)
                Write-Progress -Activity "From [$Item] -> [$gifPath]" -Status "Converting..." -PercentComplete $percentage
            }
        }
    }
}
