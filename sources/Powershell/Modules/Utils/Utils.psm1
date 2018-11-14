function Get-TypedText
{
    begin {
        $sb = [System.Text.StringBuilder]::new()
    }
    end {
        $sb.ToString()
    }
    process{
        $running = $true
        $cx = [System.Console]::CursorLeft
        $cy = [System.Console]::CursorTop
        do {            
            [System.Console]::CursorVisible = $true
            [System.Console]::CursorLeft = $cx
            [System.Console]::CursorTop = $cy

            $input = [System.Console]::ReadKey($true)            
            switch ($input.Key) {
                'Escape' {
                    $running = $false
                }
                'Home' {
                    $cx = 0
                }
                'DownArrow' {
                    $cy = $cy + 1
                }
                'UpArrow' {
                    $cy = $cy - 1
                }
                'RightArrow' {
                    $cx = $cx + 1
                }
                'LeftArrow' {
                    $cx = $cx - 1
                }
                'Enter' {                
                    $cx = 0
                    $cy = $cy + 1
                    $sb.Append([System.Environment]::NewLine) | Out-Null
                }
                default {
                    $cx = $cx + 1
                    Write-Host $input.keyChar
                    $sb.Append($input.keyChar) | Out-Null
                }
            }
        } while($running -eq $true)
    }
}