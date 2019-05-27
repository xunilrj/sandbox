function trimEmptyLines($lines)
{
    $emptyStore = @()
    $notEmpty = 0
    $lines |% {
        $isEmpty = [System.String]::IsNullOrEmpty($_.Trim())
        if($isEmpty) {
            $emptyStore += $_
        }
        if($isEmpty -eq $false) {
            $notEmpty += 1
            if($notEmpty -gt 1){
                Write-Output $emptyStore
            }
            $_
            $emptyStore = @()
        }
    }
}

function tob64($text)
{
    $Bytes = [System.Text.Encoding]::UTF8.GetBytes($text)
    [Convert]::ToBase64String($Bytes)
}

function Parse($Path)
{
    $fileLines = cat $Path

    # 0 = waiting block
    # 1 = all lines until next #
    $state = 0
    $blocks = @()
    $lines = @()
    $linesRaw = $false
    $lastBlock
    $searchFor = "# "

    function endBlock () {
        if($linesRaw -eq $false) {
            $lines = @(trimEmptyLines $lines)
        }
        if($lastBlock -ne $null) {            
            $lastBlock | Add-Member -MemberType NoteProperty -Name Lines -Value $lines
            $lastBlock
        }
    }

    $fileLines |% {
        $x = $_        

        if($x.StartsWith($searchFor)) {
            endBlock

            $lastBlock = $null
            $linesRaw = $false
            $searchFor = "# "

            $state = 1
            $params = $x.Split(' ')

           
                
            $name = $params[1]            
            if($name -eq "header"){
                $level = $params[2]                              
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;
                    Level=[System.Int32]::Parse($level.ToString());
                }                
            } elseif($name -eq "paragraph"){                                             
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;                    
                } 
            } elseif($name -eq "ol"){                                             
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;                    
                } 
            } elseif($name -eq "link"){
                $selector = $params[2]                                            
                $i = $params[3]
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;
                    Selector = $selector;
                    I = $i                  
                } 
            } elseif($name -eq "quotelink"){                                             
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;                    
                } 
            } elseif($name -eq "code"){
                $linesRaw = $true
                $searchFor = "#####"

                $lang = $params[2]
                $lines = @()
                $lastBlock = New-Object PSCustomObject -Property @{
                    Name=$name;
                    Language = $lang                
                } 
            }

            $definesEnd = ($params | select -Last 1).StartsWith("end=")
            if($definesEnd) {
                $searchFor = ($params | select -Last 1).Split("=")[1]
            }
        } else {
            $lines += $x
        }         
    }
    endBlock
}

function generateHeader($block) {
@"
<h$($block.Level) class="header">$($block.Lines[0])</h$($block.Level)>
"@
}

function generateParagraph($block) {
    $block.Lines |% {
        if([System.String]::IsNullOrEmpty($_) -eq $false){
@"
<p class="paragraph">$_</p>
"@
        }
    }
}

function generateCode($block) {
$code = $_.Lines -join [System.Environment]::NewLine
@"
<div class="code" data-language="$($_.Language)" style="height:200px">
</div>
<script>
    var x = document.currentScript.previousElementSibling;
    monacoElements.push({
        value: "$(tob64 $code)",
	    el: x,
    });
</script>
"@
}

function generateLink($block) {
$get = wget $block.Lines[0]
$NodeList = $get.ParsedHtml.querySelectorAll($block.Selector)
$innerText = $NodeList.item($block.I).innerText

$maxSize = 300
if($innerText.Length -gt $maxSize) {
    $innerText = $innerText.Substring(0, $maxSize-3) + "..."
}

$code = ($_.Lines | %{$_.Replace("<","&lt;").Replace(">","&gt;")}) -join [System.Environment]::NewLine
@"
<div class="link" style="border: solid 1px">
    <h5>$($get.ParsedHtml.title)</h5>
    <p>$innerText</p>
    <a href="$($block.Lines[0])">$($block.Lines[0])</a>
</div>
"@
}

function generateQuoteLink($block) {
$quotes = $block.Lines |Select -SkipLast 1
$url = $block.Lines |Select -Last 1
@"
<div class="quoteLink" style="margin-left:100px">
"@
    $quotes |% {
        if([System.String]::IsNullOrEmpty($_) -eq $false){
@"
<p class="paragraph">$_</p>
"@
        }
    }
@"
<a href="$($url)">$($url)</a>
</div>
"@
}

function generateOL($block) {
@"
<ol cl="ol">
"@
$block.Lines |% {
@"
    <li>$($_)</li>
"@
}
@"
</ol>
"@
}

function Generate($blocks)
{
@"
<script type="text/javascript" src="https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs/loader.js""></script>
<script>
    var monacoElements = [];
</script>
"@
    $blocks |% {
        if($_.Name -eq "header"){ generateHeader $_ }
        if($_.Name -eq "paragraph"){ generateParagraph $_ }
        if($_.Name -eq "code"){ generateCode $_ }
        if($_.Name -eq "link"){ generateLink $_ }
        if($_.Name -eq "quotelink"){ generateQuoteLink $_ }
        if($_.Name -eq "ol"){ generateOL $_ }
    }
@"
<script>
  require.config({ paths: { 'vs': 'https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs' }});
  window.MonacoEnvironment = {
    getWorkerUrl: function(workerId, label) {
      return ``data:text/javascript;charset=utf-8,`${encodeURIComponent(``
        self.MonacoEnvironment = {
          baseUrl: 'https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min'
        };
        importScripts('https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs/base/worker/workerMain.js');``
      )}``;
    }
  };

  require(["vs/editor/editor.main"], function () {    
    monacoElements.forEach(x => {
        x.el.innerText = "";
        var editor = monaco.editor.create(x.el, {
            automaticLayout:true,
	        value: window.atob(x.value),
	        language: x.el.dataset.language,
            scrollBeyondLastLine: false,
            minimap: {
	        	enabled: false
	        }
        });
        x.el.style.height = 0;
        editor.layout();
        let height = editor.getScrollHeight();        
        x.el.style.height = height + 20;
        editor.layout();
    });
  });
</script>
"@
}

function Save($Path, $lines)
{
    [System.Environment]::CurrentDirectory = (gl).Path
    $info = [System.IO.FileInfo]::new($Path)
    mkdir $info.Directory.FullName -Force | Out-Null
    [System.IO.File]::WriteAllLines($Path, $lines)
}