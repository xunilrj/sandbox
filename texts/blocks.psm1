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
    [System.Environment]::CurrentDirectory = (gl).Path

    $info = [System.IO.FileInfo]::new($Path)
    [System.Environment]::CurrentDirectory = $info.Directory.FullName
    cd $info.Directory.FullName

    Write-Verbose $Path
    Write-Verbose $info.Directory.FullName
    Write-Verbose $info.FullName

    $fileLines = [System.IO.File]::ReadAllLines($info.FullName)

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
            } elseif($name -eq "images"){                                             
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
            } elseif($name -eq "devenv"){
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
        adjustHeight: true
    });
</script>
"@
}

function generateLink($block) {
    $block.Lines |% {        
        $url = $_    
        Write-Verbose $url

        if ($url.StartsWith("//")){

        } else {            
            $get = wget $url
            $innerText = ""
            $img = ""
    
            if($url.StartsWith("https://www.amazon.com.br")) {
                $get.Content -match "bookDescEncodedData = \`".*?\`""
                $innerText = [System.Web.HttpUtility]::UrlDecode($Matches[0].Split(" ")[2].Trim('"'))
        
                $el = $get.ParsedHtml.getElementById("img-canvas")
                $obj = $el.childNodes[0].attributes["data-a-dynamic-image"].value|ConvertFrom-Json
                $imgs = $obj|gm |? MemberType -eq NoteProperty
                $img = $imgs[0].Name
            } elseif ([System.String]::IsNullOrEmpty($block.Selector) -eq $false) {
                $get = wget $url
                $NodeList = $get.ParsedHtml.querySelectorAll($block.Selector)
                $innerText = $NodeList.item($block.I).innerText
            } else {
            }
            
            $maxSize = 300
            if($innerText.Length -gt $maxSize) {
                $innerText = $innerText.Substring(0, $maxSize-3) + "..."
            }
            
            if([System.String]::IsNullOrEmpty($img) -eq $false) {
                $innerText = "<div  style='display:inline-block'><img src='$img' width='120' /></div><div style='display:inline-block;width:90%'><p>$innerText</p></div>"
            } else {
                $innerText = "<p>$innerText</p>"
            }
@"
    <div class="link" style="border: solid 1px">
        <h5>$($get.ParsedHtml.title)</h5>    
        $innerText
        <a href="$url">$url</a>
    </div>
"@
        }

       
    }    
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

function generateImages($block) {
    $block.Lines |% {
        Write-Verbose (gl).Path
        Write-Verbose $_        
        $files = ls $_
        $files |% {
            Write-Verbose $_
            "<img src='$($_)' />"
        }
    }
}

function generateDevEnv($block) {    
    $files = $block.Lines |? {[System.String]::IsNullOrEmpty($_) -eq $false}
@"
    <div>
        <script>
            var files = [
            $($files |% {
                $info = [System.IO.FileInfo]::new($_); 
                $txt = [System.IO.File]::ReadAllText($_);
                "{name:'$($info.Name)', value:'$(tob64 $txt)'},"
            })
            ];
            var events = [];
            var obj = {
                name: files[0].name,
                value: files[0].value,
                adjustHeight: false,
                onchange: {
                    add: function(f) {
                        events.push(f);
                    },
                    call: function(str) {
                        events.forEach(function(f){
                            f(str);
                        });
                    }
                }
            }
            var editor = null;
            function switchTo(name) {
                files.forEach(function(x){
                    if(x.name === name){
                        var el = document.getElementById("code")
                        obj.onchange.call(x);
                    }
                });
            }
           
        </script>
        <div>
            $($files |% {
                $info = [System.IO.FileInfo]::new($_);
@"
<button onclick="switchTo('$($info.Name)')">$($info.Name)</button>
"@
            })
        </div>
        <div id="code" style="width:600px;height:300px;display:inline-block">
        </div>
        <script>
        obj.el = document.currentScript.previousElementSibling;
        monacoElements.push(obj);
        </script>
        <iframe style="display:inline-block">
        </iframe>        
    </div>
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
        if($_.Name -eq "devenv"){ generateDevEnv $_ }
        if($_.Name -eq "images"){ generateImages $_ }
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
        function getLanguage(name) {
            if(name) {
                if(name.indexOf('.html') >= 0) return 'html';
                if(name.indexOf('.js')  >= 0) return 'javascript';
            }
            return '';
        }
        
        x.el.innerText = "";
        var language = getLanguage(x.name);
        if(x.el.dataset.language) {
            language = x.el.dataset.language;
        }        
        if(x.language) {
            language = x.language;
        }

        var value = x.value;
        var h = {editor: null};
        function createEditor() {
            if(h.editor){
                var model = h.editor.getModel()
                if(model)
                    model.dispose();
                    h.editor.dispose();
            }
            h.editor = null;
            h.editor = monaco.editor.create(x.el, {
                automaticLayout:true,
                value: window.atob(value),
                language: language,
                scrollBeyondLastLine: false,
                minimap: {
                    enabled: false
                }
            });
        }
        createEditor();
        function resizeEditor() {
            if(x.adjustHeight) {
                x.el.style.height = 0;
                h.editor.layout();
                let height = h.editor.getScrollHeight();        
                x.el.style.height = height + 20;
                h.editor.layout();
            }
        }        
        resizeEditor();
        if(x.onchange) {
            x.onchange.add(function(xx){
                value = xx.value;
                language = getLanguage(xx.name);
                createEditor();
                resizeEditor();
            });
        }
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
    Write-Verbose "Saving to $($info.FullName)"
    [System.IO.File]::WriteAllLines($info.FullName, $lines)
}

function ParseGenerateSave($From,$To)
{
    $glpath = (gl).Path
    try
    {
        $blocks = Parse $From
        $lines = Generate $blocks
        $To |% {
            Save $_ $lines    
        }
    } finally {
        [System.Environment]::CurrentDirectory = $glpath
        cd $glpath
    }
}