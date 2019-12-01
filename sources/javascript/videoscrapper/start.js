const chrome = require('puppeteer');
const firefox = require('puppeteer-firefox');
const argv = require('yargs').argv;

var exec = require('child_process').exec;
exec('pwd', function callback(error, stdout, stderr){
    // result
});

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function exe(cmd)
{
    var p = new Promise((ok,rej) =>
    {
        exec(cmd, ok);
    });
    return p;
}

var videos = [];
var i = 0;
function logRequest(r) {
    var url = r.url();
    if(url.includes(".m3u8")) {
        videos.push(`ffmpeg -y -i "${r.url()}" -c copy -bsf:a aac_adtstoasc "${argv.path}/${argv.title}${++i}.mp4"`);
    }    
  }

  async function processQueue()
  {
    var nowork = 5;
    while(nowork > 0)
    {
        if(videos.length > 0)
        {
            console.log("");
            var cmd = videos.pop();
            console.log(cmd);
            process.stdout.write("running...");
            await exe(cmd);
            process.stdout.write(" Done!");
            nowork = 5;
        }        
        await sleep(1000);
        nowork--;
        process.stdout.write(".");
    }
  }

(async () => {
  const browser = await firefox.launch({
      headless: (argv.headless || "true") == "true",
      //executablePath: 'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe'
      //executablePath: "C:/Program Files/Mozilla Firefox/firefox.exe"
  });
  const page = await browser.newPage();
  page.on('request', logRequest);

  await page.goto(argv.url);
  await processQueue();
  await browser.close();
})();