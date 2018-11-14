const puppeteer = require('puppeteer');
const fs = require('fs');

Array.prototype.forEachAsync = async function (fn) {
    for (let t of this) { await fn(t) }
}
function sleep (time) {
    return new Promise((resolve) => setTimeout(resolve, time));
}

async function getLinks(page)
{
    await page.goto("http://www.gcatholic.org/dioceses/country/BR.htm");
    await sleep(1000);
    return await page.evaluate(() => {
        var arr = Array.from(document.getElementsByClassName("typedioc"));        
        return arr
            .map(x=>x.href)
            .filter(x => !x.includes("#"))
    });
}

async function parsePage(page, url)
{    
    await page.goto(url);
    await sleep(1000);
    return JSON.parse(
        JSON.stringify(await page.evaluate(async () => {
            return gc.markers.map(x => {
                var obj = {position:x.position, title:x.title};
                
                var el = document.createElement('html');
                el.innerHTML = x.win.content;
                var l = el.querySelectorAll('a.b');
                if(l.length == 1) {
                     obj.href = l[0].href;
                }

                return JSON.stringify(obj); 
            });
        })) 
    );
}

(async () => {
    console.log('Starting puppeteer.')
    const browser = await puppeteer.launch();
    const page = await browser.newPage();

    console.log('Getting links...')
    const links = await getLinks(page);
    let i = 1;
    console.log("Size: " + links.length);
    await links.forEachAsync(async x => {
        //console.log(`${i} of ${links.length} ${x}`);
        process.stdout.write(`\r${(i).toFixed(0)}% Url: ` + x);
        try
        {
            const currentItems = await parsePage(page, x);        
            
            var stream = fs.createWriteStream("churches.txt");
            stream.once('open', function(fd) {
                currentItems.forEach(x => {
                        stream.write(x);
                });
                stream.end();
            });
        }
        catch
        {
            //console.log("error");
        }
        ++i;
    });

    console.log('Close.');
    await browser.close();
})();