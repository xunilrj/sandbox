const puppeteer = require('puppeteer');

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
        return Array.from(document.getElementsByClassName("typedioc"))
            .map(x=>x.href)
            .filter(x => !x.includes("#"))
    });
}

async function parsePage(page, url)
{
    await page.goto(url);
    await sleep(1000);
    return JSON.parse(
        JSON.stringify( await page.evaluate(async () => {
            return gc.markers.map(x=> JSON.stringify({position:x.position,title:x.title}))
        }) ) 
    );
}

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();

  const links = await getLinks(page);
  let i = 1;
  await links.forEachAsync(async x => {
    //console.log(`${i} of ${links.length} ${x}`);
    try
    {
        const currentItems = await parsePage(page, x);
        currentItems.forEach(x => console.log(x));        
    }
    catch
    {
        //console.log("error");
    }
    ++i;
  });

  await browser.close();
})();