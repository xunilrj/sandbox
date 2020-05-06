const puppeteer = require('puppeteer');
var stdin = process.openStdin();

(async () => {
    const browser = await puppeteer.launch({
        headless: false,
        //executablePath: "C:/Program Files (x86)/BraveSoftware/Brave-Browser/Application/brave.exe"
    });
   
    const page = await browser.newPage();
    page.goto('https://edx.org');

    console.log("print?")
    stdin.addListener("data", async function(d) {
        // const pages = await browser.pages();
        // const p0 = pages[0];
        const title = await page.title();
        await page.pdf({path: title + '.pdf', format: 'A4'});
        console.log("print?");
    });
})();



