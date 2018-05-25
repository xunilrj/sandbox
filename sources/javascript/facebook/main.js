const puppeteer = require('puppeteer');

(async() => {
  const browser = await puppeteer.launch();
  console.log(await browser.version());
  await browser.close();
})();