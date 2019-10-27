const puppeteer = require('puppeteer'); // v 1.1.0
const { URL } = require('url');
const fse = require('fs-extra'); // v 5.0.0
const path = require('path');

async function start(urlToFetch) {
    /* 1 */
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
  
    /* 2 */
    var pageIndex = 0;
    page.on('response', async (response) => {
      const url = new URL(response.url());
      if(response.status() != 200) return;

      var query = url.search.replace("?","")
      let filePath = path.resolve(`./output${url.pathname}${query}`);

      if(url.pathname == '/books/content')
      {
        pageIndex++;
        filePath = `./output/page${response._request._requestId}`;
        var contentType = response._headers['content-type'];
        if(contentType.endsWith("png")) filePath += ".png";
        if(contentType.endsWith("gif")) filePath += ".gif";
        if(contentType.endsWith("jpg")) filePath += ".jpg";

        await fse.outputFile(filePath, await response.buffer());
      }

      
    });
  
    /* 3 */
    await page.goto(urlToFetch, {
    });

    do
    {
      var str = await page.evaluate(function(){
        var el = document.querySelectorAll(".overflow-scrolling")[0];
        var oldTop = el.scrollTop;
        el.scrollTop += 200;
        return JSON.stringify([oldTop, el.scrollTop, el.scrollHeight]);
      });
      var [oldTop, newTop, height] = JSON.parse(str);

      process.stdout.write("\r " + (newTop / height));
    } while(newTop > oldTop)
  }
  
  start('https://books.google.co.uk/books?id=Z9SrLUzJgSwC&printsec=frontcover#v=onepage&q&f=false');