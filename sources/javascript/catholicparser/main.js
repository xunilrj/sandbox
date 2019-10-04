const puppeteer = require('puppeteer');
const fs = require('fs');
const sql = require('mssql');

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
            var obj = {};
            Array.from(document.querySelectorAll(".entries p")).forEach(x => {
                var label = x.querySelector("span.label");
                if(label && label.innerText.startsWith("Continent")){
                    obj.Continent = x.children[1].innerText.trim();                    
                } else if(label && label.innerText.startsWith("Rite")){
                    obj.Rite = {
                        Type: x.children[1].innerText.trim(),
                        Subtype: x.children[2].innerText.trim()
                    };
                } else if(label && label.innerText.startsWith("Type")){
                    obj.Type = x.children[1].innerText.trim();                    
                } else if(label && label.innerText.startsWith("Area")){
                    obj.Area = x.lastChild.data.trim();                    
                } else if(label && label.innerText.startsWith("Population")){
                    var stat = x.children[1].innerText.split("Catholics")[0].replace(/\./g,"").replace(/\,/g,"").trim();
                    var asof = x.children[2].innerText.replace(/\(/g,"").replace(/\)/g,"").trim();
                    obj.Population = {
                        Catholics: parseInt(stat),
                        AsOf: parseInt(asof),
                    };
                } else if(label && label.innerText.startsWith("Statistics")){
                    var stat = x.children[1].innerText.replace(/\(/g,",").replace(/\)/g,"").split(",");
                    obj.Statistics = { };
                    stat.forEach(xx => {
                        var parts = xx.trim().split(" ");
                        obj.Statistics[parts[1]] = parts[0];
                    })
                } else if(label && label.innerText.startsWith("Ecclesiastical Province")){
                    obj.EcclesiasticalProvince = {
                        Name: x.children[1].innerText.trim(),
                        href: x.children[1].href.trim(),
                    }
                } else if (label && label.innerText.startsWith("Neighbouring Dioceses")) {
                    const as = Array.from(x.querySelectorAll("a.typedioc"));
                    obj.Neighbouring = { 
                        Dioceses: [],
                        Archdiocese: []
                    }
                    as.forEach(xx => {
                        obj.Neighbouring.Dioceses.push({
                            Name: xx.innerText,
                            href: xx.href
                        });
                    });

                    const metr = Array.from(x.querySelectorAll("a.typemetr"));
                    metr.forEach(xx => {
                        obj.Neighbouring.Archdiocese.push({
                            Name: xx.innerText,
                            href: xx.href
                        });
                    });
                } else if(label && label.innerText.startsWith("Depends on")) {
                    obj.DependsOn = {
                        Name: x.children[1].innerText.trim(),
                        href: x.children[1].href,
                    }
                } else if(label && label.innerText.startsWith("Patron Saint")) {
                    const parts = x.lastChild.data.replace(/\(/g,",").replace(/\)/g,"").trim().split(",");
                    obj.PatronSaint = {
                        Name: parts[0].trim(),
                        href: parts[1].trim(),
                    }
                } else if(label && label.innerText.startsWith("Websites")) {
                    const as = Array.from(x.querySelectorAll("a"));
                    obj.Websites = [];
                    as.forEach(xx => {
                        obj.Websites.push({
                            Name: xx.innerText,
                            href: xx.href
                        });
                    });
                }           
            });
            o = jQuery("<div id='map2rec' style='display:none'></div>");
            jQuery("#showcap").after(o);
            o.show();
            sm();
            obj.Churches = gc.markers.map(x => {
                var xx = {lat:x.position.lat(), long: x.position.lng(), title:x.title};
                var el = document.createElement('html');
                el.innerHTML = x.win.content;
                var l = el.querySelectorAll('a.b');
                if(l.length == 1) {
                     xx.href = l[0].href;
                }
                return xx; 
            });
            return obj;
        })) 
    );
}

if(!process.argv[2] || process.argv[2] == "download")
{
    (async () => {
        console.log('Starting puppeteer.')
        const browser = await puppeteer.launch();
        const page = await browser.newPage();

        console.log('Getting links...')
        const links = await getLinks(page);
        let i = 1;
        console.log("Size: " + links.length);
        await links.forEachAsync(async x => {
            const parts = x.split("/");
            const name = parts[parts.length-1].split(".")[0];
            const targetFile = `scrapped/${name}.json`;
            process.stdout.write(`\r${(i * 100 / links.length).toFixed(2)}% Url: ` + x + " -> " + targetFile);
            try
            {
                const currentItems = await parsePage(page, x);
                console.log(currentItems);        
                fs.writeFileSync(targetFile, JSON.stringify(currentItems, null, 4));
            }
            catch (e)
            {
                console.error(e);
            }
            ++i;
        });
        await browser.close();

        
    })();
} else if (process.argv[2] == "process") {
    (async () => {
        var churches = [];

        var files = fs.readdirSync("scrapped");
        files.forEach(x => {
            var txt = fs.readFileSync("scrapped/" + x);
            var obj = JSON.parse(txt);

            obj.Churches.forEach(xx => {
                churches.push(xx);
            })
        });

        await sql.connect('mssql://sa:12345678a@localhost/Churches');
        await sql.query`TRUNCATE TABLE [dbo].[Churches]`;

        let i = 0;
        await churches.forEachAsync(async xx => {
            ++i;
            process.stdout.write(`\r${(i * 100 / churches.length).toFixed(2)}%`);
            
            const request = new sql.Request()
            await request.query(`INSERT INTO [dbo].[Churches] (Latitude, Longitude, Name) VALUES (${xx.lat}, ${xx.long}, '${xx.title}')`);

            
        });        
    })();
}