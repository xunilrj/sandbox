var fs = require("fs");

var a = JSON.parse(fs.readFileSync(process.argv[2]));
var b = JSON.parse(fs.readFileSync(process.argv[3]));

var fm = {};
var rm = {};
var r = {
    functions: [],
    records: []
};

function addf(x) {
    var name = x.name;
    if(!fm[name]) {
        fm[name] = true;
        r.functions.push(x);
    }
}
a.functions.forEach(addf);
b.functions.forEach(addf);

function addr(x) {
    var name = x.name;
    if(!rm[name]) {
        rm[name] = true;
        r.records.push(x);
    }
}
a.records.forEach(addr);
b.records.forEach(addr);

console.log(JSON.stringify(r, null, 4));