var Unit = require('deadunit');
var {search} = require('./binarySearch.js');

var itemsEven = [0,2,4,6,8,10];
var itemsOdd = [0,2,4,6,8];

Unit.test('search must find', function(t) {
    this.count(itemsEven.length*2 + itemsOdd.length*2);
    itemsEven.forEach((x,i) => {
        const [pos, item] = search(itemsEven, x);
        this.ok(pos == i);
        this.ok(item == x);
    });
    itemsOdd.forEach((x,i) => {
        const [pos, item] = search(itemsOdd, x);
        this.ok(pos == i);
        this.ok(item == x);
    });
}).writeConsole();

Unit.test('search must not find (-1)', function(t) {
    this.count(itemsEven.length*2 + itemsOdd.length*2);
    itemsEven.forEach((x,i) => {
        const [pos, item] = search(itemsEven, x - 1);
        this.ok(pos == null);
        this.ok(item == null);
    });
    itemsOdd.forEach((x,i) => {
        const [pos, item] = search(itemsOdd, x - 1);
        this.ok(pos == null);
        this.ok(item == null);
    });
}).writeConsole();

Unit.test('search must not find (+1)', function(t) {
    this.count(itemsEven.length*2 + itemsOdd.length*2);
    itemsEven.forEach((x,i) => {
        const [pos, item] = search(itemsEven, x + 1);
        this.ok(pos == null);
        this.ok(item == null);
    });
    itemsOdd.forEach((x,i) => {
        const [pos, item] = search(itemsOdd, x + 1);
        this.ok(pos == null);
        this.ok(item == null);
    });
}).writeConsole();