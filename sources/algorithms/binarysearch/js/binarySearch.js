function search(items, value, comparef, start, end)
{
    if(!comparef) comparef = (a,b) => a===b?0:(a<b)?-1:1;

    items.sort(comparef);
    return searchSorted(items, value, comparef, start, end);
}

function searchSorted(items, value, comparef, start, end)
{
    if(!items) return [null,null];
    if(!comparef) comparef = (a,b) => a===b?0:((a<b)?-1:1);
    if(start == undefined) start = 0;
    if(end == undefined) end = items.length;

    var length = end - start;
    if(length == 0) return [null, null];

    var mid = start + Math.ceil((length / 2.0) - 1);
    var v = items[mid];
    var cmp = comparef(value, v);
    if(cmp == 0) return [mid, v]
    else if (cmp == -1) return search(items, value, comparef, start, mid);
    else if (cmp == 1) return search(items, value, comparef, mid + 1, end);
    return [null,null];
}

module.exports = {search, searchSorted};
