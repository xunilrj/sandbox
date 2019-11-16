const onceSet = new Set();
console.once = function(key, obj)
{
    if(onceSet.has(key)) return;
    onceSet.add(key);
    console.log(obj);
}