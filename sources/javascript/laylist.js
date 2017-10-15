//noprotect

//we could have uses a framework for this, for example ramdajs or lodash, but,
//ramdajs, does allow for then-aware-reducers and then-aware-transducers using promises
//https://github.com/ramda/ramda/issues/2081
//it only works on fantasy-land monads
//https://github.com/ramda/ramda/blob/v0.25.0/source/internal/_reduce.js
//
//and lodash, does not work with infinite iterators
//https://github.com/lodash/lodash/issues/786

//lazy-binds - they receive iterables and return iterables
const lfilter = pred => function*(iter){ for (let value of iter){var r = pred(value);if(!!r) {yield value;}} };
const ltakeWhile = f => function* (iter){ for (let value of iter){ var r = f(value); if(!r) { break; } yield value; } };

//non-lazy-cata-morphisms - they receive iterables and return (potentially) non-iterables
const length = iter => { if(iter.length) return iter.length; return Array.from(iter).length; };
const filter = pred => iter => Array.from(lfilter(pred)(iter));
const reduce = (fn, acc, iter) => { let result = acc; for (let value of iter){ result = fn({a:result,b:value}); } return result;};

//non-lazy-cata-morphisms for promises - they receive iterables and return (potentially) non-iterables
//this method does exactly the same thing as the simpler reduce, but it is using recursion instead of 
//a loop because of the promise chaining.
function reduceP(fn, acc, iter)
{
    let lacc = acc;  
    const step = (ok,fail, liter) =>
    {
        try {
            var p = liter.next();
            if(p.done)
            {
                ok(lacc);
                return;
            }
            p.value.then(x => {
                lacc = fn({a:lacc, b:x});
                step(ok, fail, liter);
            }).catch(x => fail({error:x, acc:lacc}));
        } catch (error) {
            fail({error,acc:lacc});
        }       
    }
    return new Promise((ok,fail) => {step(ok, fail, iter);});
}
//transform the iterable and returned the reduced value
const transducerP = (xf, fn, acc, iter) => reduceP(fn, acc, xf(iter));
  
//high-order functions
// allows inspection of its parameter without changing the pipelined value
// works like the WireTapping pattern 
// http://www.enterpriseintegrationpatterns.com/patterns/messaging/WireTap.html
const tap = f => x => {f(x);return x;}
// return a function that receives the same parameters as the firt function
// and return the return of the last function
// compose([a,b,c]) = x => c(b(a(x)));
const compose = fs => x => reduce(({a,b}) => b(a), x, fs);
// slice the array, if necessary
const slice = n => x => { if(x.length > n) return x.slice(0, n); return x; } 

//returns two functions to be use in the transducer/reducer composition
const acceptNHelper = n => 
{
  let data = {accepteds:0, max:n};
  return {
    //"notSatisfied" returns if we have already accept "n" items or not
    notSatisfied: () => data.accepteds < data.max,
    //"accept" will increase the accepted count
    accept: tap(({a,b}) => data.accepteds += length(b))
  };
}

//takeNFiltered will lazyly iterate "iter" until "n" items pass the "pred" filter
//returns a promise that will resolve an array with "n" items
//"iter" is a infinite iterable of promises returning arrays
//"pred" is a predicate that will run on each item of the arrays returned by the promises
export const takeNFiltered = (n, pred) => (iter) =>
{
    const accepteds = acceptNHelper(n);    
    const liftedFilter = filter(pred);
    const filterCurrentItem = ({a,b}) => ({ a, b: liftedFilter(b) });    
    
    //transform the iterator. Used to short-circuit the infinite iterable
    //as soon as we accept "n" items we will not iterate "iter" anymore.
    //That is why "iter" can be infinite.
    const xf = ltakeWhile(accepteds.notSatisfied);
    
    const fn = compose([
        filterCurrentItem, //filter each array returned by the promises using "filter"
        accepteds.accept, //increase the count for each item that passed the filter
        ({a, b}) => a.concat(b), //flatten the array (avoid array of arrays)
        slice(n) //guarantee that just "n" items will pass (if we concat the last request we can return more than "n" items)
    ]);

    //compose the promises in one promise that will be resolved in the end of the operation
    //returning the final reduced array
    return transducerP(xf, fn, [], iter);
}
