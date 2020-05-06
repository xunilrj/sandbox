
const TL = [
    0,   0,   0,   0,   0,
  1/3, 2/3,   0,   0,   0,
    0, 1/3, 2/3,   0,   0,
    0,   0, 1/3, 2/3,   0,
    0,   0,   0, 1/3, 2/3,
];
const TR = [
  2/3, 1/3,   0,   0,   0,
    0, 2/3, 1/3,   0,   0,
    0,   0, 2/3, 1/3,   0,
    0,   0,   0, 2/3, 1/3,
    0,   0,   0,   0,   0,
];
const TS = [
  1/2, 1/2,   0,   0,   0,
  1/4, 1/2, 1/4,   0,   0,
    0, 1/4, 1/2, 1/4,   0,
    0,   0, 1/4, 1/2, 1/4,
    0,   0,   0, 1/2, 1/2,
];

const RL = [
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
];
const RR = [
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
];
const RS = [
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
  0,   0,   0,   0,   1,
];

var data = [0,0,0,0,0];
var gamma = 0.5

function step()
{
  var accum = Array.from(data);
  for(var i = 0;i < data.length; ++i)
  {    
    var l = 0, r = 0, s = 0;

    for(var j = 0;j < data.length; ++j)
    {
      l += TL[i*5+j]*(RL[i*5+j] + gamma*data[j]);
      r += TR[i*5+j]*(RR[i*5+j] + gamma*data[j]);
      s += TS[i*5+j]*(RS[i*5+j] + gamma*data[j]);
    }

    accum[i] = Math.max(l, r, s);
  }

  data = accum;
}

data = [0,0,0,0,0];
for(var i = 0;i < 10; ++i) step();
console.log("10", data);

data = [0,0,0,0,0];
for(var i = 0;i < 100; ++i) step();
console.log("100", data);

data = [0,0,0,0,0];
for(var i = 0;i < 100; ++i) step();
console.log("200", data);