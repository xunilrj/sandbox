
const TM = [
    1, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 1, 0,
  ];
  const TC = [
      1,   0,   0,   0,   0,   0,
      0, 0.3,   0, 0.7,   0,   0,
      0,   0, 0.3,   0, 0.7,   0,
      0,   0,   0, 0.3,   0, 0.7,
      0,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   1,
  ];
  
  function R(sa,a,sb)
  {
    if(sa == 0 && sb == 0)
      return 0;
    else if(sa != sb)
    {
        return Math.abs(sb - sa)**(1/3)
    }
    else
    {
      return (sa+4)**(-1/2);
    }
  }
  
  var data = [0,0,0,0,0,0];
  var gamma = 0.6
  var stepi = 0;
  function step({printQ,printV,printPI}={})
  {
    var dl = data.length;
    var accum = Array.from(data);
    for(var i = 0;i < dl; ++i)
    {    
      var l = 0, r = 0, s = 0;
      
      for(var j = 0;j < dl; ++j)
      {      
        l += TM[i*dl+j]*(R(i,0,j) + gamma*data[j]);
        r += TC[i*dl+j]*(R(i,1,j) + gamma*data[j]); 
        //console.log(i,j,l,r, TM[i*dl+j], R(i,0,j))     
      }
  
      if(printQ)
      {
        console.log(`Q*(${i},M)=`,l)
        console.log(`Q*(${i},C)=`,r)
      }
  
      //console.log(l,r)
      accum[i] = Math.max(l, r);
  
      if(printPI)
      {
        if(l >= r) console.log("PI",stepi,i,"M");
        if(l <= r) console.log("PI",stepi,i,"C");
      }
    }
  
    data = accum;
    ++stepi;
    if(printV)
    {
      console.log("V",stepi,data)
    }
  }
  
  data = [0,0,0,0,0,0];
  for(var i = 0;i < 1; ++i) step({printPI:true});
  console.log("10", data);
  
  // data = [0,0,0,0,0,0]
  // for(var i = 0;i < 100; ++i) step();
  // console.log("100", data);
  
  // data = [0,0,0,0,0,0]
  // for(var i = 0;i < 1000; ++i) step();
  // console.log("1000", data); 