function dot2(a,b){
  return a[0]*b[0]+a[1]*b[1];
}

function perceptron(points)
{
  var theta = [0,0];
  var t0 = 0;  
  for(var t = 0;t < 10; ++t)
  {
    var ok = true;
    points.forEach(x => {
      var d = x[2]*(dot2(theta, x) + t0);
      if(d <= 0)
      {
        ok = false;
        theta[0] += x[2]*x[0];
        theta[1] += x[2]*x[1];
        //t0 += x[2];
        
        console.log('mistake', [theta[0], theta[1], t0]);
      }
      else
      {
        console.log('ok')
      }
    });
    if(ok) break;
  }
  return [theta[0], theta[1], t0];
}

var r = perceptron([
  //[-1,-1,1], //x1
  [1,0,-1], //x2
  //[-1,1.5,1] //x3
  [-1,10,1], //new x3
  [-1,-1,1], //x1
  
]);
console.log('result', r)

