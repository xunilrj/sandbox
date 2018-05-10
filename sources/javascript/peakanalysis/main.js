p = [1,1,1.1,1,0.9,1,1,1.1,1,0.9,1, 1.1,1,1,0.9,1,1,1.1,1,1,1,1,1.1,0.9, 1,1.1,1,1,0.9,1,1.1,1,1,1.1,1,0.8,0.9,1,1.2, 0.9,1,1,1.1,1.2,1,1.5,1,3,2,5,3,2,1,1,1,0.9,1,1,3,
    2.6,4,3,3.2,2,1,1,0.8,4,4,2,2.5,1,1,1];
    
    const zeros = (size) => new Array(size).fill(0);
    const sum = (x, s, e) => x.slice(s,e).reduce((acc,i) => acc+i);
    const mean = (x, s, e) => sum(x,s,e)/(e-s);
    const abs = (x) => Math.abs(x);
    const std = (x, s, e) => {
        let m = mean(x,s,e);  
      let v = x.slice(s,e).reduce((acc,i) => acc + ((i-m)*(i-m)),0);
      return Math.sqrt(v/(e-s-1));
    } 
    const movingMean = (x, lag) => {
        var means = [];
        var current = 0;
        for(var i = 0; i < lag; ++i)
      {
          current += x[i];  
        means.push(null);
      }
      
        for(i = lag; i < x.length; ++i)
      {  	
          means.push(current/lag);
        current += x[i];
        current -= x[i-lag];
      }
      return means;
    }
    const smoothedZscore = (x, lag, threshold, influence) => {	
        let signals = zeros(x.length);
      let filteredx = zeros(x.length);
      for(var i = 0; i <= lag; ++ i) filteredx[i] = x[i];
      let meanx = zeros(x.length);  
      let stdx = zeros(x.length);
        
      meanx[lag] = mean(x, 0, lag);
      stdx[lag] = std(x, 0, lag);
      
      for(i = lag+1; i < x.length; ++i)
      { 
          //If new value is a specified number of deviations away
        const distMean = abs(x[i]-meanx[i-1]);
        const acceptable = threshold*stdx[i-1];
        if (distMean > acceptable) {
            if (x[i] > meanx[i-1]) {
              signals[i] = 1;
          } else {
              signals[i] = -1;
          }
          filteredx[i] = influence*x[i]+(1-influence)*filteredx[i-1];
        }
        // No signal
        else
        {        
                signals[i] = 0;
                filteredx[i] = x[i];
        }
        meanx[i] = mean(filteredx, i-lag, i);
        stdx[i] = std(filteredx, i-lag, i);
      }
      return [signals, meanx, stdx];
    }
    let signals = smoothedZscore(p, 5, 3.5, 0.5);
    var chart = c3.generate({
        data: {
            columns: [
                ['data', ...p],
                //['movingMean', ...movingMean(p, 5)],
                ['smoothedZscore-signal', ...signals[0]],
                ['smoothedZscore-mean', ...signals[1]],
                //['smoothedZscore-sd', ...signals[2]],
            ],
            types: {
                //'smoothedZscore-signal':'step',
              //'smoothedZscore-mean': 'area-spline',
              'smoothedZscore-sd': 'area-spline'
            },
            groups: [['smoothedZscore-mean', 'smoothedZscore-sd']]
        }
    });