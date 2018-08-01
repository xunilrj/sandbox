<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width">
  <title>JS Bin</title>
</head>
<body>
  <canvas id="backbuffer" width="800" height="500">
  </canvas>
  <script>
    function random(){ return (Math.random()-0.5)*2.0; }
    function isInsideAABB(pos, posAABB, sizeAABB){
      if(pos[0] < posAABB[0]) return false;
      if(pos[1] < posAABB[1]) return false;
      if(pos[0] > (posAABB[0] + sizeAABB[0])) return false;
      if(pos[1] > (posAABB[1] + sizeAABB[1])) return false;
      return true;
    }
    function GameState (){
      this.constants = {
        ballSize: 10,
        courtHeight: 500,
        minVel: 100,
        incVel: 1.05,
        paddleSize: [20,50],
        paddleX: [0, 500]
      }
      this.world = Object.create(this.constants);
      this.world.right = 250;
      this.world.rightScore = 0;
      
      this.resetMatch();
      this.you = Object.create(this.world);
      this.you.left = 250;
      this.you.leftScore = 0;
     
      var x = this;
      var handler = {
        get: function(obj, prop){                
          return x.you[prop];
        },
        set: function(obj, prop, value){          
          throw "Read-Only"
        }
      };
      this.commands = [];
      this.Data = Object.create(this.you);
    };
    GameState.prototype.resetMatch = function(){
      this.world.ball = {
          startPosition: [250.0, 250.0],
          pos: [250.0,250.0],
          vel: [random()*500,
                0]
      };
    }
    GameState.prototype.addCommand = function(c){
      this.commands.push(c);
    }
    GameState.prototype.stepLeft = function(f){
      this.you.left = Math.min(
          Math.max(0, this.you.left + f), 300
      );
    };
    GameState.prototype.step = function(dt){      
      var startPosition = this.world.ball.startPosition;
      var ballPos = this.world.ball.pos;      
      var ballVel = this.world.ball.vel;      
      ballPos[0] += this.world.ball.vel[0]*dt;
      ballPos[1] += this.world.ball.vel[1]*dt;      
      
      if(ballPos[0] > this.constants.paddleX[1] + this.constants.paddleSize[0]){
        ballPos[0] = startPosition[0];
        this.you.leftScore++;
        this.resetMatch();
      } else if (ballPos[0] < 0) {
        ballPos[0] = startPosition[0]
        this.world.rightScore++;
        this.resetMatch();
      }
      
      var minBottom = this.constants.courtHeight - this.constants.ballSize;
      if(ballPos[1] > minBottom && ballVel[1] > 0){ 
        ballPos[1] = minBottom;
        ballVel[1] *= - this.constants.incVel;
      } else if(ballPos[1] < 0 && ballVel[1] < 0){        
        ballPos[1] = 0;
        ballVel[1] *= - this.constants.incVel;
      }
      
      if(isInsideAABB(ballPos, [this.constants.paddleX[0], this.you.left], this.constants.paddleSize)
        && ballVel[0] < 0)
      {          
        //ballPos[0] = this.constants.paddleSize;
        ballVel[0] *= - this.constants.incVel;
        ballVel[1] += Math.random() * 10;
      } 
      
      if(isInsideAABB(ballPos, [this.constants.paddleX[1], this.world.right], this.constants.paddleSize)
        && ballVel[0] > 0)
      {          
        //ballPos[0] = this.constants.paddleSize;
        ballVel[0] *= - this.constants.incVel;
        ballVel[1] += Math.random() * 10;
      } 
    }
    var state = new GameState();
    var last = null;
    var canvas = document.getElementById('backbuffer');
    var ctx = canvas.getContext('2d');
    function step(timestamp) {    
      if (!last) last = timestamp;
      var dt = (timestamp - last)/1000.0;
      state.commands.forEach(function(x){
        if(x.type == "left.up") state.stepLeft(-5);
        else if(x.type == "left.down") state.stepLeft(+5);
      });
      if(state.commands.length > 0) console.log(state.commands);
      state.commands = [];
      var data = state.Data;
      
      state.step(dt);
      
      ctx.globalCompositeOperation = 'destination-over';
      ctx.clearRect(0, 0, 800, 500); // clear canvas
            
      ctx.fillRect(data.paddleX[0], data.left, data.paddleSize[0], data.paddleSize[1]);
      ctx.fillRect(data.paddleX[1], data.right, data.paddleSize[0], data.paddleSize[1]);
      ctx.fillRect(data.ball.pos[0], data.ball.pos[1], data.ballSize, data.ballSize);
      
      //ctx.font = "bold 16px Arial";
      ctx.fillText("Score", 250, 10);
      ctx.fillText(data.leftScore, 200, 30);
      ctx.fillText(data.rightScore, 300, 30);
      
      last = timestamp;
      window.requestAnimationFrame(step);      
    }
    window.requestAnimationFrame(step);
    document.addEventListener("keydown", function(e){
      if(e.keyCode == 38) state.addCommand({type:"left.up"});
      else if(e.keyCode == 40) state.addCommand({type:"left.down"});
      return false;
    }, false);
    document.addEventListener("keyup", function(e){
    }, false);
  </script>
</body>
</html>