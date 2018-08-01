// Import stylesheets
import './style.css';

Array.prototype.removeIf = function (callback) {
  var i = this.length;
  while (i--) {
    if (callback(this[i], i)) {
      this.splice(i, 1);
    }
  }
};


function syntaxHighlight(json) {
  json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
    var cls = 'number';
    if (/^"/.test(match)) {
      if (/:$/.test(match)) {
        cls = 'key';
      } else {
        cls = 'string';
      }
    } else if (/true|false/.test(match)) {
      cls = 'boolean';
    } else if (/null/.test(match)) {
      cls = 'null';
    }
    return '<span class="' + cls + '">' + match + '</span>';
  });
}


function output(id, obj) {
  var el = document.getElementById(id);
  el.innerHTML = "";
  el.appendChild(document.createElement('pre')).innerHTML = syntaxHighlight(JSON.stringify(obj, undefined, 4));
}

const newObj = (prototype, init) => {
  return Object.assign(Object.create(prototype), init);
}

function loadImage(url) {
  return new Promise(resolve => {
    let i = new Image();
    i.onload = () => { resolve(i) };
    i.src = url;
  });
}

function loadMusic(url) {  
  var music = document.createElement('audio');
  document.body.appendChild(music);
  music.src = url;  
  music.loop = false;
  return Promise.resolve(music);
}

function loadSound(url) {  
  var music = document.createElement('audio');
  document.body.appendChild(music);
  music.src = url;  
  music.loop = false;
  return Promise.resolve(music);
}

function restart(el)
{
  el.pause()
  el.currentTime = 0
  el.play() 
}

class StateMachine {
  constructor() {
    this.states = {};
    this.stateStack = [];
    this.current = new Proxy(this, {
      get: function (target, prop) {
        var control = target;
        var top = target.stateStack[target.stateStack.length - 1];
        const f = target.states[top][prop];
        return f;
      }
    });
  }
  add(name, handler, start) {
    this.states[name] = handler;
    if (start) this.push(name);
  }
  push(name, data) {
    console.log("push:" + name);
    if (this.stateStack.length > 0) {
      var top = this.stateStack[this.stateStack.length - 1];
      top = this.states[top];
      if (top && top.exit) top.exit(data);
    }

    this.stateStack.push(name);

    var top = this.stateStack[this.stateStack.length - 1];
    top = this.states[top];
    if (top && top.enter) top.enter(data);
  }
}

var startScreenState = {
  handleKey(e, data) {
    if (e.keyCode == 13) {
      data.countDown = 3;
      data.pushState("countDown");      
    }
  },
  update(dt, data) {
    data.PosBackground[0] += data.VelBackground[0] * dt;
    data.PosGround[0] += data.VelGround[0] * dt;
  },
  render(data) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.drawImage(data.ImageBackground,
      data.PosBackground[0] % data.BackgroundLoopPoint,
      data.PosBackground[1]);
    ctx.drawImage(data.ImageGround,
      data.PosGround[0] % data.GroudLoopPoint,
      data.PosGround[1]);
    ctx.font = "bold 16px Arial";
    ctx.fillText("[Press Enter]", data.ScreenWidth / 2, data.ScreenHeight / 2);
  }
};

var countDownScreenState = {
  enter(data) {
    data.PosBird = [
      (data.ScreenWidth / 2) - (data.ImageBird.width / 2),
      (data.ScreenHeight / 2) - (data.ImageBird.height / 2)
    ];
    data.VelBird = [0, 0];
    data.Obstacles = [];
    data.nextObstacleIn = 1;
    data.Score = 0;
  },
  handleKey(e, commands) {
  },
  update(dt, data) {
    data.PosBackground[0] += data.VelBackground[0] * dt;
    data.PosGround[0] += data.VelGround[0] * dt;

    data.countDown -= dt;

    if (data.countDown <= 0) {
      data.pushState("game");
    }
  },
  render(data) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.drawImage(data.ImageBackground,
      data.PosBackground[0] % data.BackgroundLoopPoint,
      data.PosBackground[1]);
    ctx.drawImage(data.ImageGround,
      data.PosGround[0] % data.GroudLoopPoint,
      data.PosGround[1]);
    ctx.drawImage(data.ImageBird, data.PosBird[0], data.PosBird[1]);
    ctx.font = "bold 16px Arial";
    ctx.fillText(data.countDown.toFixed(0).toString(),
      data.ScreenWidth / 2,
      (data.ScreenHeight / 2) - 30);
  }
};

var obstacleUpdate = {
  bottomPipe: (dt, data, x) => {
    x.Pos[0] += data.VelGround[0] * dt;
    if (!x.won && x.Pos[0] < data.PosBird[0]) {
      data.Score++;
      restart(data.SoundScore);
      x.won = true;
    }

    if (x.Pos[0] < -data.ImagePipe.width) {
      x.enabled = false;
    }

    let isLeft = (data.PosBird[0] + data.ImageBird.width) < x.Pos[0];
    let isRight = data.PosBird[0] > (x.Pos[0] + data.ImagePipe.width);
    let isAbove = (data.PosBird[1] + data.ImageBird.height) < x.Pos[1];
    let isBelow = data.PosBird[1] > (x.Pos[1] + data.ImagePipe.height);
    if(!isLeft && !isAbove && !isRight && !isBelow) {
      data.pushState("gameOver");
    }
  },
  topPipe: (dt, data, x) => {
    x.Pos[0] += data.VelGround[0] * dt;
    if (!x.won && x.Pos[0] < data.PosBird[0]) {
      data.Score++;
      restart(data.SoundScore);
      x.won = true;
    }

    if (x.Pos[0] < -data.ImagePipe.width) {
      x.enabled = false;
    }
  }
}
var obstacleRender = {
  bottomPipe: (data, x) => {    
    ctx.drawImage(data.ImagePipe, x.Pos[0], x.Pos[1]);      
  },
  topPipe: (data,x) => {
    ctx.drawImage(data.ImagePipe, x.Pos[0], -x.Pos[1], 
      data.ImagePipe.width, 
      -data.ImagePipe.height);      
  }
};

var gameScreenState = {
  handleKey(e, commands) {
    if (e.keyCode == 32) commands.flap();
  },
  update(dt, data) {
    data.process(x => {
      if (x.type == "flap") {        
        data.VelBird[1] = data.FlapVel;
        restart(data.SoundFlap);
      }
    });

    data.nextObstacleIn -= dt;

    if (data.nextObstacleIn <= 0) {
      data.Obstacles.push(newObstacle(data));
      data.nextObstacleIn = 2 + Math.random() * 2;
    }

    data.PosBackground[0] += data.VelBackground[0] * dt;
    data.PosGround[0] += data.VelGround[0] * dt;

    data.VelBird[1] += data.Gravity * dt;
    data.PosBird[1] += data.VelBird[1];

    for (let i = 0; i < data.Obstacles.length; ++i) {
      let x = data.Obstacles[i];      
      obstacleUpdate[x.type](dt, data, x);
    }

    data.Obstacles.removeIf(x => !x.enabled);

    if (data.PosBird[1] > 300) {
      data.pushState("gameOver");
    }
  },
  render(data) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    ctx.drawImage(data.ImageBackground,
      data.PosBackground[0] % data.BackgroundLoopPoint,
      data.PosBackground[1]);

    data.Obstacles.forEach(x => obstacleRender[x.type](data, x));

    ctx.drawImage(data.ImageGround,
      data.PosGround[0] % data.GroudLoopPoint,
      data.PosGround[1]);
    ctx.drawImage(data.ImageBird, data.PosBird[0], data.PosBird[1]);

    ctx.font = "bold 16px Arial";
    ctx.fillText("Score: " + data.Score.toString(), 5, 20);
  }
};

var gameOverScreenState = {
  enter(data){
    console.log("enter gameover");
    data.SoundHurt.play();
    data.SoundExplosion.play();
  },
  handleKey(e, data) {
    if (e.keyCode == 13) {
      data.countDown = 3;
      data.pushState("countDown");
    }
  },
  update(dt, data) {
    data.nextObstacleIn -= dt;

    if (data.nextObstacleIn <= 0) {
      data.Obstacles.push(newObstacle(data));
      data.nextObstacleIn = 2 + Math.random() * 2;
    }

    data.PosBackground[0] += data.VelBackground[0] * dt;
    data.PosGround[0] += data.VelGround[0] * dt;

    for (let i = 0; i < data.Obstacles.length; ++i) {
      let x = data.Obstacles[i];
      if (x.type == "bottomPipe") {
        x.Pos[0] += data.VelGround[0] * dt;
        if (x.Pos[0] < -data.ImagePipe.width) {
          x.enabled = false;
        }
      }
    }

    data.Obstacles.removeIf(x => !x.enabled);
  },
  render(data) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    ctx.drawImage(data.ImageBackground,
      data.PosBackground[0] % data.BackgroundLoopPoint,
      data.PosBackground[1]);

    data.Obstacles.forEach(x => {
      if (x.type == "bottomPipe") {
        ctx.drawImage(data.ImagePipe, x.Pos[0], x.Pos[1]);
      }
    });

    ctx.drawImage(data.ImageGround,
      data.PosGround[0] % data.GroudLoopPoint,
      data.PosGround[1]);

    ctx.font = "bold 16px Arial";
    ctx.fillText("Game Over", data.ScreenWidth / 2, data.ScreenHeight / 2);
    ctx.fillText("[Press Enter]", data.ScreenWidth / 2, (data.ScreenHeight / 2) + 20);
    ctx.fillText("Score: " + data.Score.toString(), 5, 20);
  }
};

var gameState = new StateMachine();
gameState.add("startScreen", startScreenState, true);
gameState.add("countDown", countDownScreenState);
gameState.add("game", gameScreenState);
gameState.add("gameOver", gameOverScreenState);

function newObstacle(data) {
  var type = (Math.random() * 2).toFixed();
  var y = Math.random() * 200;
  //if(type == 1){
    return {
      type: "bottomPipe",
      Pos: [data.ScreenWidth + 1, 200 - y],
      won: false,
      enabled: true
    };
  //} else {
  //   return {
  //     type: "topPipe",
  //     Pos: [data.ScreenWidth + 1, 200 - y],
  //     won: false,
  //     enabled: true
  //   };
  // }  
}

const canvas = document.getElementById("backbuffer");
const ctx = canvas.getContext("2d");
let assets = {
  ImageBackground: loadImage("http://localhost:8081/background.png"),
  ImageGround: loadImage("http://localhost:8081/ground.png"),
  ImageBird: loadImage("http://localhost:8081/bird.png"),
  ImagePipe: loadImage("http://localhost:8081/pipe.png"),
  MusicBackground: loadMusic("http://localhost:8081/marios_way.mp3"),
  SoundFlap: loadMusic("http://localhost:8081/jump.wav"),
  SoundHurt: loadSound("http://localhost:8081/hurt.wav"),
  SoundExplosion: loadSound("http://localhost:8081/explosion.wav"),
  SoundScore: loadSound("http://localhost:8081/score.wav"),
};

Promise.all(Object.keys(assets).map(x => assets[x])).then(result => {
  let i = 0;
  Object.keys(assets).forEach(x => {
    assets[x] = result[i++];
  });

  let constants = newObj(assets, {
    ScreenWidth: canvas.width,
    ScreenHeight: canvas.height,
    PosBackground: [0, 0],
    VelBackground: [-10, 0],
    BackgroundLoopPoint: 413,
    GroudLoopPoint: canvas.width,
    PosGround: [0, 288 - 16],
    VelGround: [-60, 0],
    Gravity: 20,
    FlapVel: -5
  });
  let commandQueue = [];
  let commands = newObj(constants, {
    pushState: (name) => {
      commandQueue.push({ type: "changeState", newState: name })
    },
    flap: () => {
      commandQueue.push({ type: "flap" });
    },
    process: (f) => {
      commandQueue.forEach(x => {        
        if(!x.ignore) f(x)
      });
      commandQueue = [];
    }
  });
  let data = newObj(commands, {});

  document.getElementById("pauseButton").addEventListener("click", e => {
    if (data.pause) data.pause = false;
    else data.pause = true;
  });
  let lastTimestamp = null;
  const renderFrame = (timestamp) => {
    if (!lastTimestamp) {
      data.MusicBackground.play();      
      lastTimestamp = timestamp;
      requestAnimationFrame(renderFrame); 
      return;
    }
    const dt = (timestamp - lastTimestamp) / 1000.0;
    lastTimestamp = timestamp;

    if (data.pause) { requestAnimationFrame(renderFrame); return; }

    for(var i = 0;i < commandQueue.length;++i)
    {
      let x = commandQueue[i];
      if (x.type == "changeState") {
        gameState.push(x.newState, data);
        x.ignore = true;
      }
    }
    commandQueue.removeIf(x => x.ignore == true);
    gameState.current.update(dt, data);
    gameState.current.render(data);     

    output("state", data);
    requestAnimationFrame(renderFrame);
  };

  const keyDownHandler = (e) => {
    gameState.current.handleKey(e, data);
  }
  document.addEventListener("keyup", keyDownHandler, false);
  requestAnimationFrame(renderFrame);
});