import React, { Component } from 'react';
import { render } from 'react-dom';
import Hello from './Hello';
import './style.css';

function line(ctx, center, color, from, to, width = 3)
{
 	ctx.beginPath();
  ctx.moveTo(from[0]+center[0],from[1]+center[1]);
  ctx.lineTo(to[0]+center[0],to[1]+center[1]);
  ctx.strokeStyle = color;
  ctx.lineWidth = width;
  ctx.stroke();
}

function rect(props) {
    const {ctx, x, y, width, height} = props;
    ctx.fillRect(x, y, width, height);
}

const plot = (canvas, ctx, fn, zoom) => {
    var center = [canvas.width/2, canvas.height/2];
    
    line(ctx, center, "black", [-1000,0], [1000,0]);
    line(ctx, center, "black", [0,-1000], [0,1000]);
    
    line(ctx, center, "black", [50,-10], [50,10], 1);
    line(ctx, center, "black", [100,-10], [100,10], 1);    
    line(ctx, center, "black", [150,-10], [150,10], 1);
            
    ctx.beginPath();
    ctx.moveTo(-1000 + center[0], -fn(0/zoom) + center[1]);    
    for (var x = -1000; x < 1000; ++x) {            
      ctx.lineTo(x + center[0], -fn(x/zoom) + center[1]);
    }    
    ctx.strokeStyle = "red";
    ctx.lineWidth = 3;
    ctx.stroke(); 
};

class App extends Component {
  constructor() {
    super();
    this.state = {
      zoom: 1
    };
    this.updateZoom = (e) =>{
      const newZoom = parseInt(e.target.value);
      this.setState({zoom:newZoom});
    }
  }
   componentDidMount() {
      this.updateCanvas();
  }
  componentDidUpdate() {
      this.updateCanvas();
  }
  updateCanvas() {
    const canvas = this.refs.canvas;
    const ctx = canvas.getContext('2d');  
    ctx.clearRect(0, 0, canvas.width, canvas.height);       
    plot(canvas, ctx, x => x, this.state.zoom);
  }  
  render() {
    return (
      <div>
        <div>
          <input type="number" value={this.state.zoom} onChange={this.updateZoom}/>
        </div>
        <canvas ref="canvas" width={500} height={300}/>
      </div>
    );
  }
}

render(<App />, document.getElementById('root'));
