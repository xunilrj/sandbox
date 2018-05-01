import React, { Component } from 'react';
import { render } from 'react-dom';
import Hello from './Hello';
import './style.css';

const diff = (A,B) => {
  return [A[0]-B[0],A[1]-B[1]];
}
const length = (A) => {
  return Math.sqrt((A[0]*A[0])+(A[1]*A[1]));
}

const dot = (A,B) => {
  return (A[0]*B[0])+(A[1]*B[1]);
}

const checkIsColliding = (X1,R1,V1,X2,R2,V2) => {
    const diffX1X2 = diff(X1,X2);
    const distanceC2C = length(diffX1X2);
    const penetrating = (distanceC2C - R1 - R2) <= 0; 
    
    const diffVel = diff(V1,V2);
    const receding = dot(diffVel, diff(X1,X2)) > 0;

    return penetrating && !receding;
}

class World extends Component {
  render() {
    const {ball1X, ball1Y, ball1R, ball1XV} = this.props;
    const {ball2X, ball2Y, ball2R, ball2XV} = this.props;
    const distanceX = (ball1X - ball2X);
    const distanceY = (ball1Y - ball2Y);
    const distanceC2C = Math.sqrt((distanceX*distanceX)+(distanceY*distanceY));
    const distance = distanceC2C - ball1R - ball2R;
    const penetrating = distance <= 0; 

    const vel1PosX = ball1X + ball1XV;    
    const vel2PosX = ball2X + ball2XV;
    const diffVelX = ball1XV - ball2XV;
    const receding = (diffVelX*distanceX) > 0;

    const isColliding = checkIsColliding(
      [ball1X,ball1Y],ball1R,[ball1XV,0],
      [ball2X,ball2Y],ball2R,[ball2XV,0]);
    return (
      <div width="100%">
        <svg width="800" height="250">
          <circle cx={ball1X} cy={ball1Y} r={ball1R} fill="red" />
          <circle cx={vel1PosX} cy={ball1Y} r={ball1R} fill="red" fillOpacity="0.4" />
          <line x1={ball1X} y1={ball1Y} x2={vel1PosX} y2={ball1Y} stroke="green" strokeWidth={2} />
          <circle cx={ball2X} cy={ball2Y} r={ball2R} fill="blue" />
          <circle cx={vel2PosX} cy={ball2Y} r={ball2R} fill="blue" fillOpacity="0.4" />
          <line x1={ball2X} y1={ball2Y} x2={vel2PosX} y2={ball2Y} stroke="green" strokeWidth={2} />
        </svg>
        <div>Distance</div>
        <div>DistanceX: {distanceX}</div>
        <div>Distance (Center to Center): {distanceC2C}</div>
        <div>Distance - R1 - R2: {distance}</div>
        <div>Velocity</div>
        <div>Diff Vel X: {diffVelX}</div>
        <div>Receding:{receding ? <span>true</span> : <span>false</span>}</div>
        <div>Colision</div>
        <div>Colliding:{isColliding ? <span style={{color:"red"}}>true</span> : <span>false</span>}</div>
      </div>
    );
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      name: 'React',
      ball1X: 50, ball1Y: 100, ball1R: 100, ball1XV: 10,
      ball2X: 270, ball2Y: 100, ball2R: 100, ball2XV: -10
    };
    this.change = (name) => (e) => {      
      const newValue = parseInt(e.target.value);
      const newObject = {};
      newObject[name] = newValue;
      console.log(name + ":" + newValue);
      const newState = Object.assign({}, this.state, newObject);
      this.setState(newState);
    }
  }
  render() {
    return (
      <div>
        <div><label>Ball1 X</label></div>
        <div>
          <input style={{width: 40}} type="number" value={this.state.ball1X} onChange={this.change("ball1X")} />
          <input style={{width: 40}} type="number" value={this.state.ball1Y} onChange={this.change("ball1Y")} />
          <input style={{width: 40}} type="number" value={this.state.ball1R} onChange={this.change("ball1R")} />
          <input style={{width: 40}} type="number" value={this.state.ball1XV} onChange={this.change("ball1XV")} />
        </div>
        <div><label>Ball2 X</label></div>
        <input style={{width: 40}} type="number" value={this.state.ball2X} onChange={this.change("ball2X")} />
        <input style={{width: 40}} type="number" value={this.state.ball2Y} onChange={this.change("ball2Y")} />
        <input style={{width: 40}} type="number" value={this.state.ball2R} onChange={this.change("ball2R")} />
        <input style={{width: 40}} type="number" value={this.state.ball2XV} onChange={this.change("ball2XV")} />
        <World {...this.state}/>
      </div>
    );
  }
}

render(<App />, document.getElementById('root'));
