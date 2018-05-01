import React, { Component } from 'react';
import { render } from 'react-dom';
import Hello from './Hello';
import './style.css';

const diff = (A,B) => {
  return [A[0]-B[0],A[1]-B[1]];
}

const sum = (A,B) => {
  return [A[0]+B[0],A[1]+B[1]];
}

const length = (A) => {
  return Math.sqrt((A[0]*A[0])+(A[1]*A[1]));
}

const dot = (A,B) => {
  return (A[0]*B[0])+(A[1]*B[1]);
}

const mul = (alpha, A) => {
  return [A[0]*alpha,A[1]*alpha];
}

const clamp = (alpha, min, max) => {
  const r = (alpha > max) ? max : alpha;
  return r < min ? min : r;
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
    const {ball1X, ball1Y, ball1R, ball1YV} = this.props;
    const {ball2X, ball2Y, ball2R, ball2YV} = this.props;
    const {ball3X, ball3Y, ball3R, ball3YV} = this.props;
    const A = [ball1X,ball1Y];
    const VA = [0,ball1YV];
    const B = [ball2X,ball2Y];
    const VB = [0,ball2YV];
    const C = [ball3X,ball3Y];   
    const VC = [0,ball3YV];

    const BA = diff(B,A);
    const Z = dot(BA,A);
    const Y = dot(BA,B);
    const X = dot(BA,C);

    const alpha = clamp((X-Z)/(-Z+Y), 0.0, 1.0);
    const xAlpha = sum(A,mul(alpha,BA));
    
    const distance = length(diff(C,xAlpha)) - ball1R - ball3R;
    const isPenetrating = distance < 0;

    const CXAlpha = diff(C,xAlpha);
    const vAlpha = sum(mul(1-alpha,VA),mul(alpha,VB));
    const isReceding = dot(CXAlpha, diff(VC,vAlpha)) > 0;

    const isColliding = isPenetrating && !isReceding;
    
    return (
      <div width="100%">
        <svg width="800" height="250">
          <circle cx={ball1X} cy={ball1Y} r={ball1R} fill="red" />
          <line x1={ball1X} y1={ball1Y} x2={ball1X} y2={ball1Y+ball1YV} stroke="blue" />
          <circle cx={ball2X} cy={ball2Y} r={ball2R} fill="red" />          
          <line x1={ball2X} y1={ball2Y} x2={ball2X} y2={ball2Y+ball2YV} stroke="blue" />
          <circle cx={ball3X} cy={ball3Y} r={ball3R} fill="green" />         
          <line x1={ball3X} y1={ball3Y} x2={xAlpha[0]} y2={xAlpha[1]} stroke="black" />          
          <line x1={ball3X} y1={ball3Y} x2={ball3X} y2={ball3Y+ball3YV} stroke="red" />
          <line x1={ball1X} y1={ball1Y+ball1R} x2={ball2X} y2={ball2Y+ball1R} stroke="red" />
          <line x1={ball1X} y1={ball1Y-ball1R} x2={ball2X} y2={ball2Y-ball1R} stroke="red" />
          <line x1={ball1X} y1={ball1Y} x2={ball2X} y2={ball2Y} stroke="black" />
          
        </svg>
        <div>IsPenetrating:</div>
        <div>{isPenetrating ? <span>true</span> : <span>false</span>}</div>
        <div>IsReceding:</div>
        <div>{isReceding ? <span>true</span> : <span>false</span>}</div>
        <div>IsColliding:</div>
        <div>{isColliding ? <span>true</span> : <span>false</span>}</div>
      </div>
    );
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      name: 'React',
      ball1X: 50,  ball1Y: 100, ball1R: 50, ball1YV: 10,
      ball2X: 270, ball2Y: 100, ball2R: 50, ball2YV: 10,
      ball3X: 150, ball3Y: 230, ball3R: 50, ball3YV: 15
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
        <div><label>Ball1</label></div>
        <div>
          <input style={{width: 40}} type="number" value={this.state.ball1X} onChange={this.change("ball1X")} />
          <input style={{width: 40}} type="number" value={this.state.ball1Y} onChange={this.change("ball1Y")} />
          <input style={{width: 40}} type="number" value={this.state.ball1R} onChange={this.change("ball1R")} />
          <input style={{width: 40}} type="number" value={this.state.ball1YV} onChange={this.change("ball1YV")} />
        </div>
        <div><label>Ball2</label></div>
        <input style={{width: 40}} type="number" value={this.state.ball2X} onChange={this.change("ball2X")} />
        <input style={{width: 40}} type="number" value={this.state.ball2Y} onChange={this.change("ball2Y")} />
        <input style={{width: 40}} type="number" value={this.state.ball2R} onChange={this.change("ball2R")} />
        <input style={{width: 40}} type="number" value={this.state.ball2YV} onChange={this.change("ball2YV")} />
        <div><label>Ball3</label></div>
        <input style={{width: 40}} type="number" value={this.state.ball3X} onChange={this.change("ball3X")} />
        <input style={{width: 40}} type="number" value={this.state.ball3Y} onChange={this.change("ball3Y")} />
        <input style={{width: 40}} type="number" value={this.state.ball3R} onChange={this.change("ball3R")} />
        <input style={{width: 40}} type="number" value={this.state.ball3YV} onChange={this.change("ball3YV")} />
        <World {...this.state}/>
      </div>
    );
  }
}

render(<App />, document.getElementById('root'));
