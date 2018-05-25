import React, { Component } from 'react';
import { render } from 'react-dom';
import './style.css';

//https://defuse.ca/online-x86-assembler.htm#disassembly
//http://www.stephendiehl.com/posts/monads_machine_code.html

function toHex(d) {
    return  ("0"+(Number(d).toString(16))).slice(-2).toUpperCase()
}
const Zeros = (len) => new Array(len).fill(0);
function* _MatrifixyRows (size) {
  const s = Math.sqrt(size);
  for(var j = 0;j < s;++j)
  {
      yield j;
  }
}
const rows = (a) => Array.from(_MatrifixyRows(a.length));
const cols = (a) => Array.from(_MatrifixyRows(a.length));
const atR = (a, i, j) => {
  const s = Math.sqrt(a.length);
  return a[(i*s)+j];
}
const write = (mem, pos, array) => {
  for(var i = 0;i < array.length; ++i)
  {
    mem[pos+i] = array[i];
  }
};
const NewComputer = (model) =>
{
  if(model == "simple")
  {
    return {
      Explain: "",
      Registers: {
        RIP: 0,
        RAX: 0,
      },
      Memory: {
        Main:Zeros(16),
      }
    };
  }
}

class SimpleComputer extends Component {
  constructor() {
    super();
    this.renderCell = (i, j) => {
      const {Registers, Memory} = this.props;
      const main = Memory.Main;
      const k = (i*Math.sqrt(main.length))+j;
      if(k == Registers.RIP)
      {
        return (
        <td style={{borderColor:"green",borderWidth:3, borderStyle:"solid"}}>
          {toHex(atR(main,i,j))}
        </td>);
      }
      else
      {
        return (<td>
          {toHex(atR(main,i,j))}
        </td>);
      }
    };
  }
  render() {
    const {Explain, Registers, Memory} = this.props;
    const main = Memory.Main;
    return (
      <div>
        <h2>Registers</h2>
        <ol>
          {Object.keys(Registers).map(x => <li>{x}: {Registers[x]}</li>)}
        </ol>
        <h2>Memory</h2>
        <table>
          <tbody>
            {rows(main).map(i => 
            <tr>
              {cols(main).map(j => this.renderCell(i, j))}
            </tr>
            )}
          </tbody>
        </table>
        <h2>Explain</h2>
        <div>
          {Explain}
        </div>
        <h2>Commands</h2>
        <div>
          <button onClick={this.props.onLoad}>Load</button>
          <button onClick={this.props.onReset}>Reset</button>
          <button onClick={this.props.onStep}>Step</button>
        </div>
        <div>
          <textarea>{this.props.program}</textarea>
        </div>
      </div>
      );
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      name: 'React',
      program: "mov rax, 1",
      computer: NewComputer("simple"),
    };
    this.updateComputer = (f) => {
      let {computer} = this.state;
      let cloneComputer = JSON.parse(JSON.stringify(computer));
      f(cloneComputer);
      let newState = Object.assign({}, this.state, {computer:cloneComputer}); 
      this.setState(newState);
    }
    this.onReset = () => {
      this.updateComputer(x => x.Registers.RIP = 0);
    }
    this.onLoad = () => {
      var loadat = 0;
      var lines = this.state.program.split("\n");
      if(lines[0] == "mov rax, 1")
      {
        this.updateComputer(x => {
          write(x.Memory.Main, loadat, [0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00]);
        });
      }
    };
    //http://www.stephendiehl.com/posts/monads_machine_code.html
    this.onStep = () => {
      // 0100WRXB
      let parsePrefix = (byte) => { return {
          REXW: (byte & 0b00001000) >> 3,
          REXR: (byte & 0b00000100) >> 2,
          REXX: (byte & 0b00000010) >> 1,
          REXB: (byte & 0x00000001) >> 0
        };
      }
      let parseModRM = (byte) => {
        return {
          MOD: (byte & 0b11000000) >> 6,
          REG: (byte & 0b00111000) >> 3,
          RM:  (byte & 0b00000111) >> 0
        };
      };
      let parseSIB = (byte) => {
        return {
          SCALE: (byte & 0b11000000) >> 6,
          INDEX: (byte & 0b00111000) >> 4,          
          BASE:  (byte & 0b00000111) >> 0
        };
      }
      let parseOpcode = (opcode) => {
        if(opcode == 0xC7) return "MOV";
      }
      let explainMOD = (MOD) => {
        if(MOD == 3) return "R/M denotes a register and uses the REG field";
      }
      let explainREG = (REG) => {
        if (REG == 0) return "RAX";
      }
      let {Registers, Memory} = this.state.computer;
      let {RIP} = Registers;
      let {Main} = Memory;

      const step = 4;
      const byte0 = Main[RIP+0];
      const byte1 = Main[RIP+1];
      const byte2 = Main[RIP+2];
      const constant = 0;

      const {RESXW,RESXR,RESXX,RESB} = parsePrefix(byte0);
      const opcode = byte1;
      const mnemonic = parseOpcode(byte1);
      const {MOD,REG,RM} = parseModRM(byte2);
      const REGcode = explainREG(REG);
      if(RM == 0)
      {
        const byte3 = Main[RIP+3];
        const byte4 = Main[RIP+4];
        const byte5 = Main[RIP+5];
        const byte6 = Main[RIP+6];

        constant = (byte6 << 24) | (byte5 << 16) | (byte4 << 8) | byte3;
        step = 7
      }
      else
      {
        const byte3 = Main[RIP+3];
        const {SCALE,INDEX,BASE} = parseSIB(byte3);
      }
      
      this.updateComputer(x => {
        let action = "";
        if(mnemonic == "MOV")
        {
          x.Registers[REGcode] = constant;
          action = `MOVed ${constant} to ${REGcode}`;
        }
        x.Explain = (<div>
          <h3>Internals</h3>
            <div>opcode: {mnemonic} = {toHex(opcode)}</div>
            <div>MOD: {MOD} = {explainMOD(MOD)}</div>
            <div>REG: {REG} = {explainREG(REG)}</div>
            <div>RM: {RM}</div>
            <div>constant:{constant}</div>
         <h3>ACTION</h3>
          <div>{action}</div>
        </div>);
        x.Registers.RIP+=step;
      });
    };
  }

  render() {
    return (
      <div>
        <h1>x64 Emulator</h1>
        <SimpleComputer {...this.state.computer}
          program={this.state.program}
          onLoad={this.onLoad}
          onReset={this.onReset}
          onStep={this.onStep} />
      </div>
    );
  }
}

render(<App />, document.getElementById('root'));
