"use strict";/*Compiled using Cheerp (R) by Leaning Technologies Ltd*/var y=Math.imul;var z=Math.fround;var aSlot=null;var oSlot=0;var nullArray=[null];var nullObj={d:nullArray,o:0};function s(){var f=null;f="pagetitle";f=document.getElementById(f);f.textContent;f="Literal C++ string";changeTitle(f);return;}function n(v,w){var h=0,i=0,f=null,x=null;f=String();h=v[w]|0;if((h&255)===0){return f;}else{i=0;}while(1){x=String.fromCharCode(h<<24>>24);f=f.concat(x);i=i+1|0;h=v[w+i|0]|0;if((h&255)===0){break;}}return f;}function j(f,g){var h=null;h=n(f,g);return String(h);}var u=new Uint8Array([112,97,103,101,116,105,116,108,101,0]);var t=new Uint8Array([76,105,116,101,114,97,108,32,67,43,43,32,115,116,114,105,110,103,0]);s();