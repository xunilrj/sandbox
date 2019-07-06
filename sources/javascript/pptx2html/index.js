let { AirParser } = require("airppt-parser");

let pptParser = new AirParser("../../webassembly/wasm.pptx");
waitForParsing();

async function waitForParsing() {
	//pass in the slide number and wait
	let result = await pptParser.ParsePowerPoint(1);

	//returns an array of Powerpoint Elements and some extra MetaData
	console.log(result.powerPointElements[1]);
}