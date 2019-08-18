// Import stylesheets
import './style.css';
import {parse, text, lang, incremental} from 'bennu';
import {stream} from 'nu-stream';

const renderJson = window.renderjson
  .set_icons("+", "-")
  .set_show_to_level(10)
  .set_max_string_length(100);

function p(x) {
    //console.log(...arguments);
    return parse.always(x);
}
function toString(x) {
    return stream.foldl((a,b) => a+b, "", x);
}

function parseNumber(x) {
    var r =  stream.foldl((a,b) => a+b, "", x);
    return r.replace(/\D/g,'');
}

// Write Javascript code!
const appDiv = document.getElementById('app');
appDiv.innerHTML = `<h1>JS Starter</h1>`;

const oneSpace = text.space;
const unlimitedSpace = parse.many(text.space);

const assignOperator = text.character('=');
const endCommand = text.character(';');
const navigateOperator = text.character('.');
const openObject = text.character('{');
const closeObject = text.character('}');

const autoKeyword = text.string('auto');
const letterDigit = parse.either(
    text.letter,
    text.digit);
const identifier = parse.bind(
    parse.many(letterDigit),
    x => p(toString(x))
);

const propertyAcessor = parse.bind(
  parse.next(
    navigateOperator,
    identifier
  ), (a,b,c) => p({type: "Property", id: a}));

const numberValue = parse.bind(
    parse.manyTill(text.digit, oneSpace),
    x => p({type:"NumberConstant", value: parseNumber(x)})
);

const stringBegin = text.character('"');
const stringEnd = text.character('"');
const stringContent = parse.manyTill(text.anyChar, stringEnd);   
const stringValue =  parse.bind(
lang.between(
    stringBegin,
    stringEnd,
    stringContent
    ),
    (x) => p({type:"StringConstant", value: toString(x)})
);

const constantValue = parse.either(stringValue, numberValue);

const assignProperty = parse.binds(
 parse.enumeration(
  propertyAcessor,
  unlimitedSpace,
  assignOperator,
  unlimitedSpace,
  constantValue,
  endCommand,
  unlimitedSpace,
), (a,b,c,d,e,f,g) => p({
  type: "AssignmentExpression",
  id: a,
  init: e
}));

const expressionList = parse.eager(
    parse.many(assignProperty)
);

const objectInstance = parse.binds(    
    parse.enumeration(
        parse.optional(
            lang.then(identifier, unlimitedSpace)
        ),
        lang.between(
            parse.next(openObject, unlimitedSpace),
            closeObject,
            expressionList    
        )
    ),
(a,b,c,d) => p({
    type: "Object",
    bodyType: a,
    body: b
}));

const initVariable = parse.binds(
 parse.enumeration(
  autoKeyword,
  unlimitedSpace,
  identifier,
  unlimitedSpace,
  assignOperator,
  unlimitedSpace,
  objectInstance,
  endCommand
), (a,b,c,d,e,f,g) => p({
  type: "InitVariable",
  varType: "auto",
  id: c,
  init: g
}, 'initVariable'));

const typeKeyword = text.string("type");
const fieldDeclaration = parse.binds(
    parse.enumeration(
        identifier,
        unlimitedSpace,
        identifier,    
        endCommand,
        unlimitedSpace
    ), (a,b,c,d) => p({
        type: "FieldDeclaration", 
        typeName: a, 
        name: c}
));
const fieldList =
    parse.manyTill(
        fieldDeclaration,
        closeObject
    );
const typeDeclaration = parse.binds(
    parse.enumeration(
        typeKeyword,
        unlimitedSpace,
        identifier,
        unlimitedSpace,
        parse.eager(
            lang.between(
                parse.next(openObject, unlimitedSpace),
                closeObject,
                parse.many(fieldDeclaration),
            )
        ),
        endCommand),
(a,b,c,d,e,f,g,h) => p({
    type:"TypeDeclaration",
    id: c,
    fields: e
}, 'typeDeclaration'));

const functionCall = parse.binds(
    parse.enumeration(
        identifier,
        text.character('('),
        parse.eager(
            lang.sepBy(
                text.character(','),
                identifier
            )
        ),
        text.character(')'),
    ),
(a,b,c) => p({
    type:"FunctionCall",
    name: a,
    arguments: c
}));

const expression = parse.choice(
    initVariable,
    functionCall
);
const start = parse.eager(
    lang.sepEndBy(unlimitedSpace,
        parse.choice(
            typeDeclaration,
            expression
        )
    )
);

function toJS(AST)
{
    var code = [];
    var current = AST;

    if(Array.isArray(current))
    {
        current.forEach(x => {
            var r = toJS(x);
            code.push(r);
        });
    }
    else if(current.type == "TypeDeclaration") {
        code.push(`function ${current.id} () {\n`);
        current.fields.forEach(x => {
            code.push(`this.${x.name} = null;\n`);
        });
        code.push("}\n");
    }
    else if(current.type == "InitVariable") {
        code.push("function f1 () {\n");
        code.push(`$result = {};\n`);
        var initValue = toJS(current.init);
        code.push(`${initValue}`);
        code.push("return $result;\n");
        code.push("}\n");
        code.push(`var ${current.id} = f1();\n`);
    }
    else if(current.type == "Object") {
        current.body.forEach(x => {
            var line = toJS(x);
            code.push(`${line};\n`);
        });
    }
    else if(current.type == "AssignmentExpression") {
        var initValue = toJS(current.init);
        code.push(`$return.${current.id.id} = ${initValue}`);
    }
    else if (current.type == "StringConstant") {
        return `'${current.value}'`;
    }
    else if (current.type == "NumberConstant") {
        return current.value;
    }
    else if (current.type == "FunctionCall") {
        code.push(`${current.name}(`);
        for(var i = 0;i < current.arguments.length; ++i) {
            var x = current.arguments[i];
            code.push(x);
            if(i < current.arguments.length - 1) {
                code.push(',');
            }
        };
    }

    return code.join("");
}

function setSelectionRange(input, selectionStart, selectionEnd) {
    if (input.setSelectionRange) {
      input.focus();
      input.setSelectionRange(selectionStart, selectionEnd);
    }
    else if (input.createTextRange) {
      var range = input.createTextRange();
      range.collapse(true);
      range.moveEnd('character', selectionEnd);
      range.moveStart('character', selectionStart);
      range.select();
    }
  }

function parseCode() {
    var code = document.getElementById("code");
    try
    {
        var pr = incremental.runInc(start);        
        pr = incremental.provideString(code.value, pr);
        const r = incremental.finish(pr);

        appDiv.append(renderJson(r));
        console.log(toJS(r));
    }
    catch (e)
    {
        console.error(e);
        appDiv.append(renderJson(e));
        setSelectionRange(code, e.position.index, e.position.index + 1);
    }
}

parseCode();
document.getElementById("code").addEventListener("input", parseCode);