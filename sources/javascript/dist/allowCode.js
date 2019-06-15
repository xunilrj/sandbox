import {parse} from 'acorn';
import {generate} from 'escodegen';
import {replace} from 'estraverse';

function instrumentCode (code) {
    var p = parse(code);
    p = replace(p, {
        enter: function (node) {
            if(node.visited) return node;
            ////console.log(generate(node), node);
            if(!node.visited && node.type == 'FunctionDeclaration') {
                node.visited = true;
                node.params.forEach(x => {
                    var paramNode = {
                        visited: true,
                        type: "ExpressionStatement",
                        expression: {
                            visited: true,
                            type: "CallExpression", 
                            callee: {type: "Identifier", name: "this._write"}, 
                            arguments: [
                                {type: "Literal", value: x.name, raw: x.name},
                                {type: "Identifier", name: x.name},
                                {
                                    type: 'ArrayExpression',
                                    elements: [
                                        {type: "Literal", value: x.start, raw: x.start},
                                        {type: "Literal", value: x.end, raw: x.end},
                                    ]
                                }
                            ]
                        }
                    }
                    node.body.body.splice(0, 0, paramNode);
                });
            }
            if(!node.visited && node.type == 'VariableDeclarator') {
                let oldInit = node.init;
                node.init = {
                    visited: true,
                    type: "CallExpression", 
                    callee: {type: "Identifier", name: "this._write"}, 
                    arguments: [
                        {type: "Literal", value: node.id.name, raw: node.id.name},
                        oldInit,
                        {
                            type: 'ArrayExpression',
                            elements: [
                                {type: "Literal", value: node.start, raw: node.start},
                                {type: "Literal", value: node.end, raw: node.end},
                            ]
                        }
                    ]
                };
            }
            if(!node.visited && node.type == 'AssignmentExpression') {
                let oldRight = node.right;
                node.right = {
                    visited: true,
                    type: "CallExpression", 
                    callee: {type: "Identifier", name: "this._write"}, 
                    arguments: [
                        {type: "Literal", value: node.left.name, raw: node.left.name},
                        oldRight,
                        {
                            type: 'ArrayExpression',
                            elements: [
                                {type: "Literal", value: node.start, raw: node.start},
                                {type: "Literal", value: node.end, raw: node.end},
                            ]
                        }
                    ]
                };
            }
            function instrumentPlusPlus(node) {
                if(node.visited) return node;
                if(node.type == 'UpdateExpression') {
                    if(node.operator == "++") {
                        let oldNode = JSON.parse(JSON.stringify(node));
                        let arg = node.argument;
                        node = {
                            visited: true,
                            type: 'AssignmentExpression',
                            left: arg,
                            operator: "=",
                            right: {
                                type: "CallExpression", 
                                callee: {type: "Identifier", name: "this._write"}, 
                                arguments: [
                                    {type: "Literal", value: arg.name, raw: arg.name},
                                    {
                                        type:'BinaryExpression',
                                        operator:'+',
                                        left: arg,
                                        right: {type: "Literal", value: 1, raw: "1"}
                                    },
                                    {
                                        type: 'ArrayExpression',
                                        elements: [
                                            {type: "Literal", value: oldNode.start, raw: oldNode.start},
                                            {type: "Literal", value: oldNode.end, raw: oldNode.end},
                                        ]
                                    }
                                ]
                           }
                        };
                    }
                }
                return node;
            }
            function instrumentBinaryTest(node) {
                var oldNode = JSON.parse(JSON.stringify(node));
                return {
                    visited: true,
                    type: "CallExpression", 
                    callee: {type: "Identifier", name: "this._if"}, 
                    arguments: [
                        {type: "Literal", value: generate(oldNode), raw: generate(oldNode)},
                        oldNode,
                        {
                            type: 'ArrayExpression',
                            elements: [
                                {type: "Literal", value: oldNode.start, raw: oldNode.start},
                                {type: "Literal", value: oldNode.end, raw: oldNode.end},
                            ]
                        }
                    ]
                }
            }
            if(!node.visited && node.type == 'ForStatement') {                
                if(node.test.type == 'BinaryExpression') {
                    node.test = instrumentBinaryTest(node.test);
                }
                if(node.update.type == 'UpdateExpression') {
                    node.update = instrumentPlusPlus(node.update);
                }
            }
            if(!node.visited && node.type == 'UpdateExpression') {
                node = instrumentPlusPlus(node);
            }

            if(!node.visited && node.type == 'IfStatement') {
                node.test = instrumentBinaryTest(node.test);
            }

            if(!node.visited && node.type == 'ExpressionStatement') {                
                if(node.expression.type == 'CallExpression') {
                    var oldExpression = JSON.parse(JSON.stringify(node.expression));
                    //console.log('old', oldExpression);
                    oldExpression.visited = true;
                    node.expression = {
                        visited: true,
                        type: 'CallExpression',
                        callee: {type: "Identifier", name: "this._call"},
                        arguments:[
                            { visited: true, type:'Literal', value: generate(oldExpression), raw: generate(oldExpression)},
                            {
                                visited: true,
                                type: 'ArrowFunctionExpression',
                                params: [],
                                async: false,
                                expression: false,
                                generator: false,
                                body: {
                                        visited:true,
                                        type:'BlockStatement',
                                        body:[{
                                            visited: true,
                                            type:'ExpressionStatement',
                                            expression: oldExpression
                                        }]
                                    }
                            },
                            {
                                type: 'ArrayExpression',
                                elements: [
                                    {type: "Literal", value: node.start, raw: node.start},
                                    {type: "Literal", value: node.end, raw: node.end},
                                ]
                            }
                        ]
                    };
                }

            }
            
            return node;
        }
    });
    return generate(p);
}

export default function(obj, name, def, el, ctxName) {
    if(typeof el === "string") {
        el = document.getElementById(el);
    }
    let root = el;
    el = root.querySelector(".code");

    let editor;
    let code =  def || `function ${name}() {
}`;
    let localStorageName = `allowCode.${name}.${ctxName}.code`
    if(window.localStorage[localStorageName]){
        code = window.localStorage[localStorageName];
    }
    
    let historyScope;
    let historyDecorations = [];
    var historyControlsEl = root.querySelector(".historyControls");
    var drawLineHistoryEl = root.querySelector(".history");
    //console.log(root, historyControlsEl, drawLineHistoryEl);

    const fcstyle = `style="position: absolute; left: 8px; width: 70px; border: 1px solid orange collapse; text-align: center;margin-top: 20px;"`;
    const ocstyle = `margin-left: 5em; min-width:40px; max-height: 40px; border: 1px solid orange;text-align: center`;
    function startHistoryTable() {
        if(drawLineHistoryEl){
            drawLineHistoryEl.innerHTML = `<div style="margin-left:5em; overflow-x:scroll">
            <table style="width:100%;min-height:300px;border: 1px solid orange;">
            <thead>
                <tr><td style="position: absolute; left: 8px; width: 70px; border: 1px solid orange collapse; text-align: center;">Name/Time</td></tr>
            </thead>
            <tbody>
            </tbody>
</table></div>`;
        }
    }
    function addColumn(name, value, time){
        if(!drawLineHistoryEl) return;
        let i = 0;
        var head = drawLineHistoryEl.querySelector('thead tr');
        var td = document.createElement("td");
        td.style.cssText = ocstyle;
        td.innerText = time;
        head.append(td);

        let found = false;
        Array.from(drawLineHistoryEl.querySelectorAll('tbody tr'))
            .forEach(x => {                
                if(x.firstElementChild.innerText == name){
                    found = true;
                }
            });

        if(!found) {
            var tbody = drawLineHistoryEl.querySelector('tbody');
            var tr = document.createElement('tr');
            tr.innerHTML = `<td ${fcstyle}>${name}</td>`;
            [...Array(head.childNodes.length-2).keys()].forEach(x => {
                tr.innerHTML += `<td style="${ocstyle}"></td>`;
            });
            tbody.append(tr);
        }

        Array.from(drawLineHistoryEl.querySelectorAll('tbody tr'))
            .forEach(x => {                
                var td = document.createElement("td");
                td.style.cssText = ocstyle;
                if(x.firstElementChild.innerText == name){
                    found = true;
                    td.innerHTML = value;
                } else {
                    td.innerHTML = '';
                }
                x.append(td);
                ++i;
            });
        drawLineHistoryEl.firstElementChild.scrollLeft = drawLineHistoryEl.firstElementChild.scrollWidth;
    }
    function addColumnspan(value, time) {
        if(!drawLineHistoryEl) return;

        let i = 0;
        var head = drawLineHistoryEl.querySelector('thead tr');
        var td = document.createElement("td");
        td.style.cssText = ocstyle;
        td.innerText = time;
        head.append(td);

        var rows = drawLineHistoryEl.querySelectorAll('tbody tr');
        var body = rows[0];
        var td = document.createElement("td");
        td.style.cssText = ocstyle;
        td.innerHTML = value;
        td.style.minWidth = td.innerHTML.length * 7 + "px";
        td.rowSpan = rows.length;
        body.append(td);
        drawLineHistoryEl.firstElementChild.scrollLeft = drawLineHistoryEl.firstElementChild.scrollWidth;
    }
    
    let historyi = -1;
    const Step = () => {
        if(!editor) return;

        if(historyi >= 0) {
            var current = historyScope.history[historyi];
            if(!current) return;

            if(current.type == "assignment") {
                addColumn(current.name, current.value, historyi);
            } else if (current.type == "test") {
                addColumnspan(`${current.expr} = ${current.result}` , historyi);
            } else if (current.type == "call") {
                addColumnspan(current.expr, historyi);
            }
        }

        historyi++;
        var current = historyScope.history[historyi];
        if(!current) return;
        var start = editor.getModel().getPositionAt(current.range[0]);
        var end = editor.getModel().getPositionAt(current.range[1]);
        
        historyDecorations = editor.deltaDecorations(historyDecorations, [
            { 
                range: new monaco.Range(
                    start.lineNumber,start.column,
                    end.lineNumber,end.column), 
                options: { 
                    inlineClassName: 'monacoDebugging'
                }
            },
        ]);
    }
    if(historyControlsEl){
        let resetButton = document.createElement("button");
        resetButton.innerText = "Reset";
        resetButton.addEventListener('click', e => {
            historyi = -1;
            startHistoryTable();
            return false;
        });
        historyControlsEl.append(resetButton);

        let stepButton = document.createElement("button");
        stepButton.innerText = "Step";
        stepButton.addEventListener('click', e => {
            if(historyi == -1) startHistoryTable();
            Step();
            return false;
        });
        historyControlsEl.append(stepButton);
    }
    function evaluate() {
        try{
            parse(code);
        } catch(e) {
            return;
        }

        let str = instrumentCode(code);
        //console.log(str);

        try
        {
            function Scope() {
                "use strict";
                this.history = [];
                this._write = (name, value, range) => {
                    if(typeof value === "function"){
                        value = "[function]";
                    }
                    if(Array.isArray(value)){
                        value = JSON.stringify(value);
                    }
                    this.history.push({
                        type:"assignment",
                        name,
                        value,
                        range
                    });
                    return value;
                };
                this._if = (expr, result, range) => {
                    this.history.push({
                        type:"test",
                        expr,
                        result,
                        range
                    });
                    return result;
                };
                this._call = (expr, f, range) => {
                    this.history.push({
                        type:"call",
                        expr,
                        range
                    });
                    f();
                }
            }
            
            let f = eval(`(${str})`);
            ////console.log(f);
            window.localStorage[localStorageName] = code;
            obj[name] = function() {
                try{
                    //console.log(historyi);
                    let scope = new Scope();
                    f.apply(scope, arguments);
                    if(historyi == -1) {
                        historyScope = scope;
                    }
                } catch(e) {
                    if(!f.error) {
                        f.error = true;
                        //console.error(e);
                    }
                }
            }
        } catch(e) {
        }
    }
  
   

    function configureEditor() {
        window.require.config({ paths: {
            'vs': 'https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs' }
        });
        window.require(['vs/editor/editor.main'], function() {
            monaco.languages.typescript.javascriptDefaults.setDiagnosticsOptions({
                noSemanticValidation: false,
                noSyntaxValidation: false
            });
            monaco.languages.typescript.javascriptDefaults.setCompilerOptions({
                target: monaco.languages.typescript.ScriptTarget.ES6,
                allowNonTsExtensions: true,
                alwaysStrict: true,
                noUnusedParameters: true,
                noImplicitUseStrict: true,
                noUnusedLocals: true
            });
            editor = monaco.editor.create(el, {
                value: code,
                language: 'javascript',
                scrollBeyondLastLine: false,
                minimap: {
                    enabled: false
                },
                automaticLayout: true,
            });
        
            editor.getModel().onDidChangeContent((event) => {
                code = editor.getValue();                                
                evaluate();
            });
        });
    }

    var loaderUrl =  "https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs/loader.js";
    var monacoLoader = Array.from(document.scripts)
        .filter(x => x.src == loaderUrl);        
    if(monacoLoader.length == 0){
        var s = document.createElement("script");
        s.src = loaderUrl;
        s.async = false;
        s.addEventListener("load", e => {
            configureEditor();
        });
        document.head.appendChild(s);
    } else {
        configureEditor();
    }
    evaluate();
}