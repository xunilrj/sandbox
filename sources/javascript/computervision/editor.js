import * as monaco from 'monaco-editor';

export default function editableFunction(
    element, localstorageKey,
    fname){
    self.MonacoEnvironment = {
        getWorkerUrl: function(moduleId, label) {
            if (label === 'json') return './json.worker.js';
            if (label === 'css') return './css.worker.js';
            if (label === 'html') return './html.worker.js';
            if (label === 'typescript' || label === 'javascript') return './ts.worker.js';
            return './editor.worker.js';
        },
    };

    const el = document.getElementById(element);
    var codeKey = localstorageKey + '.code';

    var code;
    if(localStorage)
        code = localStorage.getItem(codeKey);
    if(!code)
        code = el.innerText;

    el.innerText = "";
    const editor = monaco.editor.create(el, {
        value: code,
        language: 'javascript',
        automaticLayout:true,
        scrollBeyondLastLine: false,
        minimap: {
            enabled: false
        }
    });

    var handlers = [];
    var obj = {
        f: null,
        on: (f) => {
            handlers.push(f);
        }
    };

    function compile()
    {
        const code = editor.getModel().getValue();    
        localStorage.setItem(codeKey, code);
        obj.f = eval(`${code}; ${fname};`);
        handlers.forEach(xx => {
            xx(obj.f);
        });
    }

    editor.getModel()
    .onDidChangeContent((event) => {
        compile();
    });
    compile();

    return obj;
}