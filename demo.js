'use strict';


let mod = null;
const mod_promise = import('./pkg/cubiml_demo.js').then(
    m => (mod = m, mod.default()));



const HTML = `
<style>
    #container {
        height: 32em;
        position: relative;
        font: medium monospace;
    }
    #container.loading {
        opacity: 85%;
    }

    #container form {
        margin: 0;
    }
    #container, #prompt, #editor {
        background: darkslategrey;
        color: white;
    }

    #loading {
        position: absolute;
        top: 15%;
        left: 12%;
    }


    #pane1, #pane2 {
        float: left;
        width: 50%;
        height: 100%;

        display: flex;
        flex-direction: column;
    }
    #editor {
        height: 100%;
        resize: none;
        margin: 0;
    }


    #container .error {
        background: darkred;
    }

    #container pre {
        white-space: pre-wrap;
        overflow-wrap: anywhere;
        margin: 0;
    }

    #output {
        overflow-y: scroll;
    }
    #input-line {
        display: flex;
    }
    #prompt {
        flex-grow: 1;
        border: 0;
    }
    #space-below-prompt {
        flex-grow: 1;
    }
</style>


<div id=container class=loading>
    <div id=loading>Loading, please wait...</div>

    <div id=pane1>
        <textarea id=editor>
(* calculate fibonacci numbers recursively *)
let fib =
    let rec fib_sub = fun {n; a; b} ->
        if n <= 1 then
            a
        else
            fib_sub {n=n - 1; a=a + b; b=a}
    in
    fun n -> fib_sub {n; a=1; b=1};

(* matching on case types *)
let area = fun x ->
    match x with
        | \`Square x -> x.len *. x.len
        | \`Rect x -> x.height *. x.width;

area \`Square {len=4.};
area \`Rect {height=4.; width=2.5};

(* wildcard match delegates to first area function
    for the non-Circle cases in a type safe manner *)
let area = fun x ->
    match x with
        | \`Circle x -> x.radius *. x.radius *. 3.1415926
        | x -> area x;

area \`Square {len=4.};
area \`Rect {height=4.; width=2.5};
area \`Circle {radius=1.2};

(* ints are arbitrary precision *)
(* 999th fibonacci number = 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875 *)
fib 999


        </textarea>
        <button id=compile-and-run type=button>Compile and run</button>
    </div>

    <div id=pane2>
        <div id=output>
        </div>

        <form id=rhs-form>
        <pre id=input-line>&gt;&gt; <input id=prompt type="text" autocomplete="off" placeholder="Enter code here or to the left" disabled></pre>
        </form>

        <div id=space-below-prompt></div>
    </div>
</div>
`;

class CubimlDemo extends HTMLElement {
  constructor() {
    // Always call super first in constructor
    super();

    // Create a shadow root
    const shadow = this.attachShadow({mode: 'open'});
    shadow.innerHTML = HTML;

    mod_promise.then(
        wasm => initializeRepl(shadow, mod.State.new(), Printer)).catch(
        e => {root.getElementById('loading').textContent = 'Failed to load demo: ' + e});

  }
}
customElements.define('cubiml-demo', CubimlDemo);


function initializeRepl(root, compiler, Printer) {
    console.log('Initializing REPL');
    const container = root.getElementById('container');
    const output = root.getElementById('output');
    const prompt = root.getElementById('prompt');
    const editor = root.getElementById('editor');

    function addOutput(line, cls) {
        const l = document.createElement('pre');
        l.textContent = line;
        if (cls) {
            l.classList.add(cls);
        }
        output.appendChild(l);
        return l;
    }

    const $ = Object.create(null);
    const history = [];
    let history_offset = -1;

    function execCode(script) {
        let compiled;
        try {
            if (!compiler.process(script)) {return [false, compiler.get_err()];}
            compiled = '(' + compiler.get_output() + ')';
        } catch (e) {
            return [false, 'Internal compiler error: ' + e.toString()];
        }

        try {
            const val = eval(compiled);
            return [true, (new Printer).print(val)];
        } catch (e) {
            return [false, 'An error occurred during evaluation in the repl: ' + e.toString()];
        }
    }

    function processCode(script) {
        const [success, res] = execCode(script);
        addOutput(res, success ? 'success' : 'error');
        // scroll output window to the bottom
        output.scrollTop = output.scrollHeight;
        return success;
    }


    function processReplInput(line) {
        line = line.trim();
        if (!line) {return;}

        history_offset = -1;
        if (history[history.length-1] !== line) {history.push(line);}
        // \u00a0 = non breaking space
        addOutput('>>\u00a0' + line, 'input');
        processCode(line);
    }

    root.getElementById('compile-and-run').addEventListener('click', e => {
        const s = editor.value.trim();
        if (!s) {return;}

        // Clear repl output
        output.textContent = '';
        compiler.reset();
        if (processCode(s)) {prompt.focus({preventScroll: true})}
    });

    // Implement repl command history
    prompt.addEventListener('keydown', e => {
        switch (e.key) {
            case 'ArrowDown': history_offset -= 1; break;
            case 'ArrowUp': history_offset += 1; break;
            default: return;
        }
        e.preventDefault();

        if (history_offset >= history.length) {history_offset = history.length - 1;}
        if (history_offset < 0) {history_offset = 0;}
        prompt.value = history[history.length - history_offset - 1];
    });

    // If they click in the space below the prompt, focus on the prompt to make it easier to select
    root.getElementById('space-below-prompt').addEventListener('click', e => {
        e.preventDefault();
        prompt.focus({preventScroll: true});
    });

    root.getElementById('rhs-form').addEventListener('submit', e => {
        e.preventDefault();
        const s = prompt.value.trim();
        prompt.value = '';

        if (!s) {return;}
        processReplInput(s);
    });

    container.classList.remove('loading');
    prompt.disabled = false;
    container.removeChild(root.getElementById('loading'));
    console.log('Initialized REPL');

    // Run the example code
    processCode(editor.value.trim())
}

class Printer {
    constructor() {
        this.parts = [];
        this.seen = new WeakSet;
    }

    visit(e) {
        const type = typeof e;
        if (type === 'boolean' || type === 'bigint') {this.parts.push(e.toString()); return;}
        if (type === 'string') {this.parts.push(JSON.stringify(e)); return;}
        if (type === 'number') {
            let s = e.toString();
            if (/^-?\d+$/.test(s)) {s += '.0'}
            this.parts.push(s);
            return;
        }
        if (type === 'function') {this.parts.push('<fun>'); return;}
        if (type === 'symbol') {this.parts.push('<sym>'); return;}
        if (e === null) {this.parts.push('null'); return;}
        if (e === undefined) {this.parts.push('<undefined>'); return;}

        if (this.seen.has(e)) {this.parts.push('...'); return;}
        this.seen.add(e);

        if (e.$tag) {
            this.parts.push(e.$tag);
            if (!e.$val || typeof e.$val !== 'object') {
                this.parts.push(' ');
            }
            this.visit(e.$val);
        } else if ('$p' in e) {
            this.parts.push('ref ');
            this.visit(e.$p);
        } else {
            this.parts.push('{');
            let first = true;
            for (const [k, v] of Object.entries(e)) {
                if (!first) {this.parts.push('; ')}
                first = false;

                this.parts.push(k + '=');
                this.visit(v);
            }
            this.parts.push('}');
        }
    }

    print(e) {this.visit(e); return this.parts.join('');}
}
