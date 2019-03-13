# Who is afraid of parsing

In my experience, parsing text beats the hell out of developers. They do everything possible to avoid building parsers. Which is understandable, parsing is an extremely insidious task. You can never test every little detail, and there is always the possibility of parsing something wrongly.

But the real problem is, at least from my perspective, is how we developers are exposed to parsers for the first time, creating a traumatic experience.

Almost everyone is exposed to lex/yacc, and have to create an expression parser. It is practically mandatory to suffer from the lex/yacc strange production syntax and lose hours with strange errors. But I honestly do not think that this is the necessary path to take now. Having built some parsers myself, I do consider the Monadic Parser Pattern to be the easiest one to follow. Even to beginners.

A good paper that gently introduces one to this pattern is: 

Monadic Parser Combinators  
http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

Do not let the "monadic" in the name scare you. As the paper express in its abstract, the monadic parser is one of the best approaches to understand monads. Why? Because it naturally fits when combining parsers and builds something useful.

Here, we will not follow the paper. First, we will use a "Monadic Parser Combinator" library in javascript because it will give us immediate feedback on the parser. Second, we will try to build a "monadic parser combinator" library ourselves on C#. Unfortunately, C# does not have a lot of operators to overload, but we will use "Extension Methods" as "suffixes operators" and will allow us to build a nice fluent API on top of the monad.

So let us get started!

# JS Playground

Go to your favorite JS playground. If you want an online one, I suggest https://stackblitz.com/.  If you want a local one, I suggest https://parceljs.org/.

Just navigate your terminal to your desired working folder (remember that Windows have Powershell!)

> npm install -g parcel-bundler

Now create an empty index.html

bash
> touch ./index.html

powershell
> ni ./index.html

open your favorite editor and run

>parcel ./index.html --open

Parcel now is watching your file modifications and started a simple HTTP server. Just navigate to  http://localhost:1234/.

In your index.html just type

```
    <html>
        <head>
        </head>
    <body>
        <input id="code">
        <div id="result>
        </div>
        <script>
            import renderjson  from './renderjson.js';
            import {parse, text, lang, nu} from 'bennu';
            import {stream} from 'nu-stream';
        </script>
    <body>
    </html>
```

"renderjson" is the library that will display our parser result. It is simple but will put us directly on our job. "bennu" is the "monadic parser combinator" framework that we will use. "nu" is a lazy stream framework that "bennu" uses.

Now let us create the code that will call our parser.

```
    function parse(str)
    {
    	return {ok:true};
    }

    const render = renderjson
        .set_icons('+', '-')
        .set_show_to_level(10)
        .set_max_string_length(100);
    const elResult = document.getElementById('result');
    const elCode = document.getElementById('code');
    elCode.addEventListener('keyup'), x => {
    	const ast= parse(str);
    	elResult.innetHTML = render(ast);
    });

And we are good to go! You can type your string to be parsed and immediately see the parse result. You will learn to appreciate this immediate feedback.

Ok! Now we have to decide what to parse. Let us start simple. Let us parse a simple character. If you type 'a', we will return ok; otherwise, we will return an error.

```
    function parse(str)
    {
    	const parser = text.character('a');
    	const result = parse.run(parser, str);
    	return {ok:true, ast: result};    
    }

And there you have it — your first parser. Not that bad, as advertised. Not very useful either, I assume. But this "pattern" has "combinators" in its name because it is supposedly easy to "combinate" parsers. But to "combinate" things you must have at least two things. So let us create another parser and see our first combination.

```
    function parse(str)
    {
    	const parser1 = text.character('a');
    	const parser2 = text.character('b');
    	const parser = parse.then(parser1, parser2); // combination

    	var result = parse.run(parser, str);
    	return {ok:true, ast: result};    
    }

And this combines parser1 and parsers2 as it reads. You first apply parser1, and we expect to see an 'a'; then it applies the second parser, we expect to see a 'b'. In the end, we have parsed an 'ab'.

Please try. Remember that we are still not treating errors.