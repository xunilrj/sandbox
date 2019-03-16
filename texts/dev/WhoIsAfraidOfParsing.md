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
<!DOCTYPE html>
<html>
  <head>
    <title>Monadic Parser Example</title>
    <meta charset="UTF-8" />
    <script src="https://www.gitcdn.xyz/repo/caldwell/renderjson/master/renderjson.js"></script>
  </head>
  <body>
        <input id="code"/>
        <div id="result">            
        </div>
    <script src="src/index.js"></script>
  </body>
</html>
```

and your index.js
```
import { parse, text, lang, nu } from "bennu";
import { stream } from "nu-stream";
const renderjson = window.renderjson;
```

"renderjson" is the library that will display our parser result. It is simple but will put us directly on our job. "bennu" is the "monadic parser combinator" framework that we will use. "nu" is a lazy stream framework that "bennu" uses.

Now let us create the code that will call our parser.

```
function parseCode(str) {
  return { ok: true };
}

const render = renderjson
  .set_icons("+", "-")
  .set_show_to_level(10)
  .set_max_string_length(100);
const elResult = document.getElementById("result");
const elCode = document.getElementById("code");
elCode.addEventListener("keyup", e => {
  const str = e.target.value;
  const ast = parseCode(str);
  elResult.innerHTML = "";
  elResult.appendChild(render(ast));
});
```

## Next, Enumeration and Bind

And we are good to go! You can type your string to be parsed and immediately see the parse result. You will learn to appreciate this immediate feedback.

Ok! Now we have to decide what to parse. Let us start simple. Let us parse a simple character. If you type 'a', we will return ok; otherwise, we will return an error.

```
function parseCode(str) {
  const parser = text.character("a");
  try {
    const result = parse.run(parser, str);
    return { ok: true, ast: result };
  } catch (e) {
    return { ok: false, error: e };
  }
}
```

And there you have it — your first parser. Not that bad, as advertised. Not very useful either, I assume. But this "pattern" has "combinators" in its name because it is supposedly easy to "combinate" parsers. But to "combinate" things you must have at least two things. So let us create another parser and see our first combination.

```
function parseCode(str) {
  const pa = text.character('a');
  const pb = text.character('b');
  const parser = parse.next(pa, pb); // combination

  try {
    const result = parse.run(parser, str);
    return { ok: true, ast: result };
  } catch (e) {
    return { ok: false, error: e };
  }
}
```

And this combines pa and pb as it reads. You first apply pa, and we expect to see an 'a'; then it applies the second parser, we expect to see a 'b'. In the end, we have parsed an 'ab'.

Probably the first surprise is the generated "ast". 

```
-{
    "ok": true,
    "ast": "b"
}
```

Why just "b" and not "ab" if we have parsed "ab"? The answer is that the AST is not what was parsed, but what the parse generated. If you go to "bennu's" documentation you will find:

parse.next(p, q), parse.concat(p, q)  
Consumes p then q. Returns result from q  
https://github.com/mattbierner/bennu/wiki/parse#parsenextp-q-parseconcatp-q

Which explains why we are seeing just "b" in the result. To fix this let us first change the combination from "next" to "enumeration".

```
function parseCode(str) {
  const pa = text.character('a');
  const pb = text.character('b');
  const parser = parse.enumeration(pa, pb); // combination

  try {
    const result = parse.run(parser, str);
    return { ok: true, ast: result };
  } catch (e) {
    return { ok: false, error: e };
  }
}
```

Now our parser is working, but we see a very strange result.

```
-{
    "ok": true,
    "ast": -{
        "first": "a",
        "rest": undefined
    }
}
```

What is this "first" and "rest". The issue here is that "bennu" is a lazy framework. The "enumeration" combination do return the result of all its parsers (in our case pa and pb), but as a lazy list. There a couple of ways to solve this.

The most useful way is to use a "decorator" parser, that allows us the transform the combined parse result to whatever result we want. We will face here some non-intuitive choices. Let us hope that they will make perfect sense after the explanation.

```
function parseCode(str) {
  const pa = text.character('a');
  const pb = text.character('b');
  const parser = parse.binds(
    parse.enumeration(pa, pb),
    (a,b) => {    
        const prod = a + b;
        return parse.always(prod);
    });

  try {
    const result = parse.run(parser, str);
    return { ok: true, ast: result };
  } catch (e) {
    return { ok: false, error: e };
  }
}
```

We start by decorating our parser with the "binds". "binds" realize that our parser (the first argument) returns a list with two values, then call the second argument with these two values. Remember that we had a lazy list, now we have this list materialized as the two parameters. 

The values of these parameters are what you expect. They are "a" and "b". Now I can do whatever I want with them. I chose to append them in a new string called "prod" from production.

Now comes the non-intuitive part. One could be expecting that returning the new production would suffice. Unfortunately, this is not the case. We have to return a new parser, the one that will continue parsing the string.

The trick to generate the production that we want is to return a parser that never looks at the string being parsed and always returns one specific value. This is the "always" parser. 

Like the parser "character", "always" generates a parser out of nowhere. It is important to realize that "parser.character("a")" and "parser.always("a")" are completely different. One looks at the string and search the character "a" and return "a" if found; the other ignores the string being parsed and always returns "a".

The question is: given that we return the parser that will continue parsing the string, could we have used "bind" to parse "a" then parse "b"? Yes, we could.

```
function parseCode(str) {
  const pa = text.character('a');
  const pb = text.character('b');
  const parser = parse.bind(
    pa,
    (x) => {
      return pb;
    });

  try {
    const result = parse.run(parser, str);
    return { ok: true, ast: result };
  } catch (e) {
    return { ok: false, error: e };
  }
}
```

And if you run this code you will see:

```
-{
    "ok": true,
    "ast": "b"
}
```

This exactly the same result we had when using the "next" combinator. And if you see "bennu's" source code, you will see that this is precisely how they implement "next".

```
/**
 * Parse `p`, then `q`. Return value from `q`.
 */
next := \p q ->
    bind(p, constant q);
```
https://github.com/mattbierner/bennu/blob/master/lib/parse.kep#L770

Well... Ok, not precisely. Specialy because "bennu" is not written in Javascript. But we can create our own "next" with just one line of code.

```
const next = (p,q) => parse.bind(p, x => q);
```

Now we understand why the "next" combinator "throws away" the value of the first parse.

# To be continued...