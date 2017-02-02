//open System
//
//let A_Parser str =
//    if String.IsNullOrEmpty(str) then
//        (false,"")
//    else if str.[0] = 'A' then
//        let remaining = str.[1..]
//        (true,remaining)
//    else
//        (false,str)
//
//A_Parser "ABC"
//A_Parser "BC"
//
//type Result<'a> =
//    | Success of 'a
//    | Failure of string 
//
//let pchar charToMatch str = 
//    if String.IsNullOrEmpty(str) then
//        Failure "No more input"
//    else
//        let first = str.[0] 
//        if first = charToMatch then
//            let remaining = str.[1..]
//            Success (charToMatch,remaining)
//        else
//            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
//            Failure msg
//
//pchar 'A' "ABC"
//pchar 'A' "BC"
//
//let parserA = pchar 'A'
//
//parserA "ABC"
//parserA "BC"
//
//type Parser<'T> = Parser of (string -> Result<'T * string>)
//
//let pchar2 charToMatch =
//    let innerFn str = 
//        if String.IsNullOrEmpty(str) then
//            Failure "No more input"
//        else
//            let first = str.[0] 
//            if first = charToMatch then
//                let remaining = str.[1..]
//                Success (charToMatch,remaining)
//            else
//                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
//                Failure msg
//    Parser innerFn
//
//let parserA2 = pchar2 'A'
//
//let run parser input = 
//    let (Parser innerFn) = parser 
//    innerFn input
//
//run parserA2 "ABC"
//run parserA2 "BC"
//
//let andThen parser1 parser2 =
//    let innerFn input =
//        let result1 = run parser1 input
//        match result1 with
//        | Failure err -> 
//            Failure err  
//        | Success (value1,remaining1) -> 
//            let result2 =  run parser2 remaining1
//            match result2 with 
//            | Failure err ->
//                Failure err 
//            | Success (value2,remaining2) -> 
//                let newValue = (value1,value2)
//                Success (newValue,remaining2)
//    Parser innerFn
//
//let ( .>>. ) = andThen
//
//let parseA = pchar2 'A'   
//let parseB = pchar2 'B'
//let parseAThenB = parseA .>>. parseB  
//
//run parseAThenB "ABC"
//run parseAThenB "ZBC" 
//run parseAThenB "AZC" 
//
//let orElse parser1 parser2 =
//    let innerFn input =
//        let result1 = run parser1 input
//        match result1 with
//        | Success result -> 
//            result1
//        | Failure err -> 
//            let result2 = run parser2 input
//            result2 
//    Parser innerFn
//
//let ( <|> ) = orElse
//let parseAOrElseB = parseA <|> parseB 
//
//run parseAOrElseB "AZZ"
//run parseAOrElseB "BZZ"
//run parseAOrElseB "CZZ"
//
//let parseC = pchar2 'C'
//let bOrElseC = parseB <|> parseC
//let aAndThenBorC = parseA .>>. bOrElseC 
//
//run aAndThenBorC "ABZ"
//run aAndThenBorC "ACZ"
//run aAndThenBorC "QBZ"
//run aAndThenBorC "AQZ" 
//
//let choice listOfParsers = 
//    List.reduce ( <|> ) listOfParsers 
//
//let anyOf listOfChars = 
//    listOfChars
//    |> List.map pchar2
//    |> choice
//
//let parseLowercase = 
//    anyOf ['a'..'z']
//
//let parseDigit = 
//    anyOf ['0'..'9']
//
//run parseLowercase "aBC"
//run parseLowercase "ABC"
//run parseDigit "1ABC"
//run parseDigit "9ABC"
//run parseDigit "|ABC"
//
//let mapP f parser = 
//    let innerFn input =
//        let result = run parser input
//        match result with
//        | Success (value,remaining) -> 
//            let newValue = f value
//            Success (newValue, remaining)
//        | Failure err -> 
//            Failure err
//    Parser innerFn
//
//let ( <!> ) = mapP
//let ( |>> ) x f = mapP f x
//
//let parseThreeDigitsAsStr = 
//    (parseDigit .>>. parseDigit .>>. parseDigit)
//    |>> fun ((c1, c2), c3) -> String [| c1; c2; c3 |]
//
//run parseThreeDigitsAsStr "123A"
//
//let parseThreeDigitsAsInt = 
//    mapP int parseThreeDigitsAsStr 
//
//run parseThreeDigitsAsInt "123A"
//
//let returnP x = 
//    let innerFn input =
//        Success (x,input )
//    Parser innerFn 
//
//let applyP fP xP = 
//    (fP .>>. xP) 
//    |> mapP (fun (f,x) -> f x)
//
//let ( <*> ) = applyP
//
//let lift2 f xP yP =
//    returnP f <*> xP <*> yP
//
//let addP = 
//    lift2 (+)
//
//let startsWith (str:string) prefix =
//    str.StartsWith(prefix)  
//
//let startsWithP =
//    lift2 startsWith 
//
//let rec sequence parserList =
//    let cons head tail = head::tail
//    let consP = lift2 cons
//    match parserList with
//    | [] -> 
//        returnP []
//    | head::tail ->
//        consP head (sequence tail)
//
//let parsers = [ pchar2 'A'; pchar2 'B'; pchar2 'C' ]
//let combined = sequence parsers
//
//run combined "ABCD" 
//
//let charListToStr charList = 
//     String(List.toArray charList)
//
//let pstring str = 
//    str
//    |> List.ofSeq
//    |> List.map pchar2
//    |> sequence
//    |> mapP charListToStr 
//
//let parseABC = pstring "ABC"
//run parseABC "ABCDE"
//run parseABC "A|CDE"
//run parseABC "AB|DE"
//
//let rec parseZeroOrMore parser input =
//    let firstResult = run parser input 
//    match firstResult with
//    | Failure err -> 
//        ([],input)  
//    | Success (firstValue,inputAfterFirstParse) -> 
//        let (subsequentValues,remainingInput) = 
//            parseZeroOrMore parser inputAfterFirstParse
//        let values = firstValue::subsequentValues
//        (values,remainingInput)
//
//let many parser = 
//    let rec innerFn input =
//        Success (parseZeroOrMore parser input)
//    Parser innerFn
//
//let manyA = many (pchar2 'A')
//run manyA "ABCD"
//run manyA "AACD"
//run manyA "AAAD"
//run manyA "|BCD"
//
//let manyAB = many (pstring "AB")
//run manyAB "ABCD"
//run manyAB "ABABCD"
//run manyAB "ZCD"
//run manyAB "AZCD"
//
//let whitespaceChar = anyOf [' '; '\t'; '\n']
//let whitespace = many whitespaceChar 
//run whitespace "ABC"
//run whitespace " ABC"
//run whitespace "\tABC"
//
//let many1 parser = 
//    let rec innerFn input =
//        let firstResult = run parser input 
//        match firstResult with
//        | Failure err -> 
//            Failure err // failed
//        | Success (firstValue,inputAfterFirstParse) -> 
//            let (subsequentValues,remainingInput) = 
//                parseZeroOrMore parser inputAfterFirstParse
//            let values = firstValue::subsequentValues
//            Success (values,remainingInput)  
//    Parser innerFn
//
//let digit = anyOf ['0'..'9']
//let digits = many1 digit 
//
//run digits "1ABC"
//run digits "12BC"
//run digits "123C"
//run digits "1234"
//run digits "ABC" 
//
//let pint = 
//    let resultToInt digitList = 
//        String(List.toArray digitList) |> int
//    let digit = anyOf ['0'..'9']
//    let digits = many1 digit 
//    digits 
//    |> mapP resultToInt
//
//run pint "1ABC"
//run pint "12BC"
//run pint "123C"
//run pint "1234"
//run pint "ABC"
//
//let opt p = 
//    let some = p |>> Some
//    let none = returnP None
//    some <|> none
//
//let digitThenSemicolon = digit .>>. opt (pchar2 ';')
//run digitThenSemicolon "1;"
//run digitThenSemicolon "1"
//
//let pint2 = 
//    let resultToInt (sign,charList) = 
//        let i = String(List.toArray charList) |> int
//        match sign with
//        | Some ch -> -i
//        | None -> i
//    let digit = anyOf ['0'..'9']
//    let digits = many1 digit 
//    opt (pchar2 '-') .>>. digits 
//    |>> resultToInt  
//
//run pint "123C"
//run pint "-123C"
//
//let (.>>) p1 p2 = 
//    p1 .>>. p2 
//    |> mapP (fun (a,b) -> a) 
//
//let (>>.) p1 p2 = 
//    p1 .>>. p2 
//    |> mapP (fun (a,b) -> b) 
//
//let digitThenSemicolon2 = digit .>> opt (pchar2 ';')  
//run digitThenSemicolon2 "1;"
//run digitThenSemicolon2 "1"
//
//let ab = pstring "AB"
//let cd = pstring "CD"
//let ab_cd = (ab .>> whitespace) .>>. cd
//run ab_cd "AB \t\nCD"
//
//let between p1 p2 p3 = 
//    p1 >>. p2 .>> p3
//
//let pdoublequote = pchar2 '"'
//let quotedInteger = between pdoublequote pint pdoublequote
//
//run quotedInteger "\"1234\""
//run quotedInteger "1234"
//
//let sepBy1 p sep =
//    let sepThenP = sep >>. p            
//    p .>>. many sepThenP 
//    |>> fun (p,pList) -> p::pList
//
//let sepBy p sep =
//    sepBy1 p sep <|> returnP []
//
//let comma = pchar2 ',' 
//let zeroOrMoreDigitList = sepBy digit comma
//let oneOrMoreDigitList = sepBy1 digit comma
//run oneOrMoreDigitList "1;"   
//run oneOrMoreDigitList "1,2;"   
//run oneOrMoreDigitList "1,2,3;"  
//run oneOrMoreDigitList "Z;"     
//run zeroOrMoreDigitList "1;"   
//run zeroOrMoreDigitList "1,2;"   
//run zeroOrMoreDigitList "1,2,3;"
//run zeroOrMoreDigitList "Z;"

#I "C:/github/xunilrj-sandbox/sources/fsharp-parsers/ConsoleApplication1/packages/FParsec.1.0.2/lib/net40-client/"
#r "FParsec"
#r "FParsecCS"

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

//PARSERS
let pZero = pchar (char 0)
let pNewLine = pchar (char 13)
let pUntil = many1CharsTill (anyChar)
//TOKENS
let COLONSPACE = pchar ':' .>> spaces
let LETTERS = many1Chars asciiLetter
let IDENTIFIER =  many1Chars (choice [asciiLetter;pchar '-'])
//SYNTAX
let FirstLine = LETTERS .>> pNewLine
let HeaderName = IDENTIFIER .>> spaces
let HeaderValue = pUntil pNewLine
let HeaderLine = HeaderName .>> COLONSPACE .>>. HeaderValue
let Main = FirstLine .>>. many HeaderLine .>> pNewLine .>>. pUntil pZero
test Main "SOMECOMMAND\r\n\
           HEA-DER: START1234567890-=q´[]~ç;.,\|!@#$%¨&*()_+{`^}:>ÇEND\r\n\
           Hea-Der: VAL-UE\r\n\
           \r\n\
           START1234567890-=q´[]~ç;.,\|!@#$%¨&*()_+{`^}:>ÇEND\u0000"