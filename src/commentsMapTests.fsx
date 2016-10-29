#load "commentsMap.fsx"

open Expecto


[<Literal>]
let code = 
    @"~
        // test
        v

        Path. *)
    
    // this is comment for the line below
    let x = 10 (*comment inside of line*) then some (* one more comment*) text
    //open System.IO
    let y = (* only one comment *)
    test
    "

let tokenizeText =
    FSharpSourceTokenizer ([], None) |> Tokenizer.tokenizeText 

let comments =
    code 
    |> getLines  
    |> tokenizeText
    |> lpTrace "Tokenized text"