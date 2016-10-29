#load "commentsMap.fsx"
#load "utils.fsx"

open CommentsMap
open Expecto
open Utils


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

let commentsParsingTests = [   
    testCase "Test" <| fun _->
        let sourceCode = @""
        let comments = sourceCode |> getComments
        ()
    testCase "Test2" <| fun _ ->
        ()
    ]

commentsParsingTests |> List.iter (fsiRunTest>>ignore)