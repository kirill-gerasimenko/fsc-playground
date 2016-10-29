#nowarn "25" // disable warning on incomplete matched patterns

#load "commentsMap.fsx"
#load "utils.fsx"

open CommentsMap
open Expecto
open Utils

let getComments = CommentsMap.getComments >> Array.ofList

let singleLineTests = [   
    testCase "Line without comments gives empty result" <| fun _->
        // arrange, act
        let comments = @"let x = 10" |> getComments

        // assert
        comments.Length ==? 0

    testCase "Line with single ranged comment is parsed to one ranged comment result" <| fun _ ->
        // arrange, act
        let comments = @"let x = 10 (* some comment *)" |> getComments

        // assert
        comments.Length ==? 1
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.Range (11,28,true) } 

    testCase "Line with range and line comment is parsed to both comments" <| fun _ ->
        // arrange, act
        let comments = @"let x (* c *) // some line comment" |> getComments

        // assert
        comments.Length ==? 2
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.Range (6,12,false) }
        comments.[1] ==? { Line = 0; Comment = LineCommentInfo.Line (14,false) }
    
    testCase "Line with two range comments is parsed" <| fun _ ->
        // arrange, act
        let comments = @"(* c *) (* b *)" |> getComments

        // assert
        comments.Length ==? 2
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.Range (0,6,false) }
        comments.[1] ==? { Line = 0; Comment = LineCommentInfo.Range (8,14,true) }
    
    testCase "Line comment absorbs range one" <| fun _ ->
        // arrange, act
        let comments = @"let // (* inner *)" |> getComments

        // assert
        comments.Length ==? 1
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.Line (4,false) }

    testCase "Line comment is whole line" <| fun _ ->
        // arrange, act
        let comments = "// this is whole line" |> getComments

        // act
        comments.Length ==? 1
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.WholeLine false }
    ]

let mulitiLineTests = [
    testCase "3 lines multiline comment" <| fun _ ->
        // arrange, act
        let comments = 
            "(* starts \nlet x = 10\n*)" 
            |> getComments
        
        // asset
        comments.Length ==? 3
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.WholeLine true }
        comments.[1] ==? { Line = 1; Comment = LineCommentInfo.WholeLine true }
        comments.[2] ==? { Line = 2; Comment = LineCommentInfo.Range (0,1,true) }

    testCase "3 lines multiline comment inside tokens" <| fun _ ->
        // arrange, act
        let comments =
            "let x = 10; (*\n some mutiline \n*) let y = 10"
            |> getComments

        // assert
        comments.Length ==? 3
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.Line (12,true) }
        comments.[1] ==? { Line = 1; Comment = LineCommentInfo.WholeLine true }
        comments.[2] ==? { Line = 2; Comment = LineCommentInfo.Range (0,1,false) }
    
    testCase "Empty line in multiline comment" <| fun _ ->
        // arrange, act
        let comments =
            "(*\n\n*)" |> getComments
        
        // act
        comments.Length ==? 3
        comments.[0] ==? { Line = 0; Comment = LineCommentInfo.WholeLine true }
        comments.[1] ==? { Line = 1; Comment = LineCommentInfo.WholeLine true }
        comments.[2] ==? { Line = 2; Comment = LineCommentInfo.Range (0,1,true) }
    ]

[
    yield! singleLineTests
    yield! mulitiLineTests
] 
|> List.iter (fsiRunTest>>ignore)