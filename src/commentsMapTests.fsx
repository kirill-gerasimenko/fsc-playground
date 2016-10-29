#nowarn "25" // disable warning on incomplete matched patterns

#load "commentsMap.fsx"
#load "utils.fsx"

open CommentsMap
open Expecto
open Utils

let commentsParsingTests = [   
    testCase "Line without comments gives empty result" <| fun _->
        // arrange, act
        let comments = @"let x = 10" |> getComments

        // assert
        comments |> List.length ==? 0

    testCase "Line with single ranged coment is parsed to one ranged comment result" <| fun _ ->
        // arrange, act
        let comments = @"let x = 10 (* some comment *)" |> getComments
        let comment = comments |> List.head
        let (LineCommentInfo.Range (left,right,wraps)) = comment.Comment

        // assert
        comment.Line ==? 0
        left ==? 11
        right ==? 28
        wraps ==? true

    testCase "Line with range and line comment is parsed to both comments" <| fun _ ->
        // arrange, act
        let comments = @"let x (* c *) // some line comment" |> getComments
        let rangeComment = comments |> List.head
        let lineComment = comments |> List.last
        let (LineCommentInfo.Range (rangeLeft,rangeRight,rangeWraps)) = rangeComment.Comment

        // assert
        comments |> List.length ==? 2

        rangeComment.Line ==? 0
        rangeLeft ==? 6
        rangeRight ==? 12
        rangeWraps ==? false

        lineComment.Line ==? 0

    |> fsiRunTest
    ]


commentsParsingTests |> List.iter (fsiRunTest>>ignore)




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
