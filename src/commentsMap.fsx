#load "../paket-files/include-scripts/net46/include.main.group.fsx"

open Microsoft.FSharp.Compiler.SourceCodeServices

type Range = Range of left:int * right:int
type InfiniteRange = InfiniteRange of left:int

type LineCommentInfo =
    | WholeLine of wraps:bool
    | Range of left:int * right:int * wraps:bool
    | Line of left:int * wraps:bool

type LineComments =
    { Line : int
      Comment : LineCommentInfo }

let collectTokens initialState (lineTokenizer:FSharpLineTokenizer) =
    let rec loop state (currentState,tokens) =
        match lineTokenizer.ScanToken state with
        | Some tok, nextState ->
            loop nextState (nextState,(tok::tokens)) 
        | None, nextState ->
            nextState,tokens
    
    let finalState,tokensRev = loop initialState (0L,[])

    finalState,tokensRev |> List.rev

let tokenizeText (sourceTokenizer:FSharpSourceTokenizer) sourceCodeLines =
    let rec loop lines state acc =
        match lines with
        | line::restLines ->
            let (latestState,lineTokens) =
                line 
                |> sourceTokenizer.CreateLineTokenizer
                |> collectTokens state
            loop restLines latestState ((line,lineTokens)::acc)
        | [] ->
            acc

    loop sourceCodeLines 0L []
    |> List.rev
    |> List.mapi (fun i (line,tokens) -> line.Replace(" ", "."),i,tokens)

let aggregateCommentTokens (tokens:FSharpTokenInfo list) = 
    let rec foldCommentTokens (foldedToken:FSharpTokenInfo) (tokensLeft:FSharpTokenInfo list) =
        match tokensLeft with
        | [] -> 
            foldedToken,tokensLeft
        | t::ts ->
            let isSameTokenKind = 
                t.ColorClass = FSharpTokenColorKind.Comment && t.CharClass = foldedToken.CharClass
            if isSameTokenKind then
                foldCommentTokens { foldedToken with 
                                        RightColumn = t.RightColumn
                                        FullMatchedLength = t.RightColumn - foldedToken.LeftColumn + 1 } ts
            else
                foldedToken,t::ts
            
    let rec loop acc (tokensLeft:FSharpTokenInfo list) =
        match tokensLeft with
        | [] -> 
            acc
        | t::ts when t.ColorClass <> FSharpTokenColorKind.Comment ->
            loop (t::acc) ts
        | t::ts ->
            let (foldedToken,rest) = foldCommentTokens t ts
            loop (foldedToken::acc) rest

    loop [] tokens |> List.rev

let ofTokenizedLines (linesTokens:(string*int*FSharpTokenInfo list) list) =
    let isComment (token:FSharpTokenInfo) =
        token.ColorClass = FSharpTokenColorKind.Comment

    let isNotComment = isComment >> not

    let isLineComment (token:FSharpTokenInfo) =
        token.CharClass = FSharpTokenCharKind.LineComment

    let isRangeComment (token:FSharpTokenInfo) =
        token.CharClass = FSharpTokenCharKind.Comment

    let commentsRanges =
        linesTokens
        |> List.map (fun (lineBody,line,tokens) -> 
            lineBody,line,tokens |> aggregateCommentTokens)
        |> List.collect (fun (t,l,toks) ->
            match toks with
            | [] -> 
                [l,None]
            | _ -> 
                toks |> List.map (fun tok -> l,tok |> Some))

    let parsed =
        commentsRanges
        |> List.fold (fun comments (line,currToken) ->
            match currToken, comments |> List.tryHead with
            | None, None ->
                comments
            | None, Some prevComment ->
                let updatedComments =
                    match prevComment.Comment with
                    | LineCommentInfo.Line (_,true) | LineCommentInfo.WholeLine true | LineCommentInfo.Range (_,_,true) -> 
                        { prevComment with Comment = LineCommentInfo.WholeLine true }::(comments |> List.skip 1)
                    | _ -> 
                        comments
                { Line = line; Comment = LineCommentInfo.WholeLine true }::updatedComments
            | Some token, None ->
                if token |> isNotComment then
                    comments
                else
                    let comment =
                        if token |> isLineComment then
                            if token.LeftColumn = 0 then { Line = line; Comment = LineCommentInfo.WholeLine false }
                            else { Line = line; Comment = LineCommentInfo.Line (token.LeftColumn,false) }
                        else
                            { Line = line; Comment = LineCommentInfo.Range (token.LeftColumn,token.RightColumn,true) }
                    comment::comments
            | Some token, Some prevComment ->
                if token |> isRangeComment then
                    let updatedComments = 
                        match prevComment.Comment with
                        | LineCommentInfo.Range (left,_,true) when line <> prevComment.Line ->
                            if left = 0 then 
                                { prevComment with Comment = LineCommentInfo.WholeLine true }::(comments |> List.skip 1)
                            else
                                { prevComment with Comment = LineCommentInfo.Line (left,true) }::(comments |> List.skip 1)
                        | LineCommentInfo.Line (left,true) when line <> prevComment.Line ->
                            if left = 0 then 
                                { prevComment with Comment = LineCommentInfo.WholeLine true }::(comments |> List.skip 1)
                            else
                                { prevComment with Comment = LineCommentInfo.Line (left,true) }::(comments |> List.skip 1)
                        | LineCommentInfo.WholeLine true when line <> prevComment.Line ->
                            { prevComment with Comment = LineCommentInfo.WholeLine true }::(comments |> List.skip 1)
                        | _ -> 
                            comments
                    { Line = line; Comment = LineCommentInfo.Range (token.LeftColumn,token.RightColumn,true) }::updatedComments
                elif token |> isNotComment then
                    match prevComment.Comment with
                    | LineCommentInfo.Range (left,right,true) ->
                        { prevComment with Comment = LineCommentInfo.Range (left,right,false) }::(comments |> List.skip 1)
                    | _ ->
                        comments
                else
                    if token |> isLineComment then
                        let comment =
                            if token.LeftColumn = 0 then
                                { Line = line; Comment = LineCommentInfo.WholeLine false }
                            else
                                { Line = line; Comment = LineCommentInfo.Line (token.LeftColumn,false) }
                        comment::comments
                    else
                        comments
            ) []
        |> List.rev

    parsed

let tokenizeText =
    FSharpSourceTokenizer ([], None) |> Tokenizer.tokenizeText 
    
let comments =
    code 
    |> getLines  
    |> tokenizeText
    |> CommentsMap.ofTokenizedLines
