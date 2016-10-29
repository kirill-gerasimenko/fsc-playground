#load "../paket-files/include-scripts/net46/include.expecto.fsx"

open System
open System.IO

open Expecto
open Expecto.Impl

// taken from FSAC project
let inline isNotNull v = not (isNull v)

// taken from FSAC project
let getLines (str: string) =
    use reader = new StringReader(str)
    [
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            yield !line
            line := reader.ReadLine()
        if str.EndsWith("\n") then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
    ]


let fsiPrinters = 
  { TestPrinters.beforeRun = fun (test:Test) -> printfn "Running test '%s' ..." (match test with | TestLabel (label,_) -> label | _ -> "")
    beforeEach = fun n -> printfn "'%s' starting..." n
    passed = fun n d -> printfn "'%s' passed in %A." n d
    ignored = fun n m -> printfn "'%s' was ignored. %s" n m
    failed = fun n m d -> printfn "'%s' failed in %A. %s" n d m
    exn = fun n e d -> printfn "'%s' errored in %A. %A" n d e
    summary = fun summary -> printfn "Finished." }

let fsiConfig = { defaultConfig with printer = fsiPrinters }
let fsiRunTest = Expecto.Tests.runTests fsiConfig
let (==?) actual expected = Expect.equal actual expected ""