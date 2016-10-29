open System
open System.IO

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
