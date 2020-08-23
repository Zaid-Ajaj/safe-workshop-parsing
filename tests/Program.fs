module Tests

open Expecto

let testParse input expected =
    test (sprintf "Trying to parse %s" input) {
        Expect.equal (Program.parseInt input) expected "Parsed like it should"
    }

let tests = testList "Parsing numbers works" [
    test "512 can be converted" {
        match Program.parseInt "512" with
        | Ok number -> Expect.equal number 512  "Parsed correctly"
        | somethingElse -> failwithf "Not expected: %A" somethingElse
    }

    testParse "10" (Ok 10)

    testParse "512" (Ok 512)

    testParse "" (Error "Input was empty")

    test "Empty string returns error" {
        Expect.isError (Program.parseInt "") "Should be an error"
    }

    test "Non-digit characters return error" {
        Expect.isError (Program.parseInt "dawdaw") "Should be an error"
    }
]

[<EntryPoint>]
let main argv = runTests defaultConfig tests
