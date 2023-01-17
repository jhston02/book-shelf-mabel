module MabelsBookshelf.Domain.Tests.Program

open Expecto
open MabelsBookshelf.Domain.Tests.Book

let all =
    testList "All"
        [
            book
        ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all