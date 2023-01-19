module MabelsBookshelf.Domain.Tests.Book

open Expecto
open MabelsBookshelf.Domain
open MabelsBookshelf.Domain.Common

let id = Id.defaultId
let isbn = ISBN.defaultISBN
let ownerId = OwnerId.defaultOwnerId

let book =
    testList
        "Book"
        [
          testList
              "Book creation"
              [ testCase "Create invalid book returns error"
                <| fun _ ->
                    let book =
                        Book.createBook "test" "test" "test" 0us

                    Expect.isError book "Book has invalid isbn"

                testCase "Create valid book returns ok"
                <| fun _ ->
                    let book =
                        Book.createBook "test" "0000000000000" "test" 0us

                    Expect.isOk book "Book is ok"

                testCase "Create valid book contains create event"
                <| fun _ ->
                    let (Ok book) =
                        Book.createBook "test" "0000000000000" "test" 0us

                    let event =
                        List.head (book.GetEventsInOrder())

                    match event with
                    | BookCreated _ -> Expect.isTrue true "Is book created event"
                    | _ -> Expect.isTrue false "Is not book created event" ]
          testList
              "Start reading"
               [ testCase "Start reading wanted book returns ok"
                 <| fun _ ->
                    let book = {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want ; Events = []}

                    let result = Book.startReading book

                    Expect.isOk result "Book is ok"

                 testCase "Start reading already reading book returns error"
                 <| fun _ ->
                    let book = {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Reading 0us ; Events = []}

                    let result = Book.startReading book

                    Expect.isError result "Book is errored"

                 testCase "Start reading book contains StartedReading"
                 <| fun _ ->
                    let book = {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want ; Events = []}

                    let result = Result.defaultValue book (Book.startReading book)

                    let event =
                        List.head (result.GetEventsInOrder())

                    match event with
                    | BookStarted _ -> Expect.isTrue true "Is BookStarted event"
                    | _ -> Expect.isTrue false "Is not BookStarted event"
                ]]
