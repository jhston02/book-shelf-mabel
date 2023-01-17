module MabelsBookshelf.Domain.Tests.Book

open Expecto
open MabelsBookshelf.Domain

let book = testList "Book" [

    testList "Book creation" [
        testCase "Create invalid book returns error" <| fun _ ->
            let book = Book.createBook "test" "test" "test" 0us

            Expect.isError book "Book has invalid isbn"

        testCase "Create valid book returns ok" <| fun _ ->
            let book = Book.createBook "test" "0000000000000" "test" 0us

            Expect.isOk book "Book is ok"

        testCase "Create valid book contains create event" <| fun _ ->
            let (Ok book) = Book.createBook "test" "0000000000000" "test" 0us

            let event = List.head (book.GetEventsInOrder ())

            match event with
            | BookCreated _ -> Expect.isTrue true "Is book created event"
            | _ -> Expect.isTrue false "Is not book created event"
    ]
]