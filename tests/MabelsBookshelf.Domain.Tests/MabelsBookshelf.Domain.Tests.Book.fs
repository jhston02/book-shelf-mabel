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

                    let (events, _) = fst book

                    let test = (List.head events)

                    match test with
                    | BookCreated _ -> Expect.isTrue true "Is book created event"
                    | _ -> Expect.isTrue false "Is not book created event" ]
          testList
              "Start reading"
               [ testCase "Start reading wanted book returns ok"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want ;}

                    let result = Book.startReading book

                    Expect.isOk result "Book is ok"

                 testCase "Start reading already reading book returns error"
                 <| fun _ ->
                    let book =  Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Reading 0us ;}

                    let result = Book.startReading book

                    Expect.isError result "Book is errored"

                 testCase "Start reading book contains StartedReading"
                 <| fun _ ->
                    let book =  Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want ;}

                    let result = Result.defaultValue (([], BookInfo.Default), book) (Book.startReading book)

                    let (events, _) = fst result

                    let test = (List.head events)

                    match test with
                    | BookStarted _ -> Expect.isTrue true "Is BookStarted event"
                    | _ -> Expect.isTrue false "Is not BookStarted event"
                ]
          testList
              "Finish reading"
               [ testCase "Finish reading wanted book returns ok"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want}

                    let result = Book.finishReading book

                    Expect.isOk result "Book is ok"

                 testCase "Finish reading already finished book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Finished }

                    let result = Book.finishReading book

                    Expect.isError result "Book is errored"

                 testCase "Finish reading already quit book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = DNF }

                    let result = Book.finishReading book

                    Expect.isError result "Book is errored"

                 testCase "Finish reading book contains FinishedReading"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want}

                    let result = Result.defaultValue (([], BookInfo.Default), book) (Book.finishReading book)

                    let (events, _) = fst result

                    let test = (List.head events)

                    match test with
                    | BookFinished _ -> Expect.isTrue true "Is BookFinished event"
                    | _ -> Expect.isTrue false "Is not BookFinished event"
                ]
          testList
              "Quit reading"
               [ testCase "Quit reading wanted book returns ok"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want }

                    let result = Book.quitReading book

                    Expect.isOk result "Book is ok"

                 testCase "Quit reading already Quit book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = DNF }

                    let result = Book.quitReading book

                    Expect.isError result "Book is errored"

                 testCase "Quit reading book contains DNF"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want }

                    let result = Result.defaultValue (([], BookInfo.Default), book) (Book.quitReading book)

                    let (events, _) = fst result

                    let test = (List.head events)

                    match test with
                    | BookQuit _ -> Expect.isTrue true "Is BookQuit event"
                    | _ -> Expect.isTrue false "Is not BookQuit event"
                ]
          testList
              "Want to read"
               [ testCase "Want to read dnf book returns ok"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = DNF }

                    let result = Book.wantToRead book

                    Expect.isOk result "Book is ok"

                 testCase "Want to read already wanted book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = Want }

                    let result = Book.wantToRead book

                    Expect.isError result "Book is errored"

                 testCase "Want to read book contains Wanted"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = DNF }

                    let result = Result.defaultValue (([], BookInfo.Default), book) (Book.wantToRead book)

                    let (events, _) = fst result

                    let test = (List.head events)

                    match test with
                    | BookMarkedAsWanted _ -> Expect.isTrue true "Is BookMarkedAsWanted event"
                    | _ -> Expect.isTrue false "Is not BookMarkedAsWanted event"
                ]
          testList
              "Read to page"
               [ testCase "Read to page dnf book returns ok"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 20us ; OwnerId = ownerId ; Status = DNF }

                    let result = Book.readToPage book 10us

                    Expect.isOk result "Book is ok"

                 testCase "Read to page already at page book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 20us ; OwnerId = ownerId ; Status = Reading 10us }

                    let result = Book.readToPage book 10us

                    Expect.isError result "Book is errored"

                 testCase "Read to page page 0 at page book returns error"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 10us ; OwnerId = ownerId ; Status = Want }

                    let result = Book.readToPage book 0us

                    Expect.isError result "Book is errored"

                 testCase "Want to read book contains Wanted"
                 <| fun _ ->
                    let book = Book.FromBookInfo {Id = id ; ISBN = isbn ; TotalPages = 0us ; OwnerId = ownerId ; Status = DNF }

                    let result = Result.defaultValue (([], BookInfo.Default), book) (Book.wantToRead book)

                    let (events, _) = fst result

                    let test = (List.head events)

                    match test with
                    | BookMarkedAsWanted _ -> Expect.isTrue true "Is BookMarkedAsWanted event"
                    | _ -> Expect.isTrue false "Is not BookMarkedAsWanted event"
                ]]
