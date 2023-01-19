namespace MabelsBookshelf.Domain

open MabelsBookshelf.Domain
open MabelsBookshelf.Domain.Common
open FSharpPlus

type Status =
    | Want
    | Reading of uint16
    | DNF
    | Finished
    | Deleted

type Event =
    | BookCreated of
        {| Id: Id
           ISBN: ISBN
           OwnerId: OwnerId
           TotalPages: uint16 |}
    | BookDeleted of {| Id: Id; OwnerId: OwnerId |}
    | BookFinished of {| Id: Id; OwnerId: OwnerId |}
    | BookStarted of {| Id: Id; OwnerId: OwnerId |}
    | BookQuit of {| Id: Id; OwnerId: OwnerId |}
    | BookMarkedAsWanted of {| Id: Id; OwnerId: OwnerId |}
    | ReadToPage of
        {| Id: Id
           OwnerId: OwnerId
           PageNumber: uint16 |}

type Book =
    { Id: Id
      ISBN: ISBN
      OwnerId: OwnerId
      TotalPages: uint16
      Status: Status
      Events: Event list }

    static member Default =
        { Id = Id.defaultId
          ISBN = ISBN.defaultISBN
          OwnerId = OwnerId.defaultOwnerId
          TotalPages = 0us
          Status = Want
          Events = [] }

    member this.GetEventsInOrder() = List.rev this.Events


module Book =
    let private updateBook book state = { book with Status = state }


    let apply book event =
        match event with
        | BookCreated bookInfo ->
            { Id = bookInfo.Id
              ISBN = bookInfo.ISBN
              OwnerId = bookInfo.OwnerId
              TotalPages = bookInfo.TotalPages
              Events = [ event ]
              Status = Want }
        | BookDeleted _ -> updateBook book Deleted
        | BookFinished _ -> updateBook book Finished
        | BookStarted _ -> updateBook book (Reading 0us)
        | BookQuit _ -> updateBook book DNF
        | BookMarkedAsWanted _ -> updateBook book Want
        | ReadToPage eventInfo -> updateBook book (Reading eventInfo.PageNumber)

    let private whenEvent ({ Events = events } as book) event =
        let book =
            { book with Events = event :: events }

        apply book event

    let startReading book =
        match book.Status with
        | Reading _ -> Error "Already reading book"
        | _ ->
            Ok(
                whenEvent
                    book
                    (BookStarted
                        {| Id = book.Id
                           OwnerId = book.OwnerId |})
            )

    let finishReading book =
        match book.Status with
        | Finished -> Error "Already finished book"
        | DNF -> Error "Book not finished"
        | _ ->
            Ok(
                whenEvent
                    book
                    (BookFinished
                        {| Id = book.Id
                           OwnerId = book.OwnerId |})
            )

    let quitReading book =
        match book.Status with
        | DNF _ -> Error "Already quit book"
        | _ ->
            Ok(
                whenEvent
                    book
                    (BookQuit
                        {| Id = book.Id
                           OwnerId = book.OwnerId |})
            )

    let wantToRead book =
        match book.Status with
        | Want _ -> Error "Already want book"
        | _ ->
            Ok(
                whenEvent
                    book
                    (BookMarkedAsWanted
                        {| Id = book.Id
                           OwnerId = book.OwnerId |})
            )

    let private finishBookIfReadToEnd toPage book =
        if toPage = book.TotalPages then
            finishReading book
        else
            Ok book

    let private readToPageImpl book toPage =
        if toPage > 0us then
            whenEvent
                book
                (ReadToPage
                    {| Id = book.Id
                       OwnerId = book.OwnerId
                       PageNumber = toPage |})
            |> finishBookIfReadToEnd toPage
        else
            Ok(book)

    let rec readToPage book toPage =
        if toPage > book.TotalPages then
            Error "Please enter a valid page number"
        else
            match book.Status with
            | Reading pageNumber when toPage = pageNumber -> Error "Already read to that page"
            | Reading _ -> readToPageImpl book toPage
            | Finished -> Error "Already finished book"
            | _ ->
                match startReading book with
                | Ok result -> readToPage result toPage
                | error -> error

    let deleteBook book =
        match book.Status with
        | Deleted _ -> Error "Already deleted book"
        | _ ->
            Ok(
                whenEvent
                    book
                    (BookDeleted
                        {| Id = book.Id
                           OwnerId = book.OwnerId |})
            )

    let createBook id isbn ownerId totalPages =
        monad' {
            let! isbn = ISBN.create isbn
            let! ownerId = OwnerId.create ownerId
            let! id = Id.create id

            return
                whenEvent
                    Book.Default
                    (BookCreated
                        {| Id = id
                           ISBN = isbn
                           OwnerId = ownerId
                           TotalPages = totalPages |})
        }
