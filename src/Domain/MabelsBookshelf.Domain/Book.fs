namespace MabelsBookshelf.Domain

open MabelsBookshelf.Domain.Common
open MabelsBookshelf.Domain.Common.StateMachines
open FsToolkit.ErrorHandling


type Status =
    | Want
    | Reading of uint16
    | DNF
    | Finished
    | Deleted

type BookInfo =
    { Id: Id
      ISBN: ISBN
      OwnerId: OwnerId
      TotalPages: uint16
      Status: Status }
    static member Default =
        { Id = Id.defaultId
          ISBN = ISBN.defaultISBN
          OwnerId = OwnerId.defaultOwnerId
          TotalPages = 0us
          Status = Want }

type BookEvent =
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


type BookCommand =
    | Start
    | Finish
    | ReadToPage of uint16
    | Quit
    | MarkAsWanted
    | Delete
    | Create of
        {| Id: string
           ISBN: string
           OwnerId: string
           TotalPages: uint16 |}

type Book =
    | Book of Aggregate<BookCommand, BookEvent, BookInfo>
    static member Default =
        Book(FSM([], BookInfo.Default))
    static member FromBookInfo bookInfo =
        Book(FSM([], bookInfo))
type ApplyBookEvent = Apply<BookEvent, BookInfo>

module Book =
    type private EvolveBook = Evolve<BookCommand, BookEvent list * BookInfo, Book>

    let private handleStartCommand book =
        match book.Status with
        | Reading _ -> Error "Book already started"
        | Want
        | DNF
        | Finished
        | Deleted ->
            Ok(
                [ BookStarted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private handleFinishCommand book =
        match book.Status with
        | Finished
        | DNF -> Error "Cannot finish this book"
        | Want
        | Deleted ->
            Ok(
                [ BookFinished
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )
        | Reading _ ->
            Ok(
                [ BookFinished
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private handleDeleteCommand book =
        match book.Status with
        | Deleted _ -> Error "Book already deleted"
        | Want
        | DNF
        | Want
        | Finished ->
            Ok(
                [ BookDeleted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )
        | Reading _ ->
            Ok(
                [ BookDeleted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private handleQuitCommand book =
        match book.Status with
        | DNF _ -> Error "Book already quit"
        | Want
        | Deleted
        | Want
        | Finished ->
            Ok(
                [ BookQuit
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )
        | Reading _ ->
            Ok(
                [ BookQuit
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private handleWantCommand book =
        match book.Status with
        | Want -> Error "Book already wanted"
        | DNF
        | Deleted
        | Want
        | Finished ->
            Ok(
                [ BookMarkedAsWanted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )
        | Reading _ ->
            Ok(
                [ BookMarkedAsWanted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private handleReadToPageCommand pageNumber book =
        match book.Status, pageNumber with
        | x, y when y > book.TotalPages || y = 0us -> Error "Invalid page length"
        | x, y when y = book.TotalPages ->
            Ok(
                [ BookFinished
                      {| Id = book.Id
                         OwnerId = book.OwnerId |}
                  BookEvent.ReadToPage
                      {| Id = book.Id
                         OwnerId = book.OwnerId
                         PageNumber = pageNumber |} ]
            )
        | Reading _, pageNumber ->
            Ok(
                [ BookEvent.ReadToPage
                      {| Id = book.Id
                         OwnerId = book.OwnerId
                         PageNumber = pageNumber |} ]
            )
        | _, _ ->
            Ok(
                [ BookEvent.ReadToPage
                      {| Id = book.Id
                         OwnerId = book.OwnerId
                         PageNumber = pageNumber |}
                  BookStarted
                      {| Id = book.Id
                         OwnerId = book.OwnerId |} ]
            )

    let private createBookCreatedEvent id isbn ownerId totalPages =
        result {
            let! isbn = ISBN.create isbn
            and! ownerId = OwnerId.create ownerId
            and! id = Id.create id

            return
                [ BookCreated
                      {| Id = id
                         ISBN = isbn
                         OwnerId = ownerId
                         TotalPages = totalPages |} ]
        }

    let apply: ApplyBookEvent =
        fun (book: BookInfo) (event: BookEvent) ->
            let book' =
                match event with
                | BookCreated book ->
                    { Id = book.Id
                      ISBN = book.ISBN
                      OwnerId = book.OwnerId
                      TotalPages = book.TotalPages
                      Status = Want }
                | BookDeleted _ -> { book with Status = Deleted }
                | BookFinished _ -> { book with Status = Finished }
                | BookQuit _ -> { book with Status = DNF }
                | BookStarted _ -> { book with Status = Reading 0us }
                | BookMarkedAsWanted _ -> { book with Status = Want }
                | BookEvent.ReadToPage event -> { book with Status = Reading event.PageNumber }

            book'


    let private NoEvents = List.empty

    let private evolve: EvolveBook =
        fun (cmd: BookCommand) (book: Book) ->
            result {
                let (Book (FSM (events, book'))) = book

                let! events' =
                    match cmd, book' with
                    | Start, x -> handleStartCommand x
                    | Finish, x -> handleFinishCommand x
                    | Quit, x -> handleQuitCommand x
                    | MarkAsWanted, x -> handleWantCommand x
                    | ReadToPage toPage, x -> handleReadToPageCommand toPage x
                    | Create bookInfo, x ->
                        createBookCreatedEvent bookInfo.Id bookInfo.ISBN bookInfo.OwnerId bookInfo.TotalPages
                    | Delete, x -> handleDeleteCommand x

                let events' = List.rev events'

                let book'' =
                    events' |> List.fold apply book'

                let output = events', book''
                return output, (Book(FSM(events @ events', book'')))
            }

    let startReading book = evolve Start book

    let finishReading book = evolve Finish book

    let quitReading book = evolve Quit book

    let wantToRead book = evolve MarkAsWanted book

    let rec readToPage book toPage = evolve (ReadToPage toPage) book

    let deleteBook book = evolve Delete book

    let createBook id isbn ownerId totalPages =
        evolve
            (Create
                {| Id = id
                   ISBN = isbn
                   OwnerId = ownerId
                   TotalPages = totalPages |})
            Book.Default
