module MabelsBookshelf.Domain.Book1

open MabelsBookshelf.Domain.Common
open MabelsBookshelf.Domain.Common.StateMachines


type Status =
    | Want
    | Reading of uint16
    | DNF
    | Finished
    | Deleted

type BookState =
    { Id: Id
      ISBN: ISBN
      OwnerId: OwnerId
      TotalPages: uint16
      Status: Status}

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
    | Create

type Book = Book of Aggregate<BookCommand, BookEvent, BookState>
type private EvolveBook = Evolve<BookCommand, BookEvent list * BookState, Book>


let private handleStartCommand book =
    match book.Status with
    | Reading _  -> Error "Book already started"
    | Want | DNF  | Finished | Deleted -> Ok ([BookStarted {| Id = book.Id ; OwnerId = book.OwnerId |}])

let private handleFinishCommand book =
    match book.Status with
    | Finished | DNF -> Error "Cannot finish this book"
    | Want | Deleted  -> Ok ([BookFinished {| Id = book.Id ; OwnerId = book.OwnerId |}])
    | Reading _ -> Ok ([BookFinished {| Id = book.Id ; OwnerId = book.OwnerId |}])

let private handleDeleteCommand book =
    match book.Status with
    | Deleted _  -> Error "Book already deleted"
    | Want | DNF | Want | Finished -> Ok ([BookDeleted {| Id = book.Id ; OwnerId = book.OwnerId |}])
    | Reading _ -> Ok ([BookDeleted {| Id = book.Id ; OwnerId = book.OwnerId |}])

let private handleQuitCommand book =
    match book.Status with
    | DNF _  -> Error "Book already quit"
    | Want | Deleted | Want | Finished -> Ok ([BookQuit {| Id = book.Id ; OwnerId = book.OwnerId |}])
    | Reading _ -> Ok ([BookQuit {| Id = book.Id ; OwnerId = book.OwnerId |}])

let private handleWantCommand book =
    match book.Status with
    | Want  -> Error "Book already wanted"
    | DNF | Deleted | Want | Finished -> Ok ([BookMarkedAsWanted {| Id = book.Id ; OwnerId = book.OwnerId |}])
    | Reading _ -> Ok ([BookMarkedAsWanted {| Id = book.Id ; OwnerId = book.OwnerId |}])

let rec private handleReadToPageCommand pageNumber book =
    match book.Status, pageNumber with
    | x , y when y > book.TotalPages || y = 0us -> Error "Invalid page length"
    | Reading _, pageNumber -> Ok([BookEvent.ReadToPage {| Id = book.Id ; OwnerId = book.OwnerId ; PageNumber = pageNumber |}])
    | _ , _ -> Ok([BookEvent.ReadToPage {| Id = book.Id ; OwnerId = book.OwnerId ; PageNumber = pageNumber |}; BookStarted {| Id = book.Id ; OwnerId = book.OwnerId |}])

let private NoEvents = List.empty
let private evolve: EvolveBook =
    fun (cmd: BookCommand) (book: Book) ->
        let (Book(FSM (events, book'))) = book

        let events' =
            match cmd, book' with
            | Start, x -> handleStartCommand x
            | Finish, x -> handleFinishCommand x
            | Quit, x -> handleQuitCommand x
            | MarkAsWanted, x -> handleWantCommand x
            | ReadToPage toPage, x -> handleReadToPageCommand toPage x
        ()