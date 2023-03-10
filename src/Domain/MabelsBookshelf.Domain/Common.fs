module MabelsBookshelf.Domain.Common



type Author = private Author of string

type ISBN = private ISBN of string

type Title = private Title of string

type ExternalId = private ExternalId of string

type Category = private Category of string

type Id = private Id of string

type OwnerId = private OwnerId of string

module ISBN =

    let create isbn =
        if String.length isbn = 13 then
            Ok(ISBN isbn)
        else
            Error "Invalid isbn"

    let defaultISBN = ISBN "0000000000000"


module OwnerId =
    let create ownerId = Ok(OwnerId ownerId)

    let defaultOwnerId = OwnerId ""

module Id =
    let create id = Ok(Id id)

    let defaultId = Id ""

module StateMachines =
    type FSM<'Input, 'Output> = FSM of 'Output
    type Evolve<'Input, 'Output, 'FSM> = 'Input -> 'FSM -> Result<'Output * 'FSM, string>
    type Aggregate<'Command, 'Event, 'State> = FSM<'Command, 'Event list * 'State>
    type Apply<'Event, 'Aggregate> = 'Aggregate -> 'Event -> 'Aggregate
