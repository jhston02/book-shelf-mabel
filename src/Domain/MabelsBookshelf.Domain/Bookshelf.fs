module MabelsBookshelf.Domain.Bookshelf

open MabelsBookshelf.Domain.Common



type BookshelfInfo =
    { Id: Id
      Books: Id list
      Description: string option
      Name: string
      OwnerId: OwnerId }

type BookshelfEvent =
    | BookshelfCreated of
        {| Id: Id
           Books: Id list
           Description: string option
           Name: string
           OwnerId: OwnerId |}
    | BookAdded of
        {| BookId: Id
           Id: Id
           OwnerId: OwnerId |}
    | BookRemoved of
        {| BookId: Id
           Id: Id
           OwnerId: OwnerId |}
    | BookshelfDeleted of {| Id: Id; OwnerId: OwnerId |}
    | BookshelfRenamed of
        {| Name: string
           Id: Id
           OwnerId: OwnerId |}
    | DescriptionChanged of
        {| Description: string
           Id: Id
           OwnerId: OwnerId |}
