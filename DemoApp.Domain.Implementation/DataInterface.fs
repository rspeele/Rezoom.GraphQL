namespace DemoApp.Domain.Implementation
open System
open Rezoom
open DemoApp.Domain

// for when we don't care if it's a file or folder on the data side
type EntryId = EntryId of int

type IDataLayer =
    abstract CreateSession : UserId -> SessionToken Plan
    abstract GetSession : SessionToken -> (UserId * DateTime) Plan

    abstract PasswordValidator : email : string -> (UserId * (string -> bool)) Plan
    abstract AllUsers : unit -> UserId array Plan
    abstract Email : userId : UserId -> string Plan
    abstract Name : userId : UserId -> string Plan

    abstract RootFolder : unit -> FolderId Plan
    abstract ChildFolders : parent : FolderId -> FolderId array Plan
    abstract ChildFiles : parent : FolderId -> FileId array Plan

    abstract Name : id : EntryId -> string Plan
    abstract Parent : id : EntryId -> FolderId option Plan
    abstract DeleteEntry : id : EntryId -> unit Plan
    abstract MoveEntry : id : EntryId * newParent : FolderId -> unit Plan
    abstract RenameEntry : id : EntryId * newName : string -> unit Plan
    abstract GetDirectPermissionSetting : id : EntryId * userId : UserId * cap : Capability -> Permission option Plan
    abstract SetDirectPermission : id : EntryId * userId : UserId * cap : Capability * perm : Permission option -> unit Plan

    abstract CreateFolder : parentId : FolderId * name : string -> FolderId Plan
    abstract CreateFile : parentId : FolderId * name : string * content : string -> FileId Plan
    abstract ReadFile : id : FileId -> string Plan
    abstract WriteFile : id : FileId * content : string -> unit Plan

[<AutoOpen>]
module DataLayer =
    let mutable private layer = Unchecked.defaultof<IDataLayer>

    module Injection =
        let setData(implementation) =
            layer <- implementation

    let data() = layer
