namespace DemoApp.Domain
open Rezoom

type UserId = UserId of int

type SessionToken = SessionToken of string

type FolderId = FolderId of int

type FileId = FileId of int

type FolderOrFileId =
    | Folder of FolderId
    | File of FileId

type Capability =
    | Write
    | Read

type Permission =
    | Grant
    | Deny

type IAuthService =
    abstract Login
        : email : string
        * password : string
        -> SessionToken option Plan

    abstract ValidateSession : token : SessionToken -> UserId Plan

type IAuthenticatedService =
    abstract SessionToken : SessionToken

type IUserService =
    inherit IAuthenticatedService
    abstract AllUsers : unit -> UserId array Plan
    abstract Email : id : UserId -> string Plan
    abstract Name : id : UserId -> string Plan

type IFileSystemEntryService<'id> =
    inherit IAuthenticatedService
    abstract Parent : id : 'id -> FolderId option Plan
    abstract Name : id : 'id -> string Plan
    abstract Move : id : 'id * newParent : FolderId * newName : string -> unit Plan
    abstract Delete : id : 'id -> unit Plan

    abstract GetDirectPermissionSetting : id : 'id * userId : UserId * cap : Capability -> Permission option Plan
    abstract SetDirectPermission : id : 'id * userId : UserId * cap : Capability * perm : Permission option -> unit Plan

type IFolderService =
    inherit IFileSystemEntryService<FolderId>

    abstract RootFolder : unit -> FolderId Plan
    abstract ChildFolders : parentId : FolderId -> FolderId array Plan
    abstract ChildFiles : parentId : FolderId -> FileId array Plan

    abstract Create : parentId : FolderId * name : string -> FolderId Plan

type IFileService =
    inherit IFileSystemEntryService<FileId>

    abstract Create : parentId : FolderId * name : string * content : string -> FileId Plan
    abstract Read : id : FileId -> string Plan
    abstract Write : id : FileId * content : string -> unit Plan

type IDomain =
    abstract Auth : IAuthService
    abstract Users : SessionToken -> IUserService
    abstract Folders : SessionToken -> IFolderService
    abstract Files : SessionToken -> IFileService

[<AutoOpen>]
module Domain =
    let mutable private domain = Unchecked.defaultof<IDomain>

    module Injection =
        let setDomain(implementation) =
            domain <- implementation

    type SessionToken with
        static member Login(email, password) = domain.Auth.Login(email, password)
        member this.Users = domain.Users(this)
        member this.Folders = domain.Folders(this)
        member this.Files = domain.Files(this)
        member this.ValidateSession() = domain.Auth.ValidateSession(this)
        member this.RequireValid() =
            plan {
                let! _ = this.ValidateSession()
                return ()
            }

    type IFileSystemEntryService<'id> with
        member this.HasCapability(entity : 'id, userId : UserId, capability : Capability) : bool Plan =
            plan {
                let! permission = this.GetDirectPermissionSetting(entity, userId, capability)
                match permission with
                | Some Grant -> return true
                | Some Deny -> return false
                | None -> // if not explicitly granted or denied at this level, check the parent
                    let! parent = this.Parent(entity)
                    match parent with
                    | None ->
                        return false // deny by default if no permissions were set all the way up to the root
                    | Some parentFolderId ->
                        return! this.SessionToken.Folders.HasCapability(parentFolderId, userId, capability)
            }
        member this.HasCapability(entity : 'id, capability : Capability) : bool Plan =
            plan {
                let! meId = this.SessionToken.ValidateSession()
                return! this.HasCapability(entity, meId, capability)
            }
        member this.RequireCapability(entity : 'id, capability : Capability) : unit Plan =
            plan {
                let! can = this.HasCapability(entity, capability)
                if not can then
                    failwith "No access"
            }

        member this.Path(entity : 'id) : string Plan =
            plan {
                let! name = this.Name(entity)
                let! parent = this.Parent(entity)
                match parent with
                | None -> return name // "/" for root folder
                | Some parentId ->
                    let! parentPath = this.SessionToken.Folders.Path(parentId)
                    return parentPath + name
            }