module DemoApp.Data.Queries
open System
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Plans
open DemoApp.Domain
open DemoApp.Domain.Implementation
open DemoApp.Data.Passwords

type CreateSession = SQL<"""
    insert into Sessions row Token = @token, UserId = @userId, StartedUtc = sysutcdatetime()
""">

let sessionTokenCharacters =
    [|  [| 'a' .. 'z' |]
        [| 'A' .. 'Z' |]
        [| '0' .. '9' |]
    |] |> Array.concat |> String

let createSession (UserId id) =
    plan {
        let token = SecureRandomString.ofLength 16 sessionTokenCharacters
        do! CreateSession.Command(token, id).Plan()
        return SessionToken token
    }

type GetSession = SQL<"""
    select UserId, StartedUtc from Sessions where Token = @token
""">

let getSession (SessionToken token) =
    plan {
        let! row = GetSession.Command(token).ExactlyOne()
        return UserId row.UserId, row.StartedUtc
    }

type GetUserPasswordHash = SQL<"""
    select Id, PasswordHash, PasswordSalt from Users where Email = @email
""">

let passwordValidator email =
    plan {
        let! row = GetUserPasswordHash.Command(email).ExactlyOne()
        let hash =
            {   PasswordHash = row.PasswordHash
                PasswordSalt = row.PasswordSalt
            }
        return UserId row.Id, validatePassword hash
    }

type AllUsers = SQL<"""
    select Id from Users
""">

let allUsers() =
    plan {
        let! rows = AllUsers.Command().Plan()
        return [| for row in rows -> UserId row.Id |]
    }

type GetUser = SQL<"""
    select Email, Name from Users where Id = @id
""">

let userEmail (UserId id) =
    plan {
        let! row = GetUser.Command(id).ExactlyOne()
        return row.Email
    }

let userName (UserId id) =
    plan {
        let! row = GetUser.Command(id).ExactlyOne()
        return row.Name
    }

type RootFolder = SQL<"""
    select Id from Entries where ParentFolderId is null
""">

let rootFolder() =
    plan {
        let! row = RootFolder.Command().ExactlyOne()
        return FolderId row.Id
    }

type Children = SQL<"""
    select coalesce(ParentFolderId, 0) as ParentFolderId, Id, Content is null as IsFolder from Entries where ParentFolderId in @parentId
""">

let childrenFactory = SharedCommandFactory<int, _>((fun ids -> Children.Command(Seq.toArray ids)), fun row -> row.ParentFolderId)

let childFolders (FolderId parentId) =
    plan {
        let! children = Plan.ofErrand <| childrenFactory.ErrandForKey(parentId)
        return
            [| for child in children do 
                if child.IsFolder = Some true then yield FolderId child.Id
            |]
    }

let childFiles (FolderId parentId) =
    plan {
        let! children = Plan.ofErrand <| childrenFactory.ErrandForKey(parentId)
        return
            [| for child in children do 
                if child.IsFolder = Some false then yield FileId child.Id
            |]
    }

type EntryInfo = SQL<"""
    select Id, Name, ParentFolderId from Entries where Id in @id
""">

let entryInfoFactory = SharedCommandFactory<int, _>((fun ids -> EntryInfo.Command(Seq.toArray ids)), fun row -> row.Id)

let entryName (EntryId id) =
    plan {
        let! entries = Plan.ofErrand <| entryInfoFactory.ErrandForKey(id)
        return entries.[0].Name
    }

let entryParent (EntryId id) =
    plan {
        let! entries = Plan.ofErrand <| entryInfoFactory.ErrandForKey(id)
        return entries.[0].ParentFolderId |> Option.map FolderId
    }

type DeleteEntry = SQL<"""
    delete from Entries where Id = @id
""">

let deleteEntry (EntryId id) =
    DeleteEntry.Command(id).Plan()

type MoveEntry = SQL<"""
    update Entries
        set ParentFolderId = @newParentId
    where Id = @id
""">

let moveEntry (EntryId id) (FolderId newParentId) =
    MoveEntry.Command(id = id, newParentId = Some newParentId).Plan()

type RenameEntry = SQL<"""
    update Entries
        set Name = @newName
    where Id = @id
""">

let renameEntry (EntryId id) (newName : string) =
    RenameEntry.Command(id, newName).Plan()

type GetDirectPermissionSetting = SQL<"""
    select * from Permissions
    where EntryId in @entryId
""">

let permissionSettingFactory = SharedCommandFactory<int, _>((fun ids -> GetDirectPermissionSetting.Command(Seq.toArray ids)), fun row -> row.EntryId)

let capabilityToInt = function
    | Read -> 1
    | Write -> 2

let intToCapability = function
    | 1 -> Read
    | 2 -> Write
    | _ -> failwith "Invalid capability setting"

let getDirectPermissionSetting (EntryId entryId) (UserId userId) (cap : Capability) =
    plan {
        let! rows = Plan.ofErrand <| permissionSettingFactory.ErrandForKey(entryId)
        let capabilityInt = capabilityToInt cap
        let matchedRow =
            rows |> Seq.tryFind(fun r -> r.Capability = capabilityInt && r.UserId = userId)
        return
            match matchedRow with
            | None -> None
            | Some r -> Some (if r.Grant then Grant else Deny)
    }

type SetDirectPermission = SQL<"""
    -- remove existing setting, if any

    delete from Permissions
    where EntryId = @entryId
    and UserId = @userId
    and Capability = @capability;

    -- insert new permission but only if we explicitly are setting grant true/false

    insert into Permissions(EntryId, UserId, Capability, Grant)
    select @entryId, @userId, @capability, @grant
    where @grant is not null;
""">

let setDirectPermission (EntryId entryId) (UserId userId) (cap : Capability) (perm : Permission option) =
    plan {
        let capabilityInt = capabilityToInt cap
        let grantBool = perm |> Option.map ((=) Grant)
        let cmd =
            SetDirectPermission.Command
                (capability = capabilityInt, entryId = entryId, grant = grantBool, userId = userId)
        return! cmd.Plan()
    }

type CreateEntry = SQL<"""
    insert into Entries
    row ParentFolderId = @parentId
    , Name = @name
    , Content = @content
    ;

    select scope_identity() x;
""">

let createFolder (FolderId parentId) (name : string) =
    plan {
        let! id = CreateEntry.Command(parentId = Some parentId, name = name, content = None).Scalar()
        return FolderId id
    }

let createFile (FolderId parentId) (name : string) (content : string) =
    plan {
        let! id = CreateEntry.Command(parentId = Some parentId, name = name, content = Some content).Scalar()
        return FileId id
    }

type ReadFile = SQL<"""
    select Content from Entries where Id = @id and Content is not null
""">

let readFile (FileId id) =
    plan {
        let! row = ReadFile.Command(id).ExactlyOne()
        return row.Content |> Option.get
    }

type WriteFile = SQL<"""
    update Entries set Content = @content where Id = @id and Content is not null
""">

let writeFile (FileId id) (content : string) =
    WriteFile.Command(id = id, content = Some content).Plan()

let dataLayer =
    { new IDataLayer with
        member this.AllUsers() = allUsers()
        member this.ChildFiles(parent) = childFiles(parent)
        member this.ChildFolders(parent) = childFolders(parent)
        member this.CreateFile(parentId, name, content) = createFile parentId name content
        member this.CreateFolder(parentId, name) = createFolder parentId name
        member this.CreateSession(userId) = createSession userId
        member this.DeleteEntry(id) = deleteEntry id
        member this.Email(userId) = userEmail userId
        member this.GetDirectPermissionSetting(id, userId, cap) = getDirectPermissionSetting id userId cap
        member this.GetSession(token) = getSession token
        member this.MoveEntry(id, newParent) = moveEntry id newParent
        member this.Name(id: EntryId) = entryName id
        member this.Name(userId: UserId) = userName userId
        member this.Parent(id) = entryParent id
        member this.PasswordValidator(email) = passwordValidator email
        member this.ReadFile(id) = readFile id
        member this.RenameEntry(id, newName) = renameEntry id newName
        member this.RootFolder() = rootFolder()
        member this.SetDirectPermission(id, userId, cap, perm) = setDirectPermission id userId cap perm
        member this.WriteFile(id, content) = writeFile id content
    }

type Model = SQLModel<".">

let migrate() =
    Model.Migrate(Migrations.MigrationConfig.Default)