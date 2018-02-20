namespace DemoApp.GraphQL
open System
open Rezoom
open Rezoom.GraphQL
open Rezoom.GraphQL.AutoSchema
open DemoApp.Domain

type User(id : UserId) =
    inherit GraphQLEntity<SessionToken>()
    member this.Id = id
    member this.Name(session : SessionToken) = session.Users.Name(id)
    member this.Email(session : SessionToken) = session.Users.Email(id)

type Folder(id : FolderId) =
    inherit GraphQLEntity<SessionToken>()
    member this.Id = id
    member this.Name(session : SessionToken) = session.Folders.Name(id)
    member this.Parent(session : SessionToken) =
        plan {
            let! parent = session.Folders.Parent(id)
            return parent |> Option.map Folder
        }
    member this.Path(session : SessionToken) = session.Folders.Path(id)
    member this.ChildFolders(session : SessionToken) =
        plan {
            let! childFolderIds = session.Folders.ChildFolders(id)
            return [| for childFolderId in childFolderIds -> Folder(childFolderId) |]
        }
    member this.ChildFiles(session : SessionToken) =
        plan {
            let! childFileIds = session.Folders.ChildFiles(id)
            return [| for childFileId in childFileIds -> File(childFileId) |]
        }
    member this.HasCapability(session : SessionToken, capability : Capability, user : UserId option) =
        plan {
            let! userId =
                match user with
                | None -> session.ValidateSession()
                | Some userId -> Plan.ret userId
            return! session.Folders.HasCapability(id, userId, capability)
        }

and File(id : FileId) =
    inherit GraphQLEntity<SessionToken>()
    member this.Id = id
    member this.Name(session : SessionToken) = session.Files.Name(id)
    member this.Parent(session : SessionToken) =
        plan {
            let! parent = session.Files.Parent(id)
            return parent |> Option.map Folder
        }
    member this.Path(session : SessionToken) = session.Files.Path(id)
    member this.Content(session : SessionToken) = session.Files.Read(id)
    member this.HasCapability(session : SessionToken, capability : Capability, user : UserId option) =
        plan {
            let! userId =
                match user with
                | None -> session.ValidateSession()
                | Some userId -> Plan.ret userId
            return! session.Files.HasCapability(id, userId, capability)
        }

type Query() =
    inherit GraphQLRootQuery<SessionToken>()
    member this.Users(session : SessionToken) =
        plan {
            let! userIds = session.Users.AllUsers()
            return [| for userId in userIds -> User(userId) |]
        }
    member this.Root(session : SessionToken) =
        plan {
            let! rootId = session.Folders.RootFolder()
            return Folder(rootId)
        }

type Mutation() =
    inherit GraphQLEntity<SessionToken>()
    member this.DeleteFolder(session : SessionToken, id) = session.Folders.Delete(id)
    member this.DeleteFile(session : SessionToken, id) = session.Files.Delete(id)
    member this.CreateFolder(session : SessionToken, parentId, name) =
        plan {
            let! folderId = session.Folders.Create(parentId, name)
            return Folder(folderId)
        }
    member this.CreateFile(session : SessionToken, parentId, name, content) =
        plan {
            let! fileId = session.Files.Create(parentId, name, content)
            return File(fileId)
        }
    member this.EditFile(session : SessionToken, id, content) = session.Files.Write(id, content)
    member this.SetFolderPermission(session : SessionToken, folderId, userId, capability, permission) =
        session.Folders.SetDirectPermission(folderId, userId, capability, permission)
    member this.SetFilePermission(session : SessionToken, fileId, userId, capability, permission) =
        session.Files.SetDirectPermission(fileId, userId, capability, permission)

    [<Obsolete("Only for first-time setup")>]
    member this.CreateFolderTree(session : SessionToken) =
        SetUpFolderTree.setupRoot session

