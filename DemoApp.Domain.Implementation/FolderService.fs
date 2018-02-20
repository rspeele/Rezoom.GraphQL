namespace DemoApp.Domain.Implementation
open System
open Rezoom
open DemoApp.Domain

[<AbstractClass>]
type BaseEntryService<'idType>(sessionToken : SessionToken) =
    abstract ToEntryId : 'idType -> EntryId
    member this.SessionToken = sessionToken
    interface IFileSystemEntryService<'idType> with
        member this.SessionToken = sessionToken
        member this.Parent(id) =
            plan {
                do! sessionToken.RequireValid()
                let! parentEntryId = data().Parent(this.ToEntryId(id))
                return parentEntryId
            }
        member this.Name(id) =
            plan {
                do! sessionToken.RequireValid()
                return! data().Name(this.ToEntryId(id))
            }
        member this.Move(id, newParent, newName) =
            plan {
                do! this.RequireCapability(id, Read)
                do! sessionToken.Folders.RequireCapability(newParent, Write)
                do! data().MoveEntry(this.ToEntryId(id), newParent)
                do! data().RenameEntry(this.ToEntryId(id), newName)
            }
        member this.Delete(id) =
            plan {
                do! this.RequireCapability(id, Write)
                do! data().DeleteEntry(this.ToEntryId(id))
            }
        member this.GetDirectPermissionSetting(id, userId, cap) =
            plan {
                do! sessionToken.RequireValid()
                return! data().GetDirectPermissionSetting(this.ToEntryId(id), userId, cap)
            }
        member this.SetDirectPermission(id, userId, cap, perm) =
            plan {
                do! sessionToken.RequireValid()
                return! data().SetDirectPermission(this.ToEntryId(id), userId, cap, perm)
            }

type FolderService(sessionToken : SessionToken) =
    inherit BaseEntryService<FolderId>(sessionToken)
    override this.ToEntryId(FolderId id) = EntryId id
    interface IFolderService with
        member this.ChildFiles(parentId) =
            plan {
                do! this.RequireCapability(parentId, Read)
                return! data().ChildFiles(parentId)
            }
        member this.ChildFolders(parentId) =
            plan {
                do! this.RequireCapability(parentId, Read)
                return! data().ChildFolders(parentId)
            }
        member this.RootFolder() =
            plan {
                do! this.SessionToken.RequireValid()
                return! data().RootFolder()
            }
        member this.Create(parentId, name) =
            plan {
                let name = name.TrimEnd('/') + "/"
                do! this.RequireCapability(parentId, Write)
                return! data().CreateFolder(parentId, name)
            }

type FileService(sessionToken : SessionToken) =
    inherit BaseEntryService<FileId>(sessionToken)
    override this.ToEntryId(FileId id) = EntryId id
    interface IFileService with
        member this.Create(parentId, name, content) =
            plan {
                do! sessionToken.Folders.RequireCapability(parentId, Write)
                return! data().CreateFile(parentId, name, content)
            }
        member this.Read(id) =
            plan {
                do! this.RequireCapability(id, Read)
                return! data().ReadFile(id)
            }
        member this.Write(id, content) =
            plan {
                do! this.RequireCapability(id, Write)
                return! data().WriteFile(id, content)
            }