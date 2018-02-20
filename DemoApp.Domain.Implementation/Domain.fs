module DemoApp.Domain.Implementation.Domain
open System
open Rezoom
open DemoApp.Domain

let domain =
    { new IDomain with
        member this.Auth = upcast AuthService()
        member this.Users(token) = upcast UserService(token)
        member this.Folders(token) = upcast FolderService(token)
        member this.Files(token) = upcast FileService(token)
    }