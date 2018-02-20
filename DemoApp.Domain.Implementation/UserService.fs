namespace DemoApp.Domain.Implementation
open System
open Rezoom
open DemoApp.Domain

type UserService(sessionToken : SessionToken) =
    interface IUserService with
        member this.SessionToken = sessionToken
        member this.AllUsers() =
            plan {
                do! sessionToken.RequireValid()
                return! data().AllUsers()
            }
        member this.Email(userId) =
            plan {
                do! sessionToken.RequireValid()
                return! data().Email(userId)
            }
        member this.Name(userId) =
            plan {
                do! sessionToken.RequireValid()
                return! data().Name(userId)
            }