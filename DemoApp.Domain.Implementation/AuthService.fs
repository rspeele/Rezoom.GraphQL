namespace DemoApp.Domain.Implementation
open System
open Rezoom
open DemoApp.Domain

type AuthService() =
    interface IAuthService with
        member this.Login(email, password) =
            plan {
                let! userId, validator = data().PasswordValidator(email)
                if validator password then
                    let! token = data().CreateSession(userId)
                    return Some token
                else
                    return None

            }
        member this.ValidateSession(token) =
            plan {
                let! userId, created = data().GetSession(token)
                let now = DateTime.UtcNow // TODO: use DateTime.UtcNowPlan but fix it counting as a non-cached errand
                let expiration = created + TimeSpan.FromDays(1.0)
                return
                    if now >= expiration then
                        failwith "Session is expired"
                    else
                        userId
            }