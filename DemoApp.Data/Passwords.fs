module DemoApp.Data.Passwords
open System
open System.Security.Cryptography

let private saltSize = 24
let private hashSize = 20
let private iterations = 5000

let private pbkdf2 (salt : byte array) (password : string) =
    use pbkdf2 = new Rfc2898DeriveBytes(password, salt)
    pbkdf2.IterationCount <- iterations
    pbkdf2.GetBytes(hashSize)

type Hash =
    {   PasswordHash : byte array
        PasswordSalt : byte array
    }

let hashPassword (password : string) =
    let salt = Array.zeroCreate saltSize
    use rng = new RNGCryptoServiceProvider()
    rng.GetBytes(salt)
    let pwdHash = pbkdf2 salt password
    {   PasswordHash = pwdHash
        PasswordSalt = salt
    }

let validatePassword (hash : Hash) (password : string) =
    let pwdHash = pbkdf2 hash.PasswordSalt password
    (pwdHash, hash.PasswordHash) ||> Seq.map2 (=) |> Seq.reduce (&&)

