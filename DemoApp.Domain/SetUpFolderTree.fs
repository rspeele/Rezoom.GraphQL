module DemoApp.Domain.SetUpFolderTree
open System
open Rezoom
open DemoApp.Domain

let private folderNames =
    [|  "foo"
        "bar"
        "baz"
        "qux"
        "car"
        "gar"
        "ned"
    |]

let private fileNames =
    [|  "snacks.txt", "Soda\nChips\nPeanuts\nPistachios\nPretzels\nGummy Bears"
        "shakespeare.txt", "To be or not to be, that is the question."
        "benfranklin.txt", "An investment in knowledge pays the best interest"
        "konami.txt", "up up down down left right left right b a start"
    |]

let private sort x y = if x < y then x,y else y,x

let rec private setupRandomChildren depth (random : Random) (session : SessionToken) (parent : FolderId) =
    plan {
        if depth < 5 then
            let folder1 = random.Next(0, folderNames.Length)
            let folder2 = random.Next(0, folderNames.Length)
            let folder1, folder2 = sort folder1 folder2
            for name in batch folderNames.[folder1..folder2] do
                let! childId = session.Folders.Create(parent, name)
                do! setupRandomChildren (depth + 1) random session childId

            let file1 = random.Next(0, fileNames.Length)
            let file2 = random.Next(0, fileNames.Length)
            let file1, file2 = sort file1 file2
            for name, content in batch fileNames.[file1..file2] do
                let! _ = session.Files.Create(parent, name, content)
                ()
    }

let setupRoot (session : SessionToken) =
    plan {
        let random = Random()
        let! root = session.Folders.RootFolder()
        let! children = session.Folders.ChildFolders(root)
        if children.Length > 0 then
            return failwith "Already set up!"
        else
            return! setupRandomChildren 0 random session root
    }