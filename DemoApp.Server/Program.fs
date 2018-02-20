module Program
open System
open System.Collections.Generic
open System.Text
open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json

open Rezoom
open Rezoom.Execution
open Rezoom.GraphQL

open DemoApp.Domain
open DemoApp.Domain.Implementation
open DemoApp.Data
open DemoApp.GraphQL

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

// pull query string out of { query: "..." } wrapper since that's what GraphiQL wants
type RequestWrapper = { query : string }

let execConfig =
    let log() =
        let mutable queries = 0
        let mutable stepsWithQueries = 0
        let mutable queriesInStep = 0
        { new ExecutionLog() with
            override this.OnEndStep() =
                queries <- queries + queriesInStep
                if queriesInStep > 0 then
                    stepsWithQueries <- stepsWithQueries + 1
                    printfn "Ran %d queries in batch %d" queriesInStep stepsWithQueries
                queriesInStep <- 0
            override this.OnPreparingErrand(e) =
                queriesInStep <- queriesInStep + 1
        }
    { Rezoom.Execution.ExecutionConfig.Default with
        Instance = fun () ->
            printfn "%s starting execution %s" (String('-', 20)) (String('-', 20))
            ExecutionInstance(log())
    }

let execPlan plan = Rezoom.Execution.execute execConfig plan

[<EntryPoint>]
let main argv =
    Queries.migrate()
    Injection.setData(Queries.dataLayer)
    Injection.setDomain(Domain.domain)

    let (SessionToken token) as sessionToken = (execPlan (SessionToken.Login("robert.s.peele@gmail.com", "test"))).Result.Value
    printfn "using session token %s" token

    let runQuery =
        let query = Query()
        let schema = AutoSchema(query, Mutation())

        let variables =
            { new IVariableSet with
                member this.GetVariableValue(name) = raise (System.NotImplementedException())
            }
        fun query ->
            let plan = ValidatedDocument(schema, query).Execute(variables, sessionToken, schema, None)
            async {
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let! executed = Async.AwaitTask <| execPlan plan
                sw.Stop()
                printfn "Executed GraphQL operation in %d ms" sw.ElapsedMilliseconds
                let wrapper = // wrap response with { data: ... } since that's what GraphiQL wants
                    let fields = Dictionary()
                    fields.Add("data", { Source = SourceInfo.Artificial; Value = executed })
                    ObjectValue fields
                return Serialization.json wrapper
            }
    let handleReq : WebPart =
        fun (context : HttpContext) ->
            let data = Encoding.UTF8.GetString(context.request.rawForm)
            let wrapper = JsonConvert.DeserializeObject<RequestWrapper>(data)
            async {
                let! result = runQuery wrapper.query
                return! OK result context
            }
    let app =
        choose
            [   POST >=> choose
                    [   path "/gql" >=> handleReq
                    ]
            ]

    use cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf app
    
    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
    
    cts.Cancel()

    0 // return an integer exit code
