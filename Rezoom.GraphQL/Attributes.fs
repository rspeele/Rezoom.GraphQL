namespace Rezoom.GraphQL
open System
open System.Reflection

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method, Inherited = true)>]
[<AllowNullLiteral>]
type GraphQLIgnoreAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method ||| AttributeTargets.Class)>]
[<AllowNullLiteral>]
type GraphQLNameAttribute(name : string) =
    inherit Attribute()
    member this.Name = name
    static member GetName(mem : MemberInfo) =
        let attr = mem.GetCustomAttribute<GraphQLNameAttribute>()
        if isNull attr then mem.Name else attr.Name

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Method ||| AttributeTargets.Class)>]
[<AllowNullLiteral>]
type GraphQLDescriptionAttribute(description : string) =
    inherit Attribute()
    member this.Description = description
    static member GetDescription(par : ParameterInfo) =
        let attr = par.GetCustomAttribute<GraphQLDescriptionAttribute>()
        if isNull attr then None else Some attr.Description
    static member GetDescription(mem : MemberInfo) =
        let attr = mem.GetCustomAttribute<GraphQLDescriptionAttribute>()
        if isNull attr then None else Some attr.Description

type internal GraphQLDeprecationAttribute =
    static member GetDeprecatedReason(mem : MemberInfo) =
        let attr = mem.GetCustomAttribute<ObsoleteAttribute>()
        if isNull attr then None else Some (if isNull attr.Message then "Obsolete" else attr.Message)