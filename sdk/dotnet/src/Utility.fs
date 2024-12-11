module internal StarFederation.Datastar.Utility

open System
open Microsoft.FSharp.Reflection

let inline guard against =
    match against with
    | null -> raise (NullReferenceException $"Guarding against null")
    | goodThing -> goodThing

let unionCaseFromString<'a> (str:string) args =
    match FSharpType.GetUnionCases(typeof<'a>) |> Array.filter (fun unionCaseInfo -> unionCaseInfo.Name.ToLower() = str.ToLower()) with
    | [| unionCaseInfo |] -> ValueSome (FSharpValue.MakeUnion( unionCaseInfo, args ) :?> 'a)
    | _ -> ValueNone

let lowerFirstCharacter item =
    String.Concat(Char.ToLowerInvariant($"%A{item}"[0]), $"%A{item}".Substring(1))

let splitLine (line:string) = line.Split( [| "\r\n"; "\n"; "\r" |], StringSplitOptions.None)

let tryDeserialize<'T> (deserializer:string -> 'T) (str:string) =
    try
        Ok (deserializer str)
    with ex -> Error ex

module ValueOption =
    let fromEmptyString (thing:string) =
        if String.IsNullOrEmpty(thing)
        then ValueNone
        else ValueSome thing
