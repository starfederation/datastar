module internal StarFederation.Datastar.Utility

open System
open Microsoft.FSharp.Reflection

let unionCaseFromString<'a> (str:string) args =
    match FSharpType.GetUnionCases(typeof<'a>) |> Array.filter (fun unionCaseInfo -> unionCaseInfo.Name.ToLower() = str.ToLower()) with
    | [| unionCaseInfo |] -> ValueSome (FSharpValue.MakeUnion( unionCaseInfo, args ) :?> 'a)
    | _ -> ValueNone

let splitLine (line:string) = line.Split( [| "\r\n"; "\n"; "\r" |], StringSplitOptions.None)

module ValueOption =
    let fromEmptyString (thing:string) =
        if String.IsNullOrEmpty(thing)
        then ValueNone
        else ValueSome thing
