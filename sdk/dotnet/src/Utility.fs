module internal StarFederation.Datastar.Utility

open System
open System.Collections.Generic
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.FSharp.Reflection

let unionCaseFromString<'a> (str:string) args =
    match FSharpType.GetUnionCases(typeof<'a>) |> Array.filter (fun unionCaseInfo -> unionCaseInfo.Name.ToLower() = str.ToLower()) with
    | [| unionCaseInfo |] -> ValueSome (FSharpValue.MakeUnion( unionCaseInfo, args ) :?> 'a)
    | _ -> ValueNone

module internal ValueOption =
    let fromEmptyString (thing:string) =
        if String.IsNullOrEmpty(thing)
        then ValueNone
        else ValueSome thing

module internal JsonPath =
    let rec getValue (valueType:Type) (jObject:JsonObject) (path:string) =
        match path.Split('.', 2) with
        | [| segment |] -> jObject[segment].Deserialize(valueType)
        | [| segment; segments |] -> getValue valueType ((jObject.Item segment).AsObject()) segments
        | _ -> jObject.Deserialize(valueType)

module internal KeyValuePair =
    let toTuple (keyValuePair:KeyValuePair<'TKey, 'TValue>) = (keyValuePair.Key, keyValuePair.Value)

module internal String =
    let newLines = [| "\r\n"; "\n"; "\r" |]
    let split (delimiters:string seq) (line:string) = line.Split(delimiters |> Seq.toArray, StringSplitOptions.None)
    let IsPopulated = String.IsNullOrWhiteSpace >> not
    let toKebab (pascalString:string) =
        (StringBuilder(), pascalString.ToCharArray())
        ||> Seq.fold (fun stringBuilder chr ->
            if Char.IsUpper(chr)
            then stringBuilder.Append("-").Append(Char.ToLower(chr))
            else stringBuilder.Append(chr)
            )
        |> _.Replace("-", "", 0, 1).ToString()