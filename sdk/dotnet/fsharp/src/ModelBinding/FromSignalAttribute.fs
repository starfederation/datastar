namespace StarFederation.Datastar.FSharp.ModelBinding

open System
open Microsoft.AspNetCore.Mvc.ModelBinding

module DatastarSignalsBindingSource =
    let BindingSourceName = "DatastarSource"

type DatastarSignalsBindingSource(Path:string) =
    inherit BindingSource(DatastarSignalsBindingSource.BindingSourceName, DatastarSignalsBindingSource.BindingSourceName, true, true)
    member this.Path = Path

[<AttributeUsage(AttributeTargets.Parameter + AttributeTargets.Property, AllowMultiple = false, Inherited = true)>]
type FromSignalsAttribute() =
    inherit Attribute()
    member val Path = "" with get, set

    with
    interface IBindingSourceMetadata with
        member this.BindingSource = DatastarSignalsBindingSource(this.Path)