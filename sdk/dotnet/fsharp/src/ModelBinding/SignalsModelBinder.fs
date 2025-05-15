namespace StarFederation.Datastar.ModelBinding

open System.Text.Json.Nodes
open Microsoft.AspNetCore.Mvc.ModelBinding
open Microsoft.AspNetCore.Mvc.ModelBinding.Binders
open StarFederation.Datastar.DependencyInjection
open System.Text.Json
open StarFederation.Datastar.Utility

type SignalsModelBinder(signalsReader:IDatastarSignalsReaderService) =
    interface IModelBinder with
        member this.BindModelAsync(bindingContext) =
            let signalBindingContext = (bindingContext.BindingSource :?> DatastarSignalsBindingSource)
            match signalBindingContext.Path with
            | null | "" ->
                if bindingContext.ModelType.IsValueType || bindingContext.ModelType = typedefof<string> then
                    task {
                        let! signalsJson = signalsReader.ReadSignalsAsync()
                        try
                            let signalsJsonObject = JsonObject.Parse(signalsJson).AsObject()
                            let signals = JsonPath.getValue bindingContext.ModelType signalsJsonObject bindingContext.FieldName
                            bindingContext.Result <- ModelBindingResult.Success(signals)
                        with _ -> bindingContext.Result <- ModelBindingResult.Failed()
                    }
                else
                    task {
                        let! signalsJson = signalsReader.ReadSignalsAsync()
                        try
                            let signalsJsonObject = JsonObject.Parse(signalsJson).AsObject()
                            let signals =
                                if signalsJsonObject.ContainsKey bindingContext.FieldName
                                then JsonPath.getValue bindingContext.ModelType signalsJsonObject bindingContext.FieldName
                                else JsonSerializer.Deserialize(signalsJsonObject, bindingContext.ModelType)
                            bindingContext.Result <- ModelBindingResult.Success(signals)
                        with _ -> bindingContext.Result <- ModelBindingResult.Failed()
                    }
            | path ->
                task {
                    let! signalsJson = signalsReader.ReadSignalsAsync()
                    try
                        let signalsJsonObject = JsonObject.Parse(signalsJson).AsObject()
                        let signals = JsonPath.getValue bindingContext.ModelType signalsJsonObject path
                        bindingContext.Result <- ModelBindingResult.Success(signals)
                    with _ -> bindingContext.Result <- ModelBindingResult.Failed()
                }

type SignalsModelBinderProvider() =
    interface IModelBinderProvider with
        member this.GetBinder providerContext =
            let isSignalsBindingSource = true
            let isSignalsBindingSource = isSignalsBindingSource && providerContext <> null
            let isSignalsBindingSource = isSignalsBindingSource && providerContext.BindingInfo <> null
            let isSignalsBindingSource = isSignalsBindingSource && providerContext.BindingInfo.BindingSource <> null
            let isSignalsBindingSource = isSignalsBindingSource && providerContext.BindingInfo.BindingSource.DisplayName = DatastarSignalsBindingSource.BindingSourceName
            if isSignalsBindingSource
            then BinderTypeModelBinder(typedefof<SignalsModelBinder>)
            else null


