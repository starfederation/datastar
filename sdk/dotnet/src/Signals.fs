namespace StarFederation.Datastar

open System.Threading.Tasks

type ISignals = abstract Serialize : unit -> string

type IReadRawSignals = abstract ReadRawSignals: unit -> ValueTask<string voption>
type IDeserializeSignals = abstract DeserializeSignals: string -> ISignals

module Signals =
    let private readRawSignals (env:IReadRawSignals) = env.ReadRawSignals()
    let private deserializeSignals (env:IDeserializeSignals) rawSignal = env.DeserializeSignals(rawSignal)

    let readSignals env = task {
        let! rawSignals = readRawSignals env
        return rawSignals |> ValueOption.map (deserializeSignals env)
        }

type SignalsPath = string
module SignalsPath =
    let value = id
    let create (signalsPath:string) = SignalsPath signalsPath
    let tryCreate (signalsPath:string) = ValueSome (create signalsPath)
