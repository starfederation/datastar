
using System.Text.Json;
using Microsoft.FSharp.Core;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public interface IDatastarService
{
    Task StartServerEventStream(params (string, string)[] additionalHeaders);
    Task StartServerEventStream(params KeyValuePair<string, string>[] additionalHeaders);
    Task PatchElementsAsync(string fragments, PatchElementsOptions? options = null);
    Task RemoveElementAsync(string selector, RemoveFragmentOptions? options = null);
    /// <summary>
    /// Note: If TType is string then it is assumed that it is an already serialized Signals, otherwise serialize with jsonSerializerOptions
    /// </summary>
    Task PatchSignalsAsync<TType>(TType signals, JsonSerializerOptions? jsonSerializerOptions = null, PatchSignalsOptions? patchSignalsOptions = null);
    Task ExecuteScriptAsync(string script, ExecuteScriptOptions? options = null);

    /// <summary>
    /// Get the serialized signals as a stream
    /// </summary>
    Stream GetSignalsStream();

    /// <summary>
    /// Read the signals and return as a serialized string
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the serialized signals.</returns>
    Task<string?> ReadSignalsAsync();

    /// <summary>
    /// Read the signals and deserialize as a TType
    /// </summary>
    /// <returns>A task that represents the asynchronous read and deserialize operation. The result contains the deserialized data.</returns>
    Task<TType?> ReadSignalsAsync<TType>(JsonSerializerOptions? options = null);
}

internal class DatastarService(Core.ISendServerEvent sendServerEventHandler, Core.IReadSignals signalsHandler) : IDatastarService
{
    public Task StartServerEventStream(params (string, string)[] additionalHeaders)
        => sendServerEventHandler.StartServerEventStream(additionalHeaders.Select(kv => kv.AsTuple()).ToArray());

    public Task StartServerEventStream(params KeyValuePair<string, string>[] additionalHeaders)
        => sendServerEventHandler.StartServerEventStream(additionalHeaders.Select(kv => kv.AsTuple()).ToArray());

    public Task PatchElementsAsync(string fragments, PatchElementsOptions? options = null)
        => sendServerEventHandler.SendServerEvent(Core.ServerSentEventGenerator.PatchElements(fragments, options ?? new()));

    public Task RemoveElementAsync(string selector, RemoveFragmentOptions? options = null)
        => sendServerEventHandler.SendServerEvent(Core.ServerSentEventGenerator.RemoveElement(selector, options ?? new()));

    public Task PatchSignalsAsync<TType>(TType signals, JsonSerializerOptions? jsonSerializerOptions = null, PatchSignalsOptions? patchSignalsOptions = null)
        => sendServerEventHandler.SendServerEvent(Core.ServerSentEventGenerator.PatchSignals(signals as string ?? JsonSerializer.Serialize(signals, jsonSerializerOptions), patchSignalsOptions ?? new()));

    public Task ExecuteScriptAsync(string script, ExecuteScriptOptions? options = null)
        => sendServerEventHandler.SendServerEvent(Core.ServerSentEventGenerator.ExecuteScript(script, options ?? new()));

    public Stream GetSignalsStream() => signalsHandler.GetSignalsStream();

    public async Task<string?> ReadSignalsAsync()
    {
        string? signals = await signalsHandler.ReadSignalsAsync();
        return String.IsNullOrEmpty(signals) ? null : signals;
    }

    public async Task<TType?> ReadSignalsAsync<TType>(JsonSerializerOptions? jsonSerializerOptions = null)
    {
        FSharpValueOption<TType> read = await signalsHandler.ReadSignalsAsync<TType>(jsonSerializerOptions ?? Core.JsonSerializerOptions.SignalsDefault);
        return read.IsSome ? read.Value : default;
    }
}