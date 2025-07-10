using System.Text.Json;
using Microsoft.Extensions.Primitives;
using Microsoft.FSharp.Core;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public interface IDatastarService
{
    Task StartServerEventStreamAsync(IEnumerable<KeyValuePair<string, StringValues>>? additionalHeaders = null);
    Task StartServerEventStreamAsync(IEnumerable<KeyValuePair<string, string>>? additionalHeaders = null);

    Task PatchElementsAsync(string elements, PatchElementsOptions? options = null);

    Task RemoveElementAsync(string selector, RemoveElementOptions? options = null);

    /// <summary>
    /// Note: If TType is string then it is assumed that it is an already serialized Signals, otherwise serialize with jsonSerializerOptions
    /// </summary>
    Task PatchSignalsAsync<TType>(TType signals, JsonSerializerOptions? jsonSerializerOptions = null, PatchSignalsOptions? patchSignalsOptions = null);

    /// <summary>
    /// Execute a JS script on the client. Note: Do NOT include "&lt;script&gt;" encapsulation
    /// </summary>
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

internal class DatastarService(Core.ServerSentEventGenerator serverSentEventGenerator) : IDatastarService
{
    public Task StartServerEventStreamAsync(IEnumerable<KeyValuePair<string, StringValues>>? additionalHeaders) =>
        serverSentEventGenerator.StartServerEventStreamAsync(additionalHeaders ?? []);

    public Task StartServerEventStreamAsync(IEnumerable<KeyValuePair<string, string>>? additionalHeaders) =>
        serverSentEventGenerator.StartServerEventStreamAsync(additionalHeaders?.Select(kvp => new KeyValuePair<string, StringValues>(kvp.Key, new StringValues(kvp.Value))) ?? []);

    public Task PatchElementsAsync(string elements, PatchElementsOptions? options = null) =>
        serverSentEventGenerator.PatchElementsAsync(elements, options ?? Core.PatchElementsOptions.Defaults);

    public Task RemoveElementAsync(string selector, RemoveElementOptions? options = null) =>
        serverSentEventGenerator.RemoveElementAsync(selector, options ?? Core.RemoveElementOptions.Defaults);

    public Task PatchSignalsAsync<TType>(TType signals, JsonSerializerOptions? jsonSerializerOptions = null, PatchSignalsOptions? patchSignalsOptions = null) =>
        serverSentEventGenerator.PatchSignalsAsync(signals as string ?? JsonSerializer.Serialize(signals, jsonSerializerOptions), patchSignalsOptions ?? Core.PatchSignalsOptions.Defaults);

    public Task ExecuteScriptAsync(string script, ExecuteScriptOptions? options = null) =>
        serverSentEventGenerator.ExecuteScriptAsync(script, options ?? Core.ExecuteScriptOptions.Defaults);

    public Stream GetSignalsStream() => serverSentEventGenerator.GetSignalsStream();

    public async Task<string?> ReadSignalsAsync()
    {
        string? signals = await serverSentEventGenerator.ReadSignalsAsync();
        return String.IsNullOrEmpty(signals) ? null : signals;
    }

    public async Task<TType?> ReadSignalsAsync<TType>(JsonSerializerOptions? jsonSerializerOptions = null)
    {
        FSharpValueOption<TType> read = await serverSentEventGenerator.ReadSignalsAsync<TType>(jsonSerializerOptions ?? Core.JsonSerializerOptions.SignalsDefault);
        return read.IsSome ? read.Value : default;
    }
}