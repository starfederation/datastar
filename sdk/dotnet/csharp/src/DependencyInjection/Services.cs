
using System.Security.Cryptography;
using System.Text.Json;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public interface IDatastarServerSentEventService
{
    void AddHeaders(params KeyValuePair<string, string>[] httpHeaders);
    Task StartServerEventStream();
    Task MergeFragmentsAsync(string fragments, MergeFragmentsOptions? options = null);
    Task RemoveFragmentsAsync(string selector, RemoveFragmentsOptions? options = null);
    Task MergeSignalsAsync(string dataSignals, MergeSignalsOptions? options = null);
    Task RemoveSignalsAsync(IEnumerable<string> paths, EventOptions? options = null);
    Task ExecuteScriptAsync(string script, ExecuteScriptOptions? options = null);
}

public interface IDatastarSignalsReaderService
{
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

internal class ServerSentEventService(Core.ISendServerEvent handler) : IDatastarServerSentEventService
{
    public void AddHeaders(params KeyValuePair<string, string>[] httpHeaders) => _additionalHeaders.AddRange(httpHeaders ?? []);

    public Task StartServerEventStream() => handler.StartServerEventStream(_additionalHeaders.Select(kv => kv.AsTuple()).ToArray());

    public Task MergeFragmentsAsync(string fragments, MergeFragmentsOptions? options = null) => handler.SendServerEvent(Core.ServerSentEventGenerator.MergeFragments(fragments, options ?? new()));

    public Task RemoveFragmentsAsync(string selector, RemoveFragmentsOptions? options = null) => handler.SendServerEvent(Core.ServerSentEventGenerator.RemoveFragments(selector, options ?? new()));

    public Task MergeSignalsAsync(string dataSignals, MergeSignalsOptions? options = null) => handler.SendServerEvent(Core.ServerSentEventGenerator.MergeSignals(dataSignals, options ?? new()));

    public Task RemoveSignalsAsync(IEnumerable<string> paths, EventOptions? options = null) => handler.SendServerEvent(Core.ServerSentEventGenerator.RemoveSignals(paths, options ?? new()));

    public Task ExecuteScriptAsync(string script, ExecuteScriptOptions? options = null) => handler.SendServerEvent(Core.ServerSentEventGenerator.ExecuteScript(script, options ?? new()));

    private List<KeyValuePair<string, string>> _additionalHeaders = new();
}

internal class SignalsReaderService(Core.IReadSignals handler) : IDatastarSignalsReaderService
{
    public Stream GetSignalsStream() => handler.GetSignalsStream();

    public async Task<string?> ReadSignalsAsync()
    {
        string? signals = await handler.ReadSignalsAsync();
        return String.IsNullOrEmpty(signals) ? null : signals;
    }

    public async Task<TType?> ReadSignalsAsync<TType>(JsonSerializerOptions? jsonSerializerOptions = null)
    {
        FSharpValueOption<TType> read = await handler.ReadSignalsAsync<TType>(jsonSerializerOptions ?? JsonSerializerOptions.Default);
        return read.IsSome ? read.Value : default(TType?);
    }
}