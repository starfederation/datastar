using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Types;
using StarFederation.Datastar.Types.Options;

namespace StarFederation.Datastar.Interfaces;

/// <summary>
///     Interface for Datastar server-sent event service.
/// </summary>
public interface IDatastarServerSentEventService : IDatastarSignalsReaderService
{
    /// <summary>
    ///     Merges fragments into the DOM with the specified options.
    /// </summary>
    /// <param name="fragment">The fragment to merge.</param>
    /// <param name="options">The merge options.</param>
    /// <returns>A task that represents the asynchronous merge operation.</returns>
    Task MergeFragmentsAsync(string fragment, ServerSentEventMergeFragmentsOptions? options= null);
    
    /// <summary>
    ///     Merges fragments into the DOM with the specified options.
    /// </summary>
    /// <param name="fragment">The fragment to merge.</param>
    /// <param name="options">The merge options.</param>
    /// <returns>A task that represents the asynchronous merge operation.</returns>
    Task MergeFragmentsAsync(Span<char> fragment, ServerSentEventMergeFragmentsOptions? options= null);
    
    /// <summary>
    ///     Removes fragments from the DOM with the specified options.
    /// </summary>
    /// <param name="datastarSelector">The selector for the fragments to remove.</param>
    /// <param name="options">The remove options.</param>
    /// <returns>A task that represents the asynchronous remove operation.</returns>
    Task RemoveFragmentsAsync(DatastarSelector datastarSelector, ServerSentEventRemoveFragmentsOptions? options= null);
    
    /// <summary>
    ///     Merges signals with the specified options.
    /// </summary>
    /// <param name="dataDatastarSignals">The signals to merge.</param>
    /// <param name="options">The merge options.</param>
    /// <returns>A task that represents the asynchronous merge operation.</returns>
    Task MergeSignalsAsync<T>(T? dataDatastarSignals, ServerSentEventMergeSignalsOptions? options= null) where T : class;
    
    /// <summary>
    ///     Removes signals with the specified options.
    /// </summary>
    /// <param name="paths">The paths for the signals to remove.</param>
    /// <param name="options">The remove options.</param>
    /// <returns>A task that represents the asynchronous remove operation.</returns>
    Task RemoveSignalsAsync(IEnumerable<SignalPath> paths, ServerSentEventOptions? options= null);

    /// <summary>
    ///     Executes a script in the browser with the specified options.
    /// </summary>
    /// <param name="script">The script to execute.</param>
    /// <param name="options">The execution options.</param>
    /// <returns>A task that represents the asynchronous execution operation.</returns>
    Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions? options = null);

    /// <summary>
    /// Executes a browser action (Log, Error, Clear) through a server-sent event.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="message"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    Task BrowserConsoleActionAsync(BrowserConsoleAction action, string? message = null, EventOptions? options = null);
}