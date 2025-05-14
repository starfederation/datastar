using Starfederation.Datastar.Types;
using Starfederation.Datastar.Types.Options;

namespace Starfederation.Datastar.Services;

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
    Task MergeFragmentsAsync(string fragment, ServerSentEventMergeFragmentsOptions? options);
    
    /// <summary>
    ///     Merges fragments into the DOM with the specified options.
    /// </summary>
    /// <param name="fragment">The fragment to merge.</param>
    /// <param name="options">The merge options.</param>
    /// <returns>A task that represents the asynchronous merge operation.</returns>
    Task MergeFragmentsAsync(Span<char> fragment, ServerSentEventMergeFragmentsOptions? options);
    
    /// <summary>
    ///     Removes fragments from the DOM with the specified options.
    /// </summary>
    /// <param name="selector">The selector for the fragments to remove.</param>
    /// <param name="options">The remove options.</param>
    /// <returns>A task that represents the asynchronous remove operation.</returns>
    Task RemoveFragmentsAsync(Selector selector, ServerSentEventRemoveFragmentsOptions? options);
    
    /// <summary>
    ///     Merges signals with the specified options.
    /// </summary>
    /// <param name="dataDatastarSignals">The signals to merge.</param>
    /// <param name="options">The merge options.</param>
    /// <returns>A task that represents the asynchronous merge operation.</returns>
    Task MergeSignalsAsync(DatastarSignals dataDatastarSignals, ServerSentEventMergeSignalsOptions? options);
    
    /// <summary>
    ///     Removes signals with the specified options.
    /// </summary>
    /// <param name="paths">The paths for the signals to remove.</param>
    /// <param name="options">The remove options.</param>
    /// <returns>A task that represents the asynchronous remove operation.</returns>
    Task RemoveSignalsAsync(IEnumerable<SignalPath> paths, ServerSentEventOptions? options);

    /// <summary>
    ///     Executes a script in the browser with the specified options.
    /// </summary>
    /// <param name="script">The script to execute.</param>
    /// <param name="options">The execution options.</param>
    /// <returns>A task that represents the asynchronous execution operation.</returns>
    Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions? options);
}