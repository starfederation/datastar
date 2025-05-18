using System.Text;
using Microsoft.AspNetCore.Http;
using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Extensions;
using StarFederation.Datastar.Interfaces;
using StarFederation.Datastar.Types;
using StarFederation.Datastar.Types.Options;

namespace StarFederation.Datastar.Services;

/// <summary>
///     Implementation of IDatastarServerSentEventService.
/// </summary>
public class ServerSentEventService : SignalsReaderService, IDatastarServerSentEventService, IDatastarSignalsReaderService
{
    private HttpResponse HttpResponse { get; }
    
    /// <summary>
    ///     Initializes a new instance of the ServerSentEventService class.
    /// </summary>
    /// <param name="httpContextAccessor"></param>
    public ServerSentEventService(HttpContextAccessor httpContextAccessor) : base(httpContextAccessor)
    {
        ArgumentNullException.ThrowIfNull(httpContextAccessor);
        ArgumentNullException.ThrowIfNull(httpContextAccessor.HttpContext);
        ArgumentNullException.ThrowIfNull(httpContextAccessor.HttpContext.Response);
        
        HttpResponse = httpContextAccessor.HttpContext.Response;
    }

    public Task MergeFragmentsAsync(string fragment, ServerSentEventMergeFragmentsOptions? options = null)
    {
        throw new NotImplementedException();
    }

    public Task MergeFragmentsAsync(Span<char> fragment, ServerSentEventMergeFragmentsOptions? options = null)
    {
        throw new NotImplementedException();
    }

    public Task RemoveFragmentsAsync(DatastarSelector datastarSelector, ServerSentEventRemoveFragmentsOptions? options = null)
    {
        throw new NotImplementedException();
    }

    public Task MergeSignalsAsync<T>(T? dataDatastarSignals, ServerSentEventMergeSignalsOptions? options = null) where T : class
    {
        throw new NotImplementedException();
    }

    public Task RemoveSignalsAsync(IEnumerable<SignalPath> paths, ServerSentEventOptions? options = null)
    {
        throw new NotImplementedException();
    }

    public Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions? options = null)
    {
        throw new NotImplementedException();
    }

    public Task BrowserConsoleActionAsync(BrowserConsoleAction action, string? message = null, EventOptions? options = null)
    {
        throw new NotImplementedException();
    }
}