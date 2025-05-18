using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.AspNetCore.Http;
using StarFederation.Datastar.Extensions;
using StarFederation.Datastar.Interfaces;
using StarFederation.Datastar.Types;

namespace StarFederation.Datastar.Services;

/// <summary>
///     Implementation of IDatastarSignalsReaderService.
/// </summary>
public class SignalsReaderService : IDatastarSignalsReaderService
{
    private HttpRequest HttpRequest { get; }

    /// <summary>
    ///     Initializes a new instance of the SignalsHttpHandlers class.
    /// </summary>
    /// <param name="httpContextAccessor"></param>
    public SignalsReaderService(HttpContextAccessor httpContextAccessor)
    {
        ArgumentNullException.ThrowIfNull(httpContextAccessor);
        ArgumentNullException.ThrowIfNull(httpContextAccessor.HttpContext);
        
        var httpRequest = httpContextAccessor.HttpContext?.Request;
        
        ArgumentNullException.ThrowIfNull(httpRequest);
        
        HttpRequest = httpRequest;
        HttpRequest.EnableBuffering();
    }

    /// <inheritdoc/>
    public Task<string?> ReadSignalsAsStringAsync()
    {
        return HttpRequest.ReadSignalsAsStringAsync();
    }

    /// <inheritdoc/>
    public Task<JsonNode?> ReadSignalsAsJsonNodeAsync()
    {
        return HttpRequest.ReadSignalsAsJsonNodeAsync();
    }

    /// <inheritdoc/>
    public Task<T?> ReadSignalsAsync<T>(JsonSerializerOptions? jsonSerializerOptions = null) where T : class
    {
        return HttpRequest.ReadSignalsAsync<T>(jsonSerializerOptions);
    }
}