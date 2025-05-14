using System.Text;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using Microsoft.Net.Http.Headers;
using Starfederation.Datastar.Types;

namespace Starfederation.Datastar.Handlers;

/// <summary>
///     Implementation of ISendServerEvent, for sending SSEs to the HttpResponse
/// </summary>
public class ServerSentEventSenderHttpHandlers : IServerEventSender
{
    private readonly List<(string, string)> _additionalHeaders = [];
    private Task? _respondTask;

    /// <summary>
    ///     Initializes a new instance of the ServerSentEventHttpHandlers class.
    /// </summary>
    /// <param name="httpResponse">The HTTP response.</param>
    public ServerSentEventSenderHttpHandlers(HttpResponse httpResponse)
    {
        HttpResponse = httpResponse;
    }

    /// <summary>
    ///     Gets the HTTP response.
    /// </summary>
    public HttpResponse HttpResponse { get; }

    /// <summary>
    ///     Sends a server-sent event.
    /// </summary>
    /// <param name="serverSentEvent">The server-sent event to send.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public async Task SendServerEventAsync(ServerSentEvent serverSentEvent)
    {
        if (_respondTask == null)
        {
            throw new InvalidOperationException("StartResponse() must be called before sending events.");
        }

        await _respondTask;

        var serializedEvent = serverSentEvent.Serialize();
        var bytes = Encoding.UTF8.GetBytes(serializedEvent);
        await HttpResponse.Body.WriteAsync(bytes);
    }

    /// <summary>
    ///     Adds a header to the response.
    /// </summary>
    /// <param name="name">The header name.</param>
    /// <param name="content">The header content.</param>
    public void AddHeader(string name, string content)
    {
        _additionalHeaders.Add((name, content));
    }

    /// <summary>
    ///     Starts sending the response.
    /// </summary>
    /// <returns>A task that represents the asynchronous start operation.</returns>
    public Task StartResponse()
    {
        _respondTask = Task.Run(async () =>
        {
            SetHeader(HttpResponse, (HeaderNames.ContentType, "text/event-stream"));

            if (HttpResponse.HttpContext.Request.Protocol == "HTTP/1.1")
            {
                //TODO test if this is needed
                SetHeader(HttpResponse, (HeaderNames.CacheControl, "no-cache"));
                SetHeader(HttpResponse, (HeaderNames.Connection, "keep-alive"));
                //SetHeader(_httpResponse, (HeaderNames.KeepAlive, "timeout=60"));
            }

            foreach (var header in _additionalHeaders)
            {
                SetHeader(HttpResponse, header);
            }

            // Write the initial response headers
            await HttpResponse.Body.FlushAsync();
            return;

            void SetHeader(HttpResponse httpResponse, (string name, string content) header)
            {
                if (!httpResponse.Headers.ContainsKey(header.name))
                {
                    httpResponse.Headers[header.name] = new StringValues(header.content);
                }
            }
        });

        return _respondTask;
    }
}