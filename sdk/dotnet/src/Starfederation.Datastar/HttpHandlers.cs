using System.Text;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using Microsoft.Net.Http.Headers;

namespace Starfederation.Datastar
{
    /// <summary>
    /// Implementation of ISendServerEvent, for sending SSEs to the HttpResponse
    /// </summary>
    public class ServerSentEventHttpHandlers : ISendServerEvent
    {
        private readonly HttpResponse _httpResponse;
        private readonly List<(string, string)> _additionalHeaders = new();
        private Task? _respondTask = null;

        /// <summary>
        /// Initializes a new instance of the ServerSentEventHttpHandlers class.
        /// </summary>
        /// <param name="httpResponse">The HTTP response.</param>
        public ServerSentEventHttpHandlers(HttpResponse httpResponse)
        {
            _httpResponse = httpResponse;
        }

        /// <summary>
        /// Gets the HTTP response.
        /// </summary>
        public HttpResponse HttpResponse => _httpResponse;

        /// <summary>
        /// Adds a header to the response.
        /// </summary>
        /// <param name="name">The header name.</param>
        /// <param name="content">The header content.</param>
        public void AddHeader(string name, string content)
        {
            _additionalHeaders.Add((name, content));
        }

        /// <summary>
        /// Starts sending the response.
        /// </summary>
        /// <returns>A task that represents the asynchronous start operation.</returns>
        public Task StartResponse()
        {
            _respondTask = Task.Run(async () =>
            {
                SetHeader(_httpResponse, (HeaderNames.ContentType, "text/event-stream"));
                
                if (_httpResponse.HttpContext.Request.Protocol == "HTTP/1.1")
                {
                    //TODO test if this is needed
                    SetHeader(_httpResponse, (HeaderNames.CacheControl, "no-cache"));
                    SetHeader(_httpResponse, (HeaderNames.Connection, "keep-alive"));
                    //SetHeader(_httpResponse, (HeaderNames.KeepAlive, "timeout=60"));
                }
                
                foreach (var header in _additionalHeaders)
                {
                    SetHeader(_httpResponse, header);
                }
                
                // Write the initial response headers
                await _httpResponse.Body.FlushAsync();
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

        /// <summary>
        /// Sends a server-sent event.
        /// </summary>
        /// <param name="serverSentEvent">The server-sent event to send.</param>
        /// <returns>A task that represents the asynchronous send operation.</returns>
        public async Task SendServerEventAsync(ServerSentEvent serverSentEvent)
        {
            if (_respondTask == null)
            {
                throw new System.InvalidOperationException("StartResponse() must be called before sending events.");
            }
            
            await _respondTask;
            
            var serializedEvent = serverSentEvent.Serialize();
            var bytes = Encoding.UTF8.GetBytes(serializedEvent);
            await _httpResponse.Body.WriteAsync(bytes);
        }
    }

    /// <summary>
    /// Implementation of IReadSignals, for reading the Signals from the HttpRequest
    /// </summary>
    public class SignalsHttpHandlers : IReadSignals
    {
        private readonly HttpRequest _httpRequest;

        /// <summary>
        /// Initializes a new instance of the SignalsHttpHandlers class.
        /// </summary>
        /// <param name="httpRequest">The HTTP request.</param>
        public SignalsHttpHandlers(HttpRequest httpRequest)
        {
            _httpRequest = httpRequest;
            _httpRequest.EnableBuffering();
        }

        /// <summary>
        /// Gets the HTTP request.
        /// </summary>
        public HttpRequest HttpRequest => _httpRequest;

        private async Task<Optional<string>> ReadRawSignalsAsync()
        {
            if (_httpRequest.Method == "GET")
            {
                // Handle GET request (if implemented)
                return Optional<string>.None;
            }

            // Read the request body
            _httpRequest.Body.Position = 0;
                
            using var reader = new StreamReader(_httpRequest.Body, Encoding.UTF8, leaveOpen: true);
            var body = await reader.ReadToEndAsync();
                
            if (string.IsNullOrWhiteSpace(body))
            {
                return Optional<string>.None;
            }
                
            return Optional<string>.Some(body);
        }

        /// <summary>
        /// Reads the signals from the request.
        /// </summary>
        /// <returns>A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
        public async Task<Optional<Signals>> ReadSignalsAsync()
        {
            var rawSignals = await ReadRawSignalsAsync();
            return rawSignals.HasValue 
                ? Optional<Signals>.Some(new Signals(rawSignals.Value)) 
                : Optional<Signals>.None;
        }

        /// <summary>
        /// Reads the signals from the request and deserializes them to the specified type.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
        public Task<Optional<T>> ReadSignalsAsync<T>()
        {
            return ReadSignalsAsync<T>(JsonSerializerOptions.Default);
        }

        /// <summary>
        /// Reads the signals from the request and deserializes them to the specified type using the provided options.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <param name="jsonSerializerOptions">The JSON serializer options to use.</param>
        /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
        public async Task<Optional<T>> ReadSignalsAsync<T>(JsonSerializerOptions jsonSerializerOptions)
        {
            var signals = await ReadRawSignalsAsync();
            
            if (!signals.HasValue)
            {
                return Optional<T>.None;
            }
            
            try
            {
                var deserialized = JsonSerializer.Deserialize<T>(signals.Value, jsonSerializerOptions);
                return deserialized == null 
                    ? Optional<T>.None 
                    : Optional<T>.Some(deserialized);
            }
            catch
            {
                return Optional<T>.None;
            }
        }
    }
}
