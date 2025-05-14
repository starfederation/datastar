namespace Starfederation.Datastar.Types;

/// <summary>
///     Interface for sending server-sent events.
/// </summary>
public interface ISendServerEvent
{
    /// <summary>
    ///     Sends a server-sent event.
    /// </summary>
    /// <param name="serverSentEvent">The server-sent event to send.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    Task SendServerEventAsync(ServerSentEvent serverSentEvent);
}