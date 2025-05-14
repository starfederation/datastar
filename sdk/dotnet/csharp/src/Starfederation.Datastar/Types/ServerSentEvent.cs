using Cysharp.Text;
using Starfederation.Datastar.Enumerations;

namespace Starfederation.Datastar.Types;

/// <summary>
///     Represents a Server Sent Event.
/// </summary>
public class ServerSentEvent
{
    /// <summary>
    ///     Gets the type of the event.
    /// </summary>
    public EventType EventType { get; init; }

    /// <summary>
    ///     Gets the ID of the event.
    /// </summary>
    public string? Id { get; init; }

    /// <summary>
    ///     Gets the retry duration for the event.
    /// </summary>
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    /// <summary>
    ///     Gets the data lines for the event.
    /// </summary>
    public string[] DataLines { get; init; } = [];

    /// <summary>
    ///     Serializes the ServerSentEvent to a string.
    /// </summary>
    /// <returns>The serialized ServerSentEvent.</returns>
    public string Serialize()
    {
        var sb = ZString.CreateStringBuilder();

        sb.AppendLine($"event: {Consts.EventType.ToString(EventType)}");

        if (Id != null)
        {
            sb.AppendLine($"id: {Id}");
        }

        if (Retry != TimeSpan.Zero)
        {
            sb.AppendLine($"retry: {(int)Retry.TotalMilliseconds}");
        }

        foreach (var dataLine in DataLines)
        {
            sb.AppendLine($"data: {dataLine}");
        }

        // Add an extra newline at the end to complete the event
        sb.AppendLine("");

        return sb.ToString();
    }
}