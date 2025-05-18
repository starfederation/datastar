using Cysharp.Text;
using StarFederation.Datastar.Enumerations;

namespace StarFederation.Datastar.Types;

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
    public TimeSpan Retry { get; init; } = DatastarConstants.DefaultSseRetryDuration;

    /*/// <summary>
    ///     Gets the data lines for the event.
    /// </summary>
    public IEnumerable<string> DataLines { get; init; } = [];*/
    
    internal Utf8ValueStringBuilder DataLineBuilder { get; } = ZString.CreateUtf8StringBuilder();
    
    public void AddDataLine(string dataLine)
    {
        DataLineBuilder.AppendLine($"data: {dataLine}");
    }

    /// <summary>
    ///     Serializes the ServerSentEvent to a string.
    /// </summary>
    /// <returns>The serialized ServerSentEvent.</returns>
    public ReadOnlyMemory<byte> Serialize()
    {
        var sb = ZString.CreateUtf8StringBuilder();
        
        sb.AppendLine($"event: {EventType.ToStringFast(true)}");

        if (Id != null)
        {
            sb.AppendLine($"id: {Id}");
        }

        if (Retry != TimeSpan.Zero)
        {
            sb.AppendLine($"retry: {(int)Retry.TotalMilliseconds}");
        }

        var dataLines = DataLineBuilder.ToString();
        sb.Append(dataLines);

        // Add an extra newline at the end to complete the event
        sb.AppendLine(string.Empty);

        return sb.AsMemory();
    }
}