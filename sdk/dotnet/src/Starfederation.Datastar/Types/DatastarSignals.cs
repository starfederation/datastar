using System.Text.Json.Nodes;

namespace Starfederation.Datastar.Types;

/// <summary>
///     Represents signals read to and from Datastar on the front end.
/// </summary>
public class DatastarSignals
{
    /// <summary>
    ///     Initializes a new instance of the Signals class.
    /// </summary>
    /// <param name="signalsString">The signals string.</param>
    public DatastarSignals(string signalsString)
    {
        Value = signalsString ?? "{ }";
    }

    /// <summary>
    ///     Gets the string value of the signals.
    /// </summary>
    /// <returns>The string value of the signals.</returns>
    public string Value { get; }

    /// <summary>
    ///     Gets an empty signals instance.
    /// </summary>
    public static DatastarSignals Empty => new("{ }");

    /// <summary>
    ///     Returns the string representation of the signals.
    /// </summary>
    /// <returns>The string representation of the signals.</returns>
    public override string ToString()
    {
        return Value;
    }

    /// <summary>
    ///     Creates a new signals instance.
    /// </summary>
    /// <param name="signalsString">The signals string.</param>
    /// <returns>A new Signals instance.</returns>
    public static DatastarSignals Create(string signalsString)
    {
        return new DatastarSignals(signalsString);
    }

    /// <summary>
    ///     Attempts to create a new signals instance.
    /// </summary>
    /// <param name="signalsString">The signals string.</param>
    /// <returns>An Optional containing the Signals if valid, None otherwise.</returns>
    public static Optional<DatastarSignals> TryCreate(string signalsString)
    {
        try
        {
            //TODO there is a better way to validate the json
            // Validate JSON
            _ = JsonNode.Parse(signalsString);
            return Optional<DatastarSignals>.Some(new DatastarSignals(signalsString));
        }
        catch
        {
            return Optional<DatastarSignals>.None;
        }
    }
}