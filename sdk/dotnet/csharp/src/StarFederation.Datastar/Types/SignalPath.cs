using StarFederation.Datastar.Extensions;

namespace StarFederation.Datastar.Types;

/// <summary>
///     Represents a dotted path into Signals to access a key/value pair.
/// </summary>
public class SignalPath
{
    /// <summary>
    ///     Initializes a new instance of the SignalPath class.
    /// </summary>
    /// <param name="path">The signal path.</param>
    public SignalPath(string path)
    {
        Value = path ?? string.Empty;
    }

    /// <summary>
    ///     Gets the string value of the signal path.
    /// </summary>
    /// <returns>The string value of the signal path.</returns>
    public string Value { get; }

    /// <summary>
    ///     Gets the kebab case value of the signal path.
    /// </summary>
    /// <returns>The kebab case value of the signal path.</returns>
    public string KebabValue => Value.ToKebabCase();

    /// <summary>
    ///     Returns the string representation of the signal path.
    /// </summary>
    /// <returns>The string representation of the signal path.</returns>
    public override string ToString()
    {
        return Value;
    }

    /// <summary>
    ///     Checks if a key is valid for a signal path.
    /// </summary>
    /// <param name="key">The key to check.</param>
    /// <returns>True if the key is valid, false otherwise.</returns>
    public static bool IsValidKey(string key)
    {
        // Implement validation rules here
        return !string.IsNullOrEmpty(key);
    }
}