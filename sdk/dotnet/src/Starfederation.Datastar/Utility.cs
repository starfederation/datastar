using System.Text.Json;
using System.Text.Json.Nodes;

namespace Starfederation.Datastar;

/// <summary>
///     Utility methods for Datastar.
/// </summary>
internal static class Utils
{
    /// <summary>
    ///     Creates an Optional value from a string, returns None if the string is null or empty.
    /// </summary>
    /// <param name="value">The string value.</param>
    /// <returns>An Optional containing the string if non-empty, None otherwise.</returns>
    public static Optional<string> FromEmptyString(string value)
    {
        return string.IsNullOrEmpty(value)
            ? Optional<string>.None
            : Optional<string>.Some(value);
    }

    /// <summary>
    ///     Methods for working with JsonObjects and paths.
    /// </summary>
    public static class JsonPath
    {
        /// <summary>
        ///     Gets a value from a JsonObject using a dot-notation path.
        /// </summary>
        /// <param name="valueType">The type to deserialize to.</param>
        /// <param name="jObject">The JsonObject to get the value from.</param>
        /// <param name="path">The dot-notation path to the value.</param>
        /// <returns>The deserialized value.</returns>
        public static object? GetValue(Type valueType, JsonObject jObject, string path)
        {
            var parts = path.Split('.', 2);

            switch (parts.Length)
            {
                case 1:
                    return jObject[parts[0]]?.Deserialize(valueType);
                case 2:
                {
                    var node = jObject[parts[0]];
                    if (node != null)
                    {
                        return GetValue(valueType, node.AsObject(), parts[1]);
                    }

                    return null;
                }
                default:
                    return jObject.Deserialize(valueType);
            }
        }
    }
}

//TODO Remove this as this is just to get the F# version of Datastar working
/// <summary>
///     Represents an optional value, similar to F#'s ValueOption.
/// </summary>
/// <typeparam name="T">The type of the optional value.</typeparam>
public struct Optional<T>
{
    private readonly T _value;

    private Optional(T value, bool hasValue)
    {
        _value = value;
        HasValue = hasValue;
    }

    /// <summary>
    ///     Gets a value indicating whether this Optional has a value.
    /// </summary>
    public bool HasValue { get; }

    /// <summary>
    ///     Gets the value of this Optional, or throws an InvalidOperationException if it has no value.
    /// </summary>
    /// <exception cref="InvalidOperationException">Thrown when the Optional has no value.</exception>
    public T Value
    {
        get
        {
            if (!HasValue)
            {
                throw new InvalidOperationException("Optional has no value");
            }

            return _value;
        }
    }

    /// <summary>
    ///     Creates an Optional with a value.
    /// </summary>
    /// <param name="value">The value.</param>
    /// <returns>An Optional containing the value.</returns>
    #pragma warning disable CA1000
    public static Optional<T> Some(T value) => new(value, true);
    #pragma warning restore CA1000

    /// <summary>
    ///     Creates an Optional with no value.
    /// </summary>
    #pragma warning disable CA1000
    public static Optional<T> None => new(default!, false);
    #pragma warning restore CA1000

    /// <summary>
    ///     Gets the value if it exists, or returns the default value.
    /// </summary>
    /// <param name="defaultValue">The default value to return if this Optional has no value.</param>
    /// <returns>The value if it exists, or the default value.</returns>
    public T GetValueOrDefault(T defaultValue)
    {
        return HasValue ? _value : defaultValue;
    }

    /// <summary>
    ///     Gets the value if it exists, or returns the default value.
    /// </summary>
    /// <returns>The value if it exists, or the default value of T.</returns>
    public T GetValueOrDefault()
    {
        return HasValue ? _value : default!;
    }
}