using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace Starfederation.Datastar
{
    /// <summary>
    /// Utility methods for Datastar.
    /// </summary>
    internal static class Utils
    {
        /// <summary>
        /// Creates an Optional value from a string, returns None if the string is null or empty.
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
        /// Methods for working with JsonObjects and paths.
        /// </summary>
        public static class JsonPath
        {
            /// <summary>
            /// Gets a value from a JsonObject using a dot-notation path.
            /// </summary>
            /// <param name="valueType">The type to deserialize to.</param>
            /// <param name="jObject">The JsonObject to get the value from.</param>
            /// <param name="path">The dot-notation path to the value.</param>
            /// <returns>The deserialized value.</returns>
            public static object? GetValue(Type valueType, JsonObject jObject, string path)
            {
                var parts = path.Split('.', 2);
                
                if (parts.Length == 1)
                {
                    return jObject[parts[0]]?.Deserialize(valueType);
                }

                if (parts.Length == 2)
                {
                    var node = jObject[parts[0]];
                    if (node != null) 
                    {
                        return GetValue(valueType, node.AsObject(), parts[1]);
                    }
                    return null;
                }

                return jObject.Deserialize(valueType);
            }
        }

        /// <summary>
        /// String utility methods.
        /// </summary>
        public static class Strings
        {
            /// <summary>
            /// The standard newline characters.
            /// </summary>
            public static readonly string[] NewLines = { "\r\n", "\n", "\r" };

            /// <summary>
            /// Splits a string by multiple delimiters.
            /// </summary>
            /// <param name="line">The string to split.</param>
            /// <param name="delimiters">The delimiters to split on.</param>
            /// <returns>The split string.</returns>
            public static string[] Split(string line, IEnumerable<string> delimiters)
            {
                return line.Split(delimiters as string[] ?? new List<string>(delimiters).ToArray(), StringSplitOptions.None);
            }

            /// <summary>
            /// Checks if a string is populated (not null, empty, or whitespace).
            /// </summary>
            /// <param name="value">The string to check.</param>
            /// <returns>True if the string is populated, false otherwise.</returns>
            public static bool IsPopulated(string value)
            {
                return !string.IsNullOrWhiteSpace(value);
            }

            /// <summary>
            /// Converts a PascalCase string to kebab-case.
            /// </summary>
            /// <param name="pascalString">The PascalCase string.</param>
            /// <returns>The kebab-case string.</returns>
            public static string ToKebabCase(string pascalString)
            {
                if (string.IsNullOrEmpty(pascalString))
                {
                    return string.Empty;
                }

                var stringBuilder = new StringBuilder();
                
                foreach (var c in pascalString)
                {
                    if (char.IsUpper(c))
                    {
                        stringBuilder.Append('-').Append(char.ToLower(c));
                    }
                    else
                    {
                        stringBuilder.Append(c);
                    }
                }

                // Remove leading hyphen if it exists
                if (stringBuilder.Length > 0 && stringBuilder[0] == '-')
                {
                    stringBuilder.Remove(0, 1);
                }

                return stringBuilder.ToString();
            }
        }
    }

    //TODO Remove this as this is just to get the F# version of Datastar working
    /// <summary>
    /// Represents an optional value, similar to F#'s ValueOption.
    /// </summary>
    /// <typeparam name="T">The type of the optional value.</typeparam>
    public struct Optional<T>
    {
        private readonly bool _hasValue;
        private readonly T _value;

        private Optional(T value, bool hasValue)
        {
            _value = value;
            _hasValue = hasValue;
        }

        /// <summary>
        /// Gets a value indicating whether this Optional has a value.
        /// </summary>
        public bool HasValue => _hasValue;

        /// <summary>
        /// Gets the value of this Optional, or throws an InvalidOperationException if it has no value.
        /// </summary>
        /// <exception cref="InvalidOperationException">Thrown when the Optional has no value.</exception>
        public T Value
        {
            get
            {
                if (!_hasValue)
                {
                    throw new InvalidOperationException("Optional has no value");
                }
                return _value;
            }
        }

        /// <summary>
        /// Creates an Optional with a value.
        /// </summary>
        /// <param name="value">The value.</param>
        /// <returns>An Optional containing the value.</returns>
        #pragma warning disable CA1000
        public static Optional<T> Some(T value) => new Optional<T>(value, true);
        #pragma warning restore CA1000

        /// <summary>
        /// Creates an Optional with no value.
        /// </summary>
        #pragma warning disable CA1000
        public static Optional<T> None => new Optional<T>(default!, false);
        #pragma warning restore CA1000

        /// <summary>
        /// Gets the value if it exists, or returns the default value.
        /// </summary>
        /// <param name="defaultValue">The default value to return if this Optional has no value.</param>
        /// <returns>The value if it exists, or the default value.</returns>
        public T GetValueOrDefault(T defaultValue) => _hasValue ? _value : defaultValue;

        /// <summary>
        /// Gets the value if it exists, or returns the default value.
        /// </summary>
        /// <returns>The value if it exists, or the default value of T.</returns>
        public T GetValueOrDefault() => _hasValue ? _value : default!;
    }
}
