using System.Text.Json;
using System.Text.Json.Nodes;

namespace StarFederation.Datastar;

/// <summary>
///     Utility methods for Datastar.
/// </summary>
internal static class Utils
{
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