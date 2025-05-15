using Cysharp.Text;

namespace StarFederation.Datastar.Extensions;

public static class StringExtensions
{
    /// <summary>
    ///     Converts a PascalCase string to kebab-case.
    /// </summary>
    /// <param name="pascalString">The PascalCase string.</param>
    /// <returns>The kebab-case string.</returns>
    public static string ToKebabCase(this string pascalString)
    {
        if (string.IsNullOrEmpty(pascalString))
        {
            return string.Empty;
        }

        using var stringBuilder = ZString.CreateStringBuilder();

        foreach (var c in pascalString)
        {
            if (char.IsUpper(c))
            {
                stringBuilder.Append('-');
                stringBuilder.Append(char.ToLower(c));
            }
            else
            {
                stringBuilder.Append(c);
            }
        }

        // Remove leading hyphen if it exists
        if (stringBuilder.Length > 0 && stringBuilder.AsSpan()[0] == '-')
        {
            stringBuilder.Remove(0, 1);
        }

        return stringBuilder.ToString();
    }
}