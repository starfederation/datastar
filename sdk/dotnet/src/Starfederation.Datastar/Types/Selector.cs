using System.Text.RegularExpressions;

namespace Starfederation.Datastar.Types;

/// <summary>
///     Represents an HTML selector name.
/// </summary>
public partial class Selector
{
    private static readonly Regex SelectorRegex = GeneratedSelectorRegex();

    /// <summary>
    ///     Initializes a new instance of the Selector class.
    /// </summary>
    /// <param name="selector">The selector string.</param>
    public Selector(string selector)
    {
        Value = selector ?? string.Empty;
    }

    /// <summary>
    ///     Gets the string value of the selector.
    /// </summary>
    /// <returns>The string value of the selector.</returns>
    public string Value { get; }

    /// <summary>
    ///     Returns the string representation of the selector.
    /// </summary>
    /// <returns>The string representation of the selector.</returns>
    public override string ToString()
    {
        return Value;
    }

    /// <summary>
    ///     Creates a new selector instance.
    /// </summary>
    /// <param name="selector">The selector string.</param>
    /// <returns>A new Selector instance.</returns>
    public static Selector Create(string selector)
    {
        return new Selector(selector);
    }

    [GeneratedRegex("""[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s"'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['"].*['"]|[^\s"'=<>`]+))?\]|:[\w-]+(?:\(.*\))?""", RegexOptions.Compiled)]
    private static partial Regex GeneratedSelectorRegex();
}