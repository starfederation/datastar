using System.Text.RegularExpressions;

namespace StarFederation.Datastar.Types;

/// <summary>
/// Represents an HTML selector name.
/// This is just a wrapper to validate the selector
/// </summary>
public partial struct DatastarSelector
{
    private static readonly Regex SelectorRegex = GeneratedSelectorRegex();

    /// <summary>
    ///     Initializes a new instance of the Selector class.
    /// </summary>
    /// <param name="selector">The selector string.</param>
    public DatastarSelector(string selector)
    {
        Value = selector;
    }

    /// <summary>
    ///     Gets the string value of the selector.
    /// </summary>
    /// <returns>The string value of the selector.</returns>
    public string Value
    {
        get => field;
        init
        {
            field = value;
            ValidateSelector();
        }
    }

    /// <summary>
    ///     Returns the string representation of the selector.
    /// </summary>
    /// <returns>The string representation of the selector.</returns>
    public override string ToString()
    {
        return Value;
    }

    private void ValidateSelector()
    {
        if (string.IsNullOrEmpty(Value))
        {
            throw new ArgumentException("Selector cannot be null or empty.", nameof(Value));
        }

        if (!SelectorRegex.IsMatch(Value))
        {
            throw new ArgumentException($"Invalid selector: {Value}", nameof(Value));
        }
    }

    [GeneratedRegex("""[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s"'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['"].*['"]|[^\s"'=<>`]+))?\]|:[\w-]+(?:\(.*\))?""",
                    RegexOptions.Compiled)]
    private static partial Regex GeneratedSelectorRegex();
    
    public static implicit operator string?(DatastarSelector? selector)
    {
        return selector?.Value;
    }

    public static implicit operator DatastarSelector(string selector)
    {
        return new DatastarSelector(selector);
    }
}