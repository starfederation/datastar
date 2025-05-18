using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace StarFederation.Datastar.ModelBinding;

/// <summary>
///     A binding source for Datastar signals.
/// </summary>
public class DatastarSignalsBindingSource : BindingSource
{
    /// <summary>
    ///     Initializes a new instance of the DatastarSignalsBindingSource class.
    /// </summary>
    /// <param name="path">The path to the signals value.</param>
    public DatastarSignalsBindingSource(string path)
        : base(DatastarConstants.BindingSourceName, DatastarConstants.BindingSourceName, true, true)
    {
        Path = path;
    }

    /// <summary>
    ///     Gets the path to the signals value.
    /// </summary>
    public new string Path { get; }
}