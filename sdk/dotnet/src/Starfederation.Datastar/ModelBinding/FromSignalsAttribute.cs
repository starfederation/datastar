using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace Starfederation.Datastar.ModelBinding;

/// <summary>
///     Specifies that a parameter or property should be bound using the Datastar signals.
/// </summary>
[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.Property)]
public class FromSignalsAttribute : Attribute, IBindingSourceMetadata
{
    /// <summary>
    ///     Gets or sets the path to the signals value.
    /// </summary>
    public string Path { get; set; } = string.Empty;

    /// <summary>
    ///     Gets the binding source.
    /// </summary>
    BindingSource IBindingSourceMetadata.BindingSource => new DatastarSignalsBindingSource(Path);
}