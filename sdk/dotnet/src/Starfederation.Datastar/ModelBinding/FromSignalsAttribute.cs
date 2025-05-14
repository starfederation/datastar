using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace Starfederation.Datastar.ModelBinding
{
    /// <summary>
    /// Contains constants for Datastar signals binding.
    /// </summary>
    internal static class DatastarSignalsBindingSourceConst
    {
        /// <summary>
        /// The name of the binding source.
        /// </summary>
        public const string BindingSourceName = "DatastarSource";
    }

    /// <summary>
    /// A binding source for Datastar signals.
    /// </summary>
    public class DatastarSignalsBindingSource : BindingSource
    {
        /// <summary>
        /// Initializes a new instance of the DatastarSignalsBindingSource class.
        /// </summary>
        /// <param name="path">The path to the signals value.</param>
        public DatastarSignalsBindingSource(string path)
            : base(DatastarSignalsBindingSourceConst.BindingSourceName, DatastarSignalsBindingSourceConst.BindingSourceName, isGreedy: true, isFromRequest: true)
        {
            Path = path;
        }

        /// <summary>
        /// Gets the path to the signals value.
        /// </summary>
        public new string Path { get; }
    }

    /// <summary>
    /// Specifies that a parameter or property should be bound using the Datastar signals.
    /// </summary>
    [AttributeUsage(AttributeTargets.Parameter | AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    public class FromSignalsAttribute : Attribute, IBindingSourceMetadata
    {
        /// <summary>
        /// Gets or sets the path to the signals value.
        /// </summary>
        public string Path { get; set; } = string.Empty;

        /// <summary>
        /// Gets the binding source.
        /// </summary>
        BindingSource IBindingSourceMetadata.BindingSource => new DatastarSignalsBindingSource(Path);
    }
}
