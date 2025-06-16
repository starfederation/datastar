using System.Text.Json;
using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace StarFederation.Datastar.ModelBinding;

public class DatastarSignalsBindingSource(string path, JsonSerializerOptions? jsonSerializerOptions) : BindingSource(BindingSourceName, BindingSourceName, isGreedy: true, isFromRequest: true)
{
    public const string BindingSourceName = "DatastarSignalsSource";
    public string BindingPath { get; } = path;
    public JsonSerializerOptions JsonSerializerOptions { get; } = jsonSerializerOptions ?? JsonSerializerOptions.Default;
}

/// <summary>
/// FromSignals will collect the values from the signals passed.<br />
/// - value type or string without path => use parameter name as key into Signals<br />
/// - value type or string with path => use dot-separated, path name as key into Signals<br />
/// - reference type without path => deserialize Signals into parameter Type<br />
/// - reference type with path => deserialize Signals at path into parameter Type<br />
/// <b>Important:</b> Requests that have the signals in the body (POST, PUT, etc) can only have one [FromSignals] in the parameter list; all following will receive default<br />
/// <b>Important:</b> When binding to a non-string, reference type; the parameter name is always ignored, the parameter name will not be used as a key into the Signals like value types and strings
/// </summary>
[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
public class FromSignalsAttribute : Attribute, IBindingSourceMetadata
{
    public string Path { get; set; } = String.Empty;
    public JsonSerializerOptions JsonSerializerOptions { get; set; } = JsonSerializerOptions.Default;
    public BindingSource BindingSource => new DatastarSignalsBindingSource(Path, JsonSerializerOptions);
}