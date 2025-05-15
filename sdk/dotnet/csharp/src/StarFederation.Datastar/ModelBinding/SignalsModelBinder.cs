using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.AspNetCore.Mvc.ModelBinding.Binders;
using StarFederation.Datastar.Services;

namespace StarFederation.Datastar.ModelBinding;

/// <summary>
///     Model binder for Datastar signals.
/// </summary>
public class SignalsModelBinder : IModelBinder
{
    private readonly IDatastarSignalsReaderService _signalsReader;

    /// <summary>
    ///     Initializes a new instance of the <see cref="SignalsModelBinder" /> class.
    /// </summary>
    /// <param name="signalsReader">The signals reader service.</param>
    public SignalsModelBinder(IDatastarSignalsReaderService signalsReader)
    {
        _signalsReader = signalsReader;
    }

    /// <inheritdoc />
    public async Task BindModelAsync(ModelBindingContext? bindingContext)
    {
        if (bindingContext == null)
        {
            return;
        }

        var signalBindingContext = bindingContext.BindingSource as DatastarSignalsBindingSource;
        var path = signalBindingContext?.Path;

        if (string.IsNullOrEmpty(path))
        {
            if (bindingContext.ModelType.IsValueType || bindingContext.ModelType == typeof(string))
            {
                var signals = await _signalsReader.ReadSignalsAsync();
                try
                {
                    if (signals == null)
                    {
                        bindingContext.Result = ModelBindingResult.Failed();
                        return;
                    }

                    var signalsJsonObject = JsonObject.Parse(signals.Value)!.AsObject();
                    var value = Utils.JsonPath.GetValue(bindingContext.ModelType, signalsJsonObject, bindingContext.FieldName);
                    bindingContext.Result = ModelBindingResult.Success(value);
                }
                catch
                {
                    bindingContext.Result = ModelBindingResult.Failed();
                }
            }
            else
            {
                var signals = await _signalsReader.ReadSignalsAsync();
                try
                {
                    if (signals == null)
                    {
                        bindingContext.Result = ModelBindingResult.Failed();
                        return;
                    }

                    var signalsJsonObject = JsonObject.Parse(signals.Value)!.AsObject();
                    object? value;
                    if (signalsJsonObject.ContainsKey(bindingContext.FieldName))
                    {
                        value = Utils.JsonPath.GetValue(bindingContext.ModelType, signalsJsonObject, bindingContext.FieldName);
                    }
                    else
                    {
                        value = signalsJsonObject.Deserialize(bindingContext.ModelType);
                    }

                    bindingContext.Result = ModelBindingResult.Success(value);
                }
                catch
                {
                    bindingContext.Result = ModelBindingResult.Failed();
                }
            }
        }
        else
        {
            var signals = await _signalsReader.ReadSignalsAsync();
            try
            {
                if (signals == null)
                {
                    bindingContext.Result = ModelBindingResult.Failed();
                    return;
                }

                var signalsJsonObject = JsonObject.Parse(signals.Value)!.AsObject();
                var value = Utils.JsonPath.GetValue(bindingContext.ModelType, signalsJsonObject, path);
                bindingContext.Result = ModelBindingResult.Success(value);
            }
            catch
            {
                bindingContext.Result = ModelBindingResult.Failed();
            }
        }
    }
}

/// <summary>
///     Model binder provider for Datastar signals.
/// </summary>
public class SignalsModelBinderProvider : IModelBinderProvider
{
    /// <inheritdoc />
    public IModelBinder? GetBinder(ModelBinderProviderContext? context)
    {
        if (context                           == null ||
            context.BindingInfo.BindingSource == null)
        {
            return null;
        }

        var isSignalsBindingSource =
            context.BindingInfo.BindingSource.DisplayName == Consts.BindingSourceName;

        return isSignalsBindingSource
            ? new BinderTypeModelBinder(typeof(SignalsModelBinder))
            : null;
    }
}