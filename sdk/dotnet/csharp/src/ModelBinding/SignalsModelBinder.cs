using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.AspNetCore.Mvc.ModelBinding.Binders;
using StarFederation.Datastar.DependencyInjection;
using System.Text.Json;
using Microsoft.Extensions.Logging;

namespace StarFederation.Datastar.ModelBinding;

public class SignalsModelBinder(ILogger<SignalsModelBinder> logger, IDatastarSignalsReaderService signalsReader) : IModelBinder
{
    public async Task BindModelAsync(ModelBindingContext bindingContext)
    {
        DatastarSignalsBindingSource signalBindingSource = (bindingContext.BindingSource as DatastarSignalsBindingSource)!;

        // Get signals into a JsonDocument
        JsonDocument doc;
        try
        {
            doc = await ReadSignalsToJsonDocument(bindingContext);
        }
        catch (JsonException ex) when (ex is { LineNumber: 0, BytePositionInLine: 0 })
        {
            logger.LogWarning("Empty Signals. Is it possible you have multiple [FromSignals] for a not-GET request?");
            bindingContext.Result = ModelBindingResult.Failed();
            return;
        }
        catch
        {
            bindingContext.Result = ModelBindingResult.Failed();
            return;
        }

        try
        {
            if (bindingContext.ModelType.IsValueType || bindingContext.ModelType == typeof(string))
            {
                // SignalsPath: use the name of the field in the method or the one passed in the attribute
                string signalsPath = String.IsNullOrEmpty(signalBindingSource.BindingPath) ? bindingContext.FieldName : signalBindingSource.BindingPath;

                object? value = doc.RootElement.GetValueFromPath(signalsPath, bindingContext.ModelType, signalBindingSource.JsonSerializerOptions)
                                ?? (bindingContext.ModelType.IsValueType ? Activator.CreateInstance(bindingContext.ModelType) : null);
                bindingContext.Result = ModelBindingResult.Success(value);
            }
            else
            {
                object? value;
                if (String.IsNullOrEmpty(signalBindingSource.BindingPath))
                {
                    value = doc.Deserialize(bindingContext.ModelType, signalBindingSource.JsonSerializerOptions);
                }
                else
                {
                    value = doc.RootElement.GetValueFromPath(signalBindingSource.BindingPath, bindingContext.ModelType, signalBindingSource.JsonSerializerOptions);
                }

                bindingContext.Result = ModelBindingResult.Success(value);
            }
        }
        catch
        {
            bindingContext.Result = ModelBindingResult.Failed();
        }
    }

    private async ValueTask<JsonDocument> ReadSignalsToJsonDocument(ModelBindingContext bindingContext)
    {
        return bindingContext.HttpContext.Request.Method == System.Net.WebRequestMethods.Http.Get
            ? JsonDocument.Parse(await signalsReader.ReadSignalsAsync() ?? String.Empty)
            : await JsonDocument.ParseAsync(signalsReader.GetSignalsStream());
    }
}

public class SignalsModelBinderProvider : IModelBinderProvider
{
    public IModelBinder? GetBinder(ModelBinderProviderContext context)
        => context?.BindingInfo?.BindingSource?.DisplayName == DatastarSignalsBindingSource.BindingSourceName
            ? new BinderTypeModelBinder(typeof(SignalsModelBinder))
            : null;

}