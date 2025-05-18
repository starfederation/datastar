using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.AspNetCore.Mvc.ModelBinding.Binders;

namespace StarFederation.Datastar.ModelBinding;

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
            context.BindingInfo.BindingSource.DisplayName == DatastarConstants.BindingSourceName;

        return isSignalsBindingSource
            ? new BinderTypeModelBinder(typeof(SignalsModelBinder))
            : null;
    }
}