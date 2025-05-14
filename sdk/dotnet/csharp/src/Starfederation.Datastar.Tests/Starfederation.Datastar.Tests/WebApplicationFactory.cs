using Microsoft.AspNetCore.Mvc.Testing;
using Starfederation.Datastar.MinimalBlazor;
using TUnit.Core.Interfaces;

namespace Starfederation.Datastar.Tests;

public class WebApplicationFactory : WebApplicationFactory<Program>, IAsyncInitializer
{
    public Task InitializeAsync()
    {
        _ = Server;

        return Task.CompletedTask;
    }
}