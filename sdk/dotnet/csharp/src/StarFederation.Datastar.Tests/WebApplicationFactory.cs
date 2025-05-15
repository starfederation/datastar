using Microsoft.AspNetCore.Mvc.Testing;
using StarFederation.Datastar.MinimalBlazor;
using TUnit.Core.Interfaces;

namespace StarFederation.Datastar.Tests;

public class WebApplicationFactory : WebApplicationFactory<Program>, IAsyncInitializer
{
    public Task InitializeAsync()
    {
        _ = Server;

        return Task.CompletedTask;
    }
}