var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
// Learn more about configuring OpenAPI at https://aka.ms/aspnet/openapi
builder.Services.AddOpenApi();

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseHttpsRedirection();
}

app.MapOpenApi();

app.Run();

//TODO remove this program reference for unit testing as it will not be needed in .net 10
public partial class Program;