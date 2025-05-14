FROM mcr.microsoft.com/dotnet/sdk:9.0 AS publish

# Copy files
WORKDIR /src
COPY . .
ARG PROJECT_FILE="/src/Starfederation.Datastar.Blazor/Starfederation.Datastar.Blazor.csproj"
ARG BUILD_CONFIGURATION=Release
RUN dotnet publish $PROJECT_FILE -c $BUILD_CONFIGURATION -o /app/publish /p:UseAppHost=false

# Final stage/image
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS final

USER $APP_UID
WORKDIR /app
EXPOSE 8080

WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Starfederation.Datastar.RazorPages.dll"]
