FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS base
USER $APP_UID
WORKDIR /app
EXPOSE 8080
EXPOSE 8081

FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
ARG BUILD_CONFIGURATION=Release
WORKDIR /src
COPY ["StarFederation.Datastar.Example/StarFederation.Datastar.Example.csproj", "StarFederation.Datastar.Example/"]
RUN dotnet restore "StarFederation.Datastar.Example/StarFederation.Datastar.Example.csproj"
COPY . .
WORKDIR "/src/StarFederation.Datastar.Example"
RUN dotnet build "./StarFederation.Datastar.Example.csproj" -c $BUILD_CONFIGURATION -o /app/build

FROM build AS publish
ARG BUILD_CONFIGURATION=Release
RUN dotnet publish "./StarFederation.Datastar.Example.csproj" -c $BUILD_CONFIGURATION -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "StarFederation.Datastar.Example.dll"]
