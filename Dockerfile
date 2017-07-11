FROM microsoft/aspnetcore-build:1.1.2 as source

COPY . .
WORKDIR /Tweek.JPad.Cli

RUN dotnet restore
RUN dotnet publish -c release -o publish

FROM microsoft/aspnetcore:1.1.2 as release
COPY --from=source /Tweek.JPad.Cli/publish /app

ENTRYPOINT ["dotnet", "/app/Tweek.JPad.Cli.dll"]
