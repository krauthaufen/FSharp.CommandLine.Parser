msbuild ../src/FSharp.CLI.sln /t:Build /p:Configuration="Release"
nuget pack FSharp.CLI.nuspec