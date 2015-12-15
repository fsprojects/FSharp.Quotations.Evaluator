#!/bin/bash

if [ ! -f packages/FAKE/tools/FAKE.exe ]; then
  mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi

mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion

mono packages/FAKE/tools/FAKE.exe build.fsx $@ --fsiargs --define:MONO 
