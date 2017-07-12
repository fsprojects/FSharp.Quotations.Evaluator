#!/bin/bash

if [ ! -f packages/FAKE/tools/FAKE.exe ]; then
  mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi

mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion

set -x
mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
