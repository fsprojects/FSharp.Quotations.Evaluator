#!/bin/bash

dotnet tool restore
dotnet paket restore

if [ "X$OS" = "XWindows_NT" ] ; then
  # use .Net
  packages/build/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx 
else
  mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi