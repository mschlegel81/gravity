rem ..\lazarus64\lazbuild.exe burnProject.lpi --bm=Default
rem ..\lazarus64\fpc\3.2.0\bin\x86_64-win64\delp.exe . lib\x86_64-win64
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt32
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt64
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt128
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt256
..\lazarus64\fpc\3.2.0\bin\x86_64-win64\delp.exe . lib\x86_64-win64