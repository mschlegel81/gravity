rem ..\lazarus64\lazbuild.exe burnProject.lpi --bm=Default
rem ..\lazarus64\fpc\3.2.0\bin\x86_64-win64\delp.exe . lib\x86_64-win64
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt32
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt64
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt128
..\lazarus64\lazbuild.exe burnProject.lpi --bm=opt256
..\lazarus64\fpc\3.2.0\bin\x86_64-win64\delp.exe . lib\x86_64-win64
@rem mkdir 0_low_density
@rem copy /Y *.exe 0_low_density
@rem mkdir 1_high_density
@rem copy /Y *.exe 1_high_density
@rem mkdir 2_ultra_density
@rem move /Y *.exe 2_ultra_density
@rem mkdir 6_low_density
@rem copy /Y *.exe 6_low_density
@rem mkdir 7_high_density
@rem copy /Y *.exe 7_high_density
@rem mkdir 8_ultra_density
@rem move /Y *.exe 8_ultra_density