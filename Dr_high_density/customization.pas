UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =2;

  ANNIHILATION_THRESHOLD=0;
  DIFFUSION_BY_VELOCITY =0;
  DIFFUSION_BASE        =0;
VAR
  REGROWTH_FACTOR    :double = 0;
  ANNIHILATION_FACTOR:double = 0.01;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
VAR freq,strength:double;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  CONST minRelativePeriod=0.0719482421875;
        maxRelativePeriod=0.5;
        reInitIndex:array[0..167] of longint=
    (  0,  81, 115, 141, 163, 183, 201, 218, 234, 249, 263, 276, 289, 301, 313, 325, 336, 347, 358, 369,
     379, 389, 399, 409, 419, 429, 438, 447, 456, 465, 474, 483, 492, 501, 510, 519, 528, 537, 545, 553,
     561, 569, 577, 585, 593, 601, 609, 617, 625, 633, 641, 649, 657, 665, 673, 681, 689, 697, 705, 713,
     721, 729, 737, 745, 753, 761, 769, 777, 785, 793, 801, 809, 817, 825, 833, 841, 849, 857, 865, 873,
     881, 889, 897, 905, 913, 921, 929, 937, 945, 954, 963, 972, 981, 990, 999,1008,1017,1026,1035,1044,
    1053,1062,1071,1080,1089,1098,1108,1118,1128,1138,1148,1158,1168,1178,1188,1198,1209,1220,1231,1242,
    1253,1264,1275,1287,1299,1311,1323,1335,1347,1360,1373,1386,1399,1413,1427,1441,1455,1470,1485,1500,
    1516,1532,1548,1565,1582,1600,1618,1637,1657,1677,1698,1720,1743,1767,1792,1818,1845,1874,1905,1938,
    1973,2011,2053,2101,2156,2223,2314,2500);
  VAR k:longint;
      p:double;
  begin
    p:=exp(ln(minRelativePeriod)+ln(maxRelativePeriod/minRelativePeriod)*(0.5-0.5*cos(timeStepIndex*2*pi/5000)));
    if timeStepIndex<2500 then strength:=0.7 else strength:=-0.7;
    freq:=2*pi/p/SYS_SIZE;
    result:=false;
    for k in reInitIndex do if (timeStepIndex=k) or (5000-timeStepIndex=k) then exit(true);
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d,q:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
    if (d>SYS_SIZE/2) then exit(zeroVec);
    d:=strength*(0.5+0.5*cos(pi*d/SYS_SIZE/2))*(cos(freq*d))/sqr(d);
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
  begin
    case initialDensityVariant of
      id_low:  begin REGROWTH_FACTOR:=0.01; ANNIHILATION_FACTOR:=0.0001; end;
      id_high: begin REGROWTH_FACTOR:=0.1 ; ANNIHILATION_FACTOR:=0.001;  end;
      else     begin REGROWTH_FACTOR:=1   ; ANNIHILATION_FACTOR:=0.01;   end;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=10+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

