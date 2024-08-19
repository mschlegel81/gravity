UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=2;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =1;

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
VAR flag:boolean;
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  CONST switchPoint:array[0..339] of longint=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,104,106,108,110,112,114,116,118,120,122,124,126,128,130,132,134,136,138,140,142,144,146,148,150,152,154,156,158,160,162,164,166,168,170,172,174,176,178,180,182,184,186,188,190,192,194,196,198,200,203,206,209,212,215,218,221,224,227,230,233,236,239,242,245,248,251,254,257,260,263,266,269,272,275,278,281,284,287,290,293,296,299,303,307,311,315,319,323,327,331,335,339,343,347,351,355,359,363,367,371,375,379,383,387,391,395,399,404,409,414,419,424,429,434,439,444,449,454,459,464,469,474,479,484,489,494,499,507,515,523,531,539,547,555,563,571,579,587,595,603,611,619,627,635,643,651,659,667,675,683,691,699,707,715,723,731,739,747,755,763,771,779,787,795,803,811,819,835,851,867,883,899,915,931,947,963,979,995,1011,1027,1043,1059,1075,1091,1107,1123,1139,1171,1203,1235,1267,1299,1331,1363,1395,1427,1459,1491,1523,1555,1587,1619,1651,1683,1715,1747,1779,1843,1907,1971,2035,2099,2163,2227,2291,2355,2419,2483,2547,2611,2675,2739,2803,2867,2931,2995,3059,3187,3315,3443,3571,3699,3827,3955,4083,4211,4339,4595,4851);
  VAR k:longint;
  begin
    result:=false;
    flag:=false;
    for k in switchPoint do begin
      flag:=not(flag);
      if k=timeStepIndex then exit(true) else 
      if k>timeStepIndex then exit(false);
    end;    
  end;  

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR ix,iy:longint;
      d:double;
  begin  
    ix:=round(rx/8); iy:=round(ry/8);
    if (ix=0) and (iy=0) then exit(zeroVec);
    result:=zeroVec;
    d:=sqrt(rx*rx+ry*ry);
    d:=2/(d*sqr(d));
    if odd(ix xor iy) xor flag
    then result[0]:=rx*d
    else result[1]:=ry*d;
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
      mass:=1+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

