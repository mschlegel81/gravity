UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =1;

  ANNIHILATION_THRESHOLD=0;
  DIFFUSION_BY_VELOCITY =0;
  DIFFUSION_BASE        =0;
  ANNIHILATION_FACTOR   =0.01;
VAR 
  REGROWTH_FACTOR    :double =0;
  
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
VAR range:double=0;
    strength:double=0;
    lastRange:double=1000;     

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin    
    range:=SYS_SIZE/3*(0.5-0.55*cos(timeStepIndex*6*pi/5000));
    result:=(range<5) and (range>0) or (abs(range-lastRange)>0.5);
    if range<0 then begin
      range:=0;
      strength:=0;
    end else strength:=sqr(range)*0.1;
    if strength>2 then strength:=2;
    if result then lastRange:=range;    
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
    if (d<range) or (d>2*range) then exit(zeroVec);
	d:=strength*(0.5-0.5*cos(d/range*2*pi))/d;
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;      
  begin
    case initialDensityVariant of
      id_low:  REGROWTH_FACTOR:=0.01;
      id_high: REGROWTH_FACTOR:=0.1 ;
      else     REGROWTH_FACTOR:=1   ;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=0.1+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

