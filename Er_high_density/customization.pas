UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =2.5;

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
VAR phase:double;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    phase:=2*pi*timeStepIndex/500;
    result:=(timeStepIndex and 3)=0;
  end;  

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d,q:double;
  begin  
    if (abs(rx)>SYS_SIZE/2) or (abs(ry)>SYS_SIZE/2) then exit(zeroVec);
    d:=sqrt(rx*rx+ry*ry);    
    d:=0.5*(0.5+0.5*sin(2*pi*d/32-phase))/(sqr(d)*d);
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

