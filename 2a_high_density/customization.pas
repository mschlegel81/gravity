UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=5;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =4;

  DIFFUSION_BY_VELOCITY =0;
  DIFFUSION_BASE        =0;
  REGROWTH_FACTOR       =0;
  ANNIHILATION_FACTOR   =0.001;
VAR
  ANNIHILATION_THRESHOLD:double=0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    result:=false;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=rx*rx+ry*ry;
    if d<1
    then exit(zeroVec)
    else d:=2/sqr(d);
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
  begin
    case initialDensityVariant of
      id_low:  ANNIHILATION_THRESHOLD:=1;
      id_high: ANNIHILATION_THRESHOLD:=2;
      else     ANNIHILATION_THRESHOLD:=4;
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

