UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =20;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =0;
  REPULSION_QUADRATIC=0;

  ANNIHILATION_THRESHOLD=5;
  ANNIHILATION_FACTOR   =1E-2;
  
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;

  DRIFT_TO_CENTER=true;

VAR REGROWTH_FACTOR:double=1E-3;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:TmyFloat):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel:T_vectorField);
IMPLEMENTATION
VAR forceFactor:double=0;
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin  
    result:=(timeStepIndex mod 500=0);
    if timeStepIndex mod 1000<500
    then begin
      forceFactor:=-0.2;
      REGROWTH_FACTOR:=0;
    end else begin
      forceFactor:= 0.1;
      case initialDensityVariant of
        id_low:  REGROWTH_FACTOR:= 1E-3;
        id_high: REGROWTH_FACTOR:= 2E-3;
        else     REGROWTH_FACTOR:= 4E-3;
      end;
    end;
  end;

FUNCTION straightAttraction(CONST rx,ry:TmyFloat):T_2dVector;
  VAR d:TmyFloat;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>20 
	then exit(zeroVec)
	else d:=forceFactor*(sin(d*2*pi/ATTRACTION_RANGE))/d;	
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
  begin
    case initialDensityVariant of
      id_low:  massFactor:= 0.1;
      id_high: massFactor:= 1;
      else     massFactor:= 10;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel: T_vectorField);
  begin
  end;

end.

