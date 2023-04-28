UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =23;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =1;
  REPULSION_QUADRATIC=0;

  ANNIHILATION_THRESHOLD=1;
  ANNIHILATION_FACTOR   =5E-2;  
  
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;  
  
  DRIFT_TO_CENTER=true;

VAR 
  REGROWTH_FACTOR:double=0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin     	
    case initialDensityVariant of
      id_low:  REGROWTH_FACTOR:=0.5E-2*(1-cos(timeStepIndex*2*pi/1000));
      id_high: REGROWTH_FACTOR:=  1E-2*(1-cos(timeStepIndex*2*pi/1000));
      else     REGROWTH_FACTOR:=  2E-2*(1-cos(timeStepIndex*2*pi/1000));
    end;
    if REGROWTH_FACTOR<0 then REGROWTH_FACTOR:=0;
    result:=false;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;  
  VAR f:double;
  begin
    f:=sqrt(sqr(rx)+sqr(ry));
    if f<ATTRACTION_RANGE then begin
	  f:=(ATTRACTION_RANGE-f)/(ATTRACTION_RANGE*f);
      result[0]:=rx*f;
      result[1]:=ry*f;      
    end else result:=zeroVec;
  end;
	
FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
  begin
    case initialDensityVariant of
      id_low:  massFactor:= 0.25;
      id_high: massFactor:= 0.5;
      else     massFactor:= 1;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  begin
  end;

end.

