UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =20;

  REPULSION_THRESHOLD=1;
  REPULSION_LINEAR   =50;
  REPULSION_QUADRATIC=0.1;

  ANNIHILATION_THRESHOLD=5;
  ANNIHILATION_FACTOR   =0;

  REGROWTH_FACTOR=0;
  DIFFUSION_BY_VELOCITY=0.01;
  DIFFUSION_BASE       =0.6;  

  DRIFT_TO_CENTER=false;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
VAR forceFactor:double=0;
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin  
    result:=(timeStepIndex mod 20=0);
	forceFactor:=0.1E-3*(timeStepIndex mod 1000);
	if timeStepIndex mod 1000>=800 then begin
	  forceFactor:=0;	  
	end;  
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>20 
	then exit(zeroVec)
	else d:=forceFactor*(1+cos(d*pi/ATTRACTION_RANGE))/d;	
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
  begin
    case initialDensityVariant of
      id_low:  massFactor:= 0.5;
      id_high: massFactor:= 1;
      else     massFactor:= 2;
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

