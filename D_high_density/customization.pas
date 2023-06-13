UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =32;

  DRIFT_TO_CENTER=false;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =0;
  REGROWTH_FACTOR    =1E-4;
  REPULSION_QUADRATIC=0;
  ANNIHILATION_THRESHOLD=0;
  ANNIHILATION_FACTOR   =1E-3;
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;
  
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
Uses math;
VAR q0:double=1;
    q1:double=0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    result:=timeStepIndex mod 20 = 0;
    q0:=cos(timeStepIndex*2*pi/1000);
    q1:=sin(timeStepIndex*2*pi/1000);
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>ATTRACTION_RANGE then exit(zeroVec);
	d:=0.02*(1+cos(pi*d/ATTRACTION_RANGE))/d;
    result[0]:=(rx*q0+ry*q1)*d;
    result[1]:=(ry*q0-rx*q1)*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
  begin
    case initialDensityVariant of
      id_low:  begin massFactor:= 0.5; end;
      id_high: begin massFactor:= 1  ; end;
      else     begin massFactor:= 2  ; end;
    end;	
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p[0]:=0;
      p[1]:=0;
      //p[0]:= 0.5-j/(SYS_SIZE-1);
	  //p[1]:=-0.5+i/(SYS_SIZE-1);
	  //
	  //if p[0]*p[0]+p[1]*p[1]>0.25 then mass:=0;
	  //p:=p*10*mass;
	end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

