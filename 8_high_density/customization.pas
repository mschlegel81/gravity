UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=0;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =18;

  REPULSION_THRESHOLD=5;
  REPULSION_LINEAR   =2;
  REPULSION_QUADRATIC=0;

  ANNIHILATION_THRESHOLD=0;
  ANNIHILATION_FACTOR   =1E-3;  
VAR  
  REGROWTH_FACTOR       :double=1E-2;
CONST
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0.01;  
  
  DRIFT_TO_CENTER=false;

  
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
VAR forceFactor:double;
   
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin     	
    result:=timeStepIndex mod 1000=500;
	if (timeStepIndex+500) mod 2000<1000 then forceFactor:=0.125 else forceFactor:=-0.125;	
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>ATTRACTION_RANGE 
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
	REGROWTH_FACTOR*=massFactor;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p:=zeroVec;
    end;
  end;


PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  VAR i,j:longint;
      n:double; 
  begin
    n:=1-cos(timeStepIndex*2*pi/1000);
	n*=5;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
	  accel[i,j,1]+=n;
	end;
  end;

end.

