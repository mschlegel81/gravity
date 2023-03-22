UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=0;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=false;
  ATTRACTION_RANGE        =256;

  DRIFT_TO_CENTER=false;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =8;
  REPULSION_QUADRATIC=0;

  ANNIHILATION_THRESHOLD=5;
  ANNIHILATION_FACTOR   =2E-4;

  REGROWTH_FACTOR=0;
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0.1;
  
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
Uses math;
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    result:=false;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=rx*rx+ry*ry;
    d:=1/sqr(d);
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
      else     massFactor:=10;
    end;
	massFactor*=4/pi; //same mass on fewer cells
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p[0]:= 0.5-j/(SYS_SIZE-1);
	  p[1]:=-0.5+i/(SYS_SIZE-1);
	  
	  if p[0]*p[0]+p[1]*p[1]>0.25 then mass:=0;
	  p*=mass/(p[0]*p[0]+p[1]*p[1]);
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  VAR i:longint;
  begin
    for i:=0 to SYS_SIZE-1 do begin
	  accel[0         ,i,0]:=max(accel[0         ,i,0],0)+10;
	  accel[1         ,i,0]:=max(accel[1         ,i,0],0)+ 5;
	  accel[SYS_SIZE-2,i,0]:=min(accel[SYS_SIZE-2,i,0],0)-10;
	  accel[SYS_SIZE-3,i,0]:=min(accel[SYS_SIZE-3,i,0],0)- 5;
	end;  
    for i:=0 to SYS_SIZE-1 do begin
	  accel[i,0         ,1]:=max(accel[i,0         ,1],0)+10;
	  accel[i,1         ,1]:=max(accel[i,1         ,1],0)+ 5;	  
	  accel[i,SYS_SIZE-2,1]:=min(accel[i,SYS_SIZE-2,1],0)-10;
	  accel[i,SYS_SIZE-3,1]:=min(accel[i,SYS_SIZE-3,1],0)- 5;
	end;  
  end;

end.

