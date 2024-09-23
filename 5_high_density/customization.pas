UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =10;
  REGROWTH_FACTOR    =0;

  ANNIHILATION_THRESHOLD=1E10;
  ANNIHILATION_FACTOR   =0;

  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
USES math;
VAR strength:double=0;
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    result:=timeStepIndex mod 500=0;
    if odd(timeStepIndex div 500) then strength:=0 else strength:=0.01;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>32 then exit(zeroVec);
	d:=-strength*sin(2*pi*d/32);
    result[0]:=rx*d;
    result[1]:=ry*d;
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
	  p:=zeroVec;
	end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

