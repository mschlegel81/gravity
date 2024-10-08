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
VAR
  REGROWTH_FACTOR    :double = 0;
  ANNIHILATION_FACTOR:double = 0.01;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
USES math;
VAR range:double=0;
    lastRange:double=1000;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    range:=16*(0.5-0.5*cos(timeStepIndex*2*pi/5000));
    result:=odd(timeStepIndex);
    if result then lastRange:=range;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>SYS_SIZE/2 then exit(zeroVec);
	d:=sin(range*(0.5-d/SYS_SIZE)*pi)*(0.5+0.5*cos(2*pi*d/SYS_SIZE))/d*32/SYS_SIZE;
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
  begin
    case initialDensityVariant of
      id_low:  begin REGROWTH_FACTOR:=0.01; ANNIHILATION_FACTOR:=0.0001; end;
      id_high: begin REGROWTH_FACTOR:=0.1 ; ANNIHILATION_FACTOR:=0.001;  end;
      else     begin REGROWTH_FACTOR:=1   ; ANNIHILATION_FACTOR:=0.01;   end;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=10+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

