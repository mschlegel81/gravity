UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =10;

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
    strength:double=0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    range:=(0.5-0.5*cos(timeStepIndex*2*pi/1000))*SYS_SIZE;
    result:=abs(range-lastRange)>0.5;
    if result then lastRange:=range;
    if range<32 then strength:=0.2 else strength:=0.2*sqr(32/range);
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
	if d>range then exit(zeroVec);
	d:=(0.5-0.5*cos(2*pi*d/range))*strength/d;
    result[0]:=rx*d;
    result[1]:=ry*d;
  end;

FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
  begin
    case initialDensityVariant of
      id_low:  begin REGROWTH_FACTOR:=0.05; ANNIHILATION_FACTOR:=0.05; end;
      id_high: begin REGROWTH_FACTOR:=0.1 ; ANNIHILATION_FACTOR:=0.1;  end;
      else     begin REGROWTH_FACTOR:=0.2;  ANNIHILATION_FACTOR:=0.2;    end;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=2*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;
end.

