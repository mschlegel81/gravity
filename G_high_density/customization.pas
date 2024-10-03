UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =sqr(32/SYS_SIZE);

  ANNIHILATION_THRESHOLD=1E10;
  ANNIHILATION_FACTOR   =0;

  REGROWTH_FACTOR=0;
  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
USES math;
VAR freq0  ,freq1  :double;
    weight0,weight1:double;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  CONST relativeFreq:array[0..19] of double=(32,32,16,16,8,8,4,4,2,2,2,4,4,8,8,16,16,32,32,64);
  VAR k:longint;
      active:longint;
  begin
    k:=0;
    k:=timeStepIndex div 250;
    active:=timeStepIndex-k*250;
    result:=(active<=25);
    if odd(k) then begin
      weight1:=1; freq1:=relativeFreq[k  ]/SYS_SIZE;
      weight0:=0; freq0:=relativeFreq[k-1]/SYS_SIZE;
    end else begin
      weight0:=1; freq0:=relativeFreq[k  ]/SYS_SIZE;
      weight1:=0; freq1:=relativeFreq[k-1]/SYS_SIZE;
    end;
    if active<25 then begin
      weight0*=active/25;
      weight1*=active/25;
    end;
  end;

FUNCTION gridF(CONST ax:double):double;
  begin
    if ax<1
    then result:=1+sqr(ax)*((1+2.4466748187071037)*sqr(ax)-2-2.4466748187071037)
    else result:=sin(2*pi*abs(ax))*exp(-sqr(ax)/4);
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d,q0,q1:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
    q0:=    abs(rx)+abs(ry) ;
    q1:=max(abs(rx),abs(ry));
    d:=0.5*(weight0*gridF(freq0*q0)
           +weight1*gridF(freq1*q1))/d*sqr(32/SYS_SIZE);
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

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.
