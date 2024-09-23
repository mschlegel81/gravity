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
VAR freq:double;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  CONST transitIndex  :array[0..7] of longint=(    -201,
                                                714-100,
                                               1429-100,
                                               2143-100,
                                               2857-100,
                                               3571-100,
                                               4286-100,5000);
        relativePeriod:array[0..6] of double=(0.0719482421875,0.0824462890625,0.111053466796875,0.14287567138671875,0.19228210449218749,0.277447509765625,0.5076828002929687);
  VAR k:longint;
      p,r:double;
  begin
    result:=false;
    k:=0;
    while timeStepIndex>transitIndex[k+1] do inc(k);
    if timeStepIndex-transitIndex[k]<=200 then begin
      result:=true;
      p:=0.5+0.5*cos(pi/200*(timeStepIndex-transitIndex[k]));
      p:=p*relativePeriod[k-1]+(1-p)*relativePeriod[k];
      freq:=1/p/SYS_SIZE;
    end else begin
      freq:=1/relativePeriod[k]/SYS_SIZE;
    end;
  end;

FUNCTION gridF(CONST ax:double):double;
  begin
    if ax<1
    then result:=1+sqr(ax)*((1+2.4466748187071037)*sqr(ax)-2-2.4466748187071037)
    else result:=sin(2*pi*abs(ax))*exp(-sqr(ax)/4);
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d,q:double;
  begin
    d:=sqrt(rx*rx+ry*ry);
    d:=0.5*gridF(freq*d)/d*sqr(32/SYS_SIZE);
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

