UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =0;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =25;
  REPULSION_QUADRATIC=5;

  ANNIHILATION_THRESHOLD=5;
  ANNIHILATION_FACTOR   =0;

  REGROWTH_FACTOR=0;
  DIFFUSION_BY_VELOCITY=0.01;
  DIFFUSION_BASE       =0.6;  
  
  DRIFT_TO_CENTER=false;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel:T_vectorField);
IMPLEMENTATION
VAR forceFactor:double=0;
    
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin  
    result:=false;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  begin
    result:=zeroVec;	
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

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel: T_vectorField);
  VAR bgX,bgY,bgFactor,bgRad:double;
  FUNCTION bgAttraction(CONST rx,ry:double):T_2dVector;
    FUNCTION boxed(CONST z:double):double;
      begin
        result:=z;
    	    if result>0.5 then result-=1 else if result<-0.5 then result+=1;
      end;
    VAR dx,dy,f:double;
    begin
      dx:=boxed(rx-bgX); dy:=boxed(ry-bgY);
      f:=sqrt(sqr(dx)+sqr(dy));
      if (f<bgRad) then begin
    	f:=bgFactor*(1+cos(f/bgRad*pi))/f;
    	    result[0]:=dx*f;
        result[1]:=dy*f;
      end else result:=zeroVec;

      dx:=boxed(rx+bgX); dy:=boxed(ry+bgY);
      f:=sqrt(sqr(dx)+sqr(dy));
      if (f<bgRad) then begin
    	f:=bgFactor*(1+cos(f/bgRad*pi))/f;
    	    result[0]-=dx*f;
        result[1]-=dy*f;
      end;
    end;

  VAR angle:double;
      ix, iy: Integer;
  begin
    angle:=timeStepIndex/5000*20*pi;
    bgX:=0.25*cos(angle);
    bgY:=0.25*sin(angle);
    bgFactor:=exp(timeStepIndex/800);
    bgRad:=0.1+0.4*sqr(timeStepIndex/5000);
    for ix:=0 to SYS_SIZE-1 do for iy:=0 to SYS_SIZE-1 do
      accel[ix,iy]+=
            bgAttraction((ix/(SYS_SIZE-1)-0.5),
      	               (iy/(SYS_SIZE-1)-0.5));
  end;

end.

