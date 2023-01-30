UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =30;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =10;
  REPULSION_QUADRATIC=1;

  ANNIHILATION_THRESHOLD=5;
  ANNIHILATION_FACTOR   =1E-2;  
  
  DIFFUSION_BY_VELOCITY=0.05;
  DIFFUSION_BASE       =0.05;  
  
  DRIFT_TO_CENTER=true;

VAR 
  REGROWTH_FACTOR:double=1E-2;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel:T_vectorField);
IMPLEMENTATION
VAR rk,w:double;
    k0,k1:longint;
	attractionInitializedForRk:double=1E50;
	invA,invB:boolean;
   
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin     	
    case initialDensityVariant of
      id_low:  REGROWTH_FACTOR:=0.5E-2;
      id_high: REGROWTH_FACTOR:=1E-2;
      else     REGROWTH_FACTOR:=2E-2;
    end;

    rk:=2.5*(1-cos(timeStepIndex*2*pi/5000));	
	result:=(abs(rk-attractionInitializedForRk)>0.02) or (timeStepIndex=2500) or (timeStepIndex>4800);
	k0:=trunc(rk);
    k1:=k0+1;
    w:=rk-k0;
	invA:=timeStepIndex>=2500;
	invB:=(k1=5) or (timeStepIndex>=2500) ;
	if result then  attractionInitializedForRk:=rk;	
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;  
  VAR f,a,b:double;
  begin
    f:=sqrt(sqr(rx)+sqr(ry));
    if f<ATTRACTION_RANGE then begin
      a:=0.5*cos((0.5+k0)*f*pi/ATTRACTION_RANGE)*(1-w);
      b:=0.5*cos((0.5+k1)*f*pi/ATTRACTION_RANGE)*   w ;
      if invB then b*=-1;
      if invA then a*=-1;
      f:=(a+b)/f;
    end else f:=0;
    result[0]:=rx*f;
    result[1]:=ry*f;
  end;
	
FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
  begin
    case initialDensityVariant of
      id_low:  massFactor:= 0;
      id_high: massFactor:= 0.5;
      else     massFactor:= 1;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=massFactor+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:longint; VAR accel: T_vectorField);  
  begin
  end;

end.

