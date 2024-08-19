UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=1;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =sqr(32/SYS_SIZE);

  DIFFUSION_BY_VELOCITY =0;
  DIFFUSION_BASE        =0;
  REGROWTH_FACTOR       =0;
  ANNIHILATION_FACTOR   =0.001;
VAR 
  ANNIHILATION_THRESHOLD:double=0;
  
FUNCTION reinitializeAttractionFactors(CONST timeStepIndex:longint):boolean;
FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
FUNCTION getInitialState:T_systemState;
PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel:T_vectorField);
IMPLEMENTATION
USES math;
VAR freq0  ,freq1  :double;
    weight0,weight1:double;

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  CONST transitIndex  :array[0..10] of longint=(   -201-50, 500-50,
                                                   1000-50,1500-50,
                                                   2000-50,2500-50,
                                                   3000-50,3500-50,
                                                   4000-50,4500-50,                                               
                                                   5000);
        relativeFreq:array[-1..9] of double=(64,32,32,16,16,8,8,4,4,2,2);
  VAR k:longint;
      p:double;
  begin
    k:=0;    
    while timeStepIndex>=transitIndex[k+1] do inc(k);
    if timeStepIndex-transitIndex[k]<=100 then begin
      result:=true;
      p:=0.5-0.5*cos(pi/100*(timeStepIndex-transitIndex[k]));
    end else begin
      result:=false;    
      p:=1;
    end;
    if odd(k) then begin
      weight1:=  p; freq1:=relativeFreq[k  ]/SYS_SIZE;      
      weight0:=1-p; freq0:=relativeFreq[k-1]/SYS_SIZE; 
    end else begin
      weight0:=  p; freq0:=relativeFreq[k  ]/SYS_SIZE;      
      weight1:=1-p; freq1:=relativeFreq[k-1]/SYS_SIZE; 
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
  begin
    case initialDensityVariant of
      id_low:  ANNIHILATION_THRESHOLD:=1;
      id_high: ANNIHILATION_THRESHOLD:=2;
      else     ANNIHILATION_THRESHOLD:=4;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=1+0.001*random;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

