UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=0;
  dt                    =0.05;
  GRID_SIZE             =1;

  REPULSION_LINEAR   =1;

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

FUNCTION reinitializeAttractionFactors(CONST timeStepIndex: longint): boolean;
  begin
    result:=false;
  end;

FUNCTION straightAttraction(CONST rx,ry:double):T_2dVector;
  VAR d:double;
  begin
    result:=zeroVec
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
    massFactor*=SYS_SIZE*SYS_SIZE/7.0685834705770345;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      mass:=0;
      p:=zeroVec;
    end;
    i:=SYS_SIZE div 2;
    result[i-1,i-1].mass:=massFactor*0.5454060388524101;
    result[i-1,i  ].mass:=massFactor*0.971740163533408;
    result[i-1,i+1].mass:=massFactor*0.5454060388524101;
    result[i  ,i-1].mass:=massFactor*0.971740163533408;
    result[i  ,i  ].mass:=massFactor;
    result[i  ,i+1].mass:=massFactor*0.971740163533408;
    result[i+1,i-1].mass:=massFactor*0.5454060388524101;
    result[i+1,i  ].mass:=massFactor*0.971740163533408;
    result[i+1,i+1].mass:=massFactor*0.5454060388524101;
    
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);
  begin
  end;

end.

