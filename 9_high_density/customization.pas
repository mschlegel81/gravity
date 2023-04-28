UNIT customization;
INTERFACE
USES vectors,commandLineHandling;
CONST
  SYMMETRIC_CONTINUATION=0;
  dt                    =0.05;
  GRID_SIZE             =1;

  LIMITED_RANGE_ATTRACTION=true;
  ATTRACTION_RANGE        =0;

  REPULSION_THRESHOLD=0;
  REPULSION_LINEAR   =200;
  REPULSION_QUADRATIC=0;

  ANNIHILATION_THRESHOLD=100;
  ANNIHILATION_FACTOR   =0;  
  REGROWTH_FACTOR       =0;

  DIFFUSION_BY_VELOCITY=0;
  DIFFUSION_BASE       =0;  
  
  DRIFT_TO_CENTER=false;

  
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
  begin
    result:=zeroVec;
  end;
    
FUNCTION getInitialState: T_systemState;
  VAR i,j:longint;
      massFactor:double;
      accel:T_vectorField;    
  begin
    case initialDensityVariant of
      id_low:  massFactor:= 0.5;
      id_high: massFactor:= 1;
      else     massFactor:= 2;
    end;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do accel[i,j]:=zeroVec;
    addBackgroundAcceleration(0,accel);
    
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      if (accel[i,j,0]=0) and (accel[i,j,1]=0) and ((i=0) or (accel[i-1,j,0]=0)) and ((j=0) or (accel[i,j-1,0]=0))  
      then mass:=massFactor+0.001*random
      else mass:=0;
      p:=zeroVec;
    end;
  end;


PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  CONST funnelHeight=0.3;
        funnelOpening=0.05;
        normal1:T_2dVector=(5547.001962252291, 8320.502943378438); 
        normal2:T_2dVector=(5547.001962252291,-8320.502943378438);      
        funnelX0=0.5-funnelOpening;
        funnelX1=0.5+funnelOpening;
{
  0        1____funnelPos+funnelHeight
   \      /
    \    /
     \  /_______funnelPos
     /  \
    /    \
   /      \_____funnelPos-funnelHeight
 }  
  //Inside left wedge: ((x,y)-(0.5-funnelOpening,funnelPos))*normal1 < 0 ...
  VAR funnelPos:double;
  PROCEDURE addAccelerationAt(CONST x,y:double; VAR toMofiy:T_2dVector; CONST toModifiyI:longint);
    VAR inside1,inside2:double;
        toAdd:double=0;        
    begin
      if x<=funnelX0 then begin
        inside1:=-(x-funnelX0 )*normal1[0]+(y-funnelPos)*normal1[1];
        inside2:=-(x-funnelX0 )*normal2[0]+(y-funnelPos)*normal2[1];
        if (inside1>0) and (inside2>0) then begin
          if y>funnelPos then toAdd:=normal1[toModifiyI]
                         else toAdd:=normal2[toModifiyI];            
        end;
      end else if x>=funnelX1 then begin
        inside1:=(x-funnelX1 )*normal2[0]+(y-funnelPos)*normal2[1];
        inside2:=(x-funnelX1 )*normal1[0]+(y-funnelPos)*normal1[1];
        if (inside1>0) and (inside2>0) then begin          
          if y>funnelPos then toAdd:=-normal2[toModifiyI]
                         else toAdd:=-normal1[toModifiyI];
        end;
      end;
      if toAdd>0 then begin
        if toMofiy[toModifiyI]>0 then toMofiy[toModifiyI]+=toAdd else toMofiy[toModifiyI]:=toAdd;      
      end else if toAdd<0 then begin      
        if toMofiy[toModifiyI]<0 then toMofiy[toModifiyI]+=toAdd else toMofiy[toModifiyI]:=toAdd;            
      end;
    end;

  CONST halfSize=0.5/SYS_SIZE;  
  VAR i,j:longint;
      p  :T_2dVector;           
  begin    
    funnelPos:=0.5+0.5*(SYS_SIZE-2)/SYS_SIZE*cos(20*pi*cos(pi*sqr(timeStepIndex/5000)));
    for i:=0 to SYS_SIZE-1 do begin
      p[0]:=(i+0.5)/SYS_SIZE;
      for j:=0 to SYS_SIZE-1 do begin
        p[1]:=(j+0.5)/SYS_SIZE; 
        addAccelerationAt(p[0]+halfSize,p[1],accel[i,j],0);
        addAccelerationAt(p[0],p[1]+halfSize,accel[i,j],1);
      end;  
    end;  
    
  end;

end.

