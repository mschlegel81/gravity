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
	  if (accel[i,j,0]=0) 
	  then mass:=massFactor+0.001*random
	  else mass:=0;
      p:=zeroVec;
    end;
  end;


PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  CONST funnelHeight=0.3;
        funnelOpening=0.05;
		normal1:T_2dVector=(554.7001962252291, 832.0502943378438); 
		normal2:T_2dVector=(554.7001962252291,-832.0502943378438);		
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


  VAR i,j:longint;
 	  funnelPos,q:double;
	  p:T_2dVector;  	  	  
	  
  begin    
	funnelPos:=0.5+0.5*cos(20*pi*cos(pi*sqr(timeStepIndex/5000)))
    for i:=0 to SYS_SIZE-1 do begin
	  p[0]:=i/SYS_SIZE;
	  for j:=0 to SYS_SIZE-1 do begin
        p[1]:=j/SYS_SIZE;
        if p[0]<=funnelX0 then begin
		  if ((p[0]-funnelX0 )*normal1[0]+(p[1]-funnelPos)*normal1[1]<0) and 
	         ((p[0]-funnelX0 )*normal2[0]+(p[1]-funnelPos)*normal2[1]<0) then begin
	 	    if p[1]>funnelPos then accel[i,j]:=normal1
			                  else accel[i,j]:=normal2;
		  end;
		end else if p[0]>=funnelX1 then begin
		  if ((p[0]-funnelX1 )*normal2[0]+(p[1]-funnelPos)*normal2[1]>0) and 
	         ((p[0]-funnelX1 )*normal1[0]+(p[1]-funnelPos)*normal1[1]>0) then begin
	 	    if p[1]>funnelPos then accel[i,j]:=normal2*-1
			                  else accel[i,j]:=normal1*-1;
		  end;
		end;
      end;	
	end;  
	
  end;

end.

