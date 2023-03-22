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
  REPULSION_LINEAR   =50;
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
  begin
    case initialDensityVariant of
      id_low:  
	    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
	      if i+i<SYS_SIZE then mass:=1+0.001*random else mass:=0;
          p:=zeroVec;	  
        end; 	  
      id_high: 
	    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
	      if j>i then mass:=1+0.001*random else mass:=0;
          p:=zeroVec;	  
        end; 	  
      else for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
	    if sqr(i/SYS_SIZE-0.4)+sqr(j/SYS_SIZE-0.25)<sqr(0.1) then mass:=20+0.001*random else mass:=0;
        p:=zeroVec;	  
      end; 	  
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  VAR i,j:longint;
      w0,w1,n:double; 
	  d:T_2dVector;
  begin
    if (timeStepIndex<=1000)
	then for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do accel[i,j,1]+=50
	else begin
	  //d[1]:=sqr((timeStepIndex-1000)/500)*pi;
	  //d[0]:=sin(d[1])*50;
	  //d[1]:=cos(d[1])*50;
	  //for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do accel[i,j]+=d;
	  
	
	  if (round(timeStepIndex) mod 2000<1000) then begin
	    w0:=50;
		w1:=0;	  
	  end else begin
	    w0:=0;
		w1:=50;
	  end;
	  for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
		d[0]:=(i+0.5)/SYS_SIZE-0.5;
		d[1]:=(j    )/SYS_SIZE-0.5;
		n:=sqrt(d[0]*d[0]+d[1]*d[1]);
		if n<0.15 then d*=1000/n
		          else begin
		  d[0]:=d[0]*(-w1/n)+accel[i,j,0];
		end;  
		accel[i,j,0]:=d[0];
		
		d[0]:=(i    )/SYS_SIZE-0.5;
		d[1]:=(j+0.5)/SYS_SIZE-0.5;
		n:=sqrt(d[0]*d[0]+d[1]*d[1]);
		if n<0.15 then d*=1000/n
		          else begin				  
		  d[1]:=d[1]*(-w1/n)+accel[i,j,1]+w0;
		end;  
		accel[i,j,1]+=d[1];
      end;
	end;
  end;

end.

