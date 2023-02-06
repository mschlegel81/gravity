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
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with result[i,j] do begin
      if i*2=SYS_SIZE then mass:=10 else mass:=0;
      p:=zeroVec;
    end;
  end;

PROCEDURE addBackgroundAcceleration(CONST timeStepIndex:double; VAR accel: T_vectorField);  
  CONST impactTimes:array[0..99] of longint=(450,636,779,900,1006,1102,1191,1273,1350,1423,1492,1559,1622,1684,1743,1800,1855,1909,1962,2012,2062,2111,2158,2205,2250,2295,2338,2381,2423,2465,2505,2546,2585,2624,2662,2700,2737,2774,2810,2846,2881,2916,2951,2985,3019,3052,3085,3118,3150,3182,3214,3245,3276,3307,3337,3367,3397,3427,3457,3486,3515,3543,3572,3600,3628,3656,3683,3711,3738,3765,3792,3818,3845,3871,3897,3923,3949,3974,4000,4025,4050,4075,4100,4124,4149,4173,4197,4221,4245,4269,4293,4316,4340,4363,4386,4409,4432,4455,4477,4500);
        impactX:array[0..99] of double =(0.5,0.25,0.75,0.125,0.625,0.375,0.875,0.0625,0.5625,0.3125,0.8125,0.1875,0.6875,0.4375,0.9375,0.03125,0.53125,0.28125,0.78125,0.15625,0.65625,0.40625,0.90625,0.09375,0.59375,0.34375,0.84375,0.21875,0.71875,0.46875,0.96875,0.015625,0.515625,0.265625,0.765625,0.140625,0.640625,0.390625,0.890625,0.078125,0.578125,0.328125,0.828125,0.203125,0.703125,0.453125,0.953125,0.046875,0.546875,0.296875,0.796875,0.171875,0.671875,0.421875,0.921875,0.109375,0.609375,0.359375,0.859375,0.234375,0.734375,0.484375,0.984375,0.0078125,0.5078125,0.2578125,0.7578125,0.1328125,0.6328125,0.3828125,0.8828125,0.0703125,0.5703125,0.3203125,0.8203125,0.1953125,0.6953125,0.4453125,0.9453125,0.0390625,0.5390625,0.2890625,0.7890625,0.1640625,0.6640625,0.4140625,0.9140625,0.1015625,0.6015625,0.3515625,0.8515625,0.2265625,0.7265625,0.4765625,0.9765625,0.0234375,0.5234375,0.2734375,0.7734375,0.1484375);
  VAR i,j,k:longint;      
      d:T_2dVector;
	  impactY:double;
  begin
    //for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if j<=SYS_SIZE div 2 then accel[i,j,1]+=20 else accel[i,j,1]-=20;	
//	for k:=0 to 99 do if (timeStepIndex>impactTimes[k]) and (timeStepIndex<impactTimes[k]+20) then begin
//	  impactY:=(timeStepIndex-impactTimes[k])/20*1.1-0.05;
    //  if odd(k) then impactY:=1-impactY;	  	  
//	  for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
//	    d[0]:=(i+0.5)/SYS_SIZE-impactX[k];
//		d[1]:=(j+0.5)/SYS_SIZE-impactY;	  
//		if d[0]*d[0]+d[1]*d[1]<0.05*0.05 then begin
//		  d*=1000/sqrt(d[0]*d[0]+d[1]*d[1]);
//		  accel[i,j]+=d;		
//		end;		
//	  end;
//	end;
	
	
  end;

end.

