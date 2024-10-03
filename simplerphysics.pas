UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil,vectors,commandLineHandling;
TYPE
  T_cellSystem=object(T_serializable)
    private
      value:T_systemState;
      prevAccelTime:double;
      lastTick:qword;
    public
      numberOfFrames:longint;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      FUNCTION doMacroTimeStep(CONST timeStepIndex:longint):boolean;
      FUNCTION getPicture:P_rgbPicture;
      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

IMPLEMENTATION
USES sysutils,customization,math;
VAR cachedAttraction:T_vectorFieldFFT;
    attractionInitialized:boolean=false;

PROCEDURE ensureAttractionFactors(CONST stepIndex:longint);
  FUNCTION calculateAttraction(CONST x,y:longint):T_2dVector;
    VAR dx,dy:double;
    begin
      dx:=sqr(x+0.5)+sqr(y);
      dy:=sqr(x)+sqr(y+0.5);
      result[0]:=-straightAttraction(x+0.5,y)[0];
      result[1]:=-straightAttraction(x,y+0.5)[1];
      if (dx>SYS_SIZE*SYS_SIZE) then result[0]*=exp(-0.5*(dx*(1/SYS_SIZE*SYS_SIZE)-1));
      if (dy>SYS_SIZE*SYS_SIZE) then result[1]*=exp(-0.5*(dy*(1/SYS_SIZE*SYS_SIZE)-1));
    end;

  VAR ix,iy:longint;
      symX,symY:longint;
      temp:T_2dVector;

      attractionField:T_vectorField;

  PROCEDURE addSymmetricPressureTerm(CONST i,j:longint; CONST factor:double);
    begin
      attractionField[i           ,j           ,0]+=factor*REPULSION_LINEAR;
      attractionField[SYS_SIZE-1-i,j           ,0]-=factor*REPULSION_LINEAR;
      attractionField[j           ,i           ,1]+=factor*REPULSION_LINEAR;
      attractionField[j           ,SYS_SIZE-1-i,1]-=factor*REPULSION_LINEAR;
      if j=0 then exit;
      attractionField[i           ,SYS_SIZE-j  ,0]+=factor*REPULSION_LINEAR;
      attractionField[SYS_SIZE-1-i,SYS_SIZE-j  ,0]-=factor*REPULSION_LINEAR;
      attractionField[SYS_SIZE-j  ,i           ,1]+=factor*REPULSION_LINEAR;
      attractionField[SYS_SIZE-j  ,SYS_SIZE-1-i,1]-=factor*REPULSION_LINEAR;
    end;

  begin
    if reinitializeAttractionFactors(stepIndex) or not(attractionInitialized) then begin
      log.append('(Re)initializing attraction factors').appendLineBreak;

      for ix:=0 to SYS_SIZE-1 do for iy:=0 to SYS_SIZE-1 do begin
        temp:=zeroVec;
        for symX:=-SYMMETRIC_CONTINUATION-1 to SYMMETRIC_CONTINUATION do
        for symY:=-SYMMETRIC_CONTINUATION-1 to SYMMETRIC_CONTINUATION do
          temp+=calculateAttraction(ix+symX*SYS_SIZE,iy+symY*SYS_SIZE);
        attractionField[ix,iy]:=temp;
      end;
      addSymmetricPressureTerm(0,0,0.8);
      addSymmetricPressureTerm(0,1,0.1);
      cachedAttraction:=accelFFT(attractionField);
    end;
    attractionInitialized:=true;
  end;

{ T_cellSystem }
CONSTRUCTOR T_cellSystem.create;
  VAR i,j:longint;
  begin
    value:=getInitialState;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
      dp:=zeroVec;
      da:=zeroVec;
      a :=zeroVec;
    end;
    prevAccelTime:=-1E50;
    attractionInitialized:=false;
    lastTick:=GetTickCount64;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
  end;

FUNCTION T_cellSystem.doMacroTimeStep(CONST timeStepIndex:longint): boolean;
  VAR newState:T_systemState;
      staggeredAcceleration:T_vectorField;
      minCapFactor:double=1;

  CONST DT_MIN=dt/200; //not more than 200 sub steps
        MAX_TRANSPORT_RANGE    =GRID_SIZE;
  VAR dtRest:double;
      simTime:double;
      totalDrift:T_2dVector;
  FUNCTION calcTimeStep:double;
    CONST mask=SYS_SIZE-1;
    VAR i,j,ti,tj:longint;
        maxJerk:double=epsilon;
        jerk,f:double;
        maxSpeed:double=epsilon;
        totalMass:double=0;
        speed:double;
    begin
      result:=dtRest;
      totalDrift:=zeroVec;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>UPPER_C1_LEVEL then begin
        ti:=(i+mask) and mask;
        tj:=(j+mask) and mask;
        f:=1/(value[i,j].mass+value[ti,j].mass);
        speed:=sqr((value[i,j].p[0]-value[i,j].dp[0]+value[ti,j].p[0]+value[ti,j].dp[0])*f);
        jerk :=abs((value[i,j].a[0]-value[i,j].da[0]+value[ti,j].a[0]+value[ti,j].da[0])*f-staggeredAcceleration[mask,j,0]); if jerk>maxJerk then maxJerk:=jerk;

        f:=1/(value[i,j].mass+value[i,tj].mass);
        speed+=sqr((value[i,j].p[1]-value[i,j].dp[1]+value[i,tj].p[1]+value[i,tj].dp[1])*f);                                 if speed>maxSpeed then maxSpeed:=speed;
        jerk :=abs((value[i,j].a[1]-value[i,j].da[1]+value[i,tj].a[1]+value[i,tj].da[1])*f-staggeredAcceleration[i,mask,1]); if jerk>maxJerk then maxJerk:=jerk;

        totalMass +=value[i,j].mass;
        totalDrift+=value[i,j].p;
      end;
      maxSpeed:=MAX_TRANSPORT_RANGE/sqrt(maxSpeed);
      maxJerk:=maxJerk/(simTime-prevAccelTime);
      totalDrift*=0.5/totalMass;

      result:=power(1E-2/maxJerk,1/3);
      if maxSpeed<result then result:=maxSpeed;
      //if result<DT_MIN then begin
      //  result:=DT_MIN;
      //  cappingFactor:=cappingFactor*0.98+0.01;
      //end else cappingFactor:=(cappingFactor-0.01)/0.98;
      //capping:=cappingFactor<1;
      //if cappingFactor>1 then cappingFactor:=1;
      //if cappingFactor<minCapFactor then minCapFactor:=cappingFactor;

      if result>dtRest
      then result:=dtRest
      else result:=dtRest/ceil(dtRest/result);
    end;

  PROCEDURE regrowthAndAnnihilation(CONST dtEff:double);
    VAR i,j:longint;
        f:double;
    begin
//      if not capping then cappingFactor:=1;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        //Regrowing mass is added "with zero impulse"
        mass+=REGROWTH_FACTOR*dtEff;
        if mass>epsilon
        then begin
          p *= 1/mass;
          a *= 1/mass;
          dp*= 1/mass;
          da*= 1/mass;
        end else begin
          p :=zeroVec;
          a :=zeroVec;
          dp:=zeroVec;
          da:=zeroVec;
        end;
        if (ANNIHILATION_FACTOR>0) and (mass>ANNIHILATION_THRESHOLD) then begin
          f:=ANNIHILATION_FACTOR*dtEff;
          mass*=(1 + f*((ANNIHILATION_THRESHOLD-mass) + f*(sqr(mass) + ANNIHILATION_THRESHOLD*0.5*(ANNIHILATION_THRESHOLD - 3*mass))));
        end;
        p :=(p-totalDrift)*mass;
        a *=mass;
        dp*=mass;
        da*=mass;
      end;
    end;

  PROCEDURE transport(CONST dtEff:double);
    CONST mask=SYS_SIZE-1;
    VAR i,j,ti,tj:longint;
        x0,x1,y0,y1,
        cellX0,cellX1,cellY0,cellY1,
        wx,wy,wxy:double;
        new_a,new_p:T_2dVector;
        f,
        vx0,vx1,vy0,vy1,
        ax0,ax1,ay0,ay1:double;
        jerkFactor,jerk:double;
        density:double;

        row_boundary_value:array[0..SYS_SIZE-1] of record
          x,vx,ax:double;
        end;
        cell_boundary_value:record
          y,vy,ay:double;
        end;

    begin
      //a = d²x/dt²
      //j = d³x/dt³ = (a(t_current)-a(t_prev))/(t_current-t_prev)
      //            = (a(t_current)-a(t_prev))*jerkFactor
      //v = v0 + dt*a + dt²/2*j
      //  = v0 + dt*(a+dt/2*j)
      //x = x0 + dt*v + dt²/2*a + dt³/6*j
      //  = x0 + dt*(v+dt/2*(a+dt/3*j))
      jerkFactor:=dtEff*0.5/(simTime-prevAccelTime);
      prevAccelTime:=simTime;

      //Lagrangian transport:
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with newState[i,j] do begin mass:=0; p:=zeroVec; dp:=zeroVec; a:=zeroVec; da:=zeroVec; end;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        with value[i,j] do begin
          if i=0 then begin
            ti:=             mask; f:=1/(mass+value[ti,j].mass);    if f>1E10 then f:=0;
            vx0:=(p[0]-dp[0]+value[ti,j].p[0]+value[ti,j].dp[0])*f;
            ax0:=(a[0]-da[0]+value[ti,j].a[0]+value[ti,j].da[0])*f;
            f:=staggeredAcceleration[mask,j,0]; jerk:=(f-ax0)*jerkFactor; ax0:=f; x0:=i+dtEff*(vx0+dtEff*0.5*(ax0+jerk*0.6666666666666666666666)); vx0+=dtEff*(ax0+jerk);
          end else begin
            x0 :=row_boundary_value[j].x;
            vx0:=row_boundary_value[j].vx;
            ax0:=row_boundary_value[j].ax;
          end;

          ti:=(i+1   ) and mask; f:=1/(mass+value[ti,j].mass);    if f>1E10 then f:=0;
          vx1:=(p[0]+dp[0]+value[ti,j].p[0]-value[ti,j].dp[0])*f;
          ax1:=(a[0]+da[0]+value[ti,j].a[0]-value[ti,j].da[0])*f;
          f:=staggeredAcceleration[i,j,0]; jerk:=(f-ax1)*jerkFactor; ax1:=f; x1:=i+1+dtEff*(vx1+dtEff*0.5*(ax1+jerk*0.6666666666666666666666)); vx1+=dtEff*(ax1+jerk);
          row_boundary_value[j].x:=x1;
          row_boundary_value[j].vx:=vx1;
          row_boundary_value[j].ax:=ax1;

          if j=0 then begin
            tj:=             mask; f:=1/(mass+value[i,tj].mass);    if f>1E10 then f:=0;
            vy0:=(p[1]-dp[1]+value[i,tj].p[1]+value[i,tj].dp[1])*f;
            ay0:=(a[1]-da[1]+value[i,tj].a[1]+value[i,tj].da[1])*f;
            f:=staggeredAcceleration[i,mask,1]; jerk:=(f-ay0)*jerkFactor; ay0:=f; y0:=j  +dtEff*(vy0+dtEff*0.5*(ay0+jerk*0.6666666666666666666666)); vy0+=dtEff*(ay0+jerk);
          end else begin
            y0 :=cell_boundary_value.y;
            vy0:=cell_boundary_value.vy;
            ay0:=cell_boundary_value.ay;
          end;

          tj:=(j+1   ) and mask; f:=1/(mass+value[i,tj].mass);    if f>1E10 then f:=0;
          vy1:=(p[1]+dp[1]+value[i,tj].p[1]-value[i,tj].dp[1])*f;
          ay1:=(a[1]+da[1]+value[i,tj].a[1]-value[i,tj].da[1])*f;
          f:=staggeredAcceleration[i,j,1]; jerk:=(f-ay1)*jerkFactor; ay1:=f; y1:=j+1+dtEff*(vy1+dtEff*0.5*(ay1+jerk*0.6666666666666666666666)); vy1+=dtEff*(ay1+jerk);
          cell_boundary_value.y :=y1 ;
          cell_boundary_value.vy:=vy1;
          cell_boundary_value.ay:=ay1;
        end;

        if x1<x0+1 then begin x0:=(x0+x1)*0.5-0.5; x1:=x0+1; end;
        if y1<y0+1 then begin y0:=(y0+y1)*0.5-0.5; y1:=y0+1; end;

        density:=value[i,j].mass/((x1-x0)*(y1-y0));
        f:=1/(x1-x0);
        vx0*=density; vx1*=density; vx1:=(vx1-vx0)*f;
        ax0*=density; ax1*=density; ax1:=(ax1-ax0)*f;
        f:=1/(y1-y0);
        vy0*=density; vy1*=density; vy1:=(vy1-vy0)*f;
        ay0*=density; ay1*=density; ay1:=(ay1-ay0)*f;
        for ti:=floor(x0) to floor(x1+1) do begin
          //intersection of intervals [ti,ti+1] and [x0,x1]
          // = [max(ti,x0),min(x1,ti+1)] -> weight =
          cellX0:=max(x0,ti);
          cellX1:=min(x1,ti+1);
          wx:=cellX1-cellX0;
          f:=((cellX0+cellX1)*0.5-x0);
          new_p[0]:=f*vx1+vx0;
          new_a[0]:=f*ax1+ax0;
          if wx>0 then for tj:=floor(y0) to floor(y1+1) do begin
            cellY0:=max(y0,tj);
            cellY1:=min(y1,tj+1);
            wy:=cellY1-cellY0;
            wxy:=wx*wy;
            if wy>0 then with newState[ti and mask,tj and mask] do begin
              f:=((cellY0+cellY1)*0.5-y0);
              new_p[1]:=f*vy1+vy0;
              new_a[1]:=f*ay1+ay0;
              mass+=density*wxy;
              p   +=new_p  *wxy;
              a   +=new_a  *wxy;
              dp[0]+=0.5*vx1*wxy*wx;
              dp[1]+=0.5*vy1*wxy*wy;
              da[0]+=0.5*ax1*wxy*wx;
              da[1]+=0.5*ay1*wxy*wy;
            end;
          end;
        end;
      end;
      value:=newState;
    end;

  VAR subStepsTaken:longint=0;
      dtEff:double;
      i,j:longint;
      m:double=0;
      currTick:qword;
  begin
    ensureAttractionFactors(timeStepIndex);
    result:=false;

    dtRest:=dt;
    while dtRest>0 do begin
      staggeredAcceleration:=massFFT(value)*cachedAttraction;;
      simTime:=(timeStepIndex+1)*dt-dtRest;
      addBackgroundAcceleration(simTime/dt,staggeredAcceleration);
      dtEff:=calcTimeStep;
      regrowthAndAnnihilation(dtEff);
      transport(dtEff);
      dtRest-=dtEff;
      inc(subStepsTaken)
    end;

    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
    currTick:=GetTickCount64;
    log.append('Step ')
       .append(timeStepIndex)
       .append(' done: ')
       .append((currTick-lastTick)*1E-3,3)
       .append('s; ')
       .append(subStepsTaken)
       .append(' sub steps; M=')
       .append(m,3);
//    if capping then log.append('(cap: ').append(minCapFactor,3).append(')');
    log.appendLineBreak;
    lastTick:=currTick;
  end;

FUNCTION T_cellSystem.getPicture: P_rgbPicture;
  VAR i,j:longint;
  begin
    new(result,create);
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do result^.setPixel(i,j,mass);
  end;

FUNCTION T_cellSystem.getSerialVersion: dword;
  begin
    result:=31360+SYS_SIZE;
  end;

FUNCTION T_cellSystem.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not(inherited) then exit(false);
    stream.read(value,sizeOf(value));
    numberOfFrames:=stream.readWord;
    prevAccelTime:=stream.readDouble;
    result:=stream.allOkay;
  end;

PROCEDURE T_cellSystem.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.write(value,sizeOf(value));
    stream.writeWord(numberOfFrames);
    stream.writeDouble(prevAccelTime);
  end;

end.

