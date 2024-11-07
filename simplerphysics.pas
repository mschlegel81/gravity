UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil,vectors,commandLineHandling;
TYPE
  T_cellSystem=object(T_serializable)
    private
      value:T_systemState;
      prevAccelTime,lastStepReccomendation:double;
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
  //FUNCTION calculateAttraction(CONST x,y:longint):T_2dVector;
  //  VAR dx,dy:double;
  //  begin
  //    dx:=sqr(x+0.5)+sqr(y);
  //    dy:=sqr(x)+sqr(y+0.5);
  //    result[0]:=-straightAttraction(x+0.5,y)[0];
  //    result[1]:=-straightAttraction(x,y+0.5)[1];
  //    if (dx>SYS_SIZE*SYS_SIZE) then result[0]*=exp(-0.5*(dx*(1/SYS_SIZE*SYS_SIZE)-1));
  //    if (dy>SYS_SIZE*SYS_SIZE) then result[1]*=exp(-0.5*(dy*(1/SYS_SIZE*SYS_SIZE)-1));
  //  end;
  FUNCTION calculateAttraction(CONST x,y:longint):T_2dVector;
    CONST dz:array[0..2] of double=(-sqrt(3/5)/2, 0, sqrt(3/5)/2);
          WZ:array[0..2] of double=(5/18,8/18,5/18);
    VAR w:double;
    begin
      w:=sqr(x)+sqr(y);
      if w<SYS_SIZE*SYS_SIZE then w:=1
      else begin
        w:=sqrt(w)/SYS_SIZE-1;
        if w>(SYMMETRIC_CONTINUATION+0.5) then exit(zeroVec);
        w:=0.5+0.5*cos(w*pi/(SYMMETRIC_CONTINUATION+0.5));
      end;
      if w=1 then begin
        result[0]:=-(WZ[0]*straightAttraction(x+0.5,y+dz[0])[0]+
                     WZ[1]*straightAttraction(x+0.5,y+dz[1])[0]+
                     WZ[2]*straightAttraction(x+0.5,y+dz[2])[0]);
        result[1]:=-(WZ[0]*straightAttraction(x+dz[0],y+0.5)[1]+
                     WZ[1]*straightAttraction(x+dz[1],y+0.5)[1]+
                     WZ[2]*straightAttraction(x+dz[2],y+0.5)[1]);
      end else begin
        result[0]:=-w*straightAttraction(x+0.5,y)[0];
        result[1]:=-w*straightAttraction(x,0.5+y)[1];
      end;
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
    lastStepReccomendation:=1E-3*dt;
    prevAccelTime:=-1E-3*dt;
    attractionInitialized:=false;
    lastTick:=GetTickCount64;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
  end;

FUNCTION T_cellSystem.doMacroTimeStep(CONST timeStepIndex:longint): boolean;
  VAR newState:T_systemState;
      staggeredAcceleration:T_vectorField;

  CONST MAX_TRANSPORT_RANGE=GRID_SIZE;
  VAR dtRest:double;
      simTime:double;
      totalDrift:T_2dVector;
  FUNCTION calcTimeStep:double;
    VAR i,j:longint;
        maxJerk:double=epsilon;
        jerk,f:double;
        maxSpeed:double=epsilon;
        totalMass:double=0;
        speed:double;
    begin
      result:=lastStepReccomendation*1.1;
      totalDrift:=zeroVec;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>UPPER_C1_LEVEL then begin
        f:=1/(value[i,j].mass);
        speed:=abs((value[i,j].p[0]-value[i,j].dp[0])*f);                              if speed>maxSpeed then maxSpeed:=speed;
        jerk :=abs((value[i,j].a[0]-value[i,j].da[0])*f-staggeredAcceleration[i,j,0]); if jerk>maxJerk then maxJerk:=jerk;

        f:=1/(value[i,j].mass);
        speed:=abs((value[i,j].p[1]-value[i,j].dp[1])*f);                              if speed>maxSpeed then maxSpeed:=speed;
        jerk :=abs((value[i,j].a[1]-value[i,j].da[1])*f-staggeredAcceleration[i,j,1]); if jerk>maxJerk then maxJerk:=jerk;

        totalMass +=value[i,j].mass;
        totalDrift+=value[i,j].p;
      end;
      f:=MAX_TRANSPORT_RANGE/maxSpeed;
      if f<result then result:=f;

      maxJerk:=maxJerk/(simTime-prevAccelTime);
      f:=sqrt(7/maxJerk);
      if f<result then result:=f;

      totalDrift*=0.3/totalMass;

      lastStepReccomendation:=result;

      if result>dtRest
      then result:=dtRest
      else result:=dtRest/ceil(dtRest/result);
    end;

  PROCEDURE regrowthAndAnnihilation(CONST dtEff:double);
    VAR i,j:longint;
        f:double;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        //Regrowing mass is added "with zero impulse"
        mass+=REGROWTH_FACTOR*dtEff;
        if (ANNIHILATION_FACTOR>0) and (mass>ANNIHILATION_THRESHOLD) then begin
          f:=1/mass;
          p *=f;
          a *=f;
          dp*=f;
          da*=f;
          //f:=ANNIHILATION_FACTOR*dtEff;
          //mass*=(1 + f*((ANNIHILATION_THRESHOLD-mass) + f*(sqr(mass) + ANNIHILATION_THRESHOLD*0.5*(ANNIHILATION_THRESHOLD - 3*mass))));
          mass-=ANNIHILATION_FACTOR*dtEff*sqr(mass-ANNIHILATION_THRESHOLD);
          if mass<ANNIHILATION_THRESHOLD then mass:=ANNIHILATION_THRESHOLD;
          p :=(p-totalDrift)*mass;
          a *=mass;
          dp*=mass;
          da*=mass;
        end else p-=totalDrift*mass;
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
        xBorder,yBorder:byte;
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

          f:=1/mass; if f>1E10 then f:=0;
          vx0:=(p[0]-dp[0])*f;
          ax0:=(a[0]-da[0])*f;
          f:=staggeredAcceleration[(i+mask) and mask,j,0]; jerk:=(f-ax0)*jerkFactor; ax0:=f; x0:=i  +dtEff*(vx0+dtEff*0.5*(ax0+jerk*0.6666666666666666666666)); vx0+=dtEff*(ax0+jerk);

          f:=1/mass; if f>1E10 then f:=0;
          vx1:=(p[0]+dp[0])*f;
          ax1:=(a[0]+da[0])*f;
          f:=staggeredAcceleration[i,j,0];                 jerk:=(f-ax1)*jerkFactor; ax1:=f; x1:=i+1+dtEff*(vx1+dtEff*0.5*(ax1+jerk*0.6666666666666666666666)); vx1+=dtEff*(ax1+jerk);

          f:=1/mass; if f>1E10 then f:=0;
          vy0:=(p[1]-dp[1])*f;
          ay0:=(a[1]-da[1])*f;
          f:=staggeredAcceleration[i,(j+mask) and mask,1]; jerk:=(f-ay0)*jerkFactor; ay0:=f; y0:=j  +dtEff*(vy0+dtEff*0.5*(ay0+jerk*0.6666666666666666666666)); vy0+=dtEff*(ay0+jerk);

          f:=1/mass; if f>1E10 then f:=0;
          vy1:=(p[1]+dp[1])*f;
          ay1:=(a[1]+da[1])*f;
          f:=staggeredAcceleration[i,j,1];                 jerk:=(f-ay1)*jerkFactor; ay1:=f; y1:=j+1+dtEff*(vy1+dtEff*0.5*(ay1+jerk*0.6666666666666666666666)); vy1+=dtEff*(ay1+jerk);
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
          cellX0:=max(x0,ti);   if cellX0=ti   then xBorder:=1 else xBorder:=0;
          cellX1:=min(x1,ti+1); if cellX1=ti+1 then xBorder+=2;
          wx:=cellX1-cellX0;
          f:=((cellX0+cellX1)*0.5-x0);
          new_p[0]:=f*vx1+vx0;
          new_a[0]:=f*ax1+ax0;
          if wx>0 then for tj:=floor(y0) to floor(y1+1) do begin
            cellY0:=max(y0,tj);   if cellY0=tj   then yBorder:=1 else yBorder:=0;
            cellY1:=min(y1,tj+1); if cellY1=tj+1 then yBorder+=2;
            wy:=cellY1-cellY0;
            wxy:=wx*wy;
            if wy>0 then with newState[ti and mask,tj and mask] do begin
              f:=((cellY0+cellY1)*0.5-y0);
              new_p[1]:=f*vy1+vy0;
              new_a[1]:=f*ay1+ay0;
              mass+=density*wxy;
              p   +=new_p  *wxy;
              a   +=new_a  *wxy;
              case xBorder of
                1: //lower only
                   begin
                     dp[0]+=0.5*(-new_p[0]*(1-wx)+vx1*wx)*wxy;
                     da[0]+=0.5*(-new_a[0]*(1-wx)+ax1*wx)*wxy;
                   end;
                2: //upper only
                   begin
                     dp[0]+=0.5*( new_p[0]*(1-wx)+vx1*wx)*wxy;
                     da[0]+=0.5*( new_a[0]*(1-wx)+ax1*wx)*wxy;
                   end;
                3: //both
                   begin
                     dp[0]+=0.5*vx1*wxy;
                     da[0]+=0.5*ax1*wxy;
                   end;
              end;
              case yBorder of
                1: //lower only
                   begin
                     dp[1]+=0.5*(-new_p[1]*(1-wy)+vy1*wy)*wxy;
                     da[1]+=0.5*(-new_a[1]*(1-wy)+ay1*wy)*wxy;
                   end;
                2: //upper only
                   begin
                     dp[1]+=0.5*( new_p[1]*(1-wy)+vy1*wy)*wxy;
                     da[1]+=0.5*( new_a[1]*(1-wy)+ay1*wy)*wxy;
                   end;
                3: //both
                   begin
                     dp[1]+=0.5*vy1*wxy;
                     da[1]+=0.5*ay1*wxy;
                   end;
              end;
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
      Ekin:double=0;
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
      {$ifdef debugMode}
      m:=0;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
      log.append('Pre-Substep ').append(subStepsTaken).append(' done; mass=').append(m,3).append('; substep size=').append(dtEff,5).appendLineBreak;
      {$endif}

      if (timeStepIndex<=1) and (subStepsTaken=0) and (dtEff>dt/1000) then dtEff:=dt/1000;
      regrowthAndAnnihilation(dtEff);
      {$ifdef debugMode}
      m:=0;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
      log.append('Annihilation').append(subStepsTaken).append(' done; mass=').append(m,3).append('; substep size=').append(dtEff,5).appendLineBreak;
      {$endif}
      transport(dtEff);
      dtRest-=dtEff;
      inc(subStepsTaken);

    end;
    m:=0;
    Ekin:=0;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
      m+=value[i,j].mass;
      if value[i,j].mass>0 then Ekin+=(sqr(value[i,j].p[0])+sqr(value[i,j].p[1]))/value[i,j].mass;
    end;
    currTick:=GetTickCount64;
    log.append('Step ')
       .append(timeStepIndex)
       .append(' done: ')
       .append((currTick-lastTick)*1E-3,3)
       .append('s; ')
       .append(subStepsTaken)
       .append(' sub steps; M=')
       .append(m,3)
       .append('; E=')
       .append(Ekin,3)
//    if capping then log.append('(cap: ').append(minCapFactor,3).append(')');
       .appendLineBreak;
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
    result:=12+SYS_SIZE;
  end;

FUNCTION T_cellSystem.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not(inherited) then exit(false);
    stream.read(value,sizeOf(value));
    numberOfFrames:=stream.readWord;
    prevAccelTime:=stream.readDouble;
    lastStepReccomendation:=stream.readDouble;
    result:=stream.allOkay;
  end;

PROCEDURE T_cellSystem.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.write(value,sizeOf(value));
    stream.writeWord(numberOfFrames);
    stream.writeDouble(prevAccelTime);
    stream.writeDouble(lastStepReccomendation);
  end;

end.

