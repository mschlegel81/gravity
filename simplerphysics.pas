UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil,vectors,commandLineHandling;
TYPE
  T_cellSystem=object(T_serializable)
    private
      value:T_systemState;
      prevAccelTime:double;
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
  begin
    value:=getInitialState;
    prevAccelTime:=-1E50;
    attractionInitialized:=false;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
  end;

FUNCTION T_cellSystem.doMacroTimeStep(CONST timeStepIndex:longint): boolean;
  VAR newState:T_systemState;
      staggeredAcceleration:T_vectorField;

  CONST DT_MIN=dt/200; //not more than 200 sub steps
        MAX_TRANSPORT_RANGE    =2*0.7141*GRID_SIZE;
        MAX_ALLOWED_AREA_CHANGE=0.3*GRID_SIZE*GRID_SIZE;
        SPEED_CAP=0.5*MAX_TRANSPORT_RANGE/DT_MIN;
  VAR capping:boolean=false;
      dtRest:double;
  FUNCTION calcTimeStep:double;
    CONST mask=SYS_SIZE-1;
    VAR i,j:longint;
        vMax,v,areaChange,maxAreaChange:double;
    begin
      result:=dtRest;
      vMax:=epsilon;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do if mass>UPPER_C10_LEVEL then begin
        v:=(p[0]*p[0]+p[1]*p[1]+(a[0]*a[0]+a[1]*a[1])*dtRest*dtRest)/(mass*mass);
        if v>vMax then vMax:=v;
      end;
      vMax:=sqrt(vMax);
      result:=MAX_TRANSPORT_RANGE/vMax;
      if result<DT_MIN then begin
        result:=DT_MIN;
        if not(capping) then log.append('WARNING: Speed limit exceeded. Capping...').appendLineBreak;
        capping:=true;
      end else begin
        maxAreaChange := epsilon;
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>UPPER_C10_LEVEL then begin
          areaChange:=abs((staggeredAcceleration[i,j,1]-staggeredAcceleration[i,(j+1) and mask,1])
                         +(staggeredAcceleration[i,j,0]-staggeredAcceleration[(i+1) and mask,j,0]));
          if areaChange>maxAreaChange then maxAreaChange:=areaChange;
        end;
        result:=max(DT_MIN,min(result,sqrt(MAX_ALLOWED_AREA_CHANGE/maxAreaChange)));
      end;

      if result>dtRest
      then result:=dtRest
      else result:=dtRest/ceil(dtRest/result);
    end;

  VAR simTime:double;
  PROCEDURE transport(CONST dtEff:double);
    CONST mask=SYS_SIZE-1;
    VAR i,j,ti,tj:longint;
        x0,x1,y0,y1,
        cellX0,cellX1,cellY0,cellY1,
        wx,wy:double;
        acc,v:T_2dVector;
        f,
        vx0,vx1,vy0,vy1,
        ax0,ax1,ay0,ay1,
        jx0,jx1,jy0,jy1:double;
        jerkFactor:double;

    begin
      jerkFactor:=dtEff*0.5/(simTime-prevAccelTime);
      prevAccelTime:=simTime;

      //Lagrangian transport:
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with newState[i,j] do begin mass:=0; p:=zeroVec; a:=zeroVec; end;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        with value[i,j] do begin
          //Regrowing mass is added "with zero impulse"
          mass+=REGROWTH_FACTOR*dtEff;

          if mass>epsilon
          then begin
            v  :=p*(1/mass);
            acc:=a*(1/mass);
          end else begin
            v  :=zeroVec;
            acc:=zeroVec;
          end;
          if capping and (mass>=UPPER_BLACK_LEVEL) then begin //cap speed
            //squared speed:
            f:=sqr(v[0])+sqr(v[1]);
            if f>SPEED_CAP*SPEED_CAP then v*=0.99*SPEED_CAP/sqrt(f);
          end;

          if (ANNIHILATION_FACTOR>0) and (mass>ANNIHILATION_THRESHOLD) then begin
            wx:=ANNIHILATION_FACTOR*dtEff;
            mass*=(1 + wx*((ANNIHILATION_THRESHOLD-mass) + wx*(sqr(mass) + ANNIHILATION_THRESHOLD*0.5*(ANNIHILATION_THRESHOLD - 3*mass))));
          end;

        end;

        //a = d²x/dt²
        //j = d³x/dt³ = (a(t_current)-a(t_prev))/(t_current-t_prev)
        //            = (a(t_current)-a(t_prev))*jerkFactor
        //v = v0 + dt*a + dt²/2*j
        //  = v0 + dt*(a+dt/2*j)
        //x = x0 + dt*v + dt²/2*a + dt³/6*j
        //  = x0 + dt*(v+dt/2*(a+dt/3*j))

        ax0:=staggeredAcceleration[(i+mask) and mask,j,0];
        jx0:=(ax0-acc[0])*jerkFactor;
        vx0:=         v[0]+dtEff    *(ax0+jx0);
        x0 :=i+dtEff*(v[0]+dtEff*0.5*(ax0+jx0*0.66666666));

        ax1:=staggeredAcceleration[i,j,0];
        jx1:=(ax1-acc[0])*jerkFactor;
        vx1:=v[0]+dtEff*(ax1 +jx1);
        x1 :=i+1 +dtEff*(v[0]+dtEff*0.5*(ax1+jx1*0.66666666));

        ay0:=staggeredAcceleration[i,(j+mask) and mask,1];
        jy0:=(ay0-acc[1])*jerkFactor;
        vy0:=v[1]+dtEff*(ay0 +jy0);
        y0 :=j   +dtEff*(v[1]+dtEff*0.5*(ay0+jy0*0.66666666));

        ay1:=staggeredAcceleration[i,j,1];
        jy1:=(ay1-acc[1])*jerkFactor;
        vy1:=v[1]+dtEff*(ay1 +jy1);
        y1 :=j+1 +dtEff*(v[1]+dtEff*0.5*(ay1+jy1*0.66666666));

        if x1<x0+1 then begin x0:=(x0+x1)*0.5-0.5; x1:=x0+1; end;
        if y1<y0+1 then begin y0:=(y0+y1)*0.5-0.5; y1:=y0+1; end;

        value[i,j].mass*=1/((x1-x0)*(y1-y0));
        vx0*=value[i,j].mass; vx1*=value[i,j].mass; vx1:=(vx1-vx0)/(x1-x0);
        vy0*=value[i,j].mass; vy1*=value[i,j].mass; vy1:=(vy1-vy0)/(y1-y0);
        ax0*=value[i,j].mass; ax1*=value[i,j].mass; ax1:=(ax1-ax0)/(x1-x0);
        ay0*=value[i,j].mass; ay1*=value[i,j].mass; ay1:=(ay1-ay0)/(y1-y0);
        for ti:=floor(x0) to floor(x1+1) do begin
          //intersection of intervals [ti,ti+1] and [x0,x1]
          // = [max(ti,x0),min(x1,ti+1)] -> weight =
          cellX0:=max(x0,ti);
          cellX1:=min(x1,ti+1);
          wx:=cellX1-cellX0;
          f:=((cellX0+cellX1)*0.5-x0);
          v  [0]:=f*vx1+vx0;
          acc[0]:=f*ax1+ax0;
          if wx>0 then for tj:=floor(y0) to floor(y1+1) do begin
            cellY0:=max(y0,tj);
            cellY1:=min(y1,tj+1);
            wy:=(cellY1-cellY0)*wx;
            if wy>0 then with newState[ti and mask,tj and mask] do begin
              f:=((cellY0+cellY1)*0.5-y0);
              v  [1]:=f*vy1+vy0;
              acc[1]:=f*ay1+ay0;
              mass+=value[i,j].mass*wy;
              p   +=v              *wy;
              a   +=acc            *wy;
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
      start:double;

  begin
    start:=now;
    ensureAttractionFactors(timeStepIndex);
    result:=false;

    dtRest:=dt;
    while dtRest>0 do begin
      staggeredAcceleration:=massFFT(value)*cachedAttraction;;
      simTime:=(timeStepIndex+1)*dt-dtRest;
      addBackgroundAcceleration(simTime/dt,staggeredAcceleration);

      dtEff:=calcTimeStep;
      transport(dtEff);
      dtRest-=dtEff;
      inc(subStepsTaken)
    end;

    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
    log.append('Step ')
       .append(timeStepIndex)
       .append(' done: ')
       .append((now-start)*24*60*60,3)
       .append('s; ')
       .append(subStepsTaken)
       .append(' sub steps; mass=')
       .append(m,3)
       .appendLineBreak;
  end;

FUNCTION T_cellSystem.getPicture: P_rgbPicture;
  VAR i,j:longint;
  begin
    new(result,create);
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do result^.setPixel(i,j,mass);
  end;

FUNCTION T_cellSystem.getSerialVersion: dword;
  begin
    result:=31359+SYS_SIZE;
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

