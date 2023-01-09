UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil,vectors,commandLineHandling;
TYPE
  T_cellSystem=object(T_serializable)
    private
      value:T_systemState;
    public
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
CONST epsilon=1E-10;
VAR cachedAttraction:array [-SYS_SIZE+1..SYS_SIZE-1,-SYS_SIZE+1..SYS_SIZE-1] of T_2dVector;
    attractionInitialized:boolean=false;
    sinus_table:array[0..SYS_SIZE-1] of TmyFloat;
    box:array[0..SYS_SIZE-1] of set of byte;

PROCEDURE ensureAttractionFactors(CONST stepIndex:longint);
  FUNCTION calculateAttraction(CONST x,y:longint):T_2dVector;
    CONST GAUSS_LEGENDRE_WEIGHT:array[0..4,0..4] of record d,w:TmyFloat; end
    =(((d: 0.0                ; w:1.0                ),(d: 0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.28867513459481287; w:0.5                ),(d: 0.28867513459481287; w:0.5                ),(d:0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.3872983346207417 ; w:0.27777777777777779),(d: 0.0                ; w:0.4444444444444444 ),(d:0.3872983346207417 ; w:0.27777777777777779),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.43056815579702629; w:0.17392742256872692),(d:-0.16999052179242816; w:0.3260725774312731 ),(d:0.16999052179242816; w:0.3260725774312731 ),(d:0.43056815579702629; w:0.17392742256872692),(d:0.0              ; w:0.0                )),
      ((d:-0.453089922969332  ; w:0.11846344252809454),(d:-0.26923465505284155; w:0.23931433524968324),(d:0.0                ; w:0.28444444444444444),(d:0.26923465505284155; w:0.23931433524968324),(d:0.453089922969332; w:0.11846344252809454)));

    VAR distance:double;
        n,i:longint;
    begin
      distance:=x*x+y*y;
      if      distance<=sqr( 2) then n:=4
      else if distance<=sqr( 4) then n:=3
      else if distance<=sqr( 8) then n:=2
      else if distance<=sqr(16) then n:=1
      else                           n:=0;

      n:=0;

      result:=zeroVec;
      for i:=0 to n do begin
        //if (y<>0) or (abs(x-0.5)>0.6) then
        result[0]+=straightAttraction(x-0.5 ,
                                      y    +GAUSS_LEGENDRE_WEIGHT[n,i].d)[0]*
                                            GAUSS_LEGENDRE_WEIGHT[n,i].w;
        //if (x<>0) or (abs(y-0.5)>0.6) then
        result[1]+=straightAttraction(x    +GAUSS_LEGENDRE_WEIGHT[n,i].d,
                                      y-0.5                             )[1]*
                                            GAUSS_LEGENDRE_WEIGHT[n,i].w;
      end;
      if distance>SYS_SIZE*SYS_SIZE then result*=exp(-0.5*(distance*(1/SYS_SIZE*SYS_SIZE)-1));
    end;

  VAR ix,iy:longint;
      symX,symY:longint;
  begin
    if reinitializeAttractionFactors(stepIndex) or not(attractionInitialized) then begin
      log.append('(Re)initializing attraction factors').appendLineBreak;

      for ix:=-SYS_SIZE+1 to SYS_SIZE-1 do for iy:=-SYS_SIZE+1 to SYS_SIZE-1 do begin
        cachedAttraction[ix,iy]:=zeroVec;
        for symX:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
        for symY:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
          cachedAttraction[ix,iy]+=calculateAttraction(ix+symX*SYS_SIZE,iy+symY*SYS_SIZE);
      end;
    end;
    if not(attractionInitialized) then begin
      for ix:=0 to SYS_SIZE-1 do begin
        box[ix]:=[];
        for iy:=ix-ATTRACTION_RANGE+SYS_SIZE to ix+ATTRACTION_RANGE+SYS_SIZE do include(box[ix],byte(iy mod SYS_SIZE));
      end;
      for ix:=0 to SYS_SIZE-1 do sinus_table[ix]:=sin(ix*2*pi/(SYS_SIZE-1));
    end;
    attractionInitialized:=true;
  end;

{ T_cellSystem }
CONSTRUCTOR T_cellSystem.create;
  begin
    value:=getInitialState;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
  end;

FUNCTION T_cellSystem.doMacroTimeStep(CONST timeStepIndex:longint): boolean;
  VAR stateDelta:T_systemState;
      vNew,accel:T_vectorField;

  PROCEDURE setGravAcceleration;
    VAR i,j,oi,oj:longint;
        a: T_2dVector;
    begin
      if LIMITED_RANGE_ATTRACTION then begin
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
          a:=zeroVec;
          for oi in box[i] do for oj in box[j] do
            a+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
          accel[i,j]:=a;
        end;
      end else begin
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
          a:=zeroVec;
          for oi:=0 to SYS_SIZE-1 do for oj:=0 to SYS_SIZE-1 do
            a+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
          accel[i,j]:=a;
        end;
      end;
    end;

  PROCEDURE addShallowWaterAcceleration;
    CONST mask=SYS_SIZE-1;
    VAR local:array[0..1,0..1] of double;
        a:T_2dVector;
        i,j,ip,jp:longint;
    FUNCTION cap(CONST n:double):double; inline;
      begin
        if n<REPULSION_THRESHOLD
        then result:=0
        else result:=n-REPULSION_THRESHOLD;;
      end;

    begin
      if (REPULSION_LINEAR<=0) then exit;
      for i:=0 to SYS_SIZE-1 do begin
        ip:=(i+1   ) and mask;
        for j:=0 to SYS_SIZE-1 do begin
          jp:=(j+1   ) and mask;
          if j=0 then begin
            local[0,0]:=cap(value[i ,j ].mass);
            local[1,0]:=cap(value[ip,j ].mass);
          end else begin
            local[0,0]:=local[ 0, 1];
            local[1,0]:=local[ 1, 1];
          end;
          local[0,1]:=cap(value[i ,jp].mass);
          local[1,1]:=cap(value[ip,jp].mass);

          a[0]:=local[0,0]-local[1,0];
          a[1]:=local[0,0]-local[0,1];
          accel[i,j]+=a*REPULSION_LINEAR;
        end;
      end;
    end;

  PROCEDURE annihilateOrRegrow(CONST dtEff:double);
    VAR i, j: integer;
        factor: double;
        v:T_2dVector;
    begin
      if (ANNIHILATION_FACTOR<=0) and (REGROWTH_FACTOR<=0) then exit;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        if mass>ANNIHILATION_THRESHOLD then begin
          factor:=1-(mass-ANNIHILATION_THRESHOLD)*ANNIHILATION_FACTOR*dtEff+REGROWTH_FACTOR/mass*dtEff;
          mass*=factor;
          p   *=factor;
        end else if (REGROWTH_FACTOR>0) then begin
          v   :=p*(1/(epsilon+mass));
          mass+=REGROWTH_FACTOR*dtEff;
          p   :=v*mass;
        end;
      end;
    end;

  PROCEDURE addDrift;
    VAR pTot:T_2dVector=(0,0);
        mTot:TmyFloat=0;
        deltaV:T_2dVector=(0,0);
        i, j: integer;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        pTot+=p;
        mTot+=mass;
        deltaV[0]+=mass*sinus_table[i];
        deltaV[1]+=mass*sinus_table[j];
      end;
      deltaV:=(deltaV-pTot*0.5)*(20/mTot);
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do accel[i,j]+=deltaV;
    end;

  VAR capping:boolean=false;
  PROCEDURE transport(VAR dtRest:TmyFloat; OUT dtEff:TmyFloat);
    CONST mask=SYS_SIZE-1;
          DT_MIN=dt/200; //not more than 200 sub steps
          MAX_COURANT_NUMBER=0.25; //Worst case: outflow from a cell in all four directions
          SAFE_COURANT_NUMBER=0.2;
          SPEED_CAP=SAFE_COURANT_NUMBER*GRID_SIZE/DT_MIN;

    VAR i,j:longint;
        f:double;
        transportedMass:TmyFloat;
        transportedFlow:T_2dVector;

        fMax:double=1E50;
        aLocal:T_2dVector;

    begin
      dtEff:=dtRest;
      while (fMax*dtEff/GRID_SIZE>MAX_COURANT_NUMBER) do begin
        fMax:=0;
        //Calculate effective flow from ((p[i+1]+p[i])/(m[i+1]+m[i])+a[i]*dtRest)*dtRest
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
          vNew[i,j,0]:=((value[i,j].p[0]+value[(i+1) and mask,j].p[0])/
                (epsilon+value[i,j].mass+value[(i+1) and mask,j].mass)+accel[i,j,0]*dtEff);
          vNew[i,j,1]:=((value[i,j].p[1]+value[i,(j+1) and mask].p[1])/
                (epsilon+value[i,j].mass+value[i,(j+1) and mask].mass)+accel[i,j,1]*dtEff);

          f:=abs(vNew[i,j,0]); if f>fMax then fMax:=f;
          f:=abs(vNew[i,j,1]); if f>fMax then fMax:=f;
        end;
        //Reduce time step if neccessary
        if (fMax*dtEff/GRID_SIZE>MAX_COURANT_NUMBER) then begin
          dtEff:=SAFE_COURANT_NUMBER*GRID_SIZE/fMax;
          if dtEff<DT_MIN then begin
            dtEff:=DT_MIN;
            fMax:=0;
            if not(capping) then log.append('WARNING: Speed limit exceeded. Capping...').appendLineBreak;
            capping:=true;
          end;
          if dtEff>dtRest
          then dtEff:=dtRest
          else dtEff:=dtRest/ceil(dtRest/dtEff);
        end;
      end;

      if capping then for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do
        if mass<UPPER_BLACK_LEVEL then p:=zeroVec
        else begin
          //squared speed:
          f:=(sqr(p[0])+sqr(p[1]))/(epsilon+sqr(mass));
          if f>SPEED_CAP*SPEED_CAP then p*=SPEED_CAP/sqrt(f);
        end;

      //Apply acceleration to cell flows:
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        aLocal[0]:=accel[i,j,0]+accel[(i+mask) and mask,j,0];
        aLocal[1]:=accel[i,j,1]+accel[i,(j+mask) and mask,1];
        value[i,j].p+=aLocal*(value[i,j].mass*0.5*dtEff);
      end;

      //Upwind transport
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with stateDelta[i,j] do begin mass:=0; p:=zeroVec; end;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        f:=vNew[i,j,0]*dtEff/GRID_SIZE;
        if f>0 then begin
          transportedMass:=value[i,j].mass*f;
          transportedFlow:=value[i,j].p   *f;
        end else begin
          transportedMass:=value[(i+1) and mask,j].mass*f;
          transportedFlow:=value[(i+1) and mask,j].p   *f;
        end;
        stateDelta[i             ,j].mass-=transportedMass;
        stateDelta[i             ,j].p   -=transportedFlow;
        stateDelta[(i+1) and mask,j].mass+=transportedMass;
        stateDelta[(i+1) and mask,j].p   +=transportedFlow;

        f:=vNew[i,j,1]*dtEff/GRID_SIZE;
        if f>0 then begin
          transportedMass:=value[i,j].mass*f;
          transportedFlow:=value[i,j].p   *f;
        end else begin
          transportedMass:=value[i,(j+1) and mask].mass*f;
          transportedFlow:=value[i,(j+1) and mask].p   *f;
        end;
        stateDelta[i,j             ].mass-=transportedMass;
        stateDelta[i,j             ].p   -=transportedFlow;
        stateDelta[i,(j+1) and mask].mass+=transportedMass;
        stateDelta[i,(j+1) and mask].p   +=transportedFlow;
      end;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        value[i,j].mass+=stateDelta[i,j].mass;
        value[i,j].p   +=stateDelta[i,j].p;
        if (value[i,j].mass<=0) or isNan(value[i,j].mass) or isInfinite(value[i,j].mass) then begin
          value[i,j].mass:=epsilon;
          value[i,j].p   :=zeroVec;
        end;
      end;
      //decrement remaining time to take
      dtRest-=dtEff;
    end;

  VAR subStepsTaken:longint=0;
      dtRest, dtEff:TmyFloat;
      i,j:longint;
      m:double=0;
      start:double;

  begin
    start:=now;
    ensureAttractionFactors(timeStepIndex);
    result:=false;

    dtRest:=dt;
    while dtRest>0 do begin
      setGravAcceleration;
      if DRIFT_TO_CENTER then addDrift;
      addBackgroundAcceleration(timeStepIndex,accel);
      addShallowWaterAcceleration;
      transport(dtRest,dtEff);
      annihilateOrRegrow(dtEff);
      inc(subStepsTaken)
    end;

    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
    m*=GRID_SIZE*GRID_SIZE;
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
    result^.mass*=GRID_SIZE*GRID_SIZE;
  end;

FUNCTION T_cellSystem.getSerialVersion: dword;
  begin
    result:=31356+SYS_SIZE;
  end;

FUNCTION T_cellSystem.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not(inherited) then exit(false);
    stream.read(value,sizeOf(value));
    result:=stream.allOkay;
  end;

PROCEDURE T_cellSystem.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.write(value,sizeOf(value));
  end;

end.

