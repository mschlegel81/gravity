UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil,vectors,commandLineHandling;
TYPE
  T_cellSystem=object(T_serializable)
    private
      value:T_systemState;
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
        n,i,j:longint;
    begin
      distance:=x*x+y*y;
      if      distance<=sqr( 2) then n:=4
      else if distance<=sqr( 4) then n:=3
      else if distance<=sqr( 8) then n:=2
      else if distance<=sqr(16) then n:=1
      else                           n:=0;

      result:=zeroVec;
      if (x<>0) or (y<>0) then for i:=0 to n do for j:=0 to n do
        result+=straightAttraction(x+GAUSS_LEGENDRE_WEIGHT[n,i].d,
                                   y+GAUSS_LEGENDRE_WEIGHT[n,j].d)
                                 *(  GAUSS_LEGENDRE_WEIGHT[n,i].w
                                    *GAUSS_LEGENDRE_WEIGHT[n,j].w);
      if distance>SYS_SIZE*SYS_SIZE then result*=exp(-0.5*(distance*(1/SYS_SIZE*SYS_SIZE)-1));
    end;

  VAR ix,iy:longint;
      symX,symY:longint;
  begin
    if reinitializeAttractionFactors(stepIndex) or not(attractionInitialized) then begin
      log.append('(Re)initializing attraction factors').appendLineBreak;

      for ix:=-SYS_SIZE+1 to SYS_SIZE-1 do for iy:=-SYS_SIZE+1 to SYS_SIZE-1 do begin
        //Due to symmetry considerations, only half of the entries have to be calculated
        if (ix>0) or (ix=0) and (iy>0)
        then cachedAttraction[ix,iy]:=cachedAttraction[-ix,-iy]*-1
        else begin
          cachedAttraction[ix,iy]:=zeroVec;
          for symX:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
          for symY:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
            cachedAttraction[ix,iy]+=calculateAttraction(ix+symX*SYS_SIZE,iy+symY*SYS_SIZE);
        end;
      end;
    end;
    if not(attractionInitialized) then begin
      for ix:=0 to SYS_SIZE-1 do begin
        box[ix]:=[];
        for iy:=ix-ATTRACTION_RANGE+SYS_SIZE to ix+ATTRACTION_RANGE+SYS_SIZE do include(box[ix],byte(iy mod SYS_SIZE));
      end;
      for ix:=0 to SYS_SIZE-1 do sinus_table[ix]:=sin(ix*2*pi/SYS_SIZE);
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
  VAR newState:T_systemState;
      cellCenteredAcceleration:T_vectorField;
      masses:array[0..SYS_SIZE-1,0..SYS_SIZE-1] of TmyFloat;

  PROCEDURE setGravAcceleration;
    VAR i,j,oi,oj:longint;
        a: T_2dVector;
    begin
      if LIMITED_RANGE_ATTRACTION then begin
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
          a:=zeroVec;
          for oi in box[i] do for oj in box[j] do
            a+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
          cellCenteredAcceleration[i,j]:=a;
        end;
      end else begin
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do masses[i,j]:=value[i,j].mass;
        for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
          a:=zeroVec;
          for oi:=0 to SYS_SIZE-1 do for oj:=0 to SYS_SIZE-1 do
            //a+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
            a+=cachedAttraction[oi-i,oj-j]*masses[oi,oj];
          cellCenteredAcceleration[i,j]:=a;
        end;
      end;
    end;

  PROCEDURE addPressureAccelerationAndDrift;
    CONST mask=SYS_SIZE-1;
          WEIGHT:array[-2..2,-2..2] of T_2dVector=(
            (( 0.08658977081756744,0.08658977081756744),( 0.2693962929060643,0.13469814645303216),( 0.4065696597405991,0.0),( 0.2693962929060643,-0.13469814645303216),( 0.08658977081756744,-0.08658977081756744)),
            (( 0.13469814645303216,0.2693962929060643 ),( 0.523837587470595 ,0.523837587470595  ),( 1.0               ,0.0),( 0.523837587470595 ,-0.523837587470595  ),( 0.13469814645303216,-0.2693962929060643)),
            (( 0.0                ,0.4065696597405991 ),( 0.0               ,1.0                ),( 0.0               ,0.0),( 0.0               ,-1.0                ),( 0.0                ,-0.4065696597405991)),
            ((-0.13469814645303216,0.2693962929060643 ),(-0.523837587470595 ,0.523837587470595  ),(-1.0               ,0.0),(-0.523837587470595 ,-0.523837587470595  ),(-0.13469814645303216,-0.2693962929060643)),
            ((-0.08658977081756744,0.08658977081756744),(-0.2693962929060643,0.13469814645303216),(-0.4065696597405991,0.0),(-0.2693962929060643,-0.13469814645303216),(-0.08658977081756744,-0.08658977081756744)));
{MNH-source for weight:
        [-2..2].each(x,
        [-2..2].each(y,begin
          local dir:=[x,y]; x=y=0 ? void : dir/=-dir.euklideanNorm;
          dir*exp(-0.3*(sqr(x)+sqr(y)));
        end))
        ./(exp(-0.3))
        .toString
        .replace('[','(')
        .replace(']',')');
}
    VAR local:array[-2..2,-2..2] of double;
        a:T_2dVector;
        i_:array[-2..2] of longint;
        i,j,ip,jp:longint;

    VAR pTot:T_2dVector=(0,0);
        mTot:TmyFloat=0;
        deltaV:T_2dVector=(0,0);

    FUNCTION cap(CONST n:double):double; inline;
      begin
        if n<REPULSION_THRESHOLD
        then result:=0
        else begin
          result:=n-REPULSION_THRESHOLD;
          result:=result*(REPULSION_LINEAR+result*REPULSION_QUADRATIC);
        end;
      end;

    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        pTot+=p;
        mTot+=mass;
        deltaV[0]+=mass*sinus_table[i];
        deltaV[1]+=mass*sinus_table[j];
      end;
      if DRIFT_TO_CENTER
      then deltaV:=(deltaV-pTot*0.5)*( 20/mTot)
      else deltaV:=        pTot*(-      1/mTot);

      if (REPULSION_LINEAR=0) AND (REPULSION_QUADRATIC=0)
      then for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do cellCenteredAcceleration[i,j]+=deltaV
      else begin
        for i:=0 to SYS_SIZE-1 do begin
          i_[-2]:=(i+mask+mask) and mask;
          i_[-1]:=(i+mask     ) and mask;
          i_[ 0]:= i                    ;
          i_[ 1]:=(i+1        ) and mask;
          i_[ 2]:=(i+2        ) and mask;
          for j:=0 to SYS_SIZE-1 do begin
            if j=0 then begin
              local[-2,-2]:=cap(value[i_[-2],(j+mask+mask) and mask].mass);
              local[-2,-1]:=cap(value[i_[-2],(j+mask     ) and mask].mass);
              local[-2, 0]:=cap(value[i_[-2], j                    ].mass);
              local[-2, 1]:=cap(value[i_[-2],(j+1)         and mask].mass);
              local[-2, 2]:=cap(value[i_[-2],(j+2)         and mask].mass);

              local[-1,-2]:=cap(value[i_[-1],(j+mask+mask) and mask].mass);
              local[-1,-1]:=cap(value[i_[-1],(j+mask     ) and mask].mass);
              local[-1, 0]:=cap(value[i_[-1], j                    ].mass);
              local[-1, 1]:=cap(value[i_[-1],(j+1)         and mask].mass);
              local[-1, 2]:=cap(value[i_[-1],(j+2)         and mask].mass);

              local[ 0,-2]:=cap(value[i     ,(j+mask+mask) and mask].mass);
              local[ 0,-1]:=cap(value[i     ,(j+mask     ) and mask].mass);
              local[ 0, 0]:=cap(value[i     , j                    ].mass);
              local[ 0, 1]:=cap(value[i     ,(j+1)         and mask].mass);
              local[ 0, 2]:=cap(value[i     ,(j+2)         and mask].mass);

              local[ 1,-2]:=cap(value[i_[ 1],(j+mask+mask) and mask].mass);
              local[ 1,-1]:=cap(value[i_[ 1],(j+mask     ) and mask].mass);
              local[ 1, 0]:=cap(value[i_[ 1], j                    ].mass);
              local[ 1, 1]:=cap(value[i_[ 1],(j+1)         and mask].mass);
              local[ 1, 2]:=cap(value[i_[ 1],(j+2)         and mask].mass);

              local[ 2,-2]:=cap(value[i_[ 2],(j+mask+mask) and mask].mass);
              local[ 2,-1]:=cap(value[i_[ 2],(j+mask     ) and mask].mass);
              local[ 2, 0]:=cap(value[i_[ 2], j                    ].mass);
              local[ 2, 1]:=cap(value[i_[ 2],(j+1)         and mask].mass);
              local[ 2, 2]:=cap(value[i_[ 2],(j+2)         and mask].mass);
            end else begin
              local[-2,-2]:=local[-2,-1];
              local[-2,-1]:=local[-2, 0];
              local[-2, 0]:=local[-2, 1];
              local[-2, 1]:=local[-2, 2];
              local[-2, 2]:=cap(value[i_[-2],(j+2)         and mask].mass);

              local[-1,-2]:=local[-1,-1];
              local[-1,-1]:=local[-1, 0];
              local[-1, 0]:=local[-1, 1];
              local[-1, 1]:=local[-1, 2];
              local[-1, 2]:=cap(value[i_[-1],(j+2)         and mask].mass);

              local[ 0,-2]:=local[ 0,-1];
              local[ 0,-1]:=local[ 0, 0];
              local[ 0, 0]:=local[ 0, 1];
              local[ 0, 1]:=local[ 0, 2];
              local[ 0, 2]:=cap(value[i     ,(j+2)         and mask].mass);

              local[ 1,-2]:=local[ 1,-1];
              local[ 1,-1]:=local[ 1, 0];
              local[ 1, 0]:=local[ 1, 1];
              local[ 1, 1]:=local[ 1, 2];
              local[ 1, 2]:=cap(value[i_[ 1],(j+2)         and mask].mass);

              local[ 2,-2]:=local[ 2,-1];
              local[ 2,-1]:=local[ 2, 0];
              local[ 2, 0]:=local[ 2, 1];
              local[ 2, 1]:=local[ 2, 2];
              local[ 2, 2]:=cap(value[i_[ 2],(j+2)         and mask].mass);
            end;
            a:=deltaV;
            for ip:=-2 to 2 do for jp:=-2 to 2 do a+=WEIGHT[ip,jp]*local[ip,jp];
            cellCenteredAcceleration[i,j]+=a;
          end;
        end;
      end;
    end;

  VAR capping:boolean=false;
  PROCEDURE transport(VAR dtRest:TmyFloat; OUT dtEff:TmyFloat);
    CONST mask=SYS_SIZE-1;
          DT_MIN=dt/200; //not more than 200 sub steps
          MAX_TRANSPORT_RANGE =5*dt;
          SPEED_CAP=0.1*MAX_TRANSPORT_RANGE/DT_MIN;

    VAR i,j,ti,tj:longint;
        x0,x1,y0,y1,
        wx,wy:double;
        v:T_2dVector;
        f:double;
    begin
      dtEff:=dtRest;
      // |dtEff * v| <= MAX_TRANSPORT_RANGE
      // |dtEff * (dv/dx)| <= MAX_DIVERGENCE_RANGE
      // (dv/dx) =
      // abs(ax[i-1/2]-ax[i+1/2])*dtEff*dtEff <= MAX_DIVERGENCE_RANGE

      //Step size control...
      wx:=epsilon;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do if mass>UPPER_C1_LEVEL then begin
        f:=(p[0]*p[0]+p[1]*p[1])/(mass*mass);
        if f>wx then wx:=f;
      end;
      wx:=sqrt(wx);  //wx= vMax
      dtEff:=MAX_TRANSPORT_RANGE/wx;

      if dtEff<DT_MIN then begin
        dtEff:=DT_MIN;
        if not(capping) then log.append('WARNING: Speed limit exceeded. Capping...').appendLineBreak;
        capping:=true;
      end;
      if dtEff>dtRest
      then dtEff:=dtRest
      else dtEff:=dtRest/ceil(dtRest/dtEff);


      //Lagrangian transport:
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with newState[i,j] do begin mass:=0; p:=zeroVec; end;
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        with value[i,j] do begin
          v:=p*(1/(epsilon+mass));
          if capping and (mass>=UPPER_BLACK_LEVEL) then begin //cap speed
            //squared speed:
            f:=(sqr(p[0])+sqr(p[1]))/(epsilon+sqr(mass));
            if f>SPEED_CAP*SPEED_CAP then p*=SPEED_CAP/sqrt(f);
          end;

          if (ANNIHILATION_FACTOR>0) and (mass>ANNIHILATION_THRESHOLD)
          then mass-=mass*(mass-ANNIHILATION_THRESHOLD)*ANNIHILATION_FACTOR*dtEff;
          mass+=REGROWTH_FACTOR*dtEff;
        end;

        v +=cellCenteredAcceleration[i,j]*dtEff*0.5;

        f:=(DIFFUSION_BASE+DIFFUSION_BY_VELOCITY*sqrt(sqr(v[0])+sqr(v[1])))*dt;
        if f>2 then f:=2 else if f<0 then f:=0;

        x0:=i+v[0]*dtEff; x1:=x0+1+f; x0-=f;
        y0:=j+v[1]*dtEff; y1:=y0+1+f; y0-=f;

        v +=cellCenteredAcceleration[i,j]*dtEff*0.5;

        value[i,j].mass*=1/((x1-x0)*(y1-y0));
        value[i,j].p:=v*value[i,j].mass;

        for ti:=floor(x0) to floor(x1+1) do begin
          //intersection of intervals [ti,ti+1] and [x0,x1]
          // = [max(ti,x0),min(x1,ti+1)] -> weight =
          wx:=min(x1,ti+1)-max(x0,ti);
          if wx>0 then for tj:=floor(y0) to floor(y1+1) do begin
            wy:=min(y1,tj+1)-max(y0,tj);
            if wy>0 then with newState[ti and mask,tj and mask] do begin
              mass+=value[i,j].mass*(wx*wy);
              p   +=value[i,j].p   *(wx*wy);
            end;
          end;
        end;
      end;
      value:=newState;

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
      addBackgroundAcceleration(timeStepIndex,cellCenteredAcceleration);
      addPressureAccelerationAndDrift;
      transport(dtRest,dtEff);
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
    result:=31357+SYS_SIZE;
  end;

FUNCTION T_cellSystem.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not(inherited) then exit(false);
    stream.read(value,sizeOf(value));
    numberOfFrames:=stream.readWord;
    result:=stream.allOkay;
  end;

PROCEDURE T_cellSystem.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.write(value,sizeOf(value));
    stream.writeWord(numberOfFrames);
  end;

end.

