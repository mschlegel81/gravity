UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,Classes;

CONST
  SYMMETRIC_CONTINUATION=4096 div SYS_SIZE;
  dt             =0.05;
  GRID_SIZE      =1;
  MAX_ACCELERATION_RANGE=GRID_SIZE*0.5;

TYPE
  TmyFloat=double;

  T_2dVector=array[0..1] of TmyFloat;
  T_value=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of record mass:TmyFloat; p:T_2dVector; end;

  T_cellSystem=object
    private
      nextValue,
      value:T_value;
      replaying:boolean;
      handle:TFileStream;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      FUNCTION doMacroTimeStep:boolean;
      FUNCTION getPicture(CONST displayWidth,displayHeight:longint):P_rgbPicture;
  end;

CONST
  zeroVec:T_2dVector=(0,0);

FUNCTION filename_txt:string;
IMPLEMENTATION
USES sysutils;
VAR cachedAttraction:array [-SYS_SIZE+1..SYS_SIZE-1,-SYS_SIZE+1..SYS_SIZE-1] of T_2dVector;
    attractionInitialized:boolean=false;

FUNCTION filename:string;
  begin
    result:='grav'+intToStr(SYS_SIZE)+'.history';
  end;

FUNCTION filename_txt:string;
  begin
    result:='grav'+intToStr(SYS_SIZE)+'.txt';
  end;

OPERATOR *(CONST x:T_2dVector; CONST y:TmyFloat):T_2dVector;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR +(CONST x,y:T_2dVector):T_2dVector;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR -(CONST x,y:T_2dVector):T_2dVector;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

VAR zeroSystem:T_value;
    sinus_table:array[0..SYS_SIZE-1] of TmyFloat;
PROCEDURE ensureAttractionFactors;
  FUNCTION straightAttraction(CONST rx,ry:TmyFloat):T_2dVector;
    VAR f:double;
    begin
      f:=sqrt(sqr(rx)+sqr(ry));
      f:=1/(f*f*f*GRID_SIZE*GRID_SIZE);
      result[0]:=rx*f;
      result[1]:=ry*f;
    end;

  FUNCTION calculateAttraction(CONST x,y:longint):T_2dVector;
    CONST GAUSS_LEGENDRE_WEIGHT:array[0..4,0..4] of record d,w:TmyFloat; end
    =(((d: 0.0                ; w:1.0                ),(d: 0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.28867513459481287; w:0.5                ),(d: 0.28867513459481287; w:0.5                ),(d:0.0                ; w:0.0                ),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.3872983346207417 ; w:0.27777777777777779),(d: 0.0                ; w:0.4444444444444444 ),(d:0.3872983346207417 ; w:0.27777777777777779),(d:0.0                ; w:0.0                ),(d:0.0              ; w:0.0                )),
      ((d:-0.43056815579702629; w:0.17392742256872692),(d:-0.16999052179242816; w:0.3260725774312731 ),(d:0.16999052179242816; w:0.3260725774312731 ),(d:0.43056815579702629; w:0.17392742256872692),(d:0.0              ; w:0.0                )),
      ((d:-0.453089922969332  ; w:0.11846344252809454),(d:-0.26923465505284155; w:0.23931433524968324),(d:0.0                ; w:0.28444444444444444),(d:0.26923465505284155; w:0.23931433524968324),(d:0.453089922969332; w:0.11846344252809454)));

    VAR distance:longint;
        n,i,j:longint;
    begin
      distance:=x*x+y*y;
      if      distance<sqr( 5) then n:=4
      else if distance<sqr(10) then n:=3
      else if distance<sqr(20) then n:=2
      else if distance<sqr(40) then n:=1
      else                          n:=0;

      result:=zeroVec;
      for i:=0 to n do for j:=0 to n do
        result+=straightAttraction(x+GAUSS_LEGENDRE_WEIGHT[n,i].d,
                                  y+GAUSS_LEGENDRE_WEIGHT[n,j].d)*
                                   (GAUSS_LEGENDRE_WEIGHT[n,i].w*
                                    GAUSS_LEGENDRE_WEIGHT[n,j].w);
    end;

  VAR ix,iy:longint;
      symX,symY:longint;
  begin
    if not(attractionInitialized) then begin
      writeln('Initializing gravity factors');
      for ix:=-SYS_SIZE+1 to SYS_SIZE-1 do for iy:=-SYS_SIZE+1 to SYS_SIZE-1 do begin
        if (iy>0) then begin
          cachedAttraction[ix,iy]:=cachedAttraction[ix,-iy];
          cachedAttraction[ix,iy,1]:=-cachedAttraction[ix,iy,1];
        end else if (ix>0) then begin
          cachedAttraction[ix,iy]:=cachedAttraction[-ix,iy];
          cachedAttraction[ix,iy,0]:=-cachedAttraction[ix,iy,0];
        end else begin
          cachedAttraction[ix][iy]:=zeroVec;
          if (ix<>0) or (iy<>0) then
          for symX:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
          for symY:=-SYMMETRIC_CONTINUATION to SYMMETRIC_CONTINUATION do
            cachedAttraction[ix][iy]+=calculateAttraction(ix+symX*SYS_SIZE,iy+symY*SYS_SIZE);
        end;
      end;
      for ix:=0 to SYS_SIZE-1 do for iy:=0 to SYS_SIZE-1 do with zeroSystem[ix,iy] do begin
        mass:=0;
        p:=zeroVec;
      end;
      for ix:=0 to SYS_SIZE-1 do sinus_table[ix]:=sin(ix*2*pi/(SYS_SIZE-1));
      attractionInitialized:=true;
    end;
  end;

{ T_cellSystem }
CONSTRUCTOR T_cellSystem.create;
  VAR i,j:longint;
      p:T_2dVector;
  begin
    if fileExists(fileName) and not((ParamCount>=1) and (ParamStr(1)='restart')) then begin
      handle:=TFileStream.create(fileName,fmOpenReadWrite);
      handle.Seek(0,soBeginning);
      replaying:=true;
    end else begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        value[i,j].mass:=0.1*(0.95+0.05*random);
        repeat
          p[0]:=1-2*random;
          p[1]:=1-2*random;
        until p[0]*p[0]+p[1]*p[1]<1;
        value[i,j].p:=p*value[i,j].mass;
      end;

      replaying:=false;
      handle:=TFileStream.create(fileName,fmCreate);
      handle.Seek(0,soBeginning);
      handle.write(value,sizeOf(value));
    end;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
    handle.destroy;
  end;


FUNCTION T_cellSystem.doMacroTimeStep:boolean;
  VAR accel:array[0..SYS_SIZE-1,0..SYS_SIZE-1] of T_2dVector;
      timestepIndex: Int64;

  PROCEDURE resetAcceleration;
    VAR i,j:longint;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        //gradient[0]:=(value[(i+         1) mod SYS_SIZE,j].mass)-
        //             (value[(i+SYS_SIZE-1) mod SYS_SIZE,j].mass);
        //gradient[1]:=(value[i,(j+         1) mod SYS_SIZE].mass)-
        //             (value[i,(j+SYS_SIZE-1) mod SYS_SIZE].mass);
        //accel[i,j]:=gradient*(-0.5);
        accel[i,j]:=zeroVec;
      end;
    end;

  PROCEDURE addGravAcceleration;
    VAR i,j,oi,oj:longint;
    begin
      for i:=0 to SYS_SIZE-1 do
      for oi:=0 to SYS_SIZE-1 do
      for j:=0 to SYS_SIZE-1 do
      for oj:=0 to SYS_SIZE-1 do
        accel[i,j]+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
    end;

  PROCEDURE annihilate(CONST dtEff:TmyFloat);
    CONST DV:array[-1..1,-1..1] of T_2dVector=(((-7.071, -7.071),(-10,0),(-7.071, 7.071)),
                                               (( 0.0  ,-10    ),(  0,0),(     0,10    )),
                                               (( 7.071, -7.071),( 10,0),( 7.071, 7.071)));
          BLOW:array[-1..1,-1..1] of TmyFloat=((0.03850777999707,0.06868155276104,0.03850777999707),
                                               (0.06868155276104,0.0             ,0.06868155276104),
                                               (0.03850777999707,0.06868155276104,0.03850777999707));
    VAR i,j:longint;
        factor, massLoss, m_:TmyFloat;
        di,dj:longint;
        v0, v_:T_2dVector;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>5 then begin
        with value[i,j] do begin
          factor:=dtEff*5E-3*(mass-5);
          massLoss:=mass*factor;
          v0  :=p*(1/mass);
          mass*=(1-factor);
          p   *=(1-factor);
        end;

        //Blowout:
        for di:=-1 to 1 do for dj:=-1 to 1 do with nextValue[(i+di+SYS_SIZE) mod SYS_SIZE,(j+dj+SYS_SIZE) mod SYS_SIZE] do begin
          v_:=v0+DV[di,dj];
          m_:=massLoss*BLOW[di,dj];

          mass+=m_;
          p   +=v_*m_;
        end;
      end;
    end;


  PROCEDURE modifyVelocities;
    VAR pTot:T_2dVector=(0,0);
        mTot:TmyFloat=0;
        deltaV:T_2dVector=(0,0);
        i, j: Integer;
    begin

      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        pTot+=p;
        mTot+=mass;
        deltaV[0]+=mass*sinus_table[i];
        deltaV[1]+=mass*sinus_table[j];
      end;
      deltaV:=(deltaV-pTot*0.5)*(0.1/mTot);
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do
        p:=(p*(1/(mass+1E-10))+deltaV)*(mass+1E-10);
    end;

  FUNCTION getSubStepsToTake:longint;
    VAR i,j:longint;
        X:TmyFloat=0;
        N:TmyFloat;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>1E-2 then begin
        N:=sqr(accel[i,j,0])+sqr(accel[i,j,1]);
        if N>X then X:=N;
      end;
      result:=trunc(sqrt(sqrt(X)/MAX_ACCELERATION_RANGE)*dt+1);
      if result<1 then result:=1
      else if result>100 then result:=100;
    end;

  VAR subStepsToTake:longint;
      dtSub:TmyFloat;
      sub,i,j,ti,tj,tj_:longint;

      m,w:double;
      v,x:T_2dVector;
      start:double;

  begin
    start:=now;
    timestepIndex:=handle.position div sizeOf(value);
    if replaying then begin
      j:=handle.position;
      i:=handle.read(nextValue,sizeOf(value));
      if i<>sizeOf(value) then begin
        writeln('Tried to read ',sizeOf(value),' bytes @',handle.Position,' but read ',i);
        replaying:=false;
      end else begin
        value:=nextValue;
        m:=0;
        for ti:=0 to SYS_SIZE-1 do for tj:=0 to SYS_SIZE-1 do m+=value[ti,tj].mass;
        writeln('Replaying ',timestepIndex,' @',handle.position,'; mass=',m:0:6);
      end;
      handle.Seek(j+i,soBeginning);
      exit(true);
    end;
    result:=false;
    ensureAttractionFactors();

    modifyVelocities;

    resetAcceleration;
    addGravAcceleration;
    subStepsToTake:=getSubStepsToTake;
    dtSub:=dt/subStepsToTake;
    nextValue:=zeroSystem;
    annihilate(dtSub);

    for sub:=1 to subStepsToTake do begin
      if sub>1 then begin
        resetAcceleration;
        addGravAcceleration;
        nextValue:=zeroSystem;
        annihilate(dtSub);
      end;

      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        m:=value[i,j].mass;
        v:=value[i,j].p*(1/(1E-10+m))+accel[i,j]*dtSub;

        x[0]:=i+v[0]*dtSub; while x[0]<0 do x[0]+=SYS_SIZE;
        x[1]:=j+v[1]*dtSub; while x[1]<0 do x[1]+=SYS_SIZE;
        ti:=trunc(x[0]); x[0]-=ti; ti:=ti mod SYS_SIZE;
        tj:=trunc(x[1]); x[1]-=tj; tj:=tj mod SYS_SIZE;
                              tj_:=(tj+1) mod SYS_SIZE;
        v*=(1E-10+m);

        w:=(1-x[0])*(1-x[1]);
        nextValue[ti,tj].mass+=m*w;
        nextValue[ti,tj].p   +=v*w;

        w:=(1-x[0])*(  x[1]);
        nextValue[ti,tj_].mass+=m*w;
        nextValue[ti,tj_].p   +=v*w;

        ti:=(ti+1) mod SYS_SIZE;
        w:=   x[0] *(1-x[1]);
        nextValue[ti,tj].mass+=m*w;
        nextValue[ti,tj].p   +=v*w;

        w:=   x[0] *(  x[1]);
        nextValue[ti,tj_].mass+=m*w;
        nextValue[ti,tj_].p   +=v*w;
      end;
      value:=nextValue;
    end;
    m:=0;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;


    writeln('Step done: ',(now-start)*24*60*60:0:5,'s; ',subStepsToTake,' sub steps; saving ',handle.position div sizeOf(value),' @',handle.position,'; mass=',m:0:6);
    if not(replaying) then handle.write(value,sizeOf(value));
  end;

FUNCTION T_cellSystem.getPicture(CONST displayWidth,displayHeight:longint): P_rgbPicture;
  FUNCTION colorOf(CONST m:TmyFloat):T_rgbColor;
    FUNCTION validByte(CONST s:single):byte;
      begin
        if s<0 then result:=0
        else if s>1 then result:=255
        else result:=round(255*s);
      end;

    VAR k:double;
    begin
      k:=sqrt(m);
      result[0]:=0;
      result[1]:=0;
      result[2]:=0;

      if k<0.5 then result[2]:=validByte(k) else
      if k<1.5 then begin
        result[0]:=validByte(k-0.5);
        result[2]:=validByte(1-k  );
      end else begin
        result[0]:=validByte(k-0.5);
        result[1]:=validByte(k-1.5);
        result[2]:=validByte(k-2.5);
      end;
    end;

  VAR i,j:longint;
  begin
    new(result,create(SYS_SIZE,SYS_SIZE));
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do
      result^.pixel[i,j]:=colorOf(value[i,j].mass)
  end;

end.

