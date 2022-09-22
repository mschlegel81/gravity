UNIT simplerPhysics;

{$mode objfpc}{$H+}

INTERFACE
USES basicGraphics,serializationUtil;

CONST
  SYMMETRIC_CONTINUATION=1;
  dt             =0.05;
  GRID_SIZE      =1;
  MAX_ACCELERATION_RANGE=GRID_SIZE*0.5;

  PARAM_RESTART='restart';
  PARAM_REPLAY='replay';
  PARAM_CLOSE='close';
  PARAM_LOW_DENSITY='ld';
  PARAM_HIGH_DENSITY='hd';

TYPE
  TmyFloat=double;

  T_2dVector=array[0..1] of TmyFloat;
  T_value=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of record mass:TmyFloat; p:T_2dVector; end;

  { T_cellSystem }

  T_cellSystem=object(T_serializable)
    private
      nextValue,
      value:T_value;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      FUNCTION doMacroTimeStep(CONST index:longint):boolean;
      FUNCTION getPicture(CONST displayWidth,displayHeight:longint):P_rgbPicture;
      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

CONST
  zeroVec:T_2dVector=(0,0);

FUNCTION fileName_anim:string;
FUNCTION fileName_dump:string;
FUNCTION filename_txt:string;
FUNCTION hasCmdLineParameter(CONST s:string):boolean;
VAR logHandle:textFile;
IMPLEMENTATION
USES sysutils;
VAR cachedAttraction:array [-SYS_SIZE+1..SYS_SIZE-1,-SYS_SIZE+1..SYS_SIZE-1] of T_2dVector;
    attractionInitialized:boolean=false;

FUNCTION hasCmdLineParameter(CONST s:string):boolean;
  VAR i:longint;
  begin
    for i:=1 to paramCount do if paramStr(i)=s then exit(true);
    result:=false;
  end;

FUNCTION fileName_anim:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.anim');
  end;

FUNCTION fileName_dump:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.dump');
  end;

FUNCTION filename_txt:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.txt');
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
    box:array[0..SYS_SIZE-1] of set of byte;
PROCEDURE ensureAttractionFactors;
  FUNCTION straightAttraction(CONST rx,ry:TmyFloat):T_2dVector;
    VAR f:double;
    begin
      f:=(sqr(rx)+sqr(ry));
      if f<400 then f:=1/100
               else f:=0;
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
      if      distance<=sqr( 2) then n:=4
      else if distance<=sqr( 4) then n:=3
      else if distance<=sqr( 8) then n:=2
      else if distance<=sqr(16) then n:=1
      else                           n:=0;

      result:=zeroVec;
      for i:=0 to n do for j:=0 to n do
        result+=straightAttraction(x+GAUSS_LEGENDRE_WEIGHT[n,i].d,
                                   y+GAUSS_LEGENDRE_WEIGHT[n,j].d)*
                                    (GAUSS_LEGENDRE_WEIGHT[n,i].w*
                                     GAUSS_LEGENDRE_WEIGHT[n,j].w);

      if distance>SYS_SIZE*SYS_SIZE then result*=exp(-0.5*(distance*(1/SYS_SIZE*SYS_SIZE)-1));
    end;

  VAR ix,iy:longint;
      symX,symY:longint;
  begin
    if not(attractionInitialized) then begin
      append(logHandle);
	  writeln(logHandle,'Initializing gravity factors');
	  close(logHandle);
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
      for ix:=0 to SYS_SIZE-1 do sinus_table[ix]:=sin(ix*2*pi/SYS_SIZE);
      for ix:=0 to SYS_SIZE-1 do begin
        box[ix]:=[];
        for iy:=ix-20+SYS_SIZE to ix+20+SYS_SIZE do include(box[ix],byte(iy mod SYS_SIZE));
      end;
      attractionInitialized:=true;
    end;
  end;

{ T_cellSystem }
CONSTRUCTOR T_cellSystem.create;
  VAR i,j:longint;
      massFactor:double;
  begin
    assign(logHandle,ChangeFileExt(paramStr(0),'.log'));
    rewrite(logHandle);
    close(logHandle);

    if      hasCmdLineParameter(PARAM_LOW_DENSITY)  then massFactor:= 0
    else if hasCmdLineParameter(PARAM_HIGH_DENSITY) then massFactor:= 1
    else                                                 massFactor:=10;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
      mass:=massFactor+0.001*random;
      //mass:=0;
      p:=zeroVec;
    end;
    //i:=SYS_SIZE div 2; value[i,i].mass:=100;
  end;

DESTRUCTOR T_cellSystem.destroy;
  begin
  end;

FUNCTION T_cellSystem.doMacroTimeStep(CONST index:longint): boolean;
  VAR accel:array[0..SYS_SIZE-1,0..SYS_SIZE-1] of T_2dVector;

  PROCEDURE resetAcceleration;
    VAR i,j:longint;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do
        accel[i,j]:=zeroVec;
    end;

  PROCEDURE addGravAcceleration;
    VAR i,j,oi,oj:longint;
        a: T_2dVector;
    begin
      for i:=0 to SYS_SIZE-1 do
      for j:=0 to SYS_SIZE-1 do begin
        a:=accel[i,j];
        for oi in box[i] do
        for oj in box[j] do
          a+=cachedAttraction[oi-i,oj-j]*value[oi,oj].mass;
        accel[i,j]:=a;
      end;
    end;

  PROCEDURE annihilate(CONST dtEff:TmyFloat);
    CONST MASS_DIFFUSED=1E-2;
          MASS_LOST    =1E-2;
          threshold    =5;
          dv:array[-1..1,-1..1] of T_2dVector=(((-7.071, -7.071),(-10,0),(-7.071, 7.071)),
                                               (( 0.0  ,-10    ),(  0,0),(     0,10    )),
                                               (( 7.071, -7.071),( 10,0),( 7.071, 7.071)));
          BLOW:array[-1..1,-1..1] of TmyFloat=
          ((0.089812, 0.160187,0.089812),
           (0.160187,-0.999996,0.160187),
           (0.089812, 0.160187,0.089812));
    VAR i,j:longint;
        factor, massDiffusion, m_:TmyFloat;
        di,dj:longint;
        v0, v_:T_2dVector;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>threshold then begin
        with value[i,j] do begin
          v0  :=p*(1/mass);
          factor:=dtEff*(mass-threshold);
          massDiffusion:=mass*factor*MASS_DIFFUSED/GRID_SIZE;
          factor*=MASS_LOST;

          mass*=(1-factor);
          p   *=(1-factor);
          factor:=mass*0.5;
          if massDiffusion>factor then massDiffusion:=factor;
        end;

        //Blowout:
        if massDiffusion>0 then
        for di:=-1 to 1 do for dj:=-1 to 1 do with nextValue[(i+di+SYS_SIZE) mod SYS_SIZE,(j+dj+SYS_SIZE) mod SYS_SIZE] do begin
          v_:=v0+dv[di,dj];
          m_:=massDiffusion*BLOW[di,dj];
          mass+=m_;
          p   +=v_*m_;
        end;
      end else with value[i,j] do if mass<1 then begin
        v0:=p*(1/(mass+1E-10));
        mass+=5E-3*dtEff;
        p:=v0*(mass+1E-10);
      end;
    end;

  PROCEDURE modifyVelocities;
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
      deltaV:=(deltaV-pTot*0.5)*(0.1/mTot);
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do
        p:=(p*(1/(mass+1E-10))+deltaV)*(mass+1E-10);
    end;

  FUNCTION getSubStepsToTake:longint;
    VAR i,j:longint;
        X:TmyFloat=0;
        vMax:TmyFloat=0;
        N:TmyFloat;
    begin
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do if value[i,j].mass>1E-2 then begin
        N:=sqr(accel[i,j,0])+sqr(accel[i,j,1]);
        if N>X then X:=N;
        N:=(sqr(value[i,j].p[0])+sqr(value[i,j].p[1]))/sqr(value[i,j].mass);
        if N>vMax then vMax:=N;
      end;
      result:=trunc(sqrt(sqrt(X)/MAX_ACCELERATION_RANGE)*dt+1);
      i     :=trunc(sqrt(vMax)*dt*0.5+1);
      if result<i then result:=i;
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
    ensureAttractionFactors();
    start:=now;
    result:=false;
    modifyVelocities;

    resetAcceleration;
    addGravAcceleration;
    subStepsToTake:=getSubStepsToTake;
    dtSub:=dt/subStepsToTake;

    for sub:=1 to subStepsToTake do begin
      if sub>1 then begin
        resetAcceleration;
        addGravAcceleration;
      end;
      nextValue:=zeroSystem;

      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do begin
        m:=value[i,j].mass;
        v:=value[i,j].p*(1/(1E-10+m))+accel[i,j]*dtSub;

        x[0]:=i+v[0]*dtSub/GRID_SIZE; while x[0]<0 do x[0]+=SYS_SIZE;
        x[1]:=j+v[1]*dtSub/GRID_SIZE; while x[1]<0 do x[1]+=SYS_SIZE;
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

      nextValue:=zeroSystem;
      annihilate(dtSub);
      for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
        mass+=nextValue[i,j].mass;
        p   +=nextValue[i,j].p;
      end;
    end;
    m:=0;
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do m+=value[i,j].mass;
    m*=GRID_SIZE*GRID_SIZE;
    append(logHandle);
    writeln(logHandle,'Step ',index,' done: ',(now-start)*24*60*60:0:5,'s; ',subStepsToTake,' sub steps; mass=',m:0:6);
    close(logHandle);
  end;

FUNCTION T_cellSystem.getPicture(CONST displayWidth, displayHeight: longint): P_rgbPicture;
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
      totalMass:double=0;
  begin
    new(result,create(SYS_SIZE,SYS_SIZE));
    for i:=0 to SYS_SIZE-1 do for j:=0 to SYS_SIZE-1 do with value[i,j] do begin
      result^.pixel[i,j]:=colorOf(mass);
      totalMass+=mass;
    end;
    result^.mass:=totalMass*GRID_SIZE*GRID_SIZE;
  end;

FUNCTION T_cellSystem.getSerialVersion: dword;
  begin
    result:=31+SYS_SIZE;
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

