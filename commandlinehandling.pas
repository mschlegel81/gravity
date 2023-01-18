UNIT commandLineHandling;

{$mode objfpc}{$H+}

INTERFACE
TYPE T_initialDensityEnum=(id_low,id_high,id_ultraHigh);

FUNCTION hasRestartFlag:boolean;
FUNCTION hasReplayFlag:boolean;
FUNCTION hasCloseFlag:boolean;
FUNCTION initialDensityVariant:T_initialDensityEnum;
FUNCTION hasPositionParameter(OUT index:longint):boolean;

FUNCTION fileName_anim:string;
FUNCTION fileName_dump:string;
FUNCTION filename_txt:string;

FUNCTION appTitle:string;

TYPE
  T_log=class
    private
      logHandle:textFile;
      //Private because it is a singleton
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
    public
      FUNCTION append(CONST s:string):T_log;
      FUNCTION append(CONST i:longint):T_log;
      FUNCTION append(CONST f:double; CONST decimalPlaces:longint):T_log;
      FUNCTION appendLineBreak:T_log;
  end;

FUNCTION log:T_log;
IMPLEMENTATION
USES sysutils;
CONST
  PARAM_RESTART='restart';
  PARAM_REPLAY='replay';
  PARAM_CLOSE='close';
  PARAM_LOW_DENSITY='ld';
  PARAM_HIGH_DENSITY='hd';

FUNCTION hasCmdLineParameter(CONST s:string):boolean;
  VAR i:longint;
  begin
    for i:=1 to paramCount do if paramStr(i)=s then exit(true);
    result:=false;
  end;

FUNCTION hasRestartFlag: boolean; begin result:=hasCmdLineParameter(PARAM_RESTART); end;
FUNCTION hasReplayFlag: boolean;  begin result:=hasCmdLineParameter(PARAM_REPLAY);  end;
FUNCTION hasCloseFlag: boolean;   begin result:=hasCmdLineParameter(PARAM_CLOSE);  end;
FUNCTION initialDensityVariant: T_initialDensityEnum;
  begin
    if      hasCmdLineParameter(PARAM_LOW_DENSITY)  then result:=id_low
    else if hasCmdLineParameter(PARAM_HIGH_DENSITY) then result:=id_high
    else                                                 result:=id_ultraHigh;;
  end;

FUNCTION hasPositionParameter(OUT index:longint):boolean;
  VAR rest:String;
      i: Integer;
  begin
    result:=false;
    for i:=1 to ParamCount do if copy(paramstr(i),1,4)='pos=' then begin
      rest:=copy(ParamStr(i),5,length(paramstr(i)));
      index:=StrToIntDef(rest,-1);
      result:=index>=0;
      if result then exit(result);
    end;
  end;

FUNCTION fileName_anim:string;
  begin result:=ChangeFileExt(paramStr(0),'.anim'); end;

FUNCTION fileName_dump:string;
  begin result:=ChangeFileExt(paramStr(0),'.dump'); end;

FUNCTION filename_txt:string;
  begin result:=ChangeFileExt(paramStr(0),'.txt'); end;

VAR myLog:T_log;
FUNCTION log: T_log;
  begin result:=myLog; end;

{ T_log }

CONSTRUCTOR T_log.create;
  begin
    assign(logHandle,ChangeFileExt(paramStr(0),'.log'));
    rewrite(logHandle);
  end;

DESTRUCTOR T_log.destroy;
  begin
    flush(logHandle);
    close(logHandle);
  end;

FUNCTION T_log.append(CONST s: string): T_log;
  begin
    write(logHandle,s); result:=self;
    {$ifdef debugMode}
    write(s);
    {$endif}
  end;

FUNCTION T_log.append(CONST i: longint): T_log;
  begin
    write(logHandle,i); result:=self;
    {$ifdef debugMode}
    write(i);
    {$endif}
  end;

FUNCTION T_log.append(CONST f: double; CONST decimalPlaces: longint): T_log;
  begin
    write(logHandle,f:0:decimalPlaces); result:=self;
    {$ifdef debugMode}
    write(f:0:decimalPlaces);
    {$endif}
  end;

FUNCTION T_log.appendLineBreak: T_log;
  begin
    writeln(logHandle,''); result:=self;
    flush(logHandle);
    {$ifdef debugMode}
    writeln;
    {$endif}
  end;

VAR appTitle_:string='';
FUNCTION appTitle: string;
  begin
    result:=appTitle_;
  end;

INITIALIZATION
  myLog:=T_log.create;
  appTitle_:={$ifdef debugMode}'Grav[D] '{$else}'Grav '{$endif}+intToStr(SYS_SIZE)+' ';
  case initialDensityVariant of
    id_low:  appTitle_+='ld';
    id_high: appTitle_+='hd';
    else     appTitle_+='ud';
  end;

FINALIZATION;
  FreeAndNil(myLog);

end.

