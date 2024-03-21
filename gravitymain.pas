UNIT gravityMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  PopupNotifier, commandLineHandling, basicGraphics, simplerPhysics;

TYPE

  { TGravMainForm }

  TGravMainForm = class(TForm)
    IdleTimer1: TIdleTimer;
    Image1: TImage;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    PROCEDURE IdleTimer1Timer(Sender: TObject);
    PROCEDURE Image1Click(Sender: TObject);
  private
    replaying:boolean;
  public
    extension:double;

  end;

VAR
  GravMainForm: TGravMainForm;

IMPLEMENTATION
VAR queue:T_animation;
    calcFrameCount:longint=0;
    closing:boolean=false;
    threadRunning:boolean=true;

{$R *.lfm}

{ TBurnForm }

FUNCTION calcThread(p:pointer):ptrint;
  VAR queue:P_animation;
      sys:T_cellSystem;
      prevPic: P_rgbPicture=nil;
      newPic : P_rgbPicture;
      compressedAnimStream: TFileStream;

  PROCEDURE addPicture(CONST writeAnimStream:boolean);
    begin
      if writeAnimStream then begin
        {$ifdef DEBUGMODE}
        log.append('Write frame @').append(compressedAnimStream.Position).appendLineBreak;
        {$endif}
        newPic^.writeCompressed(compressedAnimStream,prevPic);
      end;
      if prevPic<>nil then queue^.addFrame(prevPic);
      prevPic:=newPic;
      inc(calcFrameCount);
    end;

  PROCEDURE replay;
    VAR replaying: Boolean;
    begin
      repeat
        new(newPic,create);
        log.append('Replay (')
           .append(calcFrameCount)
           .append(')')
           {$ifdef DEBUGMODE}
           .append(' @').append(compressedAnimStream.Position)
           {$endif}
           .appendLineBreak;
        replaying:=newPic^.loadCompressed(compressedAnimStream,prevPic);
        if replaying then addPicture(false)
      until not(replaying) or (calcFrameCount>=5000) or closing;
      if closing then begin
        new(newPic,create);
        repeat until not newPic^.loadCompressed(compressedAnimStream,prevPic);
        dispose(newPic,destroy);
      end;
    end;

  PROCEDURE transcode;
    VAR uncompressedAnimStream:TFileStream;
        replaying: Boolean;
    begin
      uncompressedAnimStream:=TFileStream.Create(fileName_anim,fmOpenReadWrite or fmShareDenyWrite);
      uncompressedAnimStream.Seek(0,soBeginning);
      repeat
        new(newPic,create);
        replaying:=newPic^.load(uncompressedAnimStream,prevPic);
        log.append('Transcode (')
           .append(calcFrameCount)
           .append(')')
           .appendLineBreak;
        if replaying then addPicture(true);
      until not(replaying);
    end;

  VAR anyCalculated:boolean=false;
      compressedReplayExists:boolean;
  begin
    randomize;
    queue:=P_animation(p);
    sys.create;
    compressedReplayExists:=FileExists(fileName_replay);
    if compressedReplayExists
    then compressedAnimStream:=TFileStream.Create(fileName_replay,fmOpenReadWrite or fmShareDenyWrite)
    else compressedAnimStream:=TFileStream.Create(fileName_replay,fmCreate        or fmShareDenyWrite);

    compressedAnimStream.Seek(0,soBeginning);

    if not(hasRestartFlag) then begin
      if fileExists(fileName_anim) and not compressedReplayExists
      then transcode
      else replay;
    end;
    if calcFrameCount<5000 then begin
      //Only try to read dump if there is still something left to calculate
      if not(hasReplayFlag) and (not(sys.loadFromFile(fileName_dump)) or (sys.numberOfFrames<>calcFrameCount)) then begin
        log.append('ERROR ON RESTORE! RESTARTING CALCULATION.').appendLineBreak;
        calcFrameCount:=0;
        compressedAnimStream.Seek(0,soBeginning);
        sys.destroy;
        sys.create;
        while queue^.dropFrame>0 do;
        prevPic:=nil;
        newPic:=sys.getPicture;
        addPicture(true);
      end;
      while not(closing) and (calcFrameCount<5000) and not(hasReplayFlag) do begin
        sys.doMacroTimeStep(calcFrameCount);
        anyCalculated:=true;
        newPic:=sys.getPicture;
        addPicture(true);
      end;
    end;
    if anyCalculated then begin
      sys.numberOfFrames:=calcFrameCount;
      sys.saveToFile(fileName_dump);
    end else if calcFrameCount>=5000 then DeleteFile(fileName_dump);

    queue^.addFrame(newPic);
    compressedAnimStream.destroy;
    sys.destroy;
    result:=0;
    threadRunning:=false;
    log.append('Calculation thread stopped').appendLineBreak;
    if hasCloseFlag then closing:=true;
  end;

{ TGravMainForm }

PROCEDURE TGravMainForm.FormCreate(Sender: TObject);
  var positionIndex: longint;
  begin
    Application.title:=appTitle;
    extension:=1;
    image1.Hint:=paramstr(0);
    caption:=appTitle;
    queue.create;
    beginThread(@calcThread,@queue);
    replaying:=false;

    if hasPositionParameter(positionIndex) then begin
      Left:=0;
      Top :=0;
      while (positionIndex>0) do begin
        while (left+2*Width+10<Screen.Width) and (positionIndex>0) do begin
          left:=left+width+5;
          dec(positionIndex);
        end;
        if positionIndex>0 then begin;
          top:=top+height+40;
          if top+height>screen.Height then top:=0;
          left:=0;
          dec(positionIndex);
        end;
      end;
    end else begin
      {$ifndef debugMode}
      WindowState:=wsMinimized;
      {$endif}
    end;
  end;

procedure TGravMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (key=187) or (key=107) then extension*=1.1;
    if (key=189) or (key=109) then begin
      extension/=1.1;
      if extension<1 then extension:=1;
    end;
  end;

PROCEDURE TGravMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=not(threadRunning);
    closing:=true;
  end;

PROCEDURE TGravMainForm.IdleTimer1Timer(Sender: TObject);
  VAR framesCached:longint;
  begin
    if replaying then begin
      queue.render(Image1,extension,ClientHeight/ClientWidth);
      framesCached:=queue.dropFrame;
      if (framesCached=0) then replaying:=false;
    end else framesCached:=queue.getFrameCount;
    caption:=appTitle+' @'+intToStr(calcFrameCount-framesCached)
    +BoolToStr(threadRunning,' ('+intToStr(framesCached)+' frames ahead)',' (done)')
    +BoolToStr(closing,' -- CLOSING','')
    +BoolToStr(replaying,'',' -- PAUSED');
    if closing and not(threadRunning) then close;
  end;

PROCEDURE TGravMainForm.Image1Click(Sender: TObject);
  begin
    replaying:=not(replaying);
  end;

end.

