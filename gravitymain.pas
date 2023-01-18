UNIT gravityMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls,commandLineHandling,basicGraphics,simplerPhysics;

TYPE

  { TGravMainForm }

  TGravMainForm = class(TForm)
    IdleTimer1: TIdleTimer;
    Image1: TImage;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE IdleTimer1Timer(Sender: TObject);
    PROCEDURE Image1Click(Sender: TObject);
  private
    replaying:boolean;
  public

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
      animStream: TFileStream;

  PROCEDURE addPicture(CONST writeAnimStream:boolean);
    begin
      if writeAnimStream then begin
        newPic^.write(animStream,prevPic);
      end else begin
        log.append('Replay (')
           .append(calcFrameCount)
           .append('); mass=')
           .append(newPic^.mass,6)
           .appendLineBreak;
      end;
      if prevPic<>nil then queue^.addFrame(prevPic);
      prevPic:=newPic;
      inc(calcFrameCount);
    end;

  PROCEDURE replay;
    VAR replaying: Boolean;
    begin
      animStream:=TFileStream.create(fileName_anim,fmOpenReadWrite or fmShareDenyWrite);
      animStream.Seek(0,soBeginning);
      repeat
        new(newPic,create);
        replaying:=newPic^.load(animStream,prevPic);
        addPicture(false);
      until not(replaying) or (calcFrameCount>=sys.numberOfFrames);
      if calcFrameCount<sys.numberOfFrames then begin
        log.append('ERROR ON RESTORE: Expected to read ')
           .append(sys.numberOfFrames)
           .append(' frames but read only ')
           .append(calcFrameCount)
           .appendLineBreak;
        calcFrameCount:=0;
        animStream.Seek(0,soBeginning);
        sys.destroy;
        sys.create;
        while queue^.dropFrame>0 do;
        prevPic:=nil;
        newPic:=sys.getPicture;
        addPicture(true);
      end;
    end;

  begin
    randomize;
    queue:=P_animation(p);
    sys.create;
    if fileExists(fileName_dump) and fileExists(fileName_anim) and not(hasRestartFlag) and sys.loadFromFile(fileName_dump)
    then replay
    else begin
      animStream:=TFileStream.create(fileName_anim,fmCreate or fmShareDenyWrite);
      animStream.Seek(0,soBeginning);
      newPic:=sys.getPicture;
      addPicture(true); //frame #0
    end;
    while not(closing) and (calcFrameCount<5000) and not(hasReplayFlag) do begin
      sys.doMacroTimeStep(calcFrameCount);
      newPic:=sys.getPicture;
      addPicture(true);
    end;
    sys.numberOfFrames:=calcFrameCount;
    sys.saveToFile(fileName_dump);
    queue^.addFrame(newPic);
    animStream.destroy;
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
    caption:=appTitle;
    queue.create;
    beginThread(@calcThread,@queue);
    replaying:=false;

    if hasPositionParameter(positionIndex) then begin
      Left:=0;
      Top :=0;
      while (top+2*Height<screen.Height) and (positionIndex>0) do begin
        while (left+2*Width<Screen.Width) and (positionIndex>0) do begin
          left:=left+width;
          dec(positionIndex);
        end;
        if positionIndex>0 then begin;
          top:=top+height+30;
          left:=0;
          dec(positionIndex);
        end;
      end;
      if positionIndex>0 then begin
        left:=0;
        top :=0;
        WindowState:=wsMinimized;
      end;
    end else begin
      {$ifndef debugMode}
      WindowState:=wsMinimized;
      {$endif}
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
      queue.render(Image1);
      framesCached:=queue.dropFrame;
      if (framesCached=0) then replaying:=false;
    end else framesCached:=queue.getFrameCount;
    caption:=appTitle+' @'+intToStr(calcFrameCount-framesCached)+' ('+intToStr(framesCached)+' frames ahead)'+BoolToStr(closing,' -- CLOSING','')+BoolToStr(replaying,'',' -- PAUSED');
    if closing and not(threadRunning) then close;
  end;

PROCEDURE TGravMainForm.Image1Click(Sender: TObject);
  begin
    replaying:=not(replaying);
  end;

end.

