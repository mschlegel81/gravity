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
      picture: P_rgbPicture;
      animStream: TFileStream;
      replaying: boolean;

  PROCEDURE addPicture(CONST writeAnimStream:boolean);
    begin
      if writeAnimStream then begin
        picture^.write(animStream);
        sys.saveToFile(fileName_dump);
      end else begin
        log.append('Replay; mass=').append(picture^.mass,6).appendLineBreak;
      end;
      queue^.addFrame(picture);
      inc(calcFrameCount);
    end;

  begin
    randomize;
    queue:=P_animation(p);
    sys.create;
    if fileExists(fileName_dump) and fileExists(fileName_anim) and not(hasRestartFlag) then begin
      if not(sys.loadFromFile(fileName_dump)) then begin
        halt(1);
      end;
      animStream:=TFileStream.create(fileName_anim,fmOpenReadWrite or fmShareDenyWrite);
      animStream.Seek(0,soBeginning);
      repeat
        new(picture,create);
        replaying:=picture^.load(animStream);
        if replaying
        then addPicture(false)
        else dispose(picture,destroy);
      until not(replaying) or closing;
    end else begin
      animStream:=TFileStream.create(fileName_anim,fmCreate or fmShareDenyWrite);
      animStream.Seek(0,soBeginning);
      picture:=sys.getPicture;
      addPicture(true);
    end;
    while not(closing) and (calcFrameCount<5000) and not(hasReplayFlag) do begin
      sys.doMacroTimeStep(calcFrameCount);
      picture:=sys.getPicture;
      addPicture(true);
    end;
    //close(handle);
    animStream.destroy;
    sys.destroy;
    result:=0;
    threadRunning:=false;
    log.append('Calculation thread stopped').appendLineBreak;
    if hasCloseFlag then closing:=true;
  end;

{ TGravMainForm }

PROCEDURE TGravMainForm.FormCreate(Sender: TObject);
  begin
    Application.title:=appTitle;
    caption:=appTitle;
    queue.create;
    beginThread(@calcThread,@queue);
    replaying:=false;
    WindowState:=wsMinimized;
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
      if (framesCached=0) then begin
                                replaying:=false;
      end;
    end else framesCached:=queue.getFrameCount;
    caption:=appTitle+' @'+intToStr(calcFrameCount-framesCached)+' ('+intToStr(framesCached)+' frames ahead)'+BoolToStr(closing,' -- CLOSING','')+BoolToStr(replaying,'',' -- PAUSED');
    if closing and not(threadRunning) then close;
  end;

PROCEDURE TGravMainForm.Image1Click(Sender: TObject);
  begin
    replaying:=not(replaying);
  end;

end.

