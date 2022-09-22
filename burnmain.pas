UNIT burnMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls,simplerPhysics,basicGraphics;

TYPE

  { TBurnForm }

  TBurnForm = class(TForm)
    IdleTimer1: TIdleTimer;
    Image1: TImage;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE IdleTimer1Timer(Sender: TObject);
    PROCEDURE Image1Click(Sender: TObject);
  private
    replaying:boolean;
  public

  end;

VAR
  BurnForm: TBurnForm;

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
      //handle:  textFile;
      animStream: TFileStream;
      replaying: boolean;

  PROCEDURE addPicture(CONST writeAnimStream:boolean);
    begin
      if writeAnimStream then begin
        picture^.write(animStream);
        sys.saveToFile(fileName_dump);
      end else begin
        writeln(logHandle,'Replay; mass=',picture^.mass:0:6);
      end;
      queue^.addFrame(picture);
      inc(calcFrameCount);
    end;

  begin
    randomize;
    //assign(handle,filename_txt);
    //rewrite(handle);
    queue:=P_animation(p);
    sys.create;
    if fileExists(fileName_dump) and fileExists(fileName_anim) and not(hasCmdLineParameter(PARAM_RESTART)) then begin
      if not(sys.loadFromFile(fileName_dump)) then begin
        halt(1);
      end;
      animStream:=TFileStream.create(fileName_anim,fmOpenReadWrite);
      animStream.Seek(0,soBeginning);
      append(logHandle);
      repeat
        new(picture,create(SYS_SIZE,SYS_SIZE));
        replaying:=picture^.load(animStream);
        if replaying
        then addPicture(false)
        else dispose(picture,destroy);
      until not(replaying) or closing;
      close(logHandle);
    end else begin
      animStream:=TFileStream.create(fileName_anim,fmCreate);
      animStream.Seek(0,soBeginning);
      picture:=sys.getPicture(BurnForm.width,BurnForm.height);
      addPicture(true);
    end;
    while not(closing) and (calcFrameCount<5000) and not(hasCmdLineParameter(PARAM_REPLAY)) do begin
      sys.doMacroTimeStep(calcFrameCount);
      picture:=sys.getPicture(BurnForm.width,BurnForm.height);
      addPicture(true);
    end;
    //close(handle);
    animStream.destroy;
    sys.destroy;
    result:=0;
    threadRunning:=false;
    append(logHandle);
    writeln(logHandle,'Calculation thread stopped');
    close(logHandle);
    if hasCmdLineParameter(PARAM_CLOSE) then closing:=true;
  end;

PROCEDURE TBurnForm.FormCreate(Sender: TObject);
  begin
    queue.create;
    beginThread(@calcThread,@queue);
    replaying:=false;
  end;

PROCEDURE TBurnForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin

  end;

PROCEDURE TBurnForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=not(threadRunning);
    closing:=true;
  end;

PROCEDURE TBurnForm.FormResize(Sender: TObject);
  begin
    queue.render(Image1);
  end;

PROCEDURE TBurnForm.IdleTimer1Timer(Sender: TObject);
  VAR framesCached:longint;
  begin
    if replaying then begin
      queue.render(Image1);
      framesCached:=queue.dropFrame;
      if (framesCached=0) then begin
                                replaying:=false;
      end;
    end else framesCached:=queue.getFrameCount;
    caption:='Gravity '+intToStr(SYS_SIZE)+' @'+intToStr(calcFrameCount-framesCached)+' ('+intToStr(framesCached)+' frames ahead)'+BoolToStr(closing,' -- CLOSING','')+BoolToStr(replaying,'',' -- PAUSED');
    if closing and not(threadRunning) then close;
  end;

PROCEDURE TBurnForm.Image1Click(Sender: TObject);
  begin
    replaying:=not(replaying);
  end;

end.

