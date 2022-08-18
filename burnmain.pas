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
    aheadTarget:longint=200;
    closing:boolean=false;
    threadRunning:boolean=true;
{$R *.lfm}

{ TBurnForm }

FUNCTION calcThread(p:pointer):ptrint;
  VAR queue:P_animation;
      sys:T_cellSystem;
      framesInQueue:longint=-1;
      picture: P_rgbPicture;
      handle:  textFile;
      handleOpen:boolean=true;
      replaying: Boolean;
  begin
    randomize;
    assign(handle,filename_txt);
    rewrite(handle);
    queue:=P_animation(p);
    sys.create;
    while not(closing) do begin
      replaying:=sys.doMacroTimeStep;
      picture:=sys.getPicture(BurnForm.width,BurnForm.height);
      writeln(handle,picture^.toString);

      framesInQueue:=queue^.addFrame(picture);
      if not(replaying) and (framesInQueue>aheadTarget) then sleep(framesInQueue-aheadTarget);
    end;
    if handleOpen then begin
      close(handle);
      handleOpen:=false;
    end;
    sys.destroy;
    result:=0;
    threadRunning:=false;
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
        if (aheadTarget<1000) then inc(aheadTarget)
                              else replaying:=false;
      end;
    end else framesCached:=queue.getFrameCount;
    if (framesCached>aheadTarget) and (aheadTarget>100) then dec(aheadTarget);
    caption:='Gravity '+intToStr(SYS_SIZE)+' ('+intToStr(framesCached)+'/'+IntToStr(aheadTarget)+' frames ahead)'+BoolToStr(closing,' -- CLOSING','')+BoolToStr(replaying,'',' -- PAUSED');
    if closing and not(threadRunning) then close;
  end;

PROCEDURE TBurnForm.Image1Click(Sender: TObject);
  begin
    replaying:=not(replaying);
  end;

end.

