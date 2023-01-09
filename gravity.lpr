PROGRAM gravity;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}
  cthreads,
  {$endif}
  {$IFDEF HASAMIGA}
  athreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, gravityMain,sysutils,commandLineHandling
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.title:='Grav '+intToStr(SYS_SIZE);
  Application.Scaled:=true;
  Application.initialize;
  Application.title:=appTitle;
  Application.CreateForm(TGravMainForm, GravMainForm);
  Application.run;
end.

