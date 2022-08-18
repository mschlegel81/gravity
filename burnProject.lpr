PROGRAM burnProject;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, burnMain,SysUtils
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.Title:='Gravity xxx';
  Application.Title:='Gravity '+IntToStr(SYS_SIZE);
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TBurnForm, BurnForm);
  Application.run;
end.

