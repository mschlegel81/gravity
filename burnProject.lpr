PROGRAM burnProject;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, burnMain,sysutils
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.title:='Gravity xxx';
  Application.title:='Gravity '+intToStr(SYS_SIZE);
  Application.initialize;
  Application.CreateForm(TBurnForm, BurnForm);
  Application.run;
end.

