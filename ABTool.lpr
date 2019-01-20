program ABTool;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  fMain,
  dMain,
  uFileUtils,
  uPackage,
  uPackageUtils,
  uPackageList, fOptions;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TfmMain, fMain.fmMain);
  Application.CreateForm(TfmOptions, fmOptions);
  Application.Run;
end.



