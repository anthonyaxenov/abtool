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
  uPackageList;

{$R *.res}

var
  // путь к файлу ABTool.exe
  ABToolExePath: String;

  // путь к директории ABTool
  ABToolDataPath: String;

  // путь к директории ABTool\Packages
  ABToolPkgPath: String;

  // путь к директории ABTool\Languages
  ABToolLangPath: String;

  // путь к директории ABTool\Logs
  ABToolLogPath: String;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;

  // настройка путей к директориям
  ABToolExePath := ExtractFilePath(ParamStr(0));
  ABToolDataPath := ABToolExePath + 'ABTool\';
  ABToolPkgPath := ABToolDataPath + 'Packages\';
  ABToolLangPath := ABToolDataPath + 'Languages\';
  ABToolLogPath := ABToolDataPath + 'Logs\';

  // подготовка директорий
  if not DirectoryExists(ABToolPkgPath) then
    ForceDirectories(ABToolPkgPath);
  if not DirectoryExists(ABToolLangPath) then
    ForceDirectories(ABToolLangPath);
  if not DirectoryExists(ABToolLogPath) then
    ForceDirectories(ABToolLogPath);

  Application.CreateForm(TMainDM, MainDM);
  Application.CreateForm(TfmMain, fMain.fmMain);
  Application.Run;
end.



