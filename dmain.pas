unit dMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Menus, uPackage, uPackageList;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    imgIcons: TImageList;
    mmMain: TMainMenu;
    mbInstallCheckAll: TMenuItem;
    mbInstallExpandTree: TMenuItem;
    mbInstallCollapseTree: TMenuItem;
    mbInstallCheckInvert: TMenuItem;
    mbInstallCheckNone: TMenuItem;
    mbTree: TMenuItem;
    mbCollapseTree: TMenuItem;
    mbExpandTree: TMenuItem;
    mbCheck: TMenuItem;
    mbCheckAll: TMenuItem;
    mbCheckInvert: TMenuItem;
    mbCheckNone: TMenuItem;
    mbOptions: TMenuItem;
    mbHelp: TMenuItem;
    mbOnlineHelp: TMenuItem;
    mbAbout: TMenuItem;
    mbOptonsMain: TMenuItem;
    mbOptionsPkg: TMenuItem;
    pmInstallTree: TPopupMenu;
    pmInstallCheck: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure mbCollapseTreeClick(Sender: TObject);
    procedure mbExpandTreeClick(Sender: TObject);
  private
    // Подготовка директорий
    procedure PrepareDirs(); 
    // Подготовка списков пакетов
    procedure PreparePackages();
  public
    { Публичные переменные, которые доступны во всех юнитах с подключенным uses ..., dMain }

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
    // Список объектов пакетов программ для установки
    SoftPackages: TPackageList; 
    // Список объектов пакетов утилит для запуска
    ToolsPackages: TPackageList;
  end;

var
  dmMain: TdmMain;

implementation

uses fMain;

{$R *.lfm}

{ TdmMain }
                       
{------------------------------------------------------------------------------
Конструктор:   TdmMain.Create()
Назначение:    Создание датамодуля, подготовка путей и списков пакетов
Вх. параметры: Sender: TObject
------------------------------------------------------------------------------}
procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  PrepareDirs();
  PreparePackages();
end;
      
{------------------------------------------------------------------------------
Процедура:  TdmMain.mbCollapseTreeClick()
Назначение: Сворачивание дерева на активной вкладке главного окна
------------------------------------------------------------------------------}
procedure TdmMain.mbCollapseTreeClick(Sender: TObject);
begin
  case (fmMain.PageControl.ActivePageIndex) of
    0: fmMain.vstSoftPkgContents.FullCollapse();
    1: fmMain.vstToolsPkgContents.FullCollapse();
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TdmMain.mbCollapseTreeClick()
Назначение: Разворачивание дерева на активной вкладке главного окна
------------------------------------------------------------------------------}
procedure TdmMain.mbExpandTreeClick(Sender: TObject);
begin
  case (fmMain.PageControl.ActivePageIndex) of
    0: fmMain.vstSoftPkgContents.FullExpand();
    1: fmMain.vstToolsPkgContents.FullExpand();
  end;
end;
                    
{------------------------------------------------------------------------------
Процедура:  TdmMain.PreparePackages()
Назначение: Подготовка директорий
------------------------------------------------------------------------------}
procedure TdmMain.PrepareDirs();
begin
  ABToolExePath := ExtractFilePath(ParamStr(0));
  ABToolDataPath := ABToolExePath + 'ABTool\';
  ABToolPkgPath := ABToolDataPath + 'Packages\';
  ABToolLangPath := ABToolDataPath + 'Languages\';
  ABToolLogPath := ABToolDataPath + 'Logs\';
  if not DirectoryExists(ABToolPkgPath) then
    ForceDirectories(ABToolPkgPath);
  if not DirectoryExists(ABToolLangPath) then
    ForceDirectories(ABToolLangPath);
  if not DirectoryExists(ABToolLogPath) then
    ForceDirectories(ABToolLogPath);
end;
       
{------------------------------------------------------------------------------
Процедура:  TdmMain.PreparePackages()
Назначение: Подготовка списков пакетов
------------------------------------------------------------------------------}
procedure TdmMain.PreparePackages();
begin
  SoftPackages  := TPackageList.Create;
  SoftPackages.Load(ptSoft);
  ToolsPackages := TPackageList.Create;
  ToolsPackages.Load(ptTools);
end;




end.

