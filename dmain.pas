unit dMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Menus, Forms, PopupNotifier, Dialogs,
  VirtualTrees, uPackage, uPackageList, fOptions, uOptions;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    imgIcons: TImageList;
    mbRefresh: TMenuItem;
    MenuItem1: TMenuItem;
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
    mbOptionsMain: TMenuItem;
    mbOptionsPkg: TMenuItem;
    pmInstallTree: TPopupMenu;
    pmInstallCheck: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure mbAboutClick(Sender: TObject);
    procedure mbCheckAllClick(Sender: TObject);
    procedure mbCheckNoneClick(Sender: TObject);
    procedure mbCollapseTreeClick(Sender: TObject);
    procedure mbExpandTreeClick(Sender: TObject);
    procedure mbOptionsMainClick(Sender: TObject);
    procedure mbOptionsPkgClick(Sender: TObject);
    procedure mbRefreshClick(Sender: TObject);
  private
    // Форма настроек
    fmOptions: TfmOptions;
    // Подготовка директорий
    procedure PrepareDirs();
    // Подготовка списков пакетов
    procedure PreparePackageLists();
  public
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
    // Установка состояния отметки для всех корневых нод указанного дерева
    procedure SetVSTCheckState(AVST: TBaseVirtualTree; AState: boolean);
    // Показ формы настроек на указанной странице и возврат модального результата
    function CallOptionsForm(APageIndex: byte = 0): TModalResult;
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
------------------------------------------------------------------------------}
procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  PrepareDirs();
  PreparePackageLists();
  TOptions.Create(dmMain.ABToolDataPath + '\abtool.ini');
end;

{------------------------------------------------------------------------------
Конструктор: TdmMain.SetVSTCheckState()
Назначение:  Установка состояния отметки для всех корневых нод указанного дерева
Вх. параметры:
  AVST: TBaseVirtualTree - дерево
  AState: boolean - состояние отметок: TRUE выбрать все, FALSE снять выбор со всех
------------------------------------------------------------------------------}
procedure TdmMain.SetVSTCheckState(AVST: TBaseVirtualTree; AState: boolean);
var
  Node: PVirtualNode;

  // Установка состояния отметки ноды-родителя дочерним нодам
  procedure CheckChildNodes(ANode: PVirtualNode);
  var
    ChildNode: PVirtualNode;
  begin
    ChildNode := AVST.GetFirstChild(ANode);
    while Assigned(ChildNode) do
    begin
      CheckChildNodes(ChildNode);
      ChildNode^.CheckState := ChildNode^.Parent^.CheckState;
      ChildNode := AVST.GetNextSibling(ChildNode);
    end;
  end;

begin
  AVST.BeginUpdate;
  try
    Node := AVST.GetFirst;
    while Assigned(Node) do
    begin
      if AState = true then
        Node^.CheckState := csCheckedNormal
      else
        Node^.CheckState := csUncheckedNormal;
      CheckChildNodes(Node);
      Node := AVST.GetNextSibling(Node);
    end;
  finally
    AVST.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
Конструктор: TdmMain.mbCheckAllClick()
Назначение:  Обработка клика п. меню "Выбрать всё"
------------------------------------------------------------------------------}
procedure TdmMain.mbCheckAllClick(Sender: TObject);
begin
  SetVSTCheckState(fmMain.vstSoftPkgContents, true);
end;

{------------------------------------------------------------------------------
Конструктор: TdmMain.mbCheckNoneClick()
Назначение:  Обработка клика п. меню "Снять выбор"
------------------------------------------------------------------------------}
procedure TdmMain.mbCheckNoneClick(Sender: TObject);
begin
  SetVSTCheckState(fmMain.vstSoftPkgContents, false);
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
Процедура:  TdmMain.mbRefreshClick()
Назначение: Обновление списка пакетов и дерева на активной вкладке главного окна
------------------------------------------------------------------------------}
procedure TdmMain.mbRefreshClick(Sender: TObject);
begin
  case (fmMain.PageControl.ActivePageIndex) of
    0: fmMain.btnSoftPkgReload.Click;
    1: fmMain.btnToolsPkgReload.Click;
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TdmMain.PrepareDirs()
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
Процедура:  TdmMain.PreparePackageLists()
Назначение: Подготовка списков пакетов
------------------------------------------------------------------------------}
procedure TdmMain.PreparePackageLists();
begin
  SoftPackages  := TPackageList.Create;
  SoftPackages.Load(ptSoft);
  ToolsPackages := TPackageList.Create;
  ToolsPackages.Load(ptTools);
end;

{------------------------------------------------------------------------------
Функция:    TdmMain.CallOptionsForm()
Назначение: Показ формы настроек на указанной странице и возврат модального
  результата
Возвращает: TModalResult
------------------------------------------------------------------------------}
function TdmMain.CallOptionsForm(APageIndex: byte = 0): TModalResult;
begin
  if fmOptions = nil then
    fmOptions := TfmOptions.Create(fmMain);
  fmOptions.PageControl.ActivePageIndex := APageIndex;
  Result := fmOptions.ShowModal;
end;

{------------------------------------------------------------------------------
Процедура:  TdmMain.mbOptionsMainClick()
Назначение: Обработка клика п. меню "Настройки - Основные..."
------------------------------------------------------------------------------}
procedure TdmMain.mbOptionsMainClick(Sender: TObject);
begin
  CallOptionsForm;
end;

{------------------------------------------------------------------------------
Процедура:  TdmMain.mbOptionsPkgClick()
Назначение: Обработка клика п. меню "Настройки - Управление пакетами..."
------------------------------------------------------------------------------}
procedure TdmMain.mbOptionsPkgClick(Sender: TObject);
begin
  case CallOptionsForm(1) of
    mrOK: ;//ShowMessage('mrOK');
    mrClose: ;//ShowMessage('mrClose');
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TdmMain.mbAboutClick()
Назначение: Обработка клика п. меню "Помощь - О программе..."
------------------------------------------------------------------------------}
procedure TdmMain.mbAboutClick(Sender: TObject);
begin
  case CallOptionsForm(2) of
    mrOK: ;//ShowMessage('mrOK');
    mrClose: ;//ShowMessage('mrClose');
  end;
end;

end.

