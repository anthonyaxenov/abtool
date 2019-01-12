unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, ActnList, EditBtn, Menus, VirtualTrees, Types,
  dMain, uFileUtils, uPackage, uPackageUtils, uPackageList;

type

  { TfmMain }

  TfmMain = class(TForm)
    btnSoftStart: TButton;
    btnToolsPkgEdit: TSpeedButton;
    btnToolsPkgReload: TSpeedButton;
    btnToolsRun: TButton;
    cmbSoftPkgSelect: TComboBox;
    cmbToolsPkgSelect: TComboBox;
    edSoftSearch: TEdit;
    edToolsSearch: TEdit;
    labSoftPkgDescription: TLabel;
    labToolsPkgDescription: TLabel;
    PageControl: TPageControl;
    pnSoftTop: TPanel;
    btnSoftPkgEdit: TSpeedButton;
    btnSoftPkgReload: TSpeedButton;
    pnToolsTop: TPanel;
    tabSoft: TTabSheet;
    tabLog: TTabSheet;
    tabSystem: TTabSheet;
    tabTools: TTabSheet;
    vstSoftPkgContents: TVirtualStringTree;
    vstToolsPkgContents: TVirtualStringTree;  
    // Создание формы и обновление выпадающих меню пакетов программ и утилит
    procedure FormCreate(Sender: TObject);
    // Обрабока клика по кнопке обновления выпадающего меню пакетов программ
    procedure btnSoftPkgReloadClick(Sender: TObject);
    // Обрабока клика по кнопке обновления выпадающего меню пакетов утилит
    procedure btnToolsPkgReloadClick(Sender: TObject);
  private
    // Перезагрузка списка пакетов и выпадающих меню пакетов по указанному типу
    procedure ReloadPackagesList(APackageType: TPackageType);
  public

  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }
          
{------------------------------------------------------------------------------
Процедура:     TfmMain.ReloadPackagesList()
Назначение:    Создание формы и обновление выпадающих меню пакетов программ и утилит
Вх. параметры: Sender: TObject
------------------------------------------------------------------------------}
procedure TfmMain.FormCreate(Sender: TObject);
begin
  ReloadPackagesList(ptSoft);
  ReloadPackagesList(ptTools);
end; 
           
{------------------------------------------------------------------------------
Процедура:     TfmMain.btnSoftPkgReloadClick()
Назначение:    Обрабока клика по кнопке обновления выпадающего меню пакетов программ
Вх. параметры: Sender: TObject
------------------------------------------------------------------------------}
procedure TfmMain.btnSoftPkgReloadClick(Sender: TObject);
begin
  ReloadPackagesList(ptSoft);
end;
       
{------------------------------------------------------------------------------
Процедура:     TfmMain.btnToolsPkgReloadClick()
Назначение:    Обрабока клика по кнопке обновления выпадающего меню пакетов утилит
Вх. параметры: Sender: TObject
------------------------------------------------------------------------------}
procedure TfmMain.btnToolsPkgReloadClick(Sender: TObject);
begin
  ReloadPackagesList(ptTools);
end;

{------------------------------------------------------------------------------
Процедура:     TfmMain.ReloadPackagesList()
Назначение:    Перезагрузка списка пакетов и выпадающих меню пакетов по указанному типу
Вх. параметры: APackageType: TPackageType - тип списка пакета
------------------------------------------------------------------------------}
procedure TfmMain.ReloadPackagesList(APackageType: TPackageType);
var
  LastIndex: integer;
  Key: Integer;
  Pkg: TPackage;
  Combo: TComboBox;
  PackageList: TPackageList;
begin 
  case (APackageType) of
    ptSoft: begin
      Combo := cmbSoftPkgSelect;
      PackageList := dmMain.SoftPackages;
    end;
    ptTools: begin
      Combo := cmbToolsPkgSelect;
      PackageList := dmMain.ToolsPackages;
    end;
    ptUnknown: raise Exception.Create('TfmMain.ReloadPackagesList(): передан ptUnknown');
  end;
  LastIndex := Combo.ItemIndex;
  if LastIndex < 0 then
    LastIndex := 0;
  Combo.Clear;
  PackageList.Reload();
  if PackageList.Count > 0 then
  begin
    for Key := 0 to PackageList.Count - 1 do
    begin
      Pkg := PackageList[Key];
      Combo.Items.Add(Pkg.Name + ' (' + ExtractFileName(Pkg.FileName) + ')');
    end;
  end;
  if Combo.Items.Count <= LastIndex then
    LastIndex := Combo.Items.Count - 1;
  Combo.ItemIndex := LastIndex;
end;

end.

