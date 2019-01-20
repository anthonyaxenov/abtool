unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, ActnList, EditBtn, Menus, virtualtreeview_package,
  VirtualTrees, dMain, uPackage, uPackageList;

type

  // Указатель на тип данных TRootNodeData
  PRootNodeData = ^TRootNodeData;
  // Тип данных для работы с корневыми нодами
  TRootNodeData = record
    FName: String;
  end;

  // Указатель на тип данных TChildNodeData
  PChildNodeData = ^TChildNodeData;
  // Тип данных для работы с дочерними нодами
  TChildNodeData = record
    FName: String;
    FPath: String;
  end;

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
    // Смена пакета программ через выпадающий список
    procedure cmbSoftPkgSelectChange(Sender: TObject);
    // Смена пакета утилит через выпадающий список
    procedure cmbToolsPkgSelectChange(Sender: TObject);
    // Обрабока клика по кнопке обновления выпадающего меню пакетов программ
    procedure btnSoftPkgReloadClick(Sender: TObject);
    // Обрабока клика по кнопке обновления выпадающего меню пакетов утилит
    procedure btnToolsPkgReloadClick(Sender: TObject);
    // Отображение отметок в нодах дерева программ
    procedure vstSoftPkgContentsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    // Отображение имён утилит в нодах дерева программ
    procedure vstSoftPkgContentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    // Отображение имён утилит в нодах дерева утилит
    procedure vstToolsPkgContentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    // Создание ноды в указанном дереве с указанными параметрами
    function AddNode(AVST: TVirtualStringTree; ARootNode: PVirtualNode; AName: String;
      APath: String = ''): PVirtualNode;
    // Создание ноды в указанном дереве с указанными параметрами
    //function AddNode(AVST: TVirtualStringTree; ARootNode: PVirtualNode; AName: String;
    //  APath: String = ''): PVirtualNode;
    // Отображение имён программ в нодах деревьев при инициализации деревьев
    procedure SetNodeText(AVST: TVirtualStringTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    // Заполнение дерева содержимым пакета
    procedure PackageFillVST(AVST: TVirtualStringTree; APackage: TPackage);
  public
    // Выбор пакета. Заполняется дерево и отображается описание пакета.
    procedure SetActivePackage(AType: TPackageType; APackageIndex: Integer);
    // Перезагрузка дерева пакетов и выпадающих списков пакетов по указанному типу
    procedure ReloadPackagesList(APackageType: TPackageType);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

{------------------------------------------------------------------------------
Функция:    TPackage.AddNode()
Назначение: Создание дочерней ноды в указанном дереве с указанными параметрами
Вх. параметры:
  AVST: TVirtualStringTree - компонент дерева, в котором нужно создать ноду
  ARootNode: PVirtualNode  - указатель на ноду-родителя (для создания ноды, вложенной в неё)
  AName: String            - название ноды (для отображения имени программы или утилиты)
  APath: String = ''       - путь к программе или утилите (не заполняется для корневых нод)
Возвращает: PVirtualNode   - указатель на созданную ноду списка
------------------------------------------------------------------------------}
function TfmMain.AddNode(AVST: TVirtualStringTree; ARootNode: PVirtualNode; AName: String;
  APath: String = ''): PVirtualNode;
var
  ptrData: PChildNodeData;
begin
  Result := AVST.AddChild(ARootNode);  
  AVST.ValidateNode(Result, False);
  ptrData := AVST.GetNodeData(Result);
  ptrData^.FName := AName;
  //if Assigned(APath) then
    ptrData^.FPath := APath;
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.SetNodeText()
Назначение: Отображение имён программ в нодах деревьев при инициализации деревьев
------------------------------------------------------------------------------}
procedure TfmMain.SetNodeText(AVST: TVirtualStringTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  PData: PRootNodeData;
begin
  PData := AVST.GetNodeData(Node);
  CellText := PData^.FName;
end;

{------------------------------------------------------------------------------
Процедура:     TfmMain.SetActivePackage()
Назначение:    Выбор пакета. Заполняется дерево и отображается описание пакета.
Вх. параметры:
  AType: TPackageType    - тип пакета
  APackageIndex: integer - номер пакета в списке (а равно в соотв. ComboBox)
------------------------------------------------------------------------------}
procedure TfmMain.SetActivePackage(AType: TPackageType; APackageIndex: Integer);
var
  Package: TPackage;
begin
  case (AType) of
    ptSoft:
    begin
      Package := dmMain.SoftPackages[APackageIndex];
      PackageFillVST(vstSoftPkgContents, Package);
      labSoftPkgDescription.Caption := Package.Description;
    end;
    ptTools:
    begin
      Package := dmMain.ToolsPackages[APackageIndex];
      PackageFillVST(vstToolsPkgContents, Package);
      labToolsPkgDescription.Caption := Package.Description;
    end;
    else
      raise Exception.Create('TfmMain.SetActivePackage(): передан неизвестный тип пакета');
  end;
end;

{------------------------------------------------------------------------------
Процедура:     TfmMain.PackageFillVST()
Назначение:    Заполнение дерева содержимым пакета
Вх. параметры:
  AVST: TVirtualStringTree - компонент дерева, которое необходимо заполнить
  APackage: TPackage       - объект пакета
------------------------------------------------------------------------------}
procedure TfmMain.PackageFillVST(AVST: TVirtualStringTree; APackage: TPackage);
var
  Sections: TStringList;
  SectionItems: TStringList;
  keySection: Integer;
  keyItem: Integer;
  Path: String;
  ptrRootNode: PVirtualNode;
  ptrChildNode: PVirtualNode;
begin
  AVST.Clear;
  Sections := TStringList.Create;
  SectionItems := TStringList.Create;
  APackage.ReadSections(Sections);
  for keySection := 0 to Sections.Count - 1 do
  begin
    if Sections[keySection] = 'PackageInfo' then
      continue;
    ptrRootNode := AddNode(AVST, nil, Sections[keySection]);
    APackage.ReadSection(Sections[keySection], SectionItems);
    if SectionItems.Count > 0 then
    begin
      for keyItem := 0 to SectionItems.Count - 1 do
      begin
        Path := APackage.ReadString(Sections[keySection], SectionItems[keyItem], '');
        AddNode(AVST, ptrRootNode, SectionItems[keyItem], Path);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.ReloadPackagesList()
Назначение: Обновление выпадающих меню пакетов при создании окна
------------------------------------------------------------------------------}
procedure TfmMain.FormCreate(Sender: TObject);
begin
  ReloadPackagesList(ptSoft);
  ReloadPackagesList(ptTools);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.btnSoftPkgReloadClick()
Назначение: Обрабока клика по кнопке обновления выпадающего списка пакетов программ
------------------------------------------------------------------------------}
procedure TfmMain.btnSoftPkgReloadClick(Sender: TObject);
begin
  ReloadPackagesList(ptSoft);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.btnToolsPkgReloadClick()
Назначение: Обрабока клика по кнопке обновления выпадающего списка пакетов утилит
------------------------------------------------------------------------------}
procedure TfmMain.btnToolsPkgReloadClick(Sender: TObject);
begin
  ReloadPackagesList(ptTools);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.vstSoftPkgContentsGetText()
Назначение: Отображение имён программ в нодах дерева программ
------------------------------------------------------------------------------}
procedure TfmMain.vstSoftPkgContentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  SetNodeText(vstSoftPkgContents, Node, Column, TextType, CellText);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.vstSoftPkgContentsGetText()
Назначение: Отображение имён утилит в нодах дерева утилит
------------------------------------------------------------------------------}
procedure TfmMain.vstToolsPkgContentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  SetNodeText(vstToolsPkgContents, Node, Column, TextType, CellText);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.vstSoftPkgContentsInitNode()
Назначение: Отображение отметок в нодах дерева программ
------------------------------------------------------------------------------}
procedure TfmMain.vstSoftPkgContentsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  //Sender.CheckType[Node] := ctTriStateCheckBox;
  case (Sender.GetNodeLevel(Node)) of
    0: Node^.CheckType := ctTriStateCheckBox;
    1: Node^.CheckType := ctCheckBox;
  end;
end;

{------------------------------------------------------------------------------
Процедура:     TfmMain.ReloadPackagesList()
Назначение:    Перезагрузка пакетов и заполнение выпадающих списков пакетов
Вх. параметры: APackageType: TPackageType - тип списка пакета
------------------------------------------------------------------------------}
procedure TfmMain.ReloadPackagesList(APackageType: TPackageType);
var
  LastIndex: Integer;
  Key: Integer;
  PackageList: TPackageList;
  Package: TPackage;
  Combo: TComboBox;
begin
  case (APackageType) of
    ptSoft:
    begin
      Combo := cmbSoftPkgSelect;
      PackageList := dmMain.SoftPackages;
    end;
    ptTools:
    begin
      Combo := cmbToolsPkgSelect;
      PackageList := dmMain.ToolsPackages;
    end;
    else raise Exception.Create('TfmMain.ReloadPackagesList(): передан неизвестный тип пакета');
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
      Package := PackageList[Key];
      Combo.Items.Add(Package.Name + ' (' + ExtractFileName(Package.FileName) + ')');
    end;
  end;
  if Combo.Items.Count <= LastIndex then
    LastIndex := Combo.Items.Count - 1;
  Combo.ItemIndex := LastIndex;
  SetActivePackage(APackageType, LastIndex);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.cmbSoftPkgSelectChange()
Назначение: Смена пакета программ через выпадающий список
------------------------------------------------------------------------------}
procedure TfmMain.cmbSoftPkgSelectChange(Sender: TObject);
begin
  SetActivePackage(ptSoft, cmbSoftPkgSelect.ItemIndex);
end;

{------------------------------------------------------------------------------
Процедура:  TfmMain.cmbSoftPkgSelectChange()
Назначение: Смена пакета утилит через выпадающий список
------------------------------------------------------------------------------}
procedure TfmMain.cmbToolsPkgSelectChange(Sender: TObject);
begin
  SetActivePackage(ptTools, cmbToolsPkgSelect.ItemIndex);
end;

end.
