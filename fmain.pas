unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, ActnList, EditBtn, Menus, VirtualTrees, Types,
  uFileUtils, uPackage, uPackageUtils;

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
    procedure btnSoftPkgReloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ReloadSoftPackagesList();
  public

  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.btnSoftPkgReloadClick(Sender: TObject);
begin
  ReloadSoftPackagesList;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ReloadSoftPackagesList;
end;


procedure TfmMain.ReloadSoftPackagesList();
begin
  cmbSoftPkgSelect.Items.AddStrings(GetPackagesIniFileList(ptSoft, False), True);
  cmbSoftPkgSelect.ItemIndex := 0;
end;

end.

