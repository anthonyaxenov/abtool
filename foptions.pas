unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, StdCtrls, Menus, Types;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    btnSetDefault: TButton;
    btnSave: TButton;
    btnClose: TButton;
    btnCheckUpdates: TButton;
    btnChangelog: TButton;
    btnBugReport: TButton;
    btnOnlineHelp: TButton;
    btnLicense: TButton;
    cbChkExitCodes: TCheckBox;
    cbDntUnChkItems: TCheckBox;
    cbExpandSoft: TCheckBox;
    cbExpandTools: TCheckBox;
    cbShowProgress: TCheckBox;
    cbColorSoftTree: TCheckBox;
    cbOnTop: TCheckBox;
    cbShowPkgDesc: TCheckBox;
    cbQuietREG: TCheckBox;
    cbHideOnTool: TCheckBox;
    cbDrawOutlines: TCheckBox;
    cbScreenSnap: TCheckBox;
    grpOptMainform: TGroupBox;
    grpOptInstall: TGroupBox;
    grpOptTools: TGroupBox;
    imgLogo: TImage;
    labAboutTitle: TLabel;
    labCopyright: TLabel;
    labDescription: TLabel;
    mmLicenseText: TMemo;
    PageControl: TPageControl;
    tabCommon: TTabSheet;
    tabPackages: TTabSheet;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tabAboutContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private

  public

  end;

var
  fmOptions: TfmOptions;

implementation

{$R *.lfm}

{ TfmOptions }

procedure TfmOptions.FormCreate(Sender: TObject);
begin

end;

procedure TfmOptions.btnCloseClick(Sender: TObject);
begin
  Self.Destroy;
end;

procedure TfmOptions.tabAboutContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

end.

