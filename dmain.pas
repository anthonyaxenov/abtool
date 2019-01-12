unit dMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Menus;

type

  { TMainDM }

  TMainDM = class(TDataModule)
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
  private

  public

  end;

var
  MainDM: TMainDM;

implementation

{$R *.lfm}

{ TMainDM }


end.

