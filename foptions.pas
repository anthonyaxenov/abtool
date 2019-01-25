unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Menus, Buttons, uOptions;

type

  { TfmOptions }

  TfmOptions = class(TForm)
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
    cmbLanguage: TComboBox;
    grpOptMainform: TGroupBox;
    grpOptInstall: TGroupBox;
    grpOptTools: TGroupBox;
    imgLogo: TImage;
    labAboutTitle: TLabel;
    labCopyright: TLabel;
    labDescription: TLabel;
    Label1: TLabel;
    mmLicenseText: TMemo;
    PageControl: TPageControl;
    pnLanguage: TPanel;
    btnSetDefault: TSpeedButton;
    tabCommon: TTabSheet;
    tabPackages: TTabSheet;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure ResetControlls;
  public

  end;

var
  fmOptions: TfmOptions;

implementation

{$R *.lfm}

{ TfmOptions }

{------------------------------------------------------------------------------
Процедура:  TfmOptions.btnSaveClick()
Назначение: Закрытие формы настроек и возврат модального результата при клике
  по кнопке "Сохранить"
Описание: Закрытие формы происходит автоматически благодаря свойству
  ModalResult = mrOK, заданному кнопке в инспекторе объектов.
------------------------------------------------------------------------------}
procedure TfmOptions.btnSaveClick(Sender: TObject);
begin
  //TODO сохранение настроек
end;

{------------------------------------------------------------------------------
Процедура:  TfmOptions.FormClose()
Назначение: Закрытие формы при её закрытии через кнопку в заголовке и возврат
  модального результата
------------------------------------------------------------------------------}
procedure TfmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ModalResult := mrClose;
end;
    
{------------------------------------------------------------------------------
Процедура:  TfmOptions.FormShow()
Назначение: Показ окна настроек
Описание: Форма настроек создаётся однажды при её первом вызове. При закрытии
  она не уничтожается. Она может быть закрыта без сохранения настроек. При
  повторном открытии окна контроллы должны принять состояние, соответствущее
  текущим (при запуске или прошлом сохранении) настройкам. В противном случае,
  повторное открытие окна отобразит прошлые несохранённые изменения - это
  введёт пользователя в заблуждение и он примет эти настройки как текущие.
------------------------------------------------------------------------------}
procedure TfmOptions.FormShow(Sender: TObject);
begin
  ResetControlls;
end;
          
{------------------------------------------------------------------------------
Процедура:  TfmOptions.ResetControlls()
Назначение: Сброс состояния контроллов в сооответствии с текущими настройками
------------------------------------------------------------------------------}
procedure TfmOptions.ResetControlls;
begin
  cmbLanguage.Items := GlobalOptions.ReadAvailableLocales(false);

  cbScreenSnap.Checked := GlobalOptions.ScreenSnap;     
  cbShowPkgDesc.Checked := GlobalOptions.ShowPkgDesc;
  cbDrawOutlines.Checked := GlobalOptions.DrawOutlines;
  cbOnTop.Checked := GlobalOptions.OnTop;

  cbChkExitCodes.Checked := GlobalOptions.ChkExitCodes;
  cbDntUnChkItems.Checked := GlobalOptions.DntUnChkItems;
  cbColorSoftTree.Checked := GlobalOptions.ColorSoftTree;
  cbShowProgress.Checked := GlobalOptions.ShowProgress;
  cbQuietREG.Checked := GlobalOptions.QuietREG;
  cbExpandSoft.Checked := GlobalOptions.ExpandSoft;
                                             
  cbHideOnTool.Checked := GlobalOptions.HideOnTool;
  cbExpandTools.Checked := GlobalOptions.ExpandTools;
end;

end.

