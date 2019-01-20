unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, StdCtrls, Menus;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    imgLogo: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmAbout: TMemo;
    PageControl: TPageControl;
    tabCommon: TTabSheet;
    tabUI: TTabSheet;
    tabPackages: TTabSheet;
    tabAbout: TTabSheet;
    procedure FormCreate(Sender: TObject);
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

end.

