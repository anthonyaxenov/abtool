unit uPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TPackageType }

  TPackageType = (ptSoft, ptTools, ptUnknown);

  { TPackage }

  TPackage = class//(TCustomIniFile)
  strict private
    FName: String;
    FDescription: String;
    FType: TPackageType;
  private
  public
    IniFile: TIniFile;
    Groups: TStringList;
    //--------
    property Name: String read FName;
    property Description: String read FDescription;
    property PackageType: TPackageType read FType;
    //--------                                                    
    constructor Create(APkgFilename: String);
    destructor Destroy(); override;
    function ExecuteItem(const AFileName, AParams: String;
      AHideMainWindow: Boolean; Out AOutExitcode: Cardinal): Boolean;
  end; // TPackage

implementation

{ TPackage }

constructor TPackage.Create(APkgFilename: String);
begin

end;

destructor TPackage.Destroy();
begin
  inherited Destroy();
end;


function TPackage.ExecuteItem(const AFileName, AParams: String;
  AHideMainWindow: Boolean; out AOutExitcode: Cardinal): Boolean;
begin

end;




end.












