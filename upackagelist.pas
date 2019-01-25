unit uPackageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFileUtils, uPackage;

type

  { TPackageList }

  // Класс списка пакетов
  TPackageList = class
  private
    // Тип списка пакетов
    FType: TPackageType;
    // Список пакетов
    FPackages: array of TPackage;    
    // Получение пакета из списка по его номеру
    function GetPackageByIndex(Index: integer): TPackage;
  public
    // Тип списка пакетов
    property PackageType: TPackageType read FType;
    // Список пакетов
    property Packages[Index: Integer]: TPackage read GetPackageByIndex; default;

    // Создание списка пакетов по указанному типу
    procedure Load(APackageType: TPackageType);
    // Заполнение списка пакетов объектами пакетов
    procedure Reload();                         
    // Получение количества пакетов в списке
    function Count(): integer;
  end; // TPackageList

implementation

{ TPackageList }

{------------------------------------------------------------------------------
Процедура:     TPackageList.Load()
Назначение:    Создание списка пакетов по указанному типу
Вх. параметры: APackageType: TPackageType - тип списка пакета
------------------------------------------------------------------------------}
procedure TPackageList.Load(APackageType: TPackageType);
begin
  FType := APackageType;
  Reload();
end;

{------------------------------------------------------------------------------
Процедура: TPackageList.Reload()
Назначение: Заполнение списка пакетов объектами пакетов
------------------------------------------------------------------------------}
procedure TPackageList.Reload();
var
  FileList: TStringList;
  FileMask: String;
  Key: Integer;
begin
  FPackages := nil;
  case (FType) of
    ptSoft: FileMask := 'soft.*.ini';
    ptTools: FileMask := 'tools.*.ini';
    ptUnknown: raise Exception.Create('TPackageList.Reload(): тип не может быть ptUnknown');
  end;
  FileList := GetABToolFileList('Packages', FileMask);
  for Key := 0 to FileList.Count - 1 do
  begin
    SetLength(FPackages, Length(FPackages)+1);
    FPackages[Key] := TPackage.Create(FileList[Key]);
  end;
end;

{------------------------------------------------------------------------------
Функция:    TPackage.GetPackageByIndex()
Назначение: Получение пакета из списка по его номеру
Возвращает: TPackage - пакет
------------------------------------------------------------------------------}
function TPackageList.GetPackageByIndex(Index: integer): TPackage;
begin
   Result := FPackages[Index];
end;

{------------------------------------------------------------------------------
Функция:    TPackage.Count()
Назначение: Получение количества пакетов в списке
Возвращает: integer - число пакетов в списке
------------------------------------------------------------------------------}
function TPackageList.Count(): integer;
begin
   Result := Length(FPackages);
end;

end.

