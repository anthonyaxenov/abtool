unit uPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TPackageType }

  // Перечисление типов пакета
  TPackageType = (
    // Пакет программ для установки
    ptSoft,
    // Пакет утилит
    ptTools,
    // Неизвестный пакет
    ptUnknown
    );

  { TPackage }

  // Класс пакета
  TPackage = class(TIniFile)
  strict private
    // Имя пакета: [PackageInfo] Name
    FName: String;
    // Описание пакета: [PackageInfo] Description
    FDescription: String;
    // Тип пакета
    FType: TPackageType;
  private
    // Установка информации о пакете
    procedure SetProperties();
    // Определение типа пакета по имени его файла
    function SetType(): TPackageType;
  public
    // Имя пакета: [PackageInfo] Name
    property Name: String read FName;
    // Описание пакета: [PackageInfo] Description
    property Description: String read FDescription;
    // Тип пакета
    property PackageType: TPackageType read FType;

    // Создание объекта пакета и установка информации о нём
    constructor Create(APkgFilename: Ansistring); overload;
    // Деструктор
    destructor Destroy(); override;

    // Запуск исполняемого файла и получение результатов запуска
    function ExecuteItem(const AFileName, AParams: String; AHideMainWindow: Boolean;
      Out AOutExitcode: Cardinal): Boolean;
  end; // TPackage

implementation

{ TPackage }

{------------------------------------------------------------------------------
Конструктор:   TPackage.Create()
Назначение:    Создание объекта пакета и установка информации о нём
Вх. параметры: APkgFilename: Ansistring - путь к ini-файлу
------------------------------------------------------------------------------}
constructor TPackage.Create(APkgFilename: Ansistring);
begin
  inherited Create(APkgFilename, [ifoStripComments, ifoStripInvalid]);
  SetProperties();
end;

{------------------------------------------------------------------------------
Процедура:     TPackage.SetType()
Назначение:    Установка информации о пакете
------------------------------------------------------------------------------}
procedure TPackage.SetProperties();
begin
  FName := ReadString('PackageInfo', 'Name', '<без названия>');
  FDescription := ReadString('PackageInfo', 'Description', '<без описания>');
  FType := SetType();
end;

{------------------------------------------------------------------------------
Функция:       TPackage.SetType()
Назначение:    Определение типа пакета по имени его файла
Возвращает:    TPackageType - тип пакета
------------------------------------------------------------------------------}
function TPackage.SetType(): TPackageType;
var
  SubStr: String;
begin
  SubStr := Copy(ExtractFileName(FileName), 1, 4);
  if (LowerCase(SubStr) = 'soft') then
    Result := ptSoft
  else if (LowerCase(SubStr) = 'tool') then
    Result := ptTools
  else
    Result := ptUnknown;
end;

{------------------------------------------------------------------------------
Деструктор:    TPackage.Destroy()
------------------------------------------------------------------------------}
destructor TPackage.Destroy();
begin
  inherited Destroy();
end;

{------------------------------------------------------------------------------
Функция:          TPackage.ExecuteItem()
Назначение:       Запуск исполняемого файла и получение результатов запуска
Вх. параметры:
  AFileName       : String - путь к исполняемому файлу
  AParams         : String - дополнительные параметры запуска
  AHideMainWindow : Boolean - скрывать главное окно ABTool (true) или нет (false)
Вых. параметры:
  AOutExitcode    : Cardinal - код завершения процесса
Возвращает:       boolean - true при успешном запуске программы, false при неудаче
------------------------------------------------------------------------------}
function TPackage.ExecuteItem(const AFileName, AParams: String; AHideMainWindow: Boolean;
  out AOutExitcode: Cardinal): Boolean;
begin

end;




end.



