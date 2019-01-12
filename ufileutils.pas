unit uFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Types, uPackage;

function GetFileList(Path, FileMask: String; FullPaths: Boolean = True): TStringList;
function GetABToolFileList(SubDir, FileMask: String;
  FullPaths: Boolean = True): TStringList;
function GetPackagesIniFileList(PackageType: TPackageType;
  FullPaths: Boolean = True): TStringList;

implementation

{------------------------------------------------------------------------------
Функция: GetFileList
Назначение: Получает список файлов в директории
Вх. параметры:
  Path: string - путь к директории, список файлов которой нужно получить
  FileMask: string - маска файлов, по которой нужно отфильтровать список файлов
  FullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов
------------------------------------------------------------------------------}
function GetFileList(Path, FileMask: String; FullPaths: Boolean = True): TStringList;
var
  SearchRec: TSearchRec;
  ResultLines: TStringList;
begin
  ResultLines := TStringList.Create;
  Path := IncludeTrailingPathDelimiter(Path);
  if FindFirst(Path + FileMask, faNormal, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr <> faDirectory) then
      begin
        if FullPaths then
          ResultLines.Add(Path + SearchRec.Name)
        else
          ResultLines.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  Result := ResultLines;
end;

{------------------------------------------------------------------------------
Функция: GetABToolFileList
Назначение: Получает список всех файлов внутри директории ABTool\
Вх. параметры:
  SubDir: string - имя директории внутри ABTool\
  FileMask: string - маска файлов, по которой нужно отфильтровать список файлов
  FullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов пакетов
------------------------------------------------------------------------------}
function GetABToolFileList(SubDir, FileMask: String;
  FullPaths: Boolean = True): TStringList;
begin
  Result := GetFileList(ExtractFilePath(ParamStr(0)) + 'ABTool\' +
    SubDir, FileMask, FullPaths);
end;

{------------------------------------------------------------------------------
Функция: GetPackagesIniFileList
Назначение: Получает список всех пакетов
Вх. параметры:
  PackageType: TPackageType - тип пакета: ptSoft либо ptTools
  FullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов пакетов
Исключения:
  TException - при попытке передать ptUnknown
------------------------------------------------------------------------------}
function GetPackagesIniFileList(PackageType: TPackageType;
  FullPaths: Boolean = True): TStringList;
begin
  case (PackageType) of
    ptSoft: Result := GetABToolFileList('Packages', 'soft.*.ini', FullPaths);
    ptTools: Result := GetABToolFileList('Packages', 'tools.*.ini', FullPaths);
    ptUnknown: raise Exception.Create(
        'GetPackagesIniFileList(): передан ptUnknown');
  end;
end;




end.
