unit uFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uPackage;

// Получение списка файлов в директории
function GetFileList(APath, AFileMask: String; AFullPaths: Boolean = True): TStringList;

// Получение списка всех файлов внутри директории ABTool
function GetABToolFileList(ASubDir, AFileMask: String; AFullPaths: Boolean = True): TStringList;

// Получение списка всех файлов пакетов
function GetPackagesIniFileList(APackageType: TPackageType; AFullPaths: Boolean = True): TStringList;

// Получение списка всех файлов локализаций
function GetLocalesIniFileList(AFullPaths: Boolean = True): TStringList;

implementation

{------------------------------------------------------------------------------
Функция: GetFileList
Назначение: Получение списка файлов в директории
Вх. параметры:
  APath: string - путь к директории, список файлов которой нужно получить
  AFileMask: string - маска файлов, по которой нужно отфильтровать список файлов
  AFullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов
------------------------------------------------------------------------------}
function GetFileList(APath, AFileMask: String; AFullPaths: Boolean = True): TStringList;
var
  SearchRec: TSearchRec;
  ResultLines: TStringList;
begin
  ResultLines := TStringList.Create;
  APath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(APath + AFileMask, faNormal, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr <> faDirectory) then
      begin
        if AFullPaths then
          ResultLines.Add(APath + SearchRec.Name)
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
Назначение: Получение списка всех файлов внутри директории ABTool\
Вх. параметры:
  ASubDir: string - имя директории внутри ABTool\
  AFileMask: string - маска файлов, по которой нужно отфильтровать список файлов
  AFullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов пакетов
------------------------------------------------------------------------------}
function GetABToolFileList(ASubDir, AFileMask: String; AFullPaths: Boolean = True): TStringList;
begin
  Result := GetFileList(ExtractFilePath(ParamStr(0)) + 'ABTool\' + ASubDir, AFileMask, AFullPaths);
end;

{------------------------------------------------------------------------------
Функция: GetPackagesIniFileList
Назначение: Получение списка всех файлов пакетов
Вх. параметры:
  APackageType: TPackageType - тип пакета: ptSoft либо ptTools
  AFullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов пакетов
Исключения:
  TException - при попытке передать любой другой тип пакета
------------------------------------------------------------------------------}
function GetPackagesIniFileList(APackageType: TPackageType; AFullPaths: Boolean = True): TStringList;
begin
  case (APackageType) of
    ptSoft: Result := GetABToolFileList('Packages', 'soft.*.ini', AFullPaths);
    ptTools: Result := GetABToolFileList('Packages', 'tools.*.ini', AFullPaths);
    else raise Exception.Create('GetPackagesIniFileList(): передан неверный тип пакета');
  end;
end;
  
{------------------------------------------------------------------------------
Функция: GetPackagesIniFileList
Назначение: Получение списка всех файлов локализаций
Вх. параметры:
  AFullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает:
  TStringList - список строк с полными путями или только с именами файлов пакетов
Исключения:
  TException - при попытке передать ptUnknown
------------------------------------------------------------------------------}
function GetLocalesIniFileList(AFullPaths: Boolean = True): TStringList;
begin
  Result := GetABToolFileList('Languages', '*.lng', AFullPaths);
end;

end.
