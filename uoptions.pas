unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IniFiles, uFileUtils;

type
  // Класс настроек

  { TOptions }

  TOptions = class(TIniFile)
  strict private
    // Файл локализации
    FLocale: string;

    // Прилипание к краям экрана
    FScreenSnap: Boolean;
    // Показывать описания пакетов
    FShowPkgDesc: Boolean;
    // Рисовать в деревьях линии уровней вложенности
    FDrawOutlines: Boolean;
    // Поверх всех окон
    FOnTop: Boolean;

    // Немедленно сообщать о коде возврата <> 0
    FChkExitCodes: Boolean;
    // Не снимать отметку с чекбокса при коде возврата <> 0
    FDntUnChkItems: Boolean;
    // Раскрашивать список программ после установки
    FColorSoftTree: Boolean;
    // Отображать общий ход установки программ
    FShowProgress: Boolean;
    // Тихий импорт REG-файлов
    FQuietREG: Boolean;
    // Раскрывать дерево после загрузки пакета
    FExpandSoft: Boolean;

    // Скрывать главное окно при запуске утилит
    FHideOnTool: Boolean;
    // Раскрывать дерево после загрузки пакета
    FExpandTools: Boolean;

    // Последняя позиция по горизонтали от левого края экрана
    FLeft: Integer;
    // Последняя позиция по вертикали от верхнего края экрана
    FTop: Integer;
    // Последняя ширина
    FWidth: Integer;
    // Последняя высота
    FHeight: Integer;

    // Параметры, с которыми запушена ABTool
    FCLParams: TStringList;
  private
    // Установка параметра локализации
    procedure SetLocale(ALngFilename: string);
  public
    // Файл локализации
    property Locale: string read FLocale write SetLocale;

    // Прилипание к краям экрана
    property ScreenSnap: Boolean read FScreenSnap write FScreenSnap default True;
    // Показывать описания пакетов
    property ShowPkgDesc: Boolean read FShowPkgDesc write FShowPkgDesc default True;
    // Рисовать в деревьях линии уровней вложенности
    property DrawOutlines: Boolean read FDrawOutlines write FDrawOutlines default True;
    // Поверх всех окон
    property OnTop: Boolean read FOnTop write FOnTop default False;

    // Немедленно сообщать о коде возврата <> 0
    property ChkExitCodes: Boolean read FChkExitCodes write FChkExitCodes default True;
    // Не снимать отметку с чекбокса при коде возврата <> 0
    property DntUnChkItems: Boolean read FDntUnChkItems write FDntUnChkItems default True;
    // Раскрашивать список программ после установки
    property ColorSoftTree: Boolean read FColorSoftTree write FColorSoftTree default True;
    // Отображать общий ход установки программ
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;
    // Тихий импорт REG-файлов
    property QuietREG: Boolean read FQuietREG write FQuietREG default True;
    // Раскрывать дерево после загрузки пакета
    property ExpandSoft: Boolean read FExpandSoft write FExpandSoft default True;

    // Скрывать главное окно при запуске утилит
    property HideOnTool: Boolean read FExpandSoft write FExpandSoft default False;
    // Раскрывать дерево после загрузки пакета
    property ExpandTools: Boolean read FExpandSoft write FExpandSoft default True;

    // Последняя позиция по горизонтали от левого края экрана
    property Left: Integer read FLeft write FLeft;
    // Последняя позиция по вертикали от верхнего края экрана
    property Top: Integer read FTop write FTop;
    // Последняя ширина
    property Width: Integer read FWidth write FWidth default 350;
    // Последняя высота
    property Height: Integer read FHeight write FHeight default 450;

    // Параметры, с которыми запушена ABTool
    property CLParams: TStringList read FCLParams;
    // Пути ко всем доступным файлам локализаций
    function ReadAvailableLocales(AFullPaths: Boolean = True): TStringList;

    // Создание объекта настроек и установка настроек
    constructor Create(AIniFilename: string); overload;
    // Деструктор объекта настроек
    destructor Destroy(); override; 
    // Определение наличия доп. ключей запуска
    function HasCLParams: Boolean;
    // Установка параметров командной строки, с которыми запущен ABTool
    procedure ReadCLParams;
    // Поиск ключа запуска среди всех, с которыми запущен ABTool
    function FindCLParam(AParamName: string): Boolean;     
    // Чтение настроек
    procedure Load;

  end; // TOptions
             
var
  // Настройки - глобальный объект, доступный всем модулям, сославшимся на этот
  GlobalOptions: TOptions;

implementation

{ TOptions }
                            
{------------------------------------------------------------------------------
Конструктор: TOptions.Create()
Назначение:  Создание объекта настроек и установка настроек
------------------------------------------------------------------------------}
constructor TOptions.Create(AIniFilename: string);
begin
  if HasCLParams then
    ReadCLParams;          
  //ReadAvailableLocales;
  inherited Create(AIniFilename, [ifoStripComments, ifoStripInvalid]);
  Load;
  GlobalOptions := Self;
end;
                    
{------------------------------------------------------------------------------
Деструктор: TOptions.Destroy()
------------------------------------------------------------------------------}
destructor TOptions.Destroy();
begin
  inherited Destroy();
end;

{------------------------------------------------------------------------------
Функция:    TOptions.HasCLParams()
Назначение: Определение наличия доп. ключей запуска
Возвращает: boolean
------------------------------------------------------------------------------}
function TOptions.HasCLParams: Boolean;
begin
  Result := ParamCount > 0;
  //if FDebug then
  //  Log('DoesABToolHaveKeys(): '+BoolToStr(Result));
end;

{------------------------------------------------------------------------------
Процедура:  TOptions.ReadCLParams()
Назначение: Установка параметров командной строки, с которыми запущен ABTool
------------------------------------------------------------------------------}
procedure TOptions.ReadCLParams;
var
  i: byte;
begin
  if HasCLParams then
  begin
    FCLParams := TStringList.Create;
    for i := 1 to ParamCount do
      FCLParams.Add(Trim(LowerCase(ParamStr(i))));
  end;
end;
     
{------------------------------------------------------------------------------
Функция:    TOptions.FindCLParam()
Назначение: Поиск ключа запуска среди всех, с которыми запущен ABTool
Возвращает: boolean
------------------------------------------------------------------------------}
function TOptions.FindCLParam(AParamName: string): Boolean;
var
  i: Smallint;
begin
  Result := False;
  if HasCLParams then
  begin
    for i := 0 to FCLParams.Count - 1 do
      if Pos(LowerCase(AParamName), FCLParams.Strings[i]) > 0 then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TOptions.Load()
Назначение: Чтение настроек
------------------------------------------------------------------------------}
procedure TOptions.Load;
begin
  try
    //TODO создание дефолтного файла настроек при отсутсвии такового
    //TODO создание файла настроек по умолчанию
    SetLocale(ReadString('Options', 'Locale', ''));

    FScreenSnap := ReadBool('Options', 'ScreenSnap', True);
    FShowPkgDesc := ReadBool('Options', 'ShowPkgDesc', True);
    FDrawOutlines := ReadBool('Options', 'DrawOutlines', True);
    FOnTop := ReadBool('Options', 'OnTop', False);

    FChkExitCodes := ReadBool('Options', 'ChkExitCodes', False);
    FDntUnChkItems := ReadBool('Options', 'DntUnChkItems', True);
    FColorSoftTree := ReadBool('Options', 'ColorSoftTree', True);
    FShowProgress := ReadBool('Options', 'ShowProgress', True);
    FQuietREG := ReadBool('Options', 'QuietREG', False);
    FExpandSoft := ReadBool('Options', 'ExpandSoft', True);

    FHideOnTool := ReadBool('Options', 'HideOnTool', False);
    FExpandTools := ReadBool('Options', 'ExpandTools', True);

    FLeft := ReadInteger('Options', 'Left', (Screen.Width div 2) - 175);
    FTop := ReadInteger('Options', 'Top', (Screen.Height div 2) - 225);
    FWidth := ReadInteger('Options', 'Width', 350);
    FHeight := ReadInteger('Options', 'Height', 450);
  except // SysErrorMessage(GetLastError)
    On E: Exception do
    begin
      raise Exception.Create('Не удалось прочитать настройки из файла abtool.ini.');
        //+ #13#10 +
        //'Сейчас он будет создан с параметрами по умолчанию.' + #13#10 +
        //'Сообщение системы: ' + #13#10 + E.Message + '(' + IntToStr(GetLastError) + ') ');
      //SaveOptions(true);
    end;
  end;
end;

{------------------------------------------------------------------------------
Процедура:  TOptions.SetLocale()
Назначение: Установка параметра локализации
------------------------------------------------------------------------------}
procedure TOptions.SetLocale(ALngFilename: string);
begin
  //TODO проверка существования файла локализации
  FLocale := ALngFilename;
end;
    
{------------------------------------------------------------------------------
Процедура:  TOptions.ReadAvailableLocales()
Назначение: Получение списка всех доступных файлов локализаций
Вх. параметры:
  FullPaths: boolean - возвращать полные пути (true) или только имена
    файлов (false). По умолчанию true.
Возвращает: TStringList
------------------------------------------------------------------------------}
function TOptions.ReadAvailableLocales(AFullPaths: Boolean = True): TStringList;
begin
  Result := GetLocalesIniFileList(AFullPaths);
end;

{
// Чтение настроек
function LoadOptions: boolean;
begin
  Result := false;
  if Options.Debug then
    Log('LoadOptions()');
  Log('Загрузка настроек из abtool.ini...');
  if not (FileExists(ABToolIniPath + 'abtool.ini')) then begin
    Log('Файла настроек не существует. Сейчас он будет создан с параметрами по умолчанию...');
    SaveOptions(true);
  end  // if FileExists
  else begin
    try
      try
        with Options do begin
          ABToolIniFile := TInifile.Create(ABToolIniPath + 'abtool.ini');
          ABToolKeys := TStringList.Create;
          ABToolKeys := GetABToolKeys;
          if Options.Debug then
            Log('LoadOptions(): ABToolKeys='+GetABToolKeys.text);
          // Непосредственно чтение настроек из INI в память
          if FindCLParam('+lang') then begin
            LangFile := ReadString('Options','LangFile','');
            if Options.Debug then
              Log('LoadOptions(): LangFile='+LangFile);
          end; // if +lang
          Left       := ReadInteger('Options','Left', (Screen.Width div 2) - 180);
          Top        := ReadInteger('Options','Top', (Screen.Height div 2) - 292);
          Width      := ReadInteger('Options','Width', 360);
          Height     := ReadInteger('Options','Height', 485);
          ChkExitCodes  := ReadBool('Options','ChkExitCodes',false);
          DntUnChkItems := ReadBool('Options','DntUnChkItems',true);
          ColorSoftTree := ReadBool('Options','ColorSoftTree',true);
          ShowProgress  := ReadBool('Options','ShowProgress',true);
          ExpandSoft    := ReadBool('Options','ExpandSoft',true);
          QuietREG      := ReadBool('Options','QuietREG',false);
          HideOnTool    := ReadBool('Options','HideOnTool',false);
          ExpandTools   := ReadBool('Options','ExpandTools',true);
          ShowLog       := ReadBool('Options','ShowLog',true);
          DrawOutlines  := ReadBool('Options','DrawOutlines',true);
          ScreenSnap    := ReadBool('Options','ScreenSnap',true);
          OnTop         := ReadBool('Options','OnTop',false);
          ShowPkgDesc   := ReadBool('Options','ShowPkgDesc',true);
          SrchVisible   := ReadBool('Options','SrchVisible',false);
          ScrollLog     := ReadBool('Options','ScrollLog',true);
          LastSoftPkg   := ReadString('Options','LastSoftPkg', '');
          LastToolPkg   := ReadString('Options','LastToolPkg', '');
          if Options.Debug then begin
            Log('LoadOptions(): Left='+Left.ToString);
            Log('LoadOptions(): Top='+Top.ToString);
            Log('LoadOptions(): Width='+Width.ToString);
            Log('LoadOptions(): Height='+Height.ToString);
            Log('LoadOptions(): ChkExitCodes='+BoolToStr(ChkExitCodes));
            Log('LoadOptions(): DntUnChkItems='+BoolToStr(DntUnChkItems));
            Log('LoadOptions(): ColorSoftTree='+BoolToStr(ColorSoftTree));
            Log('LoadOptions(): ShowProgress='+BoolToStr(ShowProgress));
            Log('LoadOptions(): ExpandSoft='+BoolToStr(ExpandSoft));
            Log('LoadOptions(): QuietREG='+BoolToStr(QuietREG));
            Log('LoadOptions(): HideOnTool='+BoolToStr(HideOnTool));
            Log('LoadOptions(): ExpandTools='+BoolToStr(ExpandTools));
            Log('LoadOptions(): DrawOutlines='+BoolToStr(DrawOutlines));
            Log('LoadOptions(): ScreenSnap='+BoolToStr(ScreenSnap));
            Log('LoadOptions(): OnTop='+BoolToStr(OnTop));
            Log('LoadOptions(): ShowPkgDesc='+BoolToStr(ShowPkgDesc));
            Log('LoadOptions(): SrchVisible='+BoolToStr(SrchVisible));
            Log('LoadOptions(): ScrollLog='+BoolToStr(ScrollLog));
            Log('LoadOptions(): LastSoftPkg='+LastSoftPkg);
            Log('LoadOptions(): ScrollLog='+LastToolPkg);
          end;
        end; // with Options
        Result := true;
      except // SysErrorMessage(GetLastError)
        On E: Exception Do Begin
          Raise Exception.Create('Не удалось прочитать настройки из файла abtool.ini.' + #13#10 +
              'Сейчас он будет создан с параметрами по умолчанию.' + #13#10 +
              'Сообщение системы: ' + #13#10 + E.Message + '(' + IntToStr(GetLastError) + ') ');
          Log('Не удалось прочитать настройки из файла abtool.ini.' + #13#10 +
              'Сейчас он будет создан с параметрами по умолчанию.' + #13#10 +
              'Сообщение системы: ' + #13#10 + E.Message + '(' + IntToStr(GetLastError) + ') ');
          if Options.Debug then
            Log('LoadOptions() EXCEPTION Stack Trace:' + #13#10 + E.StackTrace);
          SaveOptions(true);
        End; // On E: Exception
      end; // except
    finally
//      if Options.ABToolIniFile <> nil then
//        Options.Free;
    end; // finally
  end; // if not FileExists
end;
















}
end.
