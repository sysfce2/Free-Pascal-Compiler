{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Michael Van Canneyt

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    FileSystem aware compiler descendent. No support for PCU.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit Pas2JSFSCompiler;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  Pascal.UseAnalyzer,
  Pas2Js.SrcMap,
  Pas2Js.Files.Cache, Pas2Js.Compiler.Base,
  Pas2Js.Files.Fs,
  Pas2Js.Files.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils,
  PasUseAnalyzer,
  FPPJsSrcMap,
  Pas2jsFileCache, Pas2jsCompiler,
  Pas2JSFS,
  Pas2jsFileUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TPas2jsFSCompiler }

  TPas2jsFSCompiler = Class(TPas2JSCompiler)
  private
    function GetFileCache: TPas2jsFilesCache;
    function OnMacroEnv(Sender: TObject; var Params: string; Lvl: integer): boolean;
  Protected
    function CreateJSMapper: TPas2JSMapper; override;
    function OnJSMapperIsBinary(Sender: TObject; const aFilename: string): boolean; virtual;
  Public
    Procedure SetWorkingDir(const aDir: String); override;
    function CreateSetOfCompilerFiles(keyType: TKeyCompareType): TPasAnalyzerKeySet; override;
    Function CreateFS : TPas2JSFS; override;
    Procedure InitParamMacros; override;
    Property FileCache : TPas2jsFilesCache Read GetFileCache;
  end;

implementation

{$IFDEF PAS2JS}
function Pas2jsCompilerFile_FilenameToKeyName(Item: Pointer): String;
var
  aFile: TPas2jsCompilerFile absolute Item;
begin
  Result:=FilenameToKey(aFile.PasFilename);
end;

function PtrUnitnameToKeyName(Item: Pointer): String;
var
  aUnitName: string absolute Item;
begin
  Result:=LowerCase(aUnitName);
end;

function Pas2jsCompilerFile_UnitnameToKeyName(Item: Pointer): String;
var
  aFile: TPas2jsCompilerFile absolute Item;
begin
  Result:=LowerCase(aFile.PasUnitName);
end;
{$ELSE}
function CompareCompilerFiles_UnitFilename(Item1, Item2: Pointer): integer;
var
  File1: TPas2JSCompilerFile absolute Item1;
  File2: TPas2JSCompilerFile absolute Item2;
begin
  Result:=CompareFilenames(File1.UnitFilename,File2.UnitFilename);
end;

function CompareFileAndCompilerFile_UnitFilename(Filename, Item: Pointer): integer;
var
  aFile: TPas2JSCompilerFile absolute Item;
  aFilename: String;
begin
  aFilename:=AnsiString(Filename);
  Result:=CompareFilenames(aFilename,aFile.UnitFilename);
end;

function CompareCompilerFilesPasUnitname(Item1, Item2: Pointer): integer;
var
  File1: TPas2JSCompilerFile absolute Item1;
  File2: TPas2JSCompilerFile absolute Item2;
begin
  Result:=CompareText(File1.PasUnitName,File2.PasUnitName);
end;

function CompareUnitnameAndCompilerFile_PasUnitName(TheUnitname, Item: Pointer): integer;
var
  aFile: TPas2JSCompilerFile absolute Item;
  anUnitname: String;
begin
  anUnitname:=AnsiString(TheUnitname);
  Result:=CompareText(anUnitname,aFile.PasUnitName);
end;
{$ENDIF}

function TPas2jsFSCompiler.CreateFS: TPas2JSFS;

Var
  C: TPas2jsFilesCache;

begin
  C:=TPas2jsFilesCache.Create(Log);
  C.BaseDirectory:=GetCurrentDirPJ;
  Result:=C;
end;

function TPas2jsFSCompiler.GetFileCache: TPas2jsFilesCache;
begin
  Result:=FS as TPas2jsFilesCache;
end;

function TPas2jsFSCompiler.OnMacroEnv(Sender: TObject; var Params: string;
  Lvl: integer): boolean;
begin
  if Lvl=0 then ;
  if Sender=nil then ;
  Params:=GetEnvironmentVariablePJ(Params);
  Result:=true;
end;

function TPas2jsFSCompiler.CreateJSMapper: TPas2JSMapper;
begin
  Result:=inherited CreateJSMapper;
  Result.OnIsBinary:=@OnJSMapperIsBinary;
end;

function TPas2jsFSCompiler.OnJSMapperIsBinary(Sender: TObject;
  const aFilename: string): boolean;
var
  CurFile: TPas2jsCachedFile;
begin
  CurFile:=FileCache.FindFile(aFilename);
  Result:=(CurFile=nil) or (not CurFile.AllowSrcMap);
  if Sender=nil then ;
end;

procedure TPas2jsFSCompiler.SetWorkingDir(const aDir: String);
begin
  inherited SetWorkingDir(aDir);
  FileCache.BaseDirectory:=aDir;
end;

function TPas2jsFSCompiler.CreateSetOfCompilerFiles(keyType: TKeyCompareType): TPasAnalyzerKeySet;
begin
  Case keyType of
    kcFileName:
      Result:=TPasAnalyzerKeySet.Create(
          {$IFDEF Pas2js}
          @Pas2jsCompilerFile_FilenameToKeyName,@PtrFilenameToKeyName
          {$ELSE}
          @CompareCompilerFiles_UnitFilename,@CompareFileAndCompilerFile_UnitFilename
          {$ENDIF});
    kcUnitName:
      Result:=TPasAnalyzerKeySet.Create(
        {$IFDEF Pas2js}
        @Pas2jsCompilerFile_UnitnameToKeyName,@PtrUnitnameToKeyName
        {$ELSE}
        @CompareCompilerFilesPasUnitname,@CompareUnitnameAndCompilerFile_PasUnitName
        {$ENDIF});
  else
    Raise EPas2jsFileCache.CreateFmt('Internal Unknown key type: %d',[Ord(KeyType)]){%H-};
  end;
end;

procedure TPas2jsFSCompiler.InitParamMacros;
begin
  inherited InitParamMacros;
  ParamMacros.AddFunction('Env','environment variable, e.g. $Env(HOME)',@OnMacroEnv,true);
end;



end.

