{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for TLB files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit tlbreader;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC} {$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Resources.Resource;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, resource;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TTlbResourceReader }

  TTlbResourceReader = class (TAbstractResourceReader)
  private
    dummyType : TResourceDesc;
    dummyName : TResourceDesc;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Resources.DataStream, System.Resources.Factory;
{$ELSE FPC_DOTTEDUNITS}
uses
  resdatastream, resfactory;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSignature = array[0..3] of AnsiChar;

const
  TypeLibSig1 = 'MSFT';
  TypeLibSig2 = 'SLTG';

{ TTlbResourceReader }

function TTlbResourceReader.GetExtensions: string;
begin
  Result:='.tlb';
end;

function TTlbResourceReader.GetDescription: string;
begin
  Result:='TLB resource reader';
end;

procedure TTlbResourceReader.Load(aResources: TResources; aStream: TStream);
var aRes : TAbstractResource;
    RawData : TResourceDataStream;
begin
  if not CheckMagic(aStream) then
    raise EResourceReaderWrongFormatException.Create('');

  aRes:=TResourceFactory.CreateResource(dummyType,dummyName);
  SetDataSize(aRes,aStream.Size-aStream.Position);
  SetDataOffset(aRes,aStream.Position);
  RawData:=TResourceDataStream.Create(aStream,aRes,aRes.DataSize,TCachedResourceDataStream);
  SetRawData(aRes,RawData);

  try
    dummyName.ID := aResources.AddAutoID(aRes);
  except
    on e : EResourceDuplicateException do
    begin
      aRes.Free;
      raise;
    end;
  end;
end;

function TTlbResourceReader.CheckMagic(aStream: TStream): boolean;
var sig : TSignature;
    orig : int64;
begin
  orig:=aStream.Position;
  aStream.ReadBuffer(sig,4);
  Result := (sig=TypeLibSig1) or (sig=TypeLibSig2);
  aStream.Position:=orig;
end;

constructor TTlbResourceReader.Create;
begin
  dummyType:=TResourceDesc.Create;
  dummyType.Name:='TYPELIB';
  dummyName:=TResourceDesc.Create;
  dummyName.ID:=1;
end;

destructor TTlbResourceReader.Destroy;
begin
  dummyType.Free;
  dummyName.Free;
end;

initialization
  TResources.RegisterReader('.tlb',TTlbResourceReader);

end.
