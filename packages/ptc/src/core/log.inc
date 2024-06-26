{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$IFNDEF WinCE}
const
  LOG_filename = 'ptcpas.log';
{$ELSE WinCE}
function LOG_filename: WideString;
var
  RequiredBufferLength: DWord;
  ReturnedPathLength: DWord;
  TempPathBuf: PWideChar;
  dummy: Byte;
begin
  RequiredBufferLength := GetTempPathW(0, @dummy);
  TempPathBuf := GetMem(RequiredBufferLength * SizeOf(WideChar));
  try
    ReturnedPathLength := GetTempPathW(RequiredBufferLength, TempPathBuf);

    if ReturnedPathLength > RequiredBufferLength then
    begin
      { The temp path length increased between 2 consecutive calls to GetTempPath?! }
      Result := '';
      exit;
    end;

    Result := TempPathBuf;
    Result := Result + 'ptcpas.log';
  finally
    FreeMem(TempPathBuf);
  end;
end;
{$ENDIF WinCE}

var
  LOG_create: Boolean = True;
  LOG_enabled: Boolean =
  {$IFDEF DEBUG}
    True;
  {$ELSE DEBUG}
    False;
  {$ENDIF DEBUG}
  LOG_file: Text;

procedure LOG_open;
begin
  AssignFile(LOG_file, LOG_filename);
  if LOG_create then
  begin
    Rewrite(LOG_file);
    Writeln(LOG_file, '[log start]');
    LOG_create := False;
  end
  else
    Append(LOG_file);
end;

procedure LOG_close;
begin
  CloseFile(LOG_file);
end;

procedure LOG(const message: AnsiString);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: Boolean);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Write(LOG_file, message, ' = ');
  if data then
    Writeln(LOG_file, 'true')
  else
    Writeln(LOG_file, 'false');
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: Integer);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: DWord);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: Int64);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: QWord);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: Single);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: Double);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; const data: AnsiString);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ' = ', data);
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: IPTCFormat);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Write(LOG_file, message, ' = Format(');
  if data = nil then
    Write(LOG_file, 'NIL')
  else
  begin
    Write(LOG_file, data.bits:2);
    if data.direct then
    begin
      Write(LOG_file, ',$', HexStr(data.r, 8), ',$', HexStr(data.g, 8), ',$', HexStr(data.b, 8));
      if data.a <> 0 then
        Write(LOG_file, ',$', HexStr(data.a, 8));
    end;
  end;
  Writeln(LOG_file, ')');
  LOG_close;
end;

procedure LOG(const message: AnsiString; data: TPTCError);
begin
  if not LOG_enabled then
    exit;
  LOG_open;
  Writeln(LOG_file, message, ': ', data.message);
  LOG_close;
end;
