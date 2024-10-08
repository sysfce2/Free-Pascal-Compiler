{ %OPT=-O3 }

{$mode objfpc}

program tval2a;

const
  SResStr = 'Invalid2';

function TryStrToInt(const s: string; out i: LongWord): boolean; inline;
var
  Error : word;
begin
  Val(s, i, Error);
  TryStrToInt:=(Error=0)
end;

procedure DoTest;
var
  Output: LongWord;
begin
  if TryStrToInt(SResStr, Output) then
    Halt(1);

  if not TryStrToInt(SResStr[8], Output) then
    Halt(2);
	
  if Output <> 2 then
    Halt(3);
end;

begin
  DoTest(); { This is so "Output" is a local variable rather than global }
  WriteLn('ok');
end.