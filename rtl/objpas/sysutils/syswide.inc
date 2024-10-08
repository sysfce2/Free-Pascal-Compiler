{%MainUnit sysutils.pp}
{
    *********************************************************************
    Copyright (C) 2002-2005 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    *********************************************************************
}

function IsLeadChar(Ch: WideChar): Boolean;
begin
    Result := (Ch >= #$D800) and (Ch <= #$DFFF);
end;

function Trim(const S: widestring; mode: TTrimMode): widestring;
	var
	  start, ed, ns: SizeInt;
	begin
	  start := 1;
	  ns := Length(S);
	  ed := ns;
	  if mode <> TTrimMode.Right then
	    while (start <= ed) and (S[start] <= ' ') do
	      inc(start);
	  if mode <> TTrimMode.Left then
	    while (start <= ed) and (S[ed] <= ' ') do
	      dec(ed);
	  if (start = 1) and (ed = ns) then
	    Result := S
	  else
	    Result := Copy(S, start, ed - start + 1);
	end;

function Trim(const S: widestring): widestring;
	begin
	  Result := Trim(S, TTrimMode.Both);
	end;
	

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }
function TrimLeft(const S: widestring): widestring;
	begin
	  Result := Trim(S, TTrimMode.Left);
	end;
	

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }
function TrimRight(const S: widestring): widestring;
	begin
	  Result := Trim(S, TTrimMode.Right);
	end;


function WideUpperCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.UpperWideStringProc(s);
  end;


function WideLowerCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.LowerWideStringProc(s);
  end;


function WideCompareStr(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[]);
  end;


function WideSameStr(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[])=0;
  end;


function WideCompareText(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[coIgnoreCase]);
  end;
  
  
function WideSameText(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
  begin
    result:=widestringmanager.CompareWideStringProc(s1,s2,[coIgnoreCase])=0;
  end;
  

{ we've no templates, but with includes we can simulate this :) }
{$macro on}
{$define INWIDEFORMAT}
{$define TFormatString:=widestring}
{$define TFormatChar:=widechar}

Function WideFormat (Const Fmt : WideString; const Args : Array of const; Const FormatSettings: TFormatSettings) : WideString;
{$i sysformt.inc}

{$undef TFormatString}
{$undef TFormatChar}
{$undef INWIDEFORMAT}
{$macro off}

Function WideFormat (Const Fmt : WideString; const Args : Array of const) : WideString;

begin
  Result:=WideFormat(Fmt,Args,DefaultFormatSettings);
end;



Function WideFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const; Const FormatSettings: TFormatSettings) : Cardinal;
Var 
  S,F : WideString;
begin
  Setlength(F,fmtlen);
  if fmtlen > 0 then
    Move(fmt,F[1],fmtlen*sizeof(Widechar));
  S:=WideFormat (F,Args);
  If Cardinal(Length(S))<Buflen then
    Result:=Length(S)
  else
    Result:=Buflen;
  Move(S[1],Buffer,Result);
end;

Function WideFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;

begin
  Result:=WideFormatBuf(Buffer,BufLEn,Fmt,FmtLen,Args,DefaultFormatSettings);
end;

Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const; Const FormatSettings: TFormatSettings);

begin
  Res:=WideFormat(fmt,Args);
end;

Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const);

begin
  WideFmtStr(Res,Fmt,Args,DefaultFormatSettings);
end;

function StrCopy(Dest, Source: PWideChar): PWideChar; overload;
var
  counter : SizeInt;
begin
  counter := 0;
  while Source[counter] <> #0 do
  begin
    Dest[counter] := widechar(Source[counter]);
    Inc(counter);
  end;
  { terminate the string }
  Dest[counter] := #0;
  StrCopy := Dest;
end;

function StrLCopy(Dest,Source: PWideChar; MaxLen: SizeInt): PWideChar; overload;
var
  counter: SizeInt;
begin
  counter := 0;
  while (Source[counter] <> #0) and (counter < MaxLen) do
  begin
    Dest[counter] := widechar(Source[counter]);
    Inc(counter);
  end;
  { terminate the string }
  Dest[counter] := #0;
  StrLCopy := Dest;
end;

Function CharInSet(Ch:WideChar;Const CSet : TSysCharSet) : Boolean;
begin
  result:=(Ch<=#$FF) and (ansichar(byte(ch)) in CSet);
end;


{$macro on}
{$define INWIDESTRINGREPLACE}
{$define SRString:=WideString}
{$define SRUpperCase:=WideUppercase}
{$define SRPChar:=PWideChar}
{$define SRChar:=WideChar}


Function WideStringReplace(const S, OldPattern, NewPattern: Widestring;  Flags: TReplaceFlags): Widestring;

Var
  C : Integer;

begin
  Result:=WideStringReplace(S,OldPattern,NewPattern,Flags,C);
end;

function WideStringReplace(const S, OldPattern, NewPattern: WideString;  Flags: TReplaceFlags; Out aCount : Integer): WideString;
{$i syssr.inc}

{$undef INWIDESTRINGREPLACE}
{$undef SRString}
{$undef SRUpperCase}
{$undef SRPChar}
{$undef SRChar}
