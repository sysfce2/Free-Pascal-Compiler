{
    Delphi/Kylix compatibility unit: String handling routines.

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$inline on}
{$modeswitch advancedrecords}
{$IFNDEF FPC_DOTTEDUNITS}
unit StrUtils;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Types;
{$ENDIF FPC_DOTTEDUNITS}

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiResemblesText(const AText, AOther: AnsiString): Boolean;
Function AnsiContainsText(const AText, ASubText: AnsiString): Boolean;
Function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;
Function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;
function AnsiEndsText(const ASubText, AText: UnicodeString): Boolean;
Function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString;inline;
Function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;inline;
Function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer;
Function StartsText(const ASubText, AText: string): Boolean; inline;
Function EndsText(const ASubText, AText: string): Boolean; inline;

function ResemblesText(const AText, AOther: string): Boolean; inline;
function ContainsText(const AText, ASubText: string): Boolean; inline;
function MatchText(const AText: Ansistring; const AValues: array of Ansistring): Boolean; inline;
function IndexText(const AText: Ansistring; const AValues: array of Ansistring): Integer; inline;

{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean;inline;
function AnsiContainsStr(const AText, ASubText: Unicodestring): Boolean; inline;
Function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;
Function AnsiStartsStr(const ASubText, AText: UnicodeString): Boolean;
Function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;
Function AnsiEndsStr(const ASubText, AText: UnicodeString): Boolean;
Function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;inline;
Function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;inline;
Function AnsiIndexStr(const AText: Ansistring; const AValues: array of Ansistring): Integer;
Function StartsStr(const ASubText, AText: string): Boolean;
Function EndsStr(const ASubText, AText: string): Boolean;
Function MatchStr(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;
Function MatchText(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;
Function IndexStr(const AText: UnicodeString; const AValues: array of UnicodeString): Integer;
Function IndexText(const AText: UnicodeString; const AValues: array of UnicodeString): Integer;
Operator in (const AText: Ansistring; const AValues: array of Ansistring):Boolean;inline;
Operator in (const AText: UnicodeString; const AValues: array of UnicodeString):Boolean;inline;

function ContainsStr(const AText, ASubText: string): Boolean; inline;
function MatchStr(const AText: Ansistring; const AValues: array of Ansistring): Boolean; inline;
function IndexStr(const AText: Ansistring; const AValues: array of Ansistring): Integer; inline;

{ ---------------------------------------------------------------------
    Miscellaneous
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;
Function ReverseString(const AText: string): string;
Function AnsiReverseString(const AText: AnsiString): AnsiString;inline;
Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;
Function RandomFrom(const AValues: array of string): string; overload;
Function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;
Function IfThen(AValue: Boolean; const ATrue: TStringDynArray; const AFalse: TStringDynArray = nil): TStringDynArray; overload;
function NaturalCompareText (const S1 , S2 : string ): Integer ;
function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: AnsiChar): Integer;

function SplitString(const S, Delimiters: string): TRTLStringDynArray;

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;inline;
Function RightStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;
Function MidStr(const AText: AnsiString; const AStart, ACount: SizeInt): AnsiString;inline;
Function RightBStr(const AText: AnsiString; const AByteCount: SizeInt): AnsiString;inline;
Function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: SizeInt): AnsiString;inline;
Function AnsiLeftStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;inline;
Function AnsiRightStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;inline;
Function AnsiMidStr(const AText: AnsiString; const AStart, ACount: SizeInt): AnsiString;inline;
Function LeftBStr(const AText: AnsiString; const AByteCount: SizeInt): AnsiString;inline;
Function LeftStr(const AText: WideString; const ACount: SizeInt): WideString;inline;
Function RightStr(const AText: WideString; const ACount: SizeInt): WideString;
Function MidStr(const AText: WideString; const AStart, ACount: SizeInt): WideString;inline;

{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

const
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: set of AnsiChar = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];
  
resourcestring
  SErrAmountStrings        = 'Amount of search and replace strings don''t match';

type
  TStringSearchOption = (soDown, soMatchCase, soWholeWord);
  TStringSearchOptions = set of TStringSearchOption;
  TStringSeachOption = TStringSearchOption;

Function SearchBuf(Buf: PAnsiChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String; Options: TStringSearchOptions): PAnsiChar;
Function SearchBuf(Buf: PAnsiChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String): PAnsiChar;inline; // ; Options: TStringSearchOptions = [soDown]
Function PosEx(const SubStr, S: Ansistring; Offset: SizeInt): SizeInt;inline;
Function PosEx(const SubStr, S: Ansistring): SizeInt;inline;
Function PosEx(c:AnsiChar; const S: AnsiString; Offset: SizeInt): SizeInt;inline;
Function PosEx(const SubStr, S: UnicodeString; Offset: SizeInt): SizeInt;inline;
Function PosEx(c: WideChar; const S: UnicodeString; Offset: SizeInt): SizeInt;inline;
Function PosEx(const SubStr, S: UnicodeString): Sizeint;inline;
function StringsReplace(const S: Ansistring; OldPattern, NewPattern: array of Ansistring;  Flags: TReplaceFlags): string;

{ ---------------------------------------------------------------------
    Delphi compat
  ---------------------------------------------------------------------}

Function ReplaceStr(const AText, AFromText, AToText: string): string;inline;
Function ReplaceText(const AText, AFromText, AToText: string): string;inline;


{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}

type
  TSoundexLength = 1..MaxInt;

Function Soundex(const AText: string; ALength: TSoundexLength): string;
Function Soundex(const AText: string): string;inline; // ; ALength: TSoundexLength = 4

type
  TSoundexIntLength = 1..8;

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;
Function SoundexInt(const AText: string): Integer;inline; //; ALength: TSoundexIntLength = 4
Function DecodeSoundexInt(AValue: Integer): string;
Function SoundexWord(const AText: string): Word;
Function DecodeSoundexWord(AValue: Word): string;
Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;inline;
Function SoundexSimilar(const AText, AOther: string): Boolean;inline; //; ALength: TSoundexLength = 4
Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;inline;
Function SoundexCompare(const AText, AOther: string): Integer;inline; //; ALength: TSoundexLength = 4
Function SoundexProc(const AText, AOther: string): Boolean;

type
  TCompareTextProc = Function(const AText, AOther: string): Boolean;

Const
  AnsiResemblesProc: TCompareTextProc = @SoundexProc;
  ResemblesProc: TCompareTextProc = @SoundexProc;

{ ---------------------------------------------------------------------
    Other functions, based on RxStrUtils.
  ---------------------------------------------------------------------}
type
 TRomanConversionStrictness = (rcsStrict, rcsRelaxed, rcsDontCare);

resourcestring
  SInvalidRomanNumeral = '%s is not a valid Roman numeral';

function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
function DelSpace(const S: string): string;
function DelChars(const S: string; Chr: Char): string;
function DelChars(const S: string; Chars: TSysCharSet): string;
function DelSpace1(const S: string): string;
function Tab2Space(const S: string; Numb: Byte): string;
function NPos(const C: string; const S: string; N: Integer): SizeInt;

Function RPosEx(C:AnsiChar;const S : AnsiString;offs:SizeInt):SizeInt; overload;
Function RPosEx(C:Unicodechar;const S : UnicodeString;offs:SizeInt):SizeInt; overload;
Function RPosEx(Const Substr : AnsiString; Const Source : AnsiString;offs:SizeInt) : SizeInt; overload;
Function RPosEx(Const Substr : UnicodeString; Const Source : UnicodeString;offs:SizeInt) : SizeInt; overload;
Function RPos(c:AnsiChar;const S : AnsiString):SizeInt; overload; inline;
Function RPos(c:Unicodechar;const S : UnicodeString):SizeInt; overload; inline;
Function RPos(Const Substr : AnsiString; Const Source : AnsiString) : SizeInt; overload; inline;
Function RPos(Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt; overload; inline;

function AddChar(C: AnsiChar; const S: string; N: Integer): string;
function AddCharR(C: AnsiChar; const S: string; N: Integer): string;
function PadLeft(const S: string; N: Integer): string;inline;
function PadRight(const S: string; N: Integer): string;inline;
function PadCenter(const S: string; Len: SizeInt): string;
function Copy2Symb(const S: string; Symb: AnsiChar): string;
function Copy2SymbDel(var S: string; Symb: AnsiChar): string;
function Copy2Space(const S: string): string;inline;
function Copy2SpaceDel(var S: string): string;inline;
function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
function WordCount(const S: string; const WordDelims: TSysCharSet): SizeInt;
function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): SizeInt;
function ExtractWord(N: Integer; const S: string;  const WordDelims: TSysCharSet): string;inline;
{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: SizeInt): string;
{$ENDIF}
function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: Integer): string;
function ExtractDelimited(N: Integer; const S: string;  const Delims: TSysCharSet): string;
{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
function ExtractSubstr(const S: string; var Pos: SizeInt;  const Delims: TSysCharSet): string;
{$ENDIF}
function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: TSysCharSet): string;
function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
function FindPart(const HelpWilds, InputStr: string): SizeInt;
function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;
function XorString(const Key, Src: ShortString): ShortString;
function XorEncode(const Key, Source: Ansistring): Ansistring;
function XorDecode(const Key, Source: Ansistring): Ansistring;
function GetCmdLineArg(const Switch: string; SwitchChars: TSysCharSet): string;
function Numb2USA(const S: string): string;
function Hex2Dec(const S: string): Longint;
function Hex2Dec64(const S: string): int64;
function Dec2Numb(N: Longint; Len, Base: Byte): string;
function Numb2Dec(S: string; Base: Byte): Longint;
function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
function IntToBin(Value: Longint; Digits: Integer): string;
function IntToBin(Value: int64; Digits:integer): string;
function IntToRoman(Value: Longint): string;
function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness = rcsRelaxed): Boolean;
function RomanToInt(const S: string; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;
function RomanToIntDef(Const S : String; const ADefault: Longint = 0; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;
procedure BinToHex(const BinBuffer: TBytes; BinBufOffset: Integer; var HexBuffer: TBytes; HexBufOffset: Integer; Count: Integer); overload;
procedure BinToHex(BinValue: Pointer; HexValue: PWideChar; BinBufSize: Integer); overload;
procedure BinToHex(const BinValue; HexValue: PWideChar; BinBufSize: Integer); overload;
procedure BinToHex(BinValue: PAnsiChar; HexValue: PAnsiChar; BinBufSize: Integer); overload;
procedure BinToHex(BinValue: PAnsiChar; HexValue: PWideChar; BinBufSize: Integer); overload;
procedure BinToHex(const BinValue; HexValue: PAnsiChar; BinBufSize: Integer); overload;
procedure BinToHex(BinValue: Pointer; HexValue: PAnsiChar; BinBufSize: Integer); overload;
function HexToBin(HexText: PAnsiChar; BinBuffer: PAnsiChar; BinBufSize: Integer): Integer; overload;
function HexToBin(const HexText: PWideChar; HexTextOffset: Integer; var BinBuffer: TBytes; BinBufOffset: Integer; Count: Integer): Integer; overload;
function HexToBin(const HexText: TBytes; HexTextOffset: Integer; var BinBuffer: TBytes; BinBufOffset: Integer; Count: Integer): Integer; overload;
function HexToBin(HexText: PWideChar; BinBuffer: Pointer; BinBufSize: Integer): Integer; overload;
function HexToBin(const HexText: PWideChar; var BinBuffer; BinBufSize: Integer): Integer; overload;
function HexToBin(HexText: PWideChar; BinBuffer: PAnsiChar; BinBufSize: Integer): Integer; overload;
function HexToBin(HexText: PAnsiChar; var BinBuffer; BinBufSize: Integer): Integer; overload;
function HexToBin(const HexText: PAnsiChar; BinBuffer: Pointer; BinBufSize: Integer): Integer; overload;

const
  DigitChars = ['0'..'9'];
  Brackets = ['(',')','[',']','{','}'];
  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;
  StdSwitchChars = ['-','/'];

function PosSet (const c:TSysCharSet;const s : ansistring ):SizeInt;
function PosSet (const c:string;const s : ansistring ):SizeInt;
function PosSetEx (const c:TSysCharSet;const s : ansistring;count:Integer ):SizeInt;
function PosSetEx (const c:string;const s : ansistring;count:Integer ):SizeInt;

Procedure RemoveLeadingChars(VAR S : AnsiString; Const CSet:TSysCharset);
Procedure RemoveTrailingChars(VAR S : AnsiString;Const CSet:TSysCharset);
Procedure RemoveLeadingChars(VAR S : UnicodeString; Const CSet:TSysCharset);
Procedure RemoveTrailingChars(VAR S : UnicodeString;Const CSet:TSysCharset);
Procedure RemovePadChars(VAR S : AnsiString;Const CSet:TSysCharset);
procedure RemovePadChars(VAR S: UnicodeString; const CSet: TSysCharset);

function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;
Function TrimRightSet(const S: String;const CSet:TSysCharSet): String;
function TrimSet(const S: String;const CSet:TSysCharSet): String;


type
  SizeIntArray = array of SizeInt;

Function FindMatchesBoyerMooreCaseSensitive(const S,OldPattern: PAnsiChar; const SSize, OldPatternSize: SizeInt; out aMatches: SizeIntArray; const aMatchAll: Boolean) : Boolean; 
Function FindMatchesBoyerMooreCaseSensitive(const S,OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean) : Boolean; 

Function FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: PAnsiChar; const SSize, OldPatternSize: SizeInt; out aMatches: SizeIntArray; const aMatchAll: Boolean) : Boolean; 
Function FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean) : Boolean;

Type
  TStringReplaceAlgorithm = (sraDefault,    // Default algoritm as used in StringUtils.
                             sraManySmall,       // Algorithm optimized for many small replacements.
                             sraBoyerMoore  // Algorithm optimized for long replacements.
                            );

Function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; out aCount : Integer; Algorithm : TStringReplaceAlgorithm = sraDefault): string; overload;
Function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; Algorithm : TStringReplaceAlgorithm = sraDefault): string; overload;
{ We need these for backwards compatibility:
  The compiler will stop searching and convert to ansistring if the widestring version of stringreplace is used.
  They currently simply refer to sysutils, till the new mechanisms are proven to work with unicode.}
{$IF SIZEOF(CHAR)=1}
Function StringReplace(const S, OldPattern, NewPattern: unicodestring; Flags: TReplaceFlags): unicodestring; overload;
Function StringReplace(const S, OldPattern, NewPattern: widestring; Flags: TReplaceFlags): widestring; overload;
{$ENDIF}


Type
  TRawByteStringArray = Array of RawByteString;
  TUnicodeStringArray = Array of UnicodeString;

Function SplitCommandLine(S : RawByteString) : TRawByteStringArray;
Function SplitCommandLine(S : UnicodeString) : TUnicodeStringArray;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysConst; // HexDigits
{$ELSE FPC_DOTTEDUNITS}
uses sysconst; // HexDigits
{$ENDIF FPC_DOTTEDUNITS}

type
  // Shared between case-sensitive and case-insensitive versions.
  BoyerMoore = record
    DeltaJumpTable1: array[AnsiChar] of SizeInt;
    DeltaJumpTable2: SizeIntArray;
    MatchesCount: SizeInt; //Stores the amount of replaces that will take place
    MatchesAllocatedLimit: SizeInt; //Currently allocated space for matches.
    Matches: ^SizeIntArray;

  class var
    LCaseArray: array[AnsiChar] of AnsiChar; //Array of lowercased alphabet
    LCaseArrayPrepared: boolean;

    procedure Init(var aMatches: SizeIntArray); inline;
    procedure MakeDeltaJumpTables(aPattern: PAnsiChar; aPatternSize: SizeInt);
    procedure AddMatch(aPosition: SizeInt);
    class function Max(a, b: SizeInt): SizeInt; static; inline;
    class procedure IncrementEverything(var aMatches: array of SizeInt); static; // Make positions 1-based.
    class procedure PrepareLCaseArray; static;
  end;

procedure BoyerMoore.Init(var aMatches: SizeIntArray);
begin
   MatchesCount:=0;
   MatchesAllocatedLimit:=0;
   Matches:=@aMatches;
end;

procedure BoyerMoore.MakeDeltaJumpTables(aPattern: PAnsiChar; aPatternSize: SizeInt);
var
   i, Position, LastPrefixIndex, SuffixLength: SizeInt;
begin
{$if sizeof(SizeInt)=sizeof(word)} FillWord
{$elseif sizeof(SizeInt)=sizeof(dword)} FillDWord
{$elseif sizeof(SizeInt)=sizeof(qword)} FillQWord
{$else} {$error unknown SizeInt size}
{$endif}
     (DeltaJumpTable1, Length(DeltaJumpTable1), SizeUint(aPatternSize));
   //Last AnsiChar do not enter in the equation
   for i := 0 to aPatternSize - 1 - 1 do
     DeltaJumpTable1[aPattern[i]]:=aPatternSize -1 - i;

   SetLength(DeltaJumpTable2, aPatternSize);
   LastPrefixIndex:=aPatternSize-1;
   Position:=aPatternSize-1;
   while Position>=0 do begin
     if CompareByte(aPattern^, aPattern[Position+1], (aPatternSize-Position-1)*SizeOf(aPattern[0]))=0 then
       LastPrefixIndex := Position+1;
     DeltaJumpTable2[Position] := LastPrefixIndex + (aPatternSize-1 - Position);
     Dec(Position);
   end;
   Position:=0;
   while Position<aPatternSize-1 do begin
     SuffixLength:=0;
     while (SuffixLength<Position) and (aPattern[Position-SuffixLength] = aPattern[aPatternSize-1-SuffixLength]) do
       inc(SuffixLength);
     if aPattern[Position-SuffixLength] <> aPattern[aPatternSize-1-SuffixLength] then
       DeltaJumpTable2[aPatternSize - 1 - SuffixLength] := aPatternSize - 1 - Position + SuffixLength;
     Inc(Position);
   end;
end;

procedure BoyerMoore.AddMatch(aPosition: SizeInt);
begin
  if MatchesCount=MatchesAllocatedLimit then begin
    MatchesAllocatedLimit:=MatchesAllocatedLimit+4+SizeInt(SizeUint(MatchesAllocatedLimit) div 4+SizeUint(MatchesAllocatedLimit) div 8); // const + 37.5%
    SetLength(Matches^,MatchesAllocatedLimit);
  end;
  Matches^[MatchesCount]:=aPosition;
  inc(MatchesCount);
end;

class function BoyerMoore.Max(a, b: SizeInt): SizeInt;
begin
  if a>b then Result:=a else Result:=b;
end;

class procedure BoyerMoore.IncrementEverything(var aMatches: array of SizeInt); static;
var
  i: SizeInt;
begin
  for i:=0 to High(aMatches) do
    Inc(aMatches[i]);
end;

class procedure BoyerMoore.PrepareLCaseArray;
var
  c: AnsiChar;
begin
  for c in AnsiChar do
    LCaseArray[c]:=AnsiLowerCase(c)[1];
  WriteBarrier; // Write LCaseArrayPrepared only after LCaseArray contents.
  LCaseArrayPrepared:=true;
end;

(*
  FindMatchesBoyerMooreCaseSensitive

  Finds one or many ocurrences of an ansistring in another ansistring.
  It is case sensitive.

  * Parameters:
  S: The PAnsiChar to be searched in. (Read only).
  OldPattern: The PAnsiChar to be searched. (Read only).
  SSize: The size of S in Chars. (Read only).
  OldPatternSize: The size of OldPatter in chars. (Read only).
  aMatches: SizeInt array where match indexes are returned (zero based) (write only).
  aMatchAll: Finds all matches, not just the first one. (Read only).

  * Returns:
    True if at least one occurence was found.

  The function is based in the Boyer-Moore algorithm.
*)

function FindMatchesBoyerMooreCaseSensitive(const S, OldPattern: PAnsiChar;
  const SSize, OldPatternSize: SizeInt; out aMatches: SizeIntArray;
  const aMatchAll: Boolean) : Boolean;
var
  i,j: SizeInt;
  bm: BoyerMoore;
begin
  aMatches:=nil;
  if OldPatternSize=0 then
    Exit(False);
  bm.Init(aMatches);
  bm.MakeDeltaJumpTables(OldPattern,OldPatternSize);

  i:=OldPatternSize-1;
  while i < SSize do begin
    j:=OldPatternSize-1;
    while (j>=0) and (S[i] = OldPattern[j]) do begin
      dec(i);
      dec(j);
    end;
    if (j<0) then begin
      bm.AddMatch(i+1);
      //Only first match ?
      if not aMatchAll then break;
      inc(i,bm.DeltaJumpTable2[0]+1);
    end else
      i:=i + bm.Max(bm.DeltaJumpTable1[s[i]],bm.DeltaJumpTable2[j]);
  end;
  SetLength(aMatches,bm.MatchesCount);
  Result:=bm.MatchesCount>0;
end;

function FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: PAnsiChar; const SSize, OldPatternSize: SizeInt; out
  aMatches: SizeIntArray; const aMatchAll: Boolean): Boolean;
var
  i,j: SizeInt;
  lPattern: PAnsiChar; //Lowercased OldPattern
  bm: BoyerMoore;
  lPatternStore: ansistring;
begin
  aMatches:=nil;
  if OldPatternSize=0 then
    Exit(False);

  //Build an internal array of lowercase version of every possible AnsiChar.
  if not bm.LCaseArrayPrepared then
    bm.PrepareLCaseArray;
  ReadBarrier; // Read LCaseArray contents only after LCaseArrayPrepared.

  //Create the new lowercased pattern. Or avoid and reuse OldPattern if nothing to lowercase!
  lPattern:=OldPattern;
  for i := 0 to OldPatternSize-1 do
    if bm.LCaseArray[OldPattern[i]]<>OldPattern[i] then begin
      SetLength(lPatternStore,OldPatternSize);
      lPattern:=PAnsiChar(Pointer(lPatternStore));
      Move(OldPattern^,lPattern^,i*sizeof(AnsiChar));
      for j := i to OldPatternSize-1 do
        lPattern[j]:=bm.LCaseArray[OldPattern[j]];
      break;
    end;

  bm.Init(aMatches);
  bm.MakeDeltaJumpTables(lPattern,OldPatternSize);

  i:=OldPatternSize-1;
  while i < SSize do begin
    j:=OldPatternSize-1;
    while (j>=0) and (bm.LCaseArray[S[i]] = lPattern[j]) do begin
      dec(i);
      dec(j);
    end;
    if (j<0) then begin
      bm.AddMatch(i+1);
      //Only first match ?
      if not aMatchAll then break;
      inc(i,bm.DeltaJumpTable2[0]+1);
    end else
      i:=i + bm.Max(bm.DeltaJumpTable1[bm.LCaseArray[s[i]]],bm.DeltaJumpTable2[j]);
  end;
  SetLength(aMatches,bm.MatchesCount);
  Result:=bm.MatchesCount>0;
end;

function StringReplaceFast(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags; out aCount : Integer): string;
const
  MATCHESCOUNTRESIZER=100; //Arbitrary value. Memory used = MATCHESCOUNTRESIZER * sizeof(SizeInt)
var
  //Stores where a replace will take place
  Matches: array of SizeInt;
  //Stores the amount of replaces that will take place
  MatchesCount: SizeInt;
  //Currently allocated space for matches.
  MatchesAllocatedLimit: SizeInt;
  //Uppercase version of pattern
  PatternUppercase: string;
  //Lowercase version of pattern
  PatternLowerCase: string;
  //Index
  MatchIndex: SizeInt;
  MatchLimit: SizeInt;
  MatchInternal: SizeInt;
  MatchTarget: SizeInt;
  AdvanceIndex: SizeInt;

  //Miscelanous variables
  OldPatternSize: SizeInt;
  NewPatternSize: SizeInt;

  //Resizes the allocated space for replacement index
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=MatchesCount+MATCHESCOUNTRESIZER;
    SetLength(Matches,MatchesAllocatedLimit);
  end;

  //Add a match to be replaced
  procedure AddMatch(const aPosition: SizeInt); inline;
  begin
    if MatchesCount = MatchesAllocatedLimit then begin
      ResizeAllocatedMatches;
    end;
    Matches[MatchesCount]:=aPosition;
    inc(MatchesCount);
  end;
begin
  aCount:=0;
  if (OldPattern='') or (Length(OldPattern)>Length(S)) then begin
    //This cases will never match nothing.
    Result:=S;
    exit;
  end;
  Result:='';
  OldPatternSize:=Length(OldPattern);
  MatchesCount:=0;
  MatchesAllocatedLimit:=0;
  if rfIgnoreCase in Flags then begin
    //Different algorithm for case sensitive and insensitive
    //This is insensitive, so 2 new ansistrings are created for search pattern, one upper and one lower case.
    //It is easy, usually, to create 2 versions of the match pattern than uppercased and lowered case each
    //character in the "to be matched" string.
    PatternUppercase:=AnsiUpperCase(OldPattern);
    PatternLowerCase:=AnsiLowerCase(OldPattern);
    MatchIndex:=Length(OldPattern);
    MatchLimit:=Length(S);
    NewPatternSize:=Length(NewPattern);
    while MatchIndex <= MatchLimit do begin
      if (S[MatchIndex]=PatternLowerCase[OldPatternSize]) or (S[MatchIndex]=PatternUppercase[OldPatternSize]) then begin
        //Match backwards...
        MatchInternal:=OldPatternSize-1;
        MatchTarget:=MatchIndex-1;
        while MatchInternal>=1 do begin
          if (S[MatchTarget]=PatternLowerCase[MatchInternal]) or (S[MatchTarget]=PatternUppercase[MatchInternal]) then begin
            dec(MatchInternal);
            dec(MatchTarget);
          end else begin
            break;
          end;
        end;
        if MatchInternal=0 then begin
          //Match found, all AnsiChar meet the sequence
          //MatchTarget points to AnsiChar before, so matching is +1
          AddMatch(MatchTarget+1);
          inc(MatchIndex,OldPatternSize);
          if not (rfReplaceAll in Flags) then begin
            break;
          end;
        end else begin
          //Match not found
          inc(MatchIndex);
        end;
      end else begin
        inc(MatchIndex);
      end;
    end;
  end else begin
    //Different algorithm for case sensitive and insensitive
    //This is sensitive, so just 1 binary comprare
    MatchIndex:=Length(OldPattern);
    MatchLimit:=Length(S);
    NewPatternSize:=Length(NewPattern);
    while MatchIndex <= MatchLimit do begin
      if (S[MatchIndex]=OldPattern[OldPatternSize]) then begin
        //Match backwards...
        MatchInternal:=OldPatternSize-1;
        MatchTarget:=MatchIndex-1;
        while MatchInternal>=1 do begin
          if (S[MatchTarget]=OldPattern[MatchInternal]) then begin
            dec(MatchInternal);
            dec(MatchTarget);
          end else begin
            break;
          end;
        end;
        if MatchInternal=0 then begin
          //Match found, all AnsiChar meet the sequence
          //MatchTarget points to AnsiChar before, so matching is +1
          AddMatch(MatchTarget+1);
          inc(MatchIndex,OldPatternSize);
          if not (rfReplaceAll in Flags) then begin
            break;
          end;
        end else begin
          //Match not found
          inc(MatchIndex);
        end;
      end else begin
        inc(MatchIndex);
      end;
    end;
  end;
  //Create room enough for the result string
  aCount:=MatchesCount;
  SetLength(Result,Length(S)-OldPatternSize*MatchesCount+NewPatternSize*MatchesCount);
  MatchIndex:=1;
  MatchTarget:=1;
  //Matches[x] are 1 based offsets
  for MatchInternal := 0 to Pred(MatchesCount) do begin
    //Copy information up to next match
    AdvanceIndex:=Matches[MatchInternal]-MatchIndex;
    if AdvanceIndex>0 then begin
      move(S[MatchIndex],Result[MatchTarget],AdvanceIndex);
      inc(MatchTarget,AdvanceIndex);
      inc(MatchIndex,AdvanceIndex);
    end;
    //Copy the new replace information string
    if NewPatternSize>0 then begin
      move(NewPattern[1],Result[MatchTarget],NewPatternSize);
      inc(MatchTarget,NewPatternSize);
    end;
    inc(MatchIndex,OldPatternSize);
  end;
  if MatchTarget<=Length(Result) then begin
    //Add remain data at the end of source.
    move(S[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
  end;
end;

(*
  StringReplaceBoyerMoore

  Replaces one or many ocurrences of an ansistring in another ansistring by a new one.
  It can perform the compare ignoring case (ansi).

  * Parameters (Read only):
  S: The string to be searched in.
  OldPattern: The string to be searched.
  NewPattern: The string to replace OldPattern matches.
  Flags:
    rfReplaceAll: Replace all occurrences.
    rfIgnoreCase: Ignore case in OldPattern matching.

  * Returns:
    The modified string (if needed).

  It is memory conservative, just sizeof(SizeInt) per match in blocks off 100 matches
  plus Length(OldPattern)*2 in the case of ignoring case.
  Memory copies are the minimun necessary.
  Algorithm based in the Boyer-Moore string search algorithm.

  It is faster when the "S" string is very long and the OldPattern is also
  very big. As much big the OldPattern is, faster the search is too.

  It uses 2 different helper versions of Boyer-Moore algorithm, one for case
  sensitive and one for case INsensitive for speed reasons.

*)

function StringReplaceBoyerMoore(const S, OldPattern, NewPattern: string;Flags: TReplaceFlags; out aCount : Integer): string;
var
  Matches: SizeIntArray;
  OldPatternSize: SizeInt;
  NewPatternSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
begin
  aCount:=0;
  OldPatternSize:=Length(OldPattern);
  NewPatternSize:=Length(NewPattern);
  if (OldPattern='') or (Length(OldPattern)>Length(S)) then begin
    Result:=S;
    exit;
  end;

  if rfIgnoreCase in Flags then begin
    FindMatchesBoyerMooreCaseINSensitive(@s[1],@OldPattern[1],Length(S),Length(OldPattern),Matches, rfReplaceAll in Flags);
  end else begin
    FindMatchesBoyerMooreCaseSensitive(@s[1],@OldPattern[1],Length(S),Length(OldPattern),Matches, rfReplaceAll in Flags);
  end;

  MatchesCount:=Length(Matches);
  aCount:=MatchesCount;

  //Create room enougth for the result string
  SetLength(Result,Length(S)-OldPatternSize*MatchesCount+NewPatternSize*MatchesCount);
  MatchIndex:=1;
  MatchTarget:=1;
  //Matches[x] are 0 based offsets
  for MatchInternal := 0 to Pred(MatchesCount) do begin
    //Copy information up to next match
    AdvanceIndex:=Matches[MatchInternal]+1-MatchIndex;
    if AdvanceIndex>0 then begin
      move(S[MatchIndex],Result[MatchTarget],AdvanceIndex);
      inc(MatchTarget,AdvanceIndex);
      inc(MatchIndex,AdvanceIndex);
    end;
    //Copy the new replace information string
    if NewPatternSize>0 then begin
      move(NewPattern[1],Result[MatchTarget],NewPatternSize);
      inc(MatchTarget,NewPatternSize);
    end;
    inc(MatchIndex,OldPatternSize);
  end;
  if MatchTarget<=Length(Result) then begin
    //Add remain data at the end of source.
    move(S[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
  end;
end;

function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; Algorithm: TStringReplaceAlgorithm): string;

Var
  C : Integer;

begin
  Result:=StringReplace(S, OldPattern, NewPattern, Flags,C,Algorithm);
end;

Function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; out aCount : Integer; Algorithm : TStringReplaceAlgorithm = sraDefault): string; overload;


begin
  Case Algorithm of
    sraDefault    : Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(S,OldPattern,NewPattern,Flags,aCount);
    sraManySmall  : Result:=StringReplaceFast(S,OldPattern,NewPattern,Flags,aCount);
    sraBoyerMoore : Result:=StringReplaceBoyerMoore(S,OldPattern,NewPattern,Flags,aCount);
  end;
end;

{$IF SIZEOF(CHAR)=1}

function StringReplace(const S, OldPattern, NewPattern: unicodestring; Flags: TReplaceFlags): unicodestring;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(S,OldPattern,NewPattern,Flags);
end;

function StringReplace(const S, OldPattern, NewPattern: widestring; Flags: TReplaceFlags): widestring;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(S,OldPattern,NewPattern,Flags);
end;
{$ENDIF}

function FindMatchesBoyerMooreCaseSensitive(const S, OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean
  ): Boolean;
begin
  Result:=FindMatchesBoyerMooreCaseSensitive(PAnsiChar(S),PAnsiChar(OldPattern),Length(S),Length(OldPattern),aMatches,aMatchAll);
  BoyerMoore.IncrementEverything(AMatches);
end;

function FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean
  ): Boolean;
begin
  Result:=FindMatchesBoyerMooreCaseInSensitive(PAnsiChar(S),PAnsiChar(OldPattern),Length(S),Length(OldPattern),aMatches,aMatchAll);
  BoyerMoore.IncrementEverything(AMatches);
end;


{ ---------------------------------------------------------------------
   Possibly Exception raising functions
  ---------------------------------------------------------------------}


function Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$',S)=0 then
    HexStr:='$'+ S
  else
    HexStr:=S;
  Result:=StrToInt(HexStr);
end;

function Hex2Dec64(const S: string): int64;
var
  HexStr: string;
begin
  if Pos('$',S)=0 then
    HexStr:='$'+ S
  else
    HexStr:=S;
  Result:=StrToInt64(HexStr);
end;


{
  We turn off implicit exceptions, since these routines are tested, and it 
  saves 20% codesize (and some speed) and don't throw exceptions, except maybe 
  heap related. If they don't, that is consider a bug.

  In the future, be wary with routines that use strtoint, floating point 
  and/or format() derivatives. And check every divisor for 0.
}

{$IMPLICITEXCEPTIONS OFF}

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}
function AnsiResemblesText(const AText, AOther: AnsiString): Boolean;

begin
  if Assigned(AnsiResemblesProc) then
    Result:=AnsiResemblesProc(AText,AOther)
  else
    Result:=False;
end;

function AnsiContainsText(const AText, ASubText: AnsiString): Boolean;
begin
  AnsiContainsText:=AnsiPos(AnsiUppercase(ASubText),AnsiUppercase(AText))>0;
end;


function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result := (ASubText = '') or AnsiSameText(LeftStr(AText, Length(ASubText)), ASubText);
end;


function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result := (ASubText = '') or AnsiSameText(RightStr(AText, Length(ASubText)), ASubText);
end;

function AnsiEndsText(const ASubText, AText: UnicodeString): Boolean;

begin
  Result := (ASubText = '') or SameText(RightStr(AText, Length(ASubText)), ASubText);
end;



function StartsText(const ASubText, AText: String): Boolean; inline;
begin
  Result := AnsiStartsText(ASubText, AText);
end;


function EndsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiEndsText(ASubText, AText);
end;

function ResemblesText(const AText, AOther: string): Boolean;
begin
  if Assigned(ResemblesProc) then
    Result := ResemblesProc(AText, AOther)
  else
    Result := False;
end;

function ContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiContainsText(AText, ASubText);
end;

function MatchText(const AText: Ansistring; const AValues: array of Ansistring): Boolean;
begin
  Result := AnsiMatchText(AText, AValues);
end;

function IndexText(const AText: Ansistring; const AValues: array of Ansistring): Integer;
begin
  Result := AnsiIndexText(AText, AValues);
end;

function ContainsStr(const AText, ASubText: String): Boolean;
begin
  Result := AnsiContainsStr(AText, ASubText);
end;

function MatchStr(const AText: Ansistring; const AValues: array of Ansistring): Boolean;
begin
  Result := AnsiMatchStr(AText, AValues);
end;

function IndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;
begin
  Result := AnsiIndexStr(AText, AValues);
end;

function AnsiReplaceText(const AText, AFromText, AToText: Ansistring): Ansistring;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll,rfIgnoreCase]);
end;

function AnsiMatchText(const AText: Ansistring; const AValues: array of AnsiString): Boolean;
begin
  Result:=(AnsiIndexText(AText,AValues)<>-1)
end;

function AnsiIndexText(const AText: AnsiString; const AValues: array of Ansistring): Integer;
begin
  for Result := Low(AValues) to High(AValues) do
    if AnsiSameText(AValues[Result], AText) then
      Exit;
  Result := -1;
end;


{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

function AnsiContainsStr(const AText, ASubText: Ansistring): Boolean;
begin
  Result := AnsiPos(ASubText,AText)>0;
end;

function AnsiContainsStr(const AText, ASubText: Unicodestring): Boolean;
begin
  Result := AnsiPos(ASubText,AText)>0;
end;


function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result := (ASubText = '') or (LeftStr(AText, Length(ASubText)) = ASubText);
end;

function AnsiStartsStr(const ASubText, AText: UnicodeString): Boolean;
begin
  Result := (ASubText = '') or (LeftStr(AText, Length(ASubText)) = ASubText);
end;


function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result := (ASubText = '') or (RightStr(AText, Length(ASubText)) = ASubText);
end;

function AnsiEndsStr(const ASubText, AText: UnicodeString): Boolean;
begin
  Result := (ASubText = '') or (RightStr(AText, Length(ASubText)) = ASubText);
end;


function StartsStr(const ASubText, AText: string): Boolean;
begin
  if (Length(AText) >= Length(ASubText)) and (ASubText <> '') then
    Result := StrLComp(PChar(ASubText), PChar(AText), Length(ASubText)) = 0
  else
    Result := (AsubText='');
end;


function EndsStr(const ASubText, AText: string): Boolean;
begin
  if Length(AText) >= Length(ASubText) then
    Result := StrLComp(PChar(ASubText),
      PChar(AText) + Length(AText) - Length(ASubText), Length(ASubText)) = 0
  else
    Result := False;
end;


function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll]);
end;


function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result:=AnsiIndexStr(AText,Avalues)<>-1;
end;


function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;
begin
  for result:=low(AValues) to High(Avalues) do
     if (avalues[result]=AText) Then
       exit;                                    // make sure it is the first val.
  result:=-1;
end;


function MatchStr(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;
begin
  Result := IndexStr(AText,AValues) <> -1;
end;

function MatchText(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;
begin
 Result := IndexText(AText,AValues) <> -1;
end;

function IndexStr(const AText: UnicodeString; const AValues: array of UnicodeString): Integer;
begin
  for Result := Low(AValues) to High(AValues) do
     if (avalues[Result] = AText) Then
       exit;                                    // make sure it is the first val.
  Result := -1;
end;

function IndexText(const AText: UnicodeString; const AValues: array of UnicodeString): Integer;
begin
  for Result:=Low(AValues) to High(AValues) do
     if UnicodeCompareText(avalues[Result],atext)=0 Then
       exit;     // make sure it is the first val.
  Result:=-1;
end;

operator in(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result := AnsiIndexStr(AText,AValues) <>-1;   
end;


operator in(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;
begin
  Result := IndexStr(AText,AValues) <> -1;
end;
{ ---------------------------------------------------------------------
    Playthingies
  ---------------------------------------------------------------------}

function DupeString(const AText: string; ACount: Integer): string;

var
  ResLen, Rp, ToCopy: SizeInt;

begin
  if (AText = '') or (ACount <= 0) then
    Exit('');
  if ACount = 1 then
    Exit(AText);

  Rp := Length(AText);
  ResLen := ACount * Rp;
  SetLength(Result, ResLen);
  Move(Pointer(AText)^, Pointer(Result)^, Rp * SizeOf(AText[1]));

  repeat
    ToCopy := ResLen - Rp;
    if Rp < ToCopy then
      ToCopy := Rp;
    Move(Pointer(Result)^, PChar(Pointer(Result))[Rp], ToCopy * SizeOf(AText[1]));
    Inc(Rp, ToCopy);
  until Rp = ResLen;
end;

function ReverseString(const AText: string): string;

var
  i,j : SizeInt;

begin
  setlength(result,length(atext));
  i:=1; j:=length(atext);
  while (i<=j) do
    begin
      result[i]:=atext[j-i+1];
      inc(i);
    end;
end;


function AnsiReverseString(const AText: AnsiString): AnsiString;

begin
  Result:=ReverseString(AText);
end;



function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string;

var i,j,k : SizeUInt;

begin
  j:=length(ASubText);
  i:=length(AText);
  if AStart>i then 
    aStart:=i+1;
  k:=i+1-AStart;
  if ALength> k then
    ALength:=k;
  SetLength(Result,i+j-ALength);
  move (AText[1],result[1],(AStart-1)*SizeOf(Char));
  move (ASubText[1],result[AStart],j*SizeOf(Char));
  move (AText[AStart+ALength], Result[AStart+j],(i+1-AStart-ALength)*SizeOf(Char));
end;

function RandomFrom(const AValues: array of string): string;

begin
  if high(AValues)=-1 then exit('');
  result:=Avalues[random(High(AValues)+1)];
end;

function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;

begin
  if avalue then
    result:=atrue
  else
    result:=afalse;
end;

Function IfThen(AValue: Boolean; const ATrue: TStringDynArray; const AFalse: TStringDynArray = nil): TStringDynArray; overload;

begin
  if avalue then
    result:=atrue
  else
    result:=afalse;
end;

function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: AnsiChar): Integer;
{
 NaturalCompareBase compares strings in a collated order and
 so numbers are sorted too. It sorts like this:

 01
 001
 0001

 and

 0
 00
 000
 000_A
 000_B

 in a intuitive order.
 }

// All indices are zero-based to be used with PChar(Pointer(...))[Sp] form,
// which allows to omit Sp < Length check, instead reading terminating #0 at Sp = Length.

type
  TRunningNumberCompare = record
    S: string;
    Sp: SizeInt;
  end;

  function ScanText(const S: string; Sp: SizeInt): SizeInt;
  begin
    Result := Sp;
    repeat
      while not (PChar(Pointer(S))[Result] in ['0' .. '9', #0]) do
        Inc(Result);
      // End?
      if Result >= Length(S) then
        exit;
      // Undo spaces if there is a number.
      if PChar(Pointer(S))[Result] in ['0' .. '9'] then
      begin
        while (Result > Sp) and (PChar(Pointer(S))[Result - 1] in [' ']) do
          Dec(Result);
        exit;
      end;
      // Embedded #0.
      Inc(Result);
    until false;
  end;

  function InitNumber(out C: TRunningNumberCompare; const S: string; Sp: SizeInt): boolean;
  begin
    C.S := S;
    C.Sp := Sp;
    while PChar(Pointer(S))[C.Sp] in [' '] do
      Inc(C.Sp);
    while (PChar(Pointer(S))[C.Sp] in ['0']) and (PChar(Pointer(S))[C.Sp + 1] in ['0' .. '9']) do
      Inc(C.Sp);
    Result := PChar(Pointer(S))[C.Sp] in ['0' .. '9'];
  end;

  function NextDigit(var C: TRunningNumberCompare): Integer;
  var
    Ch: Char;
  begin
    Ch := PChar(Pointer(C.S))[C.Sp];
    if Ch in ['0' .. '9'] then
    begin
      Result := Ord(Ch) - Ord('0');
      Inc(C.Sp);
    end else
      Result := -1;
  end;

  function ScanAndCompareTexts(const S1: string; var S1p: SizeInt; const S2: string; var S2p: SizeInt): Integer;
  var
    S1e, S2e: SizeInt;
  begin
    S1e := ScanText(S1, S1p);
    S2e := ScanText(S2, S2p);
    Result := (S1e - S1p) - (S2e - S2p);
    if Result = 0 then { Shortcut same strings (file0000, file0001). }
      Result := CompareByte(S1[1 + S1p], S2[1 + S2p], (S1e - S1p) * SizeOf(Char));
    if Result <> 0 then
      Result := {$if sizeof(char) = 1} AnsiCompareText {$else} UnicodeCompareText {$endif}
        (Copy(S1, 1 + S1p, S1e - S1p), Copy(S2, 1 + S2p, S2e - S2p));
    S1p := S1e;
    S2p := S2e;
  end;

  function ScanAndCompareNumbers(const S1: string; var S1p: SizeInt; const S2: string; var S2p: SizeInt): Integer;
  var
    C1, C2: TRunningNumberCompare;
    Digit1, Digit2: Integer;
  begin
    if not InitNumber(C1, S1, S1p) or not InitNumber(C2, S2, S2p) then
      Exit(0);
    Result := 0;

    repeat
      Digit1 := NextDigit(C1);
      Digit2 := NextDigit(C2);
      if (Digit1 < 0) <> (Digit2 < 0) then
        Exit(2 * Ord(Digit2 < 0) - 1); // C1 > C2 if C2 ended first, and vice versa.
      if Digit1 < 0 then
        Break;
      // 'Result' remembers the result of comparison between most significant different digits, used if it turns out that amounts of digits are equal.
      if Result = 0 then
        Result := Ord(Digit1 > Digit2) - Ord(Digit1 < Digit2);
    until false;

    // Compare lengths if numbers are equal (but not characters, so '01' and ' 1' are equal).
    if Result = 0 then
      Result := Ord(C1.Sp - S1p > C2.Sp - S2p) - Ord(C1.Sp - S1p < C2.Sp - S2p);
    S1p := C1.Sp;
    S2p := C2.Sp;
  end;

var
  S1p, S1n, S2p, S2n: SizeInt;

begin
  S1p := 0;
  S2p := 0;
  S1n := Length(Str1);
  S2n := Length(Str2);
  while (S1p < S1n) and (S2p < S2n) do
  begin
    Result := ScanAndCompareTexts(Str1, S1p, Str2, S2p);
    if Result <> 0 then
      Exit;
    Result := ScanAndCompareNumbers(Str1, S1p, Str2, S2p);
    if Result <> 0 then
      Exit;
  end;
  Result := Ord(S1p < S1n) - Ord(S2p < S2n);
end;

function SplitString(const S, Delimiters: string): TRTLStringDynArray;

Var
  a : Array of Char;
  I : Integer;
  
begin
  SetLength(A,Length(Delimiters));
  For I:=1 to Length(Delimiters) do
    A[I-1]:=Delimiters[i];
  Result := S.Split(A);
end;

function NaturalCompareText (const S1 , S2 : string ): Integer ;
begin
  Result := NaturalCompareText(S1, S2,
                               DefaultFormatSettings.DecimalSeparator,
                               DefaultFormatSettings.ThousandSeparator);
end;

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

function LeftStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;

begin
  Result:=Copy(AText,1,ACount);
end;

function RightStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;

var j,l:SizeInt;

begin
  l:=length(atext);
  j:=ACount;
  if j>l then j:=l;
  Result:=Copy(AText,l-j+1,j);
end;

function MidStr(const AText: AnsiString; const AStart, ACount: SizeInt): AnsiString;

begin
  if (ACount=0) or (AStart>length(atext)) then
    exit('');
  Result:=Copy(AText,AStart,ACount);
end;



function LeftBStr(const AText: AnsiString; const AByteCount: SizeInt): AnsiString;

begin
  Result:=LeftStr(AText,AByteCount);
end;


function RightBStr(const AText: AnsiString; const AByteCount: SizeInt): AnsiString;
begin
  Result:=RightStr(Atext,AByteCount);
end;


function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: SizeInt): AnsiString;
begin
  Result:=MidStr(AText,AByteStart,AByteCount);
end;


function AnsiLeftStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;
begin
  Result := copy(AText,1,ACount);
end;


function AnsiRightStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;
begin
  Result := copy(AText,length(AText)-ACount+1,ACount);
end;


function AnsiMidStr(const AText: AnsiString; const AStart, ACount: SizeInt): AnsiString;
begin
  Result:=Copy(AText,AStart,ACount);
end;


function LeftStr(const AText: WideString; const ACount: SizeInt): WideString;
begin
  Result:=Copy(AText,1,ACount);
end;


function RightStr(const AText: WideString; const ACount: SizeInt): WideString;
var
  j,l:SizeInt;
begin
  l:=length(atext);
  j:=ACount;
  if j>l then j:=l;
  Result:=Copy(AText,l-j+1,j);
end;


function MidStr(const AText: WideString; const AStart, ACount: SizeInt): WideString;
begin
  Result:=Copy(AText,AStart,ACount);
end;


{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

type
  TEqualFunction = function (const a,b : AnsiChar) : boolean;

function EqualWithCase (const a,b : AnsiChar) : boolean;
begin
  result := (a = b);
end;

function EqualWithoutCase (const a,b : AnsiChar) : boolean;
begin
  result := (lowerCase(a) = lowerCase(b));
end;

function IsWholeWord (bufstart, bufend, wordstart, wordend : PAnsiChar) : boolean;
begin
            // Check start
  result := ((wordstart = bufstart) or ((wordstart-1)^ in worddelimiters)) and
            // Check end
            ((wordend = bufend) or ((wordend+1)^ in worddelimiters));
end;

function SearchDown(buf,aStart,endchar:PAnsiChar; SearchString:string;
    Equals : TEqualFunction; WholeWords:boolean) : PAnsiChar;
var Found : boolean;
    s, c : PAnsiChar;
begin
  result := aStart;
  Found := false;
  while not Found and (result <= endchar) do
    begin
    // Search first letter
    while (result <= endchar) and not Equals(result^,SearchString[1]) do
      inc (result);
    // Check if following is searchstring
    c := result;
    s := @(Searchstring[1]);
    Found := true;
    while (c <= endchar) and (s^ <> #0) and Found do
      begin
      Found := Equals(c^, s^);
      inc (c);
      inc (s);
      end;
    if s^ <> #0 then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,result,c-1);
    if not found then
      inc (result);
    end;
  if not Found then
    result := nil;
end;

function SearchUp(buf,aStart,endchar:PAnsiChar; SearchString:string;
    equals : TEqualFunction; WholeWords:boolean) : PAnsiChar;
var Found : boolean;
    s, c, l : PAnsiChar;
begin
  result := aStart;
  Found := false;
  l := @(SearchString[length(SearchString)]);
  while not Found and (result >= buf) do
    begin
    // Search last letter
    while (result >= buf) and not Equals(result^,l^) do
      dec (result);
    // Check if before is searchstring
    c := result;
    s := l;
    Found := true;
    while (c >= buf) and (s >= @SearchString[1]) and Found do
      begin
      Found := Equals(c^, s^);
      dec (c);
      dec (s);
      end;
    if (s >= @(SearchString[1])) then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,c+1,result);
    if found then
      result := c+1
    else
      dec (result);
    end;
  if not Found then
    result := nil;
end;

//function SearchDown(buf,aStart,endchar:PAnsiChar; SearchString:string; equal : TEqualFunction; WholeWords:boolean) : PAnsiChar;
function SearchBuf(Buf: PAnsiChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String; Options: TStringSearchOptions
  ): PAnsiChar;
var
  equal : TEqualFunction;
begin
  SelStart := SelStart + SelLength;
  if (SearchString = '') or (SelStart > BufLen) or (SelStart < 0) then
    result := nil
  else
    begin
    if soMatchCase in Options then
      Equal := @EqualWithCase
    else
      Equal := @EqualWithoutCase;
    if soDown in Options then
      result := SearchDown(buf,buf+SelStart,Buf+(BufLen-1), SearchString, Equal, (soWholeWord in Options))
    else
      result := SearchUp(buf,buf+SelStart,Buf+(Buflen-1), SearchString, Equal, (soWholeWord in Options));
    end;
end;


function SearchBuf(Buf: PAnsiChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String): PAnsiChar; // ; Options: TStringSearchOptions = [soDown]
begin
  Result:=SearchBuf(Buf,BufLen,SelStart,SelLength,SearchString,[soDown]);
end;

function PosEx(const SubStr, S: AnsiString; Offset: SizeInt): SizeInt;
begin
  Result := Pos(SubStr, S, Offset);
end;

function PosEx(c: AnsiChar; const S: Ansistring; Offset: SizeInt): SizeInt;
begin
  Result := Pos(c, S, Offset);
end; 

function PosEx(const SubStr, S: Ansistring): SizeInt;
begin
  Result := Pos(SubStr, S);
end;

function PosEx(const SubStr, S: UnicodeString; Offset: SizeInt): SizeInt;
begin
  Result := Pos(SubStr, S, Offset);
end;

function PosEx(c: WideChar; const S: UnicodeString; Offset: SizeInt): SizeInt;
begin
  Result := Pos(c, S, Offset);
end;

function PosEx(const SubStr, S: UnicodeString): Sizeint;
begin
  Result := Pos(SubStr, S);
end;


function StringsReplace(const S: AnsiString; OldPattern, NewPattern: array of AnsiString;  Flags: TReplaceFlags): string;

var pc,lastpc,litStart : PAnsiChar;
    iPattern,Rp,Ra,iFirstPattern,OldPatternLen : SizeInt;

    // Heads of the linked lists of patterns starting with a character whose code has this residue modulo length(firstPattern).
    // Length must be power of two, less = slower for large cases, more = slower for small cases.
    // 0 .. 255 (or directly [AnsiChar]) might be a bit too much because the array is initialized every time; and generalizes worse to S: UnicodeString :)
    firstPattern  : array[0 .. 63] of SizeInt;

    nextPattern   : PSizeInt; // Next pattern starting with the same character.
    nextPatternStatic: array[0 .. 63] of SizeInt;
    CompStr       : ansistring;
{$if sizeof(char) <> sizeof(ansichar)}
    tempStr       : string;
{$endif}

    procedure Append(P: PChar; N: SizeInt);
    begin
      if N>Ra-Rp then
        begin
        Ra:=Rp+N+4+Ra shr 1+Ra shr 2; // + N + const + 37.5%
        SetLength(Result,Ra);
        end;
      Move(P^,PChar(Pointer(Result))[Rp],N*SizeOf(Char));
      Rp:=Rp+N;
    end;

    // Mostly exists to force better register allocation for the main "pc < lastpc" loop, hotter than this procedure. :)
    // Returns the length of the found and replaced OldPattern item, or -1 if not found.
    function TryMatchAndReplace(pc,lastpc: PAnsiChar; iPattern: SizeInt): SizeInt;
    var
      pcc: PAnsiChar;
      OldPatternLen: SizeInt;
    begin
      repeat
        OldPatternLen:=Length(OldPattern[iPattern]);
        if (OldPatternLen <= (lastpc-pc)) and
           (CompareByte(OldPattern[iPattern,1],pc^,OldPatternLen*SizeOf(AnsiChar))=0) then
          begin
          pcc:=PAnsiChar(Pointer(S))+(pc-PAnsiChar(Pointer(CompStr)));
{$if sizeof(char)=sizeof(ansichar)}
          Append(litStart,pcc-litStart);
          Append(PChar(Pointer(NewPattern[iPattern])), Length(NewPattern[iPattern]));
{$else}
          tempStr := Copy(S,1+litStart-PAnsiChar(Pointer(S)),pcc-litStart);
          Append(PChar(Pointer(tempStr)), Length(tempStr));
          tempStr := NewPattern[iPattern];
          Append(PChar(Pointer(tempStr)), Length(tempStr));
{$endif}
          litStart := pcc+OldPatternLen;
          exit(OldPatternLen);
          end;
        iPattern := nextPattern[iPattern];
      until iPattern < 0;
      result := -1;
    end;

begin
  if High(OldPattern) <> High(NewPattern) then
    raise exception.Create(SErrAmountStrings);

  FillChar(firstPattern, sizeof(firstPattern), byte(-1));
  if High(OldPattern) <= High(nextPatternStatic) then
    nextPattern := PSizeInt(nextPatternStatic)
  else
    nextPattern := GetMem(Length(OldPattern) * sizeof(SizeInt));

  if rfIgnoreCase in Flags then
    begin
    CompStr := AnsiUpperCase(S);
    for iPattern := 0 to High(OldPattern) do
      OldPattern[iPattern] := AnsiUpperCase(OldPattern[iPattern]);
    end
  else
    CompStr := S;

  // The element added to the linked list last will be checked first, so add in reverse order.
  for iPattern := High(OldPattern) downto 0 do
    if OldPattern[iPattern] <> '' then
      begin
      iFirstPattern := ord(OldPattern[iPattern,1]) and High(firstPattern);
      nextPattern[iPattern] := firstPattern[iFirstPattern];
      firstPattern[iFirstPattern] := iPattern;
      end;

  Ra := Length(S); // Preallocation heuristic.
  SetLength(result, Ra);
  Rp := 0;
  pc := PAnsiChar(Pointer(CompStr));
  litStart := PAnsiChar(Pointer(S));
  lastpc := pc+Length(S);

  while pc < lastpc do
    begin
    iPattern := firstPattern[ord(pc^) and High(firstPattern)];
    inc(pc);
    if iPattern >= 0 then
      begin
      OldPatternLen := TryMatchAndReplace(pc-1, lastpc, iPattern);
      if OldPatternLen >= 0 then
        begin
        pc := pc-1+OldPatternLen;
        if not (rfReplaceAll in Flags) then
          break;
        end;
      end;
    end;
  if nextPattern <> PSizeInt(nextPatternStatic) then
    FreeMem(nextPattern);
  if litStart = PAnsiChar(Pointer(S)) then
    exit(S); // Unchanged string.
{$if sizeof(char)=sizeof(ansichar)}
  Append(litStart,PAnsiChar(Pointer(S))+(lastpc-PAnsiChar(Pointer(CompStr)))-litStart);
{$else}
  tempStr := Copy(S,1+litStart-PAnsiChar(Pointer(S)),PAnsiChar(Pointer(S))+(lastpc-PAnsiChar(Pointer(CompStr)))-litStart);
  Append(PChar(Pointer(tempStr)), Length(tempStr));
{$endif}
  SetLength(result,Rp);
end;

{ ---------------------------------------------------------------------
    Delphi compat
  ---------------------------------------------------------------------}

function ReplaceStr(const AText, AFromText, AToText: string): string;
begin
  result:=AnsiReplaceStr(AText, AFromText, AToText);
end;

function ReplaceText(const AText, AFromText, AToText: string): string;
begin
  result:=AnsiReplaceText(AText, AFromText, AToText);
end;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}
Const
  SScore : array[1..255] of AnsiChar =
     ('0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 1..32
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 33..64
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 65..90
      '0','0','0','0','0','0', // 91..96
      '0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 97..122
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 123..154
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 155..186
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 187..218
      '0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0', // 219..250
      '0','0','0','0','0'); // 251..255

function Soundex(const AText: string; ALength: TSoundexLength): string;

Var
  S,PS : AnsiChar;
  I,L : SizeInt;

begin
  Result:='';
  PS:=#0;
  If Length(AText)>0 then
    begin
    Result:=Upcase(AText[1]);
    I:=2;
    L:=Length(AText);
    While (I<=L) and (Length(Result)<ALength) do
      begin
      S:=SScore[Ord(AText[i])];
      If Not (S in ['0','i',PS]) then
        Result:=Result+S;
      If (S<>'i') then
        PS:=S;
      Inc(I);
      end;
    end;
  L:=Length(Result);
  If (L<ALength) then
    Result:=Result+StringOfChar('0',Alength-L);
end;



function Soundex(const AText: string): string; // ; ALength: TSoundexLength = 4

begin
  Result:=Soundex(AText,4);
end;

Const
  Ord0 = Ord('0');
  OrdA = Ord('A');

function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;

var
  SE: string;
  I: SizeInt;

begin
  Result:=-1;
  SE:=Soundex(AText,ALength);
  If Length(SE)>0 then
    begin
    Result:=Ord(SE[1])-OrdA;
    if ALength > 1 then
      begin
      Result:=Result*26+(Ord(SE[2])-Ord0);
      for I:=3 to ALength do
        Result:=(Ord(SE[I])-Ord0)+Result*7;
      end;
    Result:=ALength+Result*9;
    end;
end;


function SoundexInt(const AText: string): Integer; //; ALength: TSoundexIntLength = 4
begin
  Result:=SoundexInt(AText,4);
end;


function DecodeSoundexInt(AValue: Integer): string;

var
  I, Len: Integer;

begin
  Result := '';
  Len := AValue mod 9;
  AValue := AValue div 9;
  for I:=Len downto 3 do
    begin
    Result:=Chr(Ord0+(AValue mod 7))+Result;
    AValue:=AValue div 7;
    end;
  if Len>1 then
    begin
    Result:=Chr(Ord0+(AValue mod 26))+Result;
    AValue:=AValue div 26;
    end;
  Result:=Chr(OrdA+AValue)+Result;
end;


function SoundexWord(const AText: string): Word;

Var
  S : String;

begin
  S:=SoundEx(Atext,4);
  Result:=Ord(S[1])-OrdA;
  Result:=Result*26+ord(S[2])-48;
  Result:=Result*7+ord(S[3])-48;
  Result:=Result*7+ord(S[4])-48;
end;


function DecodeSoundexWord(AValue: Word): string;
begin
  Result := Chr(Ord0+ (AValue mod 7));
  AValue := AValue div 7;
  Result := Chr(Ord0+ (AValue mod 7)) + Result;
  AValue := AValue div 7;
  Result := IntToStr(AValue mod 26) + Result;
  AValue := AValue div 26;
  Result := Chr(OrdA+AValue) + Result;
end;


function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;
begin
  Result:=Soundex(AText,ALength)=Soundex(AOther,ALength);
end;


function SoundexSimilar(const AText, AOther: string): Boolean; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexSimilar(AText,AOther,4);
end;


function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;
begin
  Result:=AnsiCompareStr(Soundex(AText,ALength),Soundex(AOther,ALength));
end;


function SoundexCompare(const AText, AOther: string): Integer; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexCompare(AText,AOther,4);
end;


function SoundexProc(const AText, AOther: string): Boolean;
begin
  Result:=SoundexSimilar(AText,AOther);
end;

{ ---------------------------------------------------------------------
    RxStrUtils-like functions.
  ---------------------------------------------------------------------}


function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;

var
  i,l: SizeInt;

begin
  l:=Length(S);
  i:=1;
  Result:=True;
  while Result and (i<=l) do
    begin
    Result:=(S[i] in EmptyChars);
    Inc(i);
    end;
end;

function DelSpace(const S: string): string;

begin
  Result:=DelChars(S,' ');
end;

function IndexCharType(p: PChar; nchars: SizeInt; ch: Char): SizeInt; inline;
begin
  result :=
{$if sizeof(char) = sizeof(byte)} IndexByte
{$elseif sizeof(char) = sizeof(word)} IndexWord
{$else} {$error unknown char size}
{$endif}
    (p^, nchars, ord(ch));
end;

procedure FillCharType(p: PChar; nchars: SizeInt; ch: Char); inline;
begin
{$if sizeof(char) = sizeof(byte)} FillChar
{$elseif sizeof(char) = sizeof(word)} FillWord
{$else} {$error unknown char size}
{$endif}
    (p^, nchars, ord(ch));
end;

function DelChars(const S: string; Chr: Char): string;

var
  Sp, Se, Rp: PChar;
  ToCopy: SizeInt;

begin
  Sp := PChar(Pointer(S));
  Se := Sp + Length(S);
  ToCopy := IndexCharType(Sp, SizeUint(Pointer(Se) - Pointer(Sp)) div sizeof(Char), Chr);
  if ToCopy < 0 then
    exit(S); // Unchanged string.
  SetLength(result, SizeUint(Pointer(Se) - Pointer(Sp)) div sizeof(Char));
  Rp := PChar(Pointer(Result));
  repeat
    Move(Sp^, Rp^, ToCopy * sizeof(Char));
    Inc(Sp, ToCopy);
    Inc(Rp, ToCopy);
    repeat
      Inc(Sp); // Can increment to Se + 1.
    until (Sp >= Se) or (Sp^ <> Chr);
    if Sp >= Se then
      break;
    ToCopy := IndexCharType(Sp, SizeUint(Pointer(Se) - Pointer(Sp)) div sizeof(Char), Chr);
    if ToCopy < 0 then
      ToCopy := SizeUint(Pointer(Se) - Pointer(Sp)) div sizeof(Char);
  until false;
  SetLength(result, SizeUint(Pointer(Rp) - Pointer(Result)) div sizeof(Char));
end;

function DelChars(const S: string; Chars: TSysCharSet): string;

var
  Ss, Sp, Se, Rp: PChar;
  aDelta : SizeInt;
  
begin
  Ss := PChar(Pointer(S));
  Sp := Ss;
  Se := Sp + Length(S);
  while (Sp < Se) and not (Sp^ in Chars) do
    Inc(Sp);
  if Sp >= Se then
    Exit(S); // Unchanged string.
  SetLength(result, SizeUint(Pointer(Se) - Pointer(Ss)) div sizeof(Char));
  Rp := PChar(Pointer(Result));
  repeat
    aDelta:=(Pointer(Sp) - Pointer(Ss));
    Move(Ss^, Rp^, aDelta);
    Inc(Pointer(Rp), aDelta);
    repeat
      Inc(Sp); // Can increment to Se + 1.
    until (Sp >= Se) or not (Sp^ in Chars);
    if Sp >= Se then
      break;
    Ss := Sp;
    repeat
      Inc(Sp);
    until (Sp >= Se) or (Sp^ in Chars);
  until false;
  SetLength(result, SizeUint(Pointer(Rp) - Pointer(Result)) div sizeof(Char));
end;


function FindSpacePrecededBySpace(Sp, Se: PChar): PChar;
var
  SpacePos: SizeInt;
begin
  repeat
    SpacePos := IndexCharType(Sp, SizeUint(Pointer(Se) - Pointer(Sp)) div sizeof(Char), ' ');
    if SpacePos < 0 then
      Exit(Se);
    Inc(Sp, SpacePos+1);
  until (Sp>=Se) or (Sp^=' ');
  Result:=Sp;
end;


function DelSpace1(const S: string): string;

var
  Ss, Sp, Se, Rp: PChar;

begin
  Ss := PChar(Pointer(S));
  Se := Ss + Length(S);
  Sp := FindSpacePrecededBySpace(Ss, Se);
  if Sp >= Se then
    Exit(S); // Unchanged string.
  SetLength(result, SizeUint(Pointer(Se) - Pointer(Ss)) div sizeof(Char));
  Rp := PChar(Pointer(Result));
  repeat
    Move(Ss^, Rp^, Pointer(Sp) - Pointer(Ss));
    Inc(Pointer(Rp), Pointer(Sp) - Pointer(Ss));
    repeat
      Inc(Sp); // Can increment to Se + 1.
    until (Sp >= Se) or (Sp^ <> ' ');
    if Sp >= Se then
      break;
    Ss := Sp;
    Sp := FindSpacePrecededBySpace(Sp, Se);
  until false;
  SetLength(result, SizeUint(Pointer(Rp) - Pointer(Result)) div sizeof(Char));
end;

function Tab2Space(const S: string; Numb: Byte): string;

var
  I: SizeInt;

begin
  I:=1;
  Result:=S;
  while I <= Length(Result) do
    if Result[I]<>Chr(9) then
      inc(I)
    else
      begin
      Result[I]:=' ';
      If (Numb>1) then
        Insert(StringOfChar(' ',Numb-1),Result,I);
      Inc(I,Numb);
      end;
end;

function NPos(const C: string; const S: string; N: Integer): SizeInt;

begin
  Result:=0;
  if N>=1 then
    Repeat
      Result:=Pos(C,S,Result+1);
      dec(N);
    until (N=0) or (Result=0);
end;

function AddChar(C: AnsiChar; const S: string; N: Integer): string;

Var
  l : SizeInt;

begin
  l:=Length(S);
  if l>=N then
    Exit(S);
  SetLength(Result,N);
  FillCharType(Pointer(Result),N-l,Char(C));
  Move(Pointer(S)^,PChar(Pointer(Result))[N-l],l*sizeof(Char));
end;

function AddCharR(C: AnsiChar; const S: string; N: Integer): string;

Var
  l : SizeInt;

begin
  l:=Length(S);
  if l>=N then
    Exit(S);
  SetLength(Result,N);
  Move(Pointer(S)^,Pointer(Result)^,l*sizeof(Char));
  FillCharType(PChar(Pointer(Result))+l,N-l,Char(C));
end;


function PadRight(const S: string; N: Integer): string;inline;
begin
  Result:=AddCharR(' ',S,N);
end;


function PadLeft(const S: string; N: Integer): string;inline;
begin
  Result:=AddChar(' ',S,N);
end;


function Copy2Symb(const S: string; Symb: AnsiChar): string;

var
  p: SizeInt;

begin
  p:=Pos(Symb,S);
  if p=0 then
    p:=Length(S)+1;
  Result:=Copy(S,1,p-1);
end;

function Copy2SymbDel(var S: string; Symb: AnsiChar): string;

var
  p: SizeInt;

begin
  p:=Pos(Symb,S);
  if p=0 then
    begin
      result:=s;
      s:='';
    end
  else
    begin	
      Result:=Copy(S,1,p-1);
      delete(s,1,p);		
    end;
end;

function Copy2Space(const S: string): string;inline;
begin
  Result:=Copy2Symb(S,' ');
end;

function Copy2SpaceDel(var S: string): string;inline;
begin
  Result:=Copy2SymbDel(S,' ');
end;

function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;

var
  P,PE : PAnsiChar;

begin
  Result:=AnsiLowerCase(S);
  P:=PAnsiChar(pointer(Result));
  PE:=P+Length(Result);
  while (P<PE) do
    begin
    while (P<PE) and (P^ in WordDelims) do
      inc(P);
    if (P<PE) then
      P^:=UpCase(P^);
    while (P<PE) and not (P^ in WordDelims) do
      inc(P);
    end;
end;

function WordCount(const S: string; const WordDelims: TSysCharSet): SizeInt;

var
  P,PE : PAnsiChar;
  WasDelim, NowDelim : Boolean;

begin
  Result:=0;
  P:=PAnsiChar(pointer(S));
  PE:=P+Length(S);
  WasDelim:=true;
  while (P<PE) do
    begin
      NowDelim := P^ in WordDelims;
      Inc(Result,ord(WasDelim and not NowDelim));
      WasDelim := NowDelim;
      Inc(P);
    end;
end;

function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): SizeInt;

var
  P,PE : PChar;
  Count: Integer;

begin
  P:=PChar(pointer(S));
  PE:=P+Length(S);
  Count:=N;
  while (P<PE) and (P^ in WordDelims) do
    Inc(P);
  while (P<PE) do
    begin
      Dec(Count);
      if Count<=0 then
        break;
      repeat
        Inc(P);
      until (P>=PE) or (P^ in WordDelims);
      repeat
        Inc(P); // Can increment to PE + 1.
      until (P>=PE) or not (P^ in WordDelims);
    end;
  if (P<PE) and (Count=0) then
    exit(P-PChar(pointer(S))+1);
  Result:=0;
end;


function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;inline;
var
  i: SizeInt;
begin
  Result:=ExtractWordPos(N,S,WordDelims,i);
end;


function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: Integer): string;

var
  i,j: SizeInt;

begin
  i:=WordPosition(N, S, WordDelims);
  if not ((I>0) and (I<=High(Integer))) then
    begin
    Result:='';
    Pos:=0;
    Exit;
    end;
  Pos:=i;
  j:=PosSetEx(WordDelims, S, i+1);
  if j=0 then
    j:=Length(S)+1;
  Result:=Copy(S,i,j-i);
end;

{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; Out Pos: SizeInt): string;
var
  i,j: SizeInt;

begin
  i:=WordPosition(N, S, WordDelims);
  Pos:=i;
  if I<=0 then
    begin
    Result:='';
    Exit;
    end;
  j:=PosSetEx(WordDelims, S, i+1);
  if j=0 then
    j:=Length(S)+1;
  Result:=Copy(S,i,j-i);
end;
{$ENDIF}

function ExtractDelimited(N: Integer; const S: string; const Delims: TSysCharSet): string;
var
  i,start,len: SizeInt;
begin
  i:=1;
  len:=Length(S);
  while (i<=len) and (N>1) do
    begin
      dec(N,ord(S[i] in Delims));
      inc(i);
    end;
  if N<>1 then
    exit('');
  start:=i;
  while (i<=len) and not (S[i] in Delims) do
    inc(i);
  exit(Copy(S,start,i-start));
end;

{$IF SIZEOF(SIZEINT)<>SIZEOF(INTEGER)}
function ExtractSubstr(const S: string; var Pos: SizeInt; const Delims: TSysCharSet): string;

var
  i,l: SizeInt;

begin
  i:=Pos;
  l:=Length(S);
  while (i<=l) and not (S[i] in Delims) do
    inc(i);
  Result:=Copy(S,Pos,i-Pos);
  while (i<=l) and (S[i] in Delims) do
    inc(i);
  Pos:=i;
end;
{$ENDIF}

function ExtractSubstr(const S: string; var Pos: Integer; const Delims: TSysCharSet): string;

var
  i,l: SizeInt;

begin
  i:=Pos;
  l:=Length(S);
  while (i<=l) and not (S[i] in Delims) do
    inc(i);
  Result:=Copy(S,Pos,i-Pos);
  while (i<=l) and (S[i] in Delims) do
    inc(i);
  if I>MaxInt then
    Pos:=MaxInt
  else
    Pos:=i;
end;

function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;

var
  P,PE : PChar;
  Wn,Sn,Wi : SizeInt;
  Wstartc : Char;

begin
  Wn:=Length(W);
  Sn:=Length(S);
  if (Sn=0) or (Wn=0) or (Wn>Sn) then
    exit(false);
  Wstartc:=W[1];
  P:=PChar(pointer(S))-1; { Loop starts with an extra increment. }
  PE:=P+2+Sn-Wn;
  repeat
    P:=P+1;
    Wi:=IndexCharType(P,SizeUint(Pointer(PE)-Pointer(P)) div SizeOf(Char),Wstartc);
    P:=P+Wi;
  until (Wi<0) or ((P=PChar(Pointer(S))) or (P[-1] in WordDelims)) and ((P+1=PE) or (P[Wn] in WordDelims)) and (CompareByte(P^,Pointer(W)^,Wn*SizeOf(Char))=0);
  result:=Wi>=0;
end;


function Numb2USA(const S: string): string;
var
  i, NA: Integer;
begin
  i:=Length(S);
  Result:=S;
  NA:=0;
  while (i > 0) do begin
    if ((Length(Result) - i + 1 - NA) mod 3 = 0) and (i <> 1) then
    begin
      insert(',', Result, i);
      inc(NA);
    end;
    Dec(i);
  end;
end;

function PadCenter(const S: string; Len: SizeInt): string;
var
  Ns,Nfirstspaces: SizeInt;
begin
  Ns:=Length(S);
  if Ns>=Len then
    exit(S);
  SetLength(Result,Len);
  Nfirstspaces:=SizeUint(Len) div 2-SizeUint(Ns) div 2;
  FillCharType(Pointer(Result),Nfirstspaces,' ');
  FillCharType(PChar(Pointer(Result))+Ns+Nfirstspaces,Len-Ns-Nfirstspaces,' ');
  Move(Pointer(S)^,PChar(Pointer(Result))[Nfirstspaces],Ns*sizeof(char));
end;


function Dec2Numb(N: Longint; Len, Base: Byte): string;

var
  C: Integer;
  Number: Longint;

begin
  if N=0 then
    Result:='0'
  else
    begin
    Number:=N;
    Result:='';
    while Number>0 do
      begin
      C:=Number mod Base;
      if C>9 then
        C:=C+55
      else
        C:=C+48;
      Result:=Chr(C)+Result;
      Number:=Number div Base;
      end;
    end;
  if (Result<>'') then
    Result:=AddChar('0',Result,Len);
end;

function Numb2Dec(S: string; Base: Byte): Longint;

var
  i, P: sizeint;

begin
  i:=Length(S);
  Result:=0;
  S:=UpperCase(S);
  P:=1;
  while (i>=1) do
    begin
    if (S[i]>'@') then
      Result:=Result+(Ord(S[i])-55)*P
    else
      Result:=Result+(Ord(S[i])-48)*P;
    Dec(i);
    P:=P*Base;
    end;
end;


function RomanToIntDontCare(const S: String): Longint;
{This was the original implementation of RomanToInt,
 it is internally used in TryRomanToInt when Strictness = rcsDontCare}
const
  RomanChars  = ['C','D','I','L','M','V','X'];
  RomanValues : array['C'..'X'] of Word
              = (100,500,0,0,0,0,1,0,0,50,1000,0,0,0,0,0,0,0,0,5,0,10);

var
  index, Next: AnsiChar;
  i,l: SizeInt;
  Negative: Boolean;

begin
  Result:=0;
  i:=0;
  Negative:=(Length(S)>0) and (S[1]='-');
  if Negative then
    inc(i);
  l:=Length(S);
  while (i<l) do
    begin
    inc(i);
    index:=UpCase(S[i]);
    if index in RomanChars then
      begin
      if Succ(i)<=l then
        Next:=UpCase(S[i+1])
      else
        Next:=#0;
      if (Next in RomanChars) and (RomanValues[index]<RomanValues[Next]) then
        begin
        inc(Result, RomanValues[Next]);
        Dec(Result, RomanValues[index]);
        inc(i);
        end
      else
        inc(Result, RomanValues[index]);
      end
    else
      begin
      Result:=0;
      Exit;
      end;
    end;
  if Negative then
    Result:=-Result;
end;


{ TryRomanToInt: try to convert a roman numeral to an integer
  Parameters:
  S: Roman numeral (like: 'MCMXXII')
  N: Integer value of S (only meaningfull if the function succeeds)
  Stricness: controls how strict the parsing of S is
    - rcsStrict:
      * Follow common subtraction rules
         - only 1 preceding subtraction character allowed: IX = 9, but IIX <> 8
         - from M you can only subtract C
         - from D you can only subtract C
         - from C you can only subtract X
         - from L you can only subtract X
         - from X you can only subtract I
         - from V you can only subtract I
      *  The numeral is parsed in "groups" (first M's, then D's etc.), the next group to be parsed
         must always be of a lower denomination than the previous one.
         Example: 'MMDCCXX' is allowed but 'MMCCXXDD' is not
      * There can only ever be 3 consecutive M's, C's, X's or I's
      * There can only ever be 1 D, 1 L and 1 V
      * After IX or IV there can be no more characters
      * Negative numbers are not supported
      // As a consequence the maximum allowed Roman numeral is MMMCMXCIX = 3999, also N can never become 0 (zero)

    - rcsRelaxed: Like rcsStrict but with the following exceptions:
      * An infinite number of (leading) M's is allowed
      * Up to 4 consecutive M's, C's, X's and I's are allowed
      // So this is allowed: 'MMMMMMCXIIII'  = 6124

    - rcsDontCare:
      * no checking on the order of "groups" is done
      * there are no restrictions on the number of consecutive chars
      * negative numbers are supported
      * an empty string as input will return True and N will be 0
      * invalid input will return false
      // for backwards comatibility: it supports rather ludicrous input like '-IIIMIII' -> -(2+(1000-1)+3)=-1004
}

function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness = rcsRelaxed): Boolean;

var
  i, Len: SizeInt;
  Terminated: Boolean;

begin
  Result := (False);
  S := UpperCase(S);  //don't use AnsiUpperCase please
  Len := Length(S);
  if (Strictness = rcsDontCare) then
  begin
    N := RomanToIntDontCare(S);
    if (N = 0) then
    begin
      Result := (Len = 0);
    end
    else
      Result := True;
    Exit;
  end;
  if (Len = 0) then
  begin
    Result:=true;
    N:=0;
    Exit;
  end;
  i := 1;
  N := 0;
  Terminated := False;
  //leading M's
  while (i <= Len) and ((Strictness <> rcsStrict) or (i < 4)) and (S[i] = 'M') do
  begin
    //writeln('TryRomanToInt: Found 1000');
    Inc(i);
    N := N + 1000;
  end;
  //then CM or or CD or D or (C, CC, CCC, CCCC)
  if (i <= Len) and (S[i] = 'D') then
  begin
    //writeln('TryRomanToInt: Found 500');
    Inc(i);
    N := N + 500;
  end
  else if (i + 1 <= Len) and (S[i] = 'C') then
  begin
    if (S[i+1] = 'M') then
    begin
      //writeln('TryRomanToInt: Found 900');
      Inc(i,2);
      N := N + 900;
    end
    else if (S[i+1] = 'D') then
    begin
      //writeln('TryRomanToInt: Found 400');
      Inc(i,2);
      N := N + 400;
    end;
  end ;
  //next max 4 or 3 C's, depending on Strictness
  if (i <= Len) and (S[i] = 'C') then
  begin
    //find max 4 C's
    //writeln('TryRomanToInt: Found 100');
    Inc(i);
    N := N + 100;
    if (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
    if (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
  end;

  //then XC or XL
  if (i + 1 <= Len) and (S[i] = 'X') then
  begin
    if (S[i+1] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 90');
      Inc(i,2);
      N := N + 90;
    end
    else if  (S[i+1] = 'L') then
    begin
      //writeln('TryRomanToInt: Found 40');
      Inc(i,2);
      N := N + 40;
    end;
  end;

  //then L
  if (i <= Len) and (S[i] = 'L') then
  begin
    //writeln('TryRomanToInt: Found 50');
    Inc(i);
    N := N + 50;
  end;

  //then (X, xx, xxx, xxxx)
  if (i <= Len) and (S[i] = 'X') then
  begin
    //find max 3 or 4 X's, depending on Strictness
    //writeln('TryRomanToInt: Found 10');
    Inc(i);
    N := N + 10;
    if (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
    if (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
  end;

  //then IX or IV
  if (i + 1 <= Len) and (S[i] = 'I') then
  begin
    if (S[i+1] = 'X') then
    begin
      Terminated := (True);
      //writeln('TryRomanToInt: Found 9');
      Inc(i,2);
      N := N + 9;
    end
    else if (S[i+1] = 'V') then
    begin
      Terminated := (True);
      //writeln('TryRomanToInt: Found 4');
      Inc(i,2);
      N := N + 4;
    end;
  end;

  //then V
  if (not Terminated) and (i <= Len) and (S[i] = 'V') then
  begin
    //writeln('TryRomanToInt: Found 5');
    Inc(i);
    N := N + 5;
  end;


  //then I
  if (not Terminated) and (i <= Len) and (S[i] = 'I') then
  begin
    Terminated := (True);
    //writeln('TryRomanToInt: Found 1');
    Inc(i);
    N := N + 1;
    //Find max 2 or 3 closing I's, depending on strictness
    if (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
    if (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
  end;

  //writeln('TryRomanToInt: Len = ',Len,' i = ',i);
  Result := (i > Len);
  //if Result then writeln('TryRomanToInt: N = ',N);

end;

function RomanToInt(const S: string; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;
begin
  if not TryRomanToInt(S, Result, Strictness) then
    raise EConvertError.CreateFmt(SInvalidRomanNumeral,[S]);
end;

function RomanToIntDef(const S: String; const ADefault: Longint;
  Strictness: TRomanConversionStrictness): Longint;
begin
  if not TryRomanToInt(S, Result, Strictness) then
    Result := ADefault;
end;




function IntToRoman(Value: Longint): string;

const
  Arabics : Array[1..13] of Integer
          = (1,4,5,9,10,40,50,90,100,400,500,900,1000);
  Romans  :  Array[1..13] of String
          = ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M');

var
  i: Integer;

begin
  Result:='';
  for i:=13 downto 1 do
    while (Value >= Arabics[i]) do
      begin
        Value:=Value-Arabics[i];
        Result:=Result+Romans[i];
      end;
end;

function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
var endpos : integer;
    p,p2:PChar;
    k: integer;
begin
  Result:='';
  if (Digits>32) then
    Digits:=32;
  if (spaces=0) then
   begin
     result:=inttobin(value,digits);
     exit;
   end;
  endpos:=digits+ (digits-1) div spaces;
  setlength(result,endpos);
  p:=@result[endpos];
  p2:=@result[1];
  k:=spaces;
  while (p>=p2) do
    begin
      if k=0 then
       begin
         p^:=' ';
         dec(p);
         k:=spaces;
       end;
      p^:=chr(48+(cardinal(value) and 1));
      value:=cardinal(value) shr 1;
      dec(p); 
      dec(k);
   end;
end;

function IntToBin(Value: Longint; Digits: Integer): string;
var p,p2 : PChar;
begin
  result:='';
  if digits<=0 then exit;
  setlength(result,digits);
  p:=PChar(pointer(@result[digits]));
  p2:=PChar(pointer(@result[1]));
  // typecasts because we want to keep intto* delphi compat and take an integer
  while (p>=p2) and (cardinal(value)>0) do     
    begin
       p^:=chr(48+(cardinal(value) and 1));
       value:=cardinal(value) shr 1;
       dec(p); 
    end;
  digits:=p-p2+1;
  if digits>0 then
    fillchar(result[1],digits,#48);
end;

function intToBin(Value: int64; Digits:integer): string;
var p,p2 : PChar;
begin
  result:='';
  if digits<=0 then exit;
  setlength(result,digits);
  p:=PChar(pointer(@result[digits]));
  p2:=PChar(pointer(@result[1]));
  // typecasts because we want to keep intto* delphi compat and take a signed val
  // and avoid warnings
  while (p>=p2) and (qword(value)>0) do     
    begin
       p^:=chr(48+(cardinal(value) and 1));
       value:=qword(value) shr 1;
       dec(p); 
    end;
  digits:=p-p2+1;
  if digits>0 then
    fillchar(result[1],digits,#48);
end;


function FindPart(const HelpWilds, InputStr: string): SizeInt;
var
  i, J, NWilds: SizeInt;

begin
  if Pos('?',HelpWilds)=0 then
    Exit(Pos(HelpWilds, inputStr));
  NWilds:=Length(HelpWilds);
  for i:=0 to Length(inputStr) - NWilds do
    begin
    J:=1;
    while (J<=NWilds) and ((inputStr[i + J] = HelpWilds[J]) or (HelpWilds[J] = '?')) do
      Inc(J);
    if J>NWilds then
      Exit(i+1);
    end;
  Result:=0;
end;

function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;
var
  Wp,We,Ip,Ie,WpBack,IpBack: PChar;
begin
  if ignoreCase then { upcase all letters }
    begin
    inputStr:=AnsiUpperCase(inputStr);
    Wilds:=AnsiUpperCase(Wilds);
    end;
  Wp:=PChar(Pointer(Wilds));
  We:=Wp+Length(Wilds);
  Ip:=PChar(Pointer(InputStr));
  Ie:=Ip+Length(InputStr);
  WpBack:=nil;
  while Ip<Ie do
    begin
    if Wp<We then
      if (Wp^=Ip^) or (Wp^='?') then
        begin
        Inc(Ip);
        Inc(Wp);
        continue;
        end
      else if Wp^='*' then
        begin
        Inc(Wp);
        WpBack:=Wp;
        IpBack:=Ip;
        continue;
        end;
    if not Assigned(WpBack) then
      exit(false);
    Wp:=WpBack;
    Inc(IpBack);
    Ip:=IpBack;
    end;
  while (Wp<We) and (Wp^='*') do
    Inc(Wp);
  Result:=Wp=We;
end;


function XorString(const Key, Src: ShortString): ShortString;
var
  i: SizeInt;
begin
  Result:=Src;
  if Length(Key) > 0 then
    for i:=1 to Length(Src) do
      Result[i]:=Chr(Byte(Key[1 + ((i - 1) mod Length(Key))]) xor Ord(Src[i]));
end;

function XorEncode(const Key, Source: Ansistring): Ansistring;

var
  i: Integer;
  C: Byte;

begin
  Result:='';
  for i:=1 to Length(Source) do
    begin
    if Length(Key) > 0 then
      C:=Byte(Key[1 + ((i - 1) mod Length(Key))]) xor Byte(Source[i])
    else
      C:=Byte(Source[i]);
    Result:=Result+AnsiLowerCase(intToHex(C, 2));
    end;
end;

function XorDecode(const Key, Source: Ansistring): Ansistring;
var
  i: Integer;
  C: AnsiChar;
begin
  Result:='';
  for i:=0 to Length(Source) div 2 - 1 do
    begin
    C:=Chr(StrTointDef('$' + Copy(Source, (i * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C:=Chr(Byte(Key[1 + (i mod Length(Key))]) xor Byte(C));
    Result:=Result + C;
    end;
end;

function GetCmdLineArg(const Switch: string; SwitchChars: TSysCharSet): string;
var
  i: Integer;
  S: string;
begin
  i:=1;
  Result:='';
  while (Result='') and (i<=ParamCount) do
    begin
    S:=ParamStr(i);
    if (SwitchChars=[]) or ((S[1] in SwitchChars) and (Length(S) > 1)) and
       (AnsiCompareText(Copy(S,2,Length(S)-1),Switch)=0) then
      begin
      inc(i);
      if i<=ParamCount then
        Result:=ParamStr(i);
      end;
    inc(i);
    end;
end;

function RPosEx(C: AnsiChar; const S: AnsiString; offs: SizeInt): SizeInt;

var p,p2: PAnsiChar;

Begin
 If (offs>0) and (offs<=Length(S)) Then
   begin
     p:=@s[offs];
     p2:=@s[1];
     while (p2<=p) and (p^<>c) do dec(p);
     RPosEx:=(p-p2)+1;
   end
  else
    RPosEX:=0;
End;

function RPos(c: AnsiChar; const S: AnsiString): SizeInt;

Begin
 Result:=RPosEx(c,S,Length(S)); { Length(S) must be used because character version returns 0 on offs > length. }
End;

function RPos(const Substr: AnsiString; const Source: AnsiString): SizeInt;
begin
  Result:=RPosEx(Substr,Source,High(Result)); { High(Result) is possible because string version clamps offs > length to offs = length. }
end;

function RPosEx(const Substr: AnsiString; const Source: AnsiString; offs: SizeInt): SizeInt;
var
  MaxLen,llen : SizeInt;
  c : AnsiChar;
  pc,pc2 : PAnsiChar;
begin
  llen:=Length(SubStr);
  maxlen:=length(source);
  if offs<maxlen then maxlen:=offs;
  if (llen>0) and (maxlen>0) and ( llen<=maxlen)  then
   begin
     pc:=@source[maxlen-llen+1];
     pc2:=@source[1];
     c:=substr[1];
     repeat
       if (c=pc^) and
          (CompareByte(Substr[1],pc^,llen)=0) then
        begin
          rPosex:=pc-pc2+1;
          exit;
        end;
       dec(pc);
     until pc<pc2;
   end;
  rPosex:=0;
end;

function RPosEx(C: unicodechar; const S: UnicodeString; offs: SizeInt): SizeInt;

var p,p2: PUnicodeChar;

Begin
 If (offs>0) and (offs<=Length(S)) Then
   begin
     p:=@s[offs];
     p2:=@s[1];
     while (p2<=p) and (p^<>c) do dec(p);
     RPosEx:=SizeUint(pointer(p)-pointer(p2)) div sizeof(unicodechar)+1; { p-p2+1 but avoids signed division... }
   end
  else
    RPosEX:=0;
End;

function RPos(c: Unicodechar; const S: UnicodeString): SizeInt;

Begin
 Result:=RPosEx(c,S,Length(S)); { Length(S) must be used because character version returns 0 on offs > length. }
End;

function RPos(const Substr: UnicodeString; const Source: UnicodeString): SizeInt;
begin
  Result:=RPosEx(Substr,Source,High(Result)); { High(Result) is possible because string version clamps offs > length to offs = length. }
end;

function RPosEx(const Substr: UnicodeString; const Source: UnicodeString; offs: SizeInt): SizeInt;
var
  MaxLen,llen : SizeInt;
  c : unicodechar;
  pc,pc2 : punicodechar;
begin
  llen:=Length(SubStr);
  maxlen:=length(source);
  if offs<maxlen then maxlen:=offs;
  if (llen>0) and (maxlen>0) and ( llen<=maxlen)  then
   begin
     pc:=@source[maxlen-llen+1];
     pc2:=@source[1];
     c:=substr[1];
     repeat
       if (c=pc^) and
          (Compareword(Substr[1],pc^,llen)=0) then
        begin
          rPosex:=SizeUint(pointer(pc)-pointer(pc2)) div sizeof(unicodechar)+1; { pc-pc2+1 but avoids signed division... }
          exit;
        end;
       dec(pc);
     until pc<pc2;
   end;
  rPosex:=0;
end;

procedure BinToHex(BinValue: PAnsiChar; HexValue: PAnsiChar; BinBufSize: Integer);

 var
   i : longint;
 begin
  for i:=0 to BinBufSize-1 do
  begin
    HexValue[0]:=HexDigits[((Ord(BinValue[i]) shr 4))];
    HexValue[1]:=HexDigits[((Ord(BinValue[i]) and 15))];
    Inc(HexValue,2);
  end;
end;

procedure BinToHex(BinValue: PAnsiChar; HexValue: PWideChar; BinBufSize: Integer);

var
  i : longint;
begin
  for i:=0 to BinBufSize-1 do
  begin
    HexValue[0]:=HexDigitsW[((Ord(BinValue[i]) shr 4))];
    HexValue[1]:=HexDigitsW[((Ord(BinValue[i]) and 15))];
    Inc(HexValue,2);
  end;
end;

procedure BinToHex(const BinBuffer: TBytes; BinBufOffset: Integer; var HexBuffer: TBytes; HexBufOffset: Integer; Count: Integer);

var
  i : longint;
begin
  if (Length(HexBuffer)-HexBufOffset) div 2 < Count then
    Count:=(Length(HexBuffer)-HexBufOffset) div 2;
  if (Length(BinBuffer)-BinBufOffset) < Count then
    Count:=Length(BinBuffer)-BinBufOffset;
  for i:=0 to Count-1 do
  begin
    HexBuffer[HexBufOffset+2*i+0]:=Byte(HexDigits[(BinBuffer[BinBufOffset + i] shr 4)]);
    HexBuffer[HexBufOffset+2*i+1]:=Byte(HexDigits[(BinBuffer[BinBufOffset + i] and 15)]);
  end;
end;

procedure BinToHex(BinValue: Pointer; HexValue: PAnsiChar; BinBufSize: Integer);
begin
  BinToHex(PAnsiChar(BinValue), HexValue, BinBufSize);
end;

procedure BinToHex(BinValue: Pointer; HexValue: PWideChar; BinBufSize: Integer);
begin
  BinToHex(PAnsiChar(BinValue), HexValue, BinBufSize);
end;

procedure BinToHex(const BinValue; HexValue: PAnsiChar; BinBufSize: Integer);
begin
  BinToHex(PAnsiChar(BinValue), HexValue, BinBufSize);
 end;
 
procedure BinToHex(const BinValue; HexValue: PWideChar; BinBufSize: Integer);
begin
  BinToHex(PAnsiChar(BinValue), HexValue, BinBufSize);
end;


function HexToBin(const HexText: PWideChar; HexTextOffset: Integer; var BinBuffer: TBytes; BinBufOffset: Integer; Count: Integer): Integer;
var
  i : Integer;
  PText : PWideChar;
  PBinBuf : PAnsiChar;
begin
  PText:=HexText+HexTextOffset;
  PBinBuf:=PAnsiChar(BinBuffer)+BinBufOffset;
  i:=Count;
  if (Length(BinBuffer)-BinBufOffset)<i then
    i:=Length(BinBuffer)-BinBufOffset;
  if i<=0 then
    Result:=0
  else
    Result:=HexToBin(PText, PBinBuf, i);
end;

function HexToBin(const HexText: TBytes; HexTextOffset: Integer; var BinBuffer: TBytes; BinBufOffset: Integer; Count: Integer): Integer;
var
  i : Integer;
  PText : PAnsiChar;
  PBinBuf : PAnsiChar;
begin
  PText:=PAnsiChar(HexText)+HexTextOffset;
  PBinBuf:=PAnsiChar(BinBuffer)+BinBufOffset;
  i:=Count;
  if (Length(HexText)-HexTextOffset) div 2 < i then
    i:=(Length(HexText)-HexTextOffset) div 2;
  if (Length(BinBuffer)-BinBufOffset) < i then
    i:=Length(BinBuffer)-BinBufOffset;
  if i<=0 then
    Result:=0
  else
    Result:=HexToBin(PText, PBinBuf, i);
end;

function HexToBin(HexText: PWideChar; BinBuffer: Pointer; BinBufSize: Integer): Integer;
begin
  Result:=HexToBin(HexText, PAnsiChar(BinBuffer), BinBufSize);
end;

function HexToBin(const HexText: PWideChar; var BinBuffer; BinBufSize: Integer): Integer;
begin
  Result:=HexToBin(HexText, PAnsiChar(BinBuffer), BinBufSize);
end;

function HexToBin(HexText: PAnsiChar; BinBuffer: PAnsiChar; BinBufSize: Integer): Integer;

var
  i,num : integer;

begin
  i:=BinBufSize;
  while (i>0) do
    begin
    // get value of first character
    case HexText^ of
      '0'..'9':
        num:=ord(HexText^)-ord('0');
      'a'..'f':
        num:=ord(HexText^)-(ord('a')-10);
      'A'..'F':
        num:=ord(HexText^)-(ord('A')-10);
      else
        break;
    end;

    // add value of second character
    case HexText[1] of
      '0'..'9':
        num:=num shl 4 or (ord(HexText[1])-ord('0'));
      'a'..'f':
        num:=num shl 4 or (ord(HexText[1])-(ord('a')-10));
      'A'..'F':
        num:=num shl 4 or (ord(HexText[1])-(ord('A')-10));
      else
        break;
    end;

    BinBuffer^:=AnsiChar(num);
    inc(BinBuffer);

    inc(HexText,2);
    dec(i);
    end;
  Result:=BinBufSize-i;
end;


function HexToBin(HexText: PWideChar; BinBuffer: PAnsiChar; BinBufSize: Integer): Integer;
var
  i,num : integer;
begin
  i:=BinBufSize;
  while (i>0) do
  begin
    // get value of first character
    case HexText^ of
      '0'..'9':
        num:=ord(HexText^)-ord('0');
      'a'..'f':
        num:=ord(HexText^)-(ord('a')-10);
      'A'..'F':
        num:=ord(HexText^)-(ord('A')-10);
      else // this includes >#255.
        break;
     end;

    // add value of second character
    case HexText[1] of
      '0'..'9':
        num:=num shl 4 or (ord(HexText[1])-ord('0'));
      'a'..'f':
        num:=num shl 4 or (ord(HexText[1])-(ord('a')-10));
      'A'..'F':
        num:=num shl 4 or (ord(HexText[1])-(ord('A')-10));
      else // this includes >#255.
        break;
    end;

    BinBuffer^:=AnsiChar(num);
    inc(BinBuffer);

    inc(HexText,2);
    dec(i);
  end;
  Result:=BinBufSize-i;
end;

function HexToBin(HexText: PAnsiChar; var BinBuffer; BinBufSize: Integer): Integer;
begin
  Result:=HexToBin(HexText, PAnsiChar(BinBuffer), BinBufSize);
end;

function HexToBin(const HexText: PAnsiChar; BinBuffer: Pointer; BinBufSize: Integer): Integer;
begin
  Result:=HexToBin(HexText, PAnsiChar(BinBuffer), BinBufSize);
end;

function PosSetEx(const c: TSysCharSet; const s: ansistring; count: Integer): SizeInt;

var i,j:SizeInt;

begin
 i:=length(s);
 j:=count;
 while (j<=i) and (not (s[j] in c)) do inc(j);
 if (j>i) then
  j:=0;                                         // not found.
 result:=j;
end;

function PosSet(const c: TSysCharSet; const s: ansistring): SizeInt;

begin
  result:=possetex(c,s,1);
end;

function StringToCharset(const c: string): TSysCharSet;
var
  i: SizeInt;
begin
  result:=[];
  for i:=1 to length(c) do
    include(result,c[i]);
end;

function PosSetEx(const c: string; const s: ansistring; count: Integer): SizeInt;
begin
  result:=0;
  if length(c)>0 then
    result:=possetex(StringToCharset(c),s,count);
end;

function PosSet(const c: string; const s: ansistring): SizeInt;
begin
  result:=0;
  if length(c)>0 then
    result:=possetex(StringToCharset(c),s,1);
end;


procedure Removeleadingchars(VAR S: AnsiString; const CSet: TSysCharset);

VAR I,J : Longint;

Begin
 I:=Length(S); 
 J:=1;
 While (J<=I) And (S[J] IN CSet) DO
  INC(J);
 IF J>1 Then
  Delete(S,1,J-1);
End;


procedure Removeleadingchars(VAR S: UnicodeString; const CSet: TSysCharset);

VAR I,J : Longint;

Begin
 I:=Length(S);
 J:=1;
 While (J<=I) And (S[J] IN CSet) DO
  INC(J);
 IF J>1 Then
  Delete(S,1,J-1);
End;



function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;

begin
  result:=s;
  removeleadingchars(result,cset); 
end;

procedure RemoveTrailingChars(VAR S: AnsiString; const CSet: TSysCharset);

VAR I,J: LONGINT;

Begin
 I:=Length(S);
 J:=I;
 While (j>0) and (S[J] IN CSet) DO DEC(J);
 IF J<>I Then
  SetLength(S,J);
End;

procedure RemoveTrailingChars(VAR S: UnicodeString; const CSet: TSysCharset);

VAR I,J: LONGINT;

Begin
 I:=Length(S);
 J:=I;
 While (j>0) and (S[J] IN CSet) DO DEC(J);
 IF J<>I Then
  SetLength(S,J);
End;


function TrimRightSet(const S: String; const CSet: TSysCharSet): String;

begin
  result:=s;
  RemoveTrailingchars(result,cset); 
end;

procedure RemovePadChars(VAR S: AnsiString; const CSet: TSysCharset);

VAR J,K: SizeInt;

Begin
 J:=Length(S);
 While (j>0) and (S[J] IN CSet) DO DEC(J);
 k:=1;
 While (k<=J) And (S[k] IN CSet) DO
   INC(k);
 IF k>1 Then
   move(s[k],s[1],(j-k+1)*sizeof(S[1]));
 setlength(s,j-k+1);
End;

procedure RemovePadChars(VAR S: UnicodeString; const CSet: TSysCharset);

VAR J,K: SizeInt;

Begin
 J:=Length(S);
 While (j>0) and (S[J] IN CSet) DO DEC(J);
 k:=1;
 While (k<=J) And (S[k] IN CSet) DO
   INC(k);
 IF k>1 Then
   move(s[k],s[1],(j-k+1)*sizeof(S[1]));
 setlength(s,j-k+1);
End;


function TrimSet(const S: String;const CSet:TSysCharSet): String;

begin
  result:=s;
  RemovePadChars(result,cset); 
end;


Function SplitCommandLine(S : RawByteString) : TRawByteStringArray;

  Function GetNextWord : RawByteString;

  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : AnsiChar;

    Procedure AppendToResult;

    begin
      Result:=Result+Copy(S,WStart,WEnd-WStart);
      WStart:=Wend+1;
    end;

  begin
    Result:='';
    WStart:=1;
    While (WStart<=Length(S)) and charinset(S[WStart],WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not charinset(S[Wend],WhiteSpace) or InLiteral) do
      begin
      if charinset(S[Wend],Literals) then
        If InLiteral then
          begin
          InLiteral:=Not (S[Wend]=LastLiteral);
          if not InLiteral then
            AppendToResult;
          end
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          AppendToResult;
          end;
       inc(wend);
       end;
     AppendToResult;
     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);
  end;

Var
  W : RawByteString;
  len : Integer;

begin
  Len:=0;
  Result:=Default(TRawByteStringArray);
  SetLength(Result,(Length(S) div 2)+1);
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      begin
      Result[Len]:=W;
      Inc(Len);
      end;
    end;
  SetLength(Result,Len);
end;


Function SplitCommandLine(S : UnicodeString) : TUnicodeStringArray;

  Function GetNextWord : UnicodeString;

  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : AnsiChar;

    Procedure AppendToResult;

    begin
      Result:=Result+Copy(S,WStart,WEnd-WStart);
      WStart:=Wend+1;
    end;

  begin
    Result:='';
    WStart:=1;
    While (WStart<=Length(S)) and charinset(S[WStart],WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not charinset(S[Wend],WhiteSpace) or InLiteral) do
      begin
      if charinset(S[Wend],Literals) then
        If InLiteral then
          begin
          InLiteral:=Not (S[Wend]=LastLiteral);
          if not InLiteral then
            AppendToResult;
          end
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          AppendToResult;
          end;
       inc(wend);
       end;
     AppendToResult;
     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);
  end;

Var
  W : UnicodeString;
  len : Integer;

begin
  Len:=0;
  Result:=Default(TUnicodeStringArray);
  SetLength(Result,(Length(S) div 2)+1);
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      begin
      Result[Len]:=W;
      Inc(Len);
      end;
    end;
  SetLength(Result,Len);
end;


end.
