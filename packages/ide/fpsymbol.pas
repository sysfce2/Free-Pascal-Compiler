{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Symbol browse support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit FPSymbol;

interface

uses Objects,Drivers,Views,Menus,Dialogs,
{$ifdef HASOUTLINE}
     Outline,
{$endif HASOUTLINE}
     BrowCol,
     WViews,
     FPViews;

const
      { Browser tab constants }
      btScope       = 0;
      btReferences  = 1;
      btInheritance = 2;
      btMemInfo     = 3;
      btUnitInfo    = 4;
      btBreakWatch  = 7;

      {Symbol Flags}
      bfUnits            = $00000001;
      bfLabels           = $00000002;
      bfConstants        = $00000004;
      bfTypes            = $00000008;
      bfVariables        = $00000010;
      bfProcedures       = $00000020;
      bfInherited        = $00000040;
      {Display Flags}
      bfQualifiedSymbols = $40000000;
      bfSortAlways       = $80000000;

const
      DefaultSymbolFlags : longint = bfUnits or
         bfLabels or bfConstants or bfTypes or bfVariables or bfProcedures;
      DefaultDispayFlags : longint = (bfQualifiedSymbols) shr 30;
      { Note: default browser flags will be created with formula:
        BrowserFlags:=DefaultDispayFlags shl 30 or DefaultSymbolFlags;
      }
      DefaultBrowserSub  : longint = 0;
      DefaultBrowserPane : longint = 0;


type
    PBrowserWindow = ^TBrowserWindow;

    PGDBValueCollection = ^TGDBValueCollection;

    PGDBValue = ^TGDBValue;
    TGDBValue = Object(TObject)
      constructor Init(Const AExpr : String;ASym : PSymbol);
      procedure GetValue;
      function  GetText : String;
      destructor Done;virtual;
    private
      expr : Pstring;
      St   : Pstring;
      S    : PSymbol;
      GDBI : longint;
      end;

    TGDBValueCollection = Object(TCollection)
      function  At(Index: sw_Integer): PGDBValue;
      end;

      {Shell of TSymbol used to filter inherited and to display qualified symbols }
    PHollowSymbol = ^THollowSymbol;
    THollowSymbol = object(TSymbol)
        Sym        : PSymbol; { orginal symbol, need for unit info save}
        Parent     : PSymbol; { to get object name from }
        NeedPrefix : Boolean; { GetName will add object prefix if needed }
        constructor Init(ASymbol,AParent:PSymbol);
        function    GetName: string; virtual;
        destructor  Done; virtual;
      end;

    PHollowSymbolCollection=^THollowSymbolCollection;
    THollowSymbolCollection = Object(TSortedSymbolCollection)
        function  At(Index: Sw_Integer): PHollowSymbol;
      end;


    PFilteredSym = ^TFilteredSym;
    TFilteredSym = Object(TObject)
        constructor Init(AItemSym:Sw_Integer;ASym : PSymbol);
        function GetText:String;
        destructor Done;virtual;
      private
        Sym:PSymbol;
        ItemSym : Sw_Integer;
      end;

    PFilteredSymCollection=^TFilteredSymCollection;
    TFilteredSymCollection = Object(TCollection)
      function  At(Index: sw_Integer): PFilteredSym;
      end;


    PSymbolView = ^TSymbolView;
    TSymbolView = object(THSListBox)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      destructor   Done;virtual;
      procedure    HandleEvent(var Event: TEvent); virtual;
      procedure    SetState(AState: Word; Enable: Boolean); virtual;
      function     GotoItem(Item: sw_integer): boolean; virtual;
      function     TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
      function     GetPalette: PPalette; virtual;
      function     GetLocalMenu: PMenu; virtual;
      procedure    ClearHighlights;
      procedure    AutoTrackSource; virtual;
      procedure    Browse; virtual;
      procedure    GotoSource; virtual;
      procedure    TrackSource; virtual;
      procedure    OptionsDlg; virtual;
    private
      MyBW         : PBrowserWindow;
      function     TrackReference(R: PReference; AutoTrack: boolean): boolean; virtual;
      function     GotoReference(R: PReference): boolean; virtual;
    end;

    PSymbolScopeView = ^TSymbolScopeView;
    TSymbolScopeView = object(TSymbolView)
      constructor Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
      destructor  Done; virtual;
      procedure   SetGDBCol;
      procedure   FilterSymbols(AFilter:boolean);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Draw; virtual;
      procedure   LookUp(S: string); virtual;
      function    GotoItem(Item: sw_integer): boolean; virtual;
      function    TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
    private
      Inh       : Boolean; {filter for inheritance is possible}
      ObjSymbol : PSymbol;
      OrgSymbols: PSymbolCollection;
      FilteredSym: PFilteredSymCollection;
      Symbols  : PHollowSymbolCollection;
      SymbolsValue : PGDBValueCollection;
      LookupStr: string;
      procedure   CopyOrgSymbols;
      procedure   PullInInheritance;
    end;

    PSymbolReferenceView = ^TSymbolReferenceView;
    TSymbolReferenceView = object(TSymbolView)
      constructor Init(var Bounds: TRect; AReferences: PReferenceCollection; AHScrollBar, AVScrollBar: PScrollBar);
      destructor  Done; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   SelectItem(Item: Sw_Integer); virtual;
      function    GotoItem(Item: sw_integer): boolean; virtual;
      function    TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
      procedure   Browse; virtual;
    private
      References: PReferenceCollection;
    end;

    PSymbolMemInfoView = ^TSymbolMemInfoView;
    TSymbolMemInfoView = object(TStaticText)
      constructor  Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
      destructor  Done; virtual;
      procedure    GetText(var S: String); virtual;
      function     GetPalette: PPalette; virtual;
    private
      MemInfo: PSymbolMemInfo;
      MyBW   : PBrowserWindow;
    end;

    PSymbolMemoView = ^TSymbolMemoView;
    TSymbolMemoView = object(TFPMemo)
      function    GetPalette: PPalette; virtual;
    end;

        PSymbolInheritanceView = ^TSymbolInheritanceView;
{$ifdef HASOUTLINE}
    TSymbolInheritanceView = object(TLocalMenuOutlineViewer)
{$else notHASOUTLINE}
    TSymbolInheritanceView = object(TLocalMenuListBox)
{$endif HASOUTLINE}
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
      destructor   Done; virtual;
      function     GetRoot: Pointer; virtual;
      function     HasChildren(Node: Pointer): Boolean; virtual;
      function     GetChild(Node: Pointer; I: sw_Integer): Pointer; virtual;
      function     GetNumChildren(Node: Pointer): sw_Integer; virtual;
      function     GetNumChildrenExposed(Node: Pointer) : sw_Integer; virtual;
      procedure    Adjust(Node: Pointer; Expand: Boolean); virtual;
      function     IsExpanded(Node: Pointer): Boolean; virtual;
      function     NodeCountToFoc(aFoc:Sw_Integer):Sw_Integer;
{$ifdef HASOUTLINE}
      function     GetText(Node: Pointer): String; virtual;
{$else not HASOUTLINE}
      procedure    ExpandAll(Node: Pointer);
      function     GetNode(I : sw_Integer) : Pointer; virtual;
      function     GetLineNode(Item : sw_Integer) : Pointer; virtual;
      function     GetText(Item,MaxLen: Sw_Integer): String; virtual;
{$endif HASOUTLINE}
      procedure    NodeSelected(P: pointer); virtual;
      procedure    Selected(I: sw_Integer); virtual;
      procedure    HandleEvent(var Event: TEvent); virtual;
      function     GetPalette: PPalette; virtual;
      function     GetLocalMenu: PMenu; virtual;
      function     SaveToFile(const AFileName: string): boolean; virtual;
      function     SaveAs: Boolean; virtual;
    private
      Root         : PObjectSymbol;
      MyBW         : PBrowserWindow;
    end;

    PBrowserTabItem = ^TBrowserTabItem;
    TBrowserTabItem = record
      Sign  : AnsiChar;
      Link  : PView;
      Next  : PBrowserTabItem;
    end;

    PBrowserTab = ^TBrowserTab;
    TBrowserTab = object(TView)
      Items: PBrowserTabItem;
      constructor Init(var Bounds: TRect; AItems: PBrowserTabItem);
      function    GetItemCount: sw_integer; virtual;
      function    GetItem(Index: sw_integer): PBrowserTabItem; virtual;
      procedure   SetParams(AFlags: word; ACurrent: Sw_integer); virtual;
      procedure   SelectItem(Index: Sw_integer); virtual;
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      Flags   : word;
      Current : Sw_integer;
    end;

    PUnitInfoPanel = ^TUnitInfoPanel;
    TUnitInfoPanel = object(TPanel)
      InOwnerCall: boolean;
      UnitInfoUsed: PSymbolScopeView;
      UnitInfoDependent: PSymbolScopeView;
      UsedVSB: PScrollBar;
      DependVSB: PScrollBar;
      UsedCST: PColorStaticText;
      DependCST: PColorStaticText;
      procedure SetState(AState: Word; Enable: Boolean); virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PBrowserLinkedCollection=^TBrowserLinkedCollection;
    PBrowserLinked = ^TBrowserLinked;

    TBrowserWindow = object(TFPWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
                    AInheritance: PObjectSymbol; AMemInfo: PSymbolMemInfo);
      procedure   HandleEvent(var Event: TEvent); virtual;
      {procedure   SetState(AState: Word; Enable: Boolean); virtual;}
      procedure   UpdateCommands; Virtual;
      procedure   Close; virtual;
      procedure   SelectTab(BrowserTab: Sw_integer); virtual;
      function    GetPalette: PPalette; virtual;
      function    Disassemble : boolean;
      function    GetFlags: longint; virtual;
      procedure   SetFlags(AFlags: longint); virtual;
      procedure   SizeLimits (Var Min, Max: TPoint); Virtual;
      procedure   OnResize; Virtual; { called on window resize event }
      destructor  Done;virtual;
    private
      BrowserFlags  : Longint;
      PrevSize      : TPoint;
      PageTab       : PBrowserTab;
      ST            : PStaticText;
      Sym           : PSymbol;
      ScopeView     : PSymbolScopeView;
      ReferenceView : PSymbolReferenceView;
      InheritanceView: PSymbolInheritanceView;
      MemInfoView   : PSymbolMemInfoView;
      UnitInfoText  : PSymbolMemoView;
      UnitInfoUsed  : PSymbolScopeView;
      UnitInfoDependent : PSymbolScopeView;
      UnitInfo      : PUnitInfoPanel;
      Prefix        : PString;
      IsValid       : boolean;
      DebuggerValue : PGDBValue;
      BrowserLinked : PBrowserLinked;
    end;

    { Tree to go to previous browser windows }
    { Holds all parametrs to recreate closed previous window if needed to be}
    TBrowserLinked = Object(TObject)
        BrowserWindow : PBrowserWindow;
        Previous : PBrowserLinked;
        Branches : PBrowserLinkedCollection;
        Origin, Size : TPoint;
        Title: TTitleStr;
        Number: Sw_Integer;
        Name : String;
        Prefix: string;
        Sym : PSymbol;
        Symbols: PSymbolCollection;
        References: PReferenceCollection;
        Inheritance: PObjectSymbol;
        MemInfo: PSymbolMemInfo;
        BrowserFlags  : Longint;
        Tab : sw_integer;
        ScopeTop : sw_integer;
        ScopeFocused:sw_integer;
        ReferenceTop : sw_integer;
        ReferenceFocused:sw_integer;
        InheritanceTop : sw_integer;
        InheritanceFocused : sw_integer;
        UnitInfoUsedTop : sw_integer;
        UnitInfoUsedFocused:sw_integer;
        UnitInfoDependentTop : sw_integer;
        UnitInfoDependentFocused:sw_integer;
        constructor Init(ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
                    AInheritance: PObjectSymbol; AMemInfo: PSymbolMemInfo);
        procedure InsertWindow(BW : PBrowserWindow);
        procedure PreviousWindow; { activate previous window }
        procedure CreateNewWindow;
        procedure LeaveTree; { cut itself from tree }
        destructor Done;virtual;
      end;

    TBrowserLinkedCollection = Object(TCollection)
      function  At(Index: sw_Integer): PBrowserLinked;
      end;

function OpenSymbolBrowser(X,Y,W,H: Sw_integer;const Name,Line: string;S : PSymbol;
            ParentBrowser : PBrowserWindow;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo):PBrowserWindow;

function IsSymbolInfoAvailable: boolean;

procedure OpenOneSymbolBrowser(Name : String);

procedure CloseAllBrowsers;

procedure RemoveBrowsersCollection;

const
   GlobalsCollection : PSortedCollection = nil;
   ProcedureCollection : PSortedCollection = nil;
   ModulesCollection : PSortedCollection = nil;

var BrowserRoot : PBrowserLinked;

implementation

uses App,Strings,Stddlg,Keyboard,
     FVConsts,
{$ifdef BROWSERCOL}
     symconst,
{$endif BROWSERCOL}
     WUtils,WEditor,WConsts,
     FPConst,FPUtils,FPVars,{$ifndef FPDEBUG}FPDebug{$endif},FPIDE;

{$ifdef USERESSTRINGS}
resourcestring
{$else}
const
{$endif}
                msg_symbolnotfound = #3'Symbol %s not found';
                msg_nobrowserinfoavailable = 'No Browser info available';
                msg_cantfindfile = 'Can''t find %s';

                menu_local_gotosource = '~G~oto source';
                menu_local_tracksource = '~T~rack source';
                menu_local_options = '~O~ptions...';
                menu_local_clear = '~C~lear';
                menu_local_saveas = 'Save ~a~s';

                { Symbol view local menu items }
                menu_symlocal_browse = '~B~rowse';
                menu_symlocal_previous = '~P~revious';
                menu_symlocal_gotosource = '~G~oto source';
                menu_symlocal_tracksource = '~T~rack source';
                menu_symlocal_saveas = 'Save ~a~s';
                menu_symlocal_options = '~O~ptions...';

                { Symbol browser meminfo page }
                msg_sizeinmemory = 'Size in memory';
                msg_sizeonstack = 'Size on stack';

                msg_usedfirstin = 'Used first in';
                msg_mainsource = 'Main source';
                msg_sourcefiles = 'Source files';

                dialog_browse = 'Browse: %s';

const           { Symbol browser tabs }
                { must be AnsiChar constants (so cannot be resourcestring)}
                label_browsertab_scope = 'S';
                label_browsertab_reference = 'R';
                label_browsertab_inheritance = 'I';
                label_browsertab_memory = 'M';
                label_browsertab_unit = 'U';

function ReplaceCurrent:longint;
var K:TKeyEvent;
    ShiftState : byte;
begin
   K:=PollShiftStateEvent;
   ShiftState:=GetKeyEventShiftState(K);
   if (ShiftState and kbShift)=0 then
     ReplaceCurrent:=1
   else
     ReplaceCurrent:=0; { Reverse replace current }
end;

procedure CloseAllBrowsers;
  procedure SendCloseIfBrowser(P: PView);
  begin
    if assigned(P) and
       ((TypeOf(P^)=TypeOf(TBrowserWindow)) or
       (TypeOf(P^)=TypeOf(TSymbolView)) or
       (TypeOf(P^)=TypeOf(TSymbolScopeView)) or
       (TypeOf(P^)=TypeOf(TSymbolReferenceView)) or
       (TypeOf(P^)=TypeOf(TSymbolMemInfoView)) or
       (TypeOf(P^)=TypeOf(TSymbolInheritanceView)) or
       (TypeOf(P^)=TypeOf(TSymbolMemoView))) then
      Message(P,evCommand,cmClose,nil);
  end;

begin
  Desktop^.ForEach(TCallbackProcParam(@SendCloseIfBrowser));
end;

procedure RemoveBrowsersCollection;
begin
  if assigned(GlobalsCollection) then
    begin
      GlobalsCollection^.deleteAll;
      Dispose(GlobalsCollection,done);
      GlobalsCollection:=nil;
    end;
  if assigned(ProcedureCollection) then
    begin
      ProcedureCollection^.deleteAll;
      Dispose(ProcedureCollection,done);
      ProcedureCollection:=nil;
    end;
  if assigned(ModulesCollection) then
    begin
      ModulesCollection^.deleteAll;
      Dispose(ModulesCollection,done);
      ModulesCollection:=nil;
    end;
end;

function NewBrowserTabItem(ASign: AnsiChar; ALink: PView; ANext: PBrowserTabItem): PBrowserTabItem;
var P: PBrowserTabItem;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do begin Sign:=ASign; Link:=ALink; Next:=ANext; end;
  NewBrowserTabItem:=P;
end;

procedure DisposeBrowserTabItem(P: PBrowserTabItem);
begin
  if P<>nil then Dispose(P);
end;

procedure DisposeBrowserTabList(P: PBrowserTabItem);
begin
  if P<>nil then
  begin
    if P^.Next<>nil then DisposeBrowserTabList(P^.Next);
    DisposeBrowserTabItem(P);
  end;
end;

function IsSymbolInfoAvailable: boolean;
begin
  IsSymbolInfoAvailable:=BrowCol.Modules<>nil;
end;

procedure OpenOneSymbolBrowser(Name : String);

var Index : sw_integer;
    PS,S,MS : PSymbol;
    Anc : PObjectSymbol;
    P : Pstring;
    Symbols: PSymbolCollection;
    PB : PBrowserWindow;

  function Search(P : PSymbol) : boolean;
  begin
    Search:=UpcaseStr(P^.Items^.LookUp(Name,Index))=Name;
  end;
  function SearchModule(P : PSymbol) : boolean;
  begin
    SearchModule:=UpcaseStr(P^.Name^)=Name;
  end;

begin
   Name:=UpcaseStr(Name);
   If BrowCol.Modules<>nil then
     begin
       PS:=BrowCol.Modules^.FirstThat(TCallbackFunBoolParam(@Search));
       MS:=BrowCol.Modules^.FirstThat(TCallbackFunBoolParam(@SearchModule));
       If assigned(PS) then
         begin
           S:=PS^.Items^.At(Index);
           Symbols:=S^.Items;
           if (not assigned(symbols) or (symbols^.count=0)) and
              assigned(S^.Ancestor) then
             Symbols:=S^.Ancestor^.Items;
           if (S^.Flags and (sfObject or sfClass))=0 then
             Anc:=nil
           else if S^.Ancestor=nil then
             Anc:=ObjectTree
           else
             Anc:=SearchObjectForSymbol(S^.Ancestor);
           PB:=OpenSymbolBrowser(0,20,0,0,
                PS^.Items^.At(Index)^.GetName,
                PS^.Items^.At(Index)^.GetText,
                PS^.Items^.At(Index),nil,
                Symbols,PS^.Items^.At(Index)^.References,Anc,PS^.MemInfo);
           BrowserRoot^.InsertWindow(PB);
         end
       else If assigned(MS) then
         begin
           Symbols:=MS^.Items;
           PB:=OpenSymbolBrowser(0,20,0,0,
                MS^.GetName,
                MS^.GetText,
                MS,nil,
                Symbols,MS^.References,nil,nil);
           BrowserRoot^.InsertWindow(PB);
         end
       else
         begin
           P:=@Name;
           ErrorBox(msg_symbolnotfound,@P);
         end;
     end
   else
     ErrorBox(msg_nobrowserinfoavailable,nil);
end;

(*procedure ReadBrowseLog(FileName: string);
var f: text;
    IOOK,EndOfFile: boolean;
    Line: string;
procedure NextLine;
begin
  readln(f,Line);
  EndOfFile:=Eof(f);
end;
var Level: integer;
procedure ProcessSymTable(Indent: integer; Owner: PSymbolCollection);
var IndentS,S,Source: string;
    Sym: PSymbol;
    Ref: PSymbolReference;
    P: byte;
    PX: TPoint;
    PS: PString;
    PCount: integer;
    Params: array[0..30] of PString;
    Typ: tsymtyp;
    ExitBack: boolean;
begin
  Inc(Level);
  IndentS:=CharStr(' ',Indent); ExitBack:=false;
  Sym:=nil;
  repeat
    if copy(Line,1,length(IndentS))<>IndentS then ExitBack:=true else
    if copy(Line,Indent+1,3)='***' then
      { new symbol }
      begin
        S:=copy(Line,Indent+1+3,255);
        P:=Pos('***',S); if P=0 then P:=length(S)+1;
        S:=Trim(copy(S,1,P-1));
        if (copy(S,1,1)='_') and (Pos('$$',S)>0) then
          begin
            repeat
              P:=Pos('$$',S);
              if P>0 then Delete(S,1,P+1);
            until P=0;
            P:=Pos('$',S);
            Delete(S,1,P);
            PCount:=0;
            repeat
              P:=Pos('$',S); if P=0 then P:=length(S)+1;
              Params[PCount]:=TypeNames^.Add(copy(S,1,P-1));
              Inc(PCount);
              Delete(S,1,P);
            until S='';
            Sym^.Typ:=procsym;
            Sym^.SetParams(PCount,@Params);
          end
        else
          New(Sym, Init(S, varsym, 0, nil));
        Owner^.Insert(Sym);
        NextLine;
      end else
    if copy(Line,Indent+1,3)='---' then
      { child symtable }
      begin
        S:=Trim(copy(Line,Indent+1+12,255));
        if Level=1 then Typ:=unitsym else
          Typ:=typesym;
        if (Sym<>nil) and (Sym^.GetName=S) then
        else
          begin
            New(Sym, Init(S, Typ, 0, nil));
            Owner^.Insert(Sym);
          end;
        Sym^.Typ:=Typ;
        NextLine;
        New(Sym^.Items, Init(0,50));
        ProcessSymTable(Indent+2,Sym^.Items);
      end else
{    if Sym<>nil then}
    if copy(Line,Indent+1,1)=' ' then
      { reference }
      begin
        S:=copy(Line,Indent+1+2,255);
        P:=Pos('(',S); if P=0 then P:=length(S)+1;
        Source:=Trim(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        PX.Y:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(')',S); if P=0 then P:=length(S)+1;
        PX.X:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        PS:=ModuleNames^.Add(Source);
        New(Ref, Init(PS, PX));
        if Sym^.References=nil then
          New(Sym^.References, Init(10,50));
        Sym^.References^.Insert(Ref);
      end;
    if ExitBack=false then
      NextLine;
  until EndOfFile or ExitBack;
  Dec(Level);
end;
begin
  DoneSymbolBrowser;
  InitSymbolBrowser;

{$I-}
  Assign(f,FileName);
  Reset(f);
  Level:=0;
  NextLine;
  while (IOResult=0) and (EndOfFile=false) do
    ProcessSymTable(0,Modules);
  Close(f);
  EatIO;
{$I+}
end;*)


{****************************************************************************
                               TGDBValue
****************************************************************************}

constructor TGDBValue.Init(Const AExpr : String;ASym : PSymbol);
begin
  St := nil;
  S := ASym;
  Expr:=NewStr(AExpr);
  GDBI:=-1;
end;

destructor TGDBValue.Done;
begin
  If Assigned(St) then
    begin
      DisposeStr(St);
      st:=nil;
    end;
  If Assigned(Expr) then
    begin
      DisposeStr(Expr);
      Expr:=nil;
    end;
end;

procedure TGDBValue.GetValue;
var
  p : PAnsiChar;
begin
{$ifdef BROWSERCOL}
{$ifndef NODEBUG}
  if not assigned(Debugger) then
    exit;
  if not Debugger^.IsRunning then
    exit;
  if (S^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym]) or (GDBI=Debugger^.RunCount) then
    exit;
  If Assigned(St) then
    DisposeStr(St);
  if assigned(Expr) then
    begin
      { avoid infinite recursion here }
      GDBI:=Debugger^.RunCount;
      p:=Debugger^.GetValue(Expr^);
      St:=NewStr(GetPChar(p));
      if assigned(p) then
        StrDispose(p);
    end;
{$endif ndef NODEBUG}
{$endif BROWSERCOL}
end;

function TGDBValue.GetText : String;
begin
  GetValue;
  if assigned(St) then
    GetText:=S^.GetText+' = '+GetStr(St)
  else
    GetText:=S^.GetText;
end;

{****************************************************************************
                               TGDBValueCollection
****************************************************************************}
function  TGDBValueCollection.At(Index: sw_Integer): PGDBValue;
begin
  At:= Inherited At(Index);
end;

{****************************************************************************
                               THollowSymbol
****************************************************************************}
constructor THollowSymbol.init(ASymbol,AParent:PSymbol);
begin
  TObject.Init;
  Name       := ASymbol^.Name;
  Typ        := ASymbol^.Typ;
  varoptions := ASymbol^.varoptions;
  varspez    := ASymbol^.varspez;
  Params     := ASymbol^.Params;
  References := ASymbol^.References;
  Items      := ASymbol^.Items;
  DType      := ASymbol^.DType;
  VType      := ASymbol^.VType;
  TypeID     := ASymbol^.TypeID;
  RelatedTypeID := ASymbol^.RelatedTypeID;
  DebuggerCount := ASymbol^.DebuggerCount;
  Ancestor   := ASymbol^.Ancestor;
  Flags      := ASymbol^.Flags;
  MemInfo    := ASymbol^.MemInfo;
  Sym        := ASymbol;
  Parent     := AParent;
  NeedPrefix := false;
end;

function THollowSymbol.GetName: string;
begin
  if (not NeedPrefix) or (not assigned(Parent)) then
    GetName:=inherited GetName
  else
    GetName:=Parent^.Name^+'.'+inherited GetName;
end;

destructor THollowSymbol.done;
begin
  { Skip TSymbol.Done because we do not own any of actual pointers here }
  TObject.Done;
end;

{****************************************************************************
                               THollowSymbolCollection
****************************************************************************}
function THollowSymbolCollection.At(Index: Sw_Integer): PHollowSymbol;
begin
  At:=TCollection.At(Index);
end;

{****************************************************************************
                               TFilteredSym
****************************************************************************}
constructor TFilteredSym.Init(AItemSym:Sw_Integer;ASym : PSymbol);
begin
   inherited Init;
   ItemSym:=AItemSym;
   Sym:=ASym;
end;

function TFilteredSym.GetText:String;
begin
   GetText:=Sym^.GetText;
end;

destructor TFilteredSym.Done;
begin
   inherited Done;
end;

{****************************************************************************
                               TFilteredSymCollection
****************************************************************************}
function TFilteredSymCollection.At(Index: sw_Integer): PFilteredSym;
begin
  At:= Inherited At(Index);
end;

{****************************************************************************
                               TSymbolView
****************************************************************************}

constructor TSymbolView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AHScrollBar,AVScrollBar);
  {HScrollBar:=AHScrollBar;}
  MyBW:=nil;
  if assigned(HScrollBar) then
    begin
      HScrollBar^.SetRange(1,80);
    end;
  Options:=Options or (ofSelectable+ofTopSelect);
  EventMask:=EventMask or evBroadcast;
end;

procedure TSymbolView.ClearHighlights;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TSymbolView.AutoTrackSource;
begin
  if Range>0 then
    TrackSource;
end;

procedure TSymbolView.OptionsDlg;
begin
  if MyBW<> nil then
    Message(@IDEApp, evCommand, cmBrowserOptions, MyBW);   { Send message }
end;

destructor TSymbolView.Done;
begin
  EventMask:=EventMask and not evBroadcast;
  Inherited Done;
end;

procedure TSymbolView.SetState(AState: Word; Enable: Boolean);
var OState: longint;
begin
  OState:=State;
  inherited SetState(AState,Enable);
  if ((OState xor State) and sfFocused)<>0 then
    if GetState(sfFocused) then
      begin
        if (MiscOptions and moAutoTrackSource)<>0 then
          AutoTrackSource;
      end
    else
      Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TSymbolView.Browse;
begin
  SelectItem(Focused);
end;

procedure TSymbolView.GotoSource;
begin
  if GotoItem(Focused) then
    PutCommand(Owner,evCommand,cmClose,nil);
end;

procedure TSymbolView.TrackSource;
begin
  TrackItem(Focused,false);
end;

procedure TSymbolView.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Browse;
          kbCtrlEnter,kbCtrlG :
            GotoSource;
          kbSpaceBar,kbCtrlT :
            TrackSource;
          kbCtrlP :
            Message(MyBW,evCommand,cmSymPrevious,nil);
          kbF2 :
            SaveAs;
          kbCtrlO :
            OptionsDlg;
          kbRight,kbLeft :
            if HScrollBar<>nil then
              HScrollBar^.HandleEvent(Event);
          kbTab:
            Message(Owner,evBroadcast,cmSymTabKeyPress,@Self);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouseDown :
      begin
        if Event.double then
          begin
            Browse;
            ClearEvent(Event);
          end;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmSymBrowse :
            Browse;
          cmSymPrevious :
            Message(MyBW,evCommand,cmSymPrevious,nil);
          cmSymGotoSource :
            GotoSource;
          cmSymTrackSource :
            TrackSource;
          cmSymSaveAs,cmSaveAs :
            SaveAs;
          cmSymOptions :
            OptionsDlg;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
         if Event.InfoPtr=@Self then
          if (MiscOptions and moAutoTrackSource)<>0 then
            if GetState(sfFocused) then
              AutoTrackSource;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TSymbolView.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;

function TSymbolView.GetLocalMenu: PMenu;
begin
  GetLocalMenu:=NewMenu(
    NewItem(menu_symlocal_browse,'',kbNoKey,cmSymBrowse,hcSymBrowse,
    NewItem(menu_symlocal_previous,'Ctrl+P',kbCtrlP,cmSymPrevious,hcSymPrevious,
    NewItem(menu_symlocal_gotosource,'Ctrl+G',kbCtrlG,cmSymGotoSource,hcSymGotoSource,
    NewItem(menu_symlocal_tracksource,'Ctrl+T',kbCtrlT,cmSymTrackSource,hcSymTrackSource,
    NewLine(
    NewItem(menu_symlocal_saveas,'F2',kbF2,cmSymSaveAs,hcSymSaveAs,
    NewItem(menu_symlocal_options,'Ctrl+O',kbCtrlO,cmSymOptions,hcSymOptions,
    nil))))))));
end;

function TSymbolView.GotoItem(Item: sw_integer): boolean;
begin
  SelectItem(Item);
  GotoItem:=true;
end;

function TSymbolView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
begin
  SelectItem(Item);
  TrackItem:=true;
end;

function LastBrowserWindow: PBrowserWindow;
var BW: PBrowserWindow;
procedure IsBW(P: PView);
begin
  if (P^.HelpCtx=hcBrowserWindow) then
    BW:=pointer(P);
end;
begin
  BW:=nil;
  Desktop^.ForEach(TCallbackProcParam(@IsBW));
  LastBrowserWindow:=BW;
end;

function TSymbolView.TrackReference(R: PReference; AutoTrack: boolean): boolean;
var W: PSourceWindow;
    BW: PBrowserWindow;
    P: TPoint;
begin
  ClearHighlights;
  Desktop^.Lock;
  P.X:=R^.Position.X-1; P.Y:=R^.Position.Y-1;
  if AutoTrack then
    W:=SearchOnDesktop(R^.GetFileName,false)
  else
    W:=TryToOpenFile(nil,R^.GetFileName,P.X,P.Y,true);
  if not assigned(W) then
    begin
      Desktop^.Unlock;
      if IDEApp.OpenSearch(R^.GetFileName+'*') then
        begin
          W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
          if Assigned(W) then
            W^.Select;
        end;
      Desktop^.Lock;
    end;
  if W<>nil then
  begin
    BW:=LastBrowserWindow;
    if BW=nil then
      W^.Select
    else
      begin
        Desktop^.Delete(W);
        Desktop^.InsertBefore(W,BW^.NextView);
      end;
    W^.Editor^.SetLineFlagExclusive(lfHighlightRow,P.Y);
  end;
  Desktop^.UnLock;
  if Assigned(W)=false then
    ErrorBox(FormatStrStr(msg_cantfindfile,R^.GetFileName),nil);

  TrackReference:=W<>nil;
end;

function TSymbolView.GotoReference(R: PReference): boolean;
var W: PSourceWindow;
begin
  Desktop^.Lock;
  W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
  if Assigned(W) then
    W^.Select
  else
    begin
      Desktop^.Unlock;
      if IDEApp.OpenSearch(R^.GetFileName+'*') then
        begin
          W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
          if Assigned(W) then
            W^.Select;
        end;
      Desktop^.Lock;
    end;
  Desktop^.UnLock;
  if Assigned(W)=false then
    ErrorBox(FormatStrStr(msg_cantfindfile,R^.GetFileName),nil);
  GotoReference:=W<>nil;
end;

{****************************************************************************
                               TSymbolScopeView
****************************************************************************}

constructor TSymbolScopeView.Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  OrgSymbols:=ASymbols;
  Inh:=false; { use inheritance filter (set to true only if view object or class) }
  ObjSymbol:=nil;

  New(SymbolsValue,Init(50,50));
  New(FilteredSym,Init(50,50));
  New(Symbols,Init(50,50));

  CopyOrgSymbols;
  FilterSymbols(false); {select all}
  NewList(FilteredSym);
  SetRange(FilteredSym^.Count);
end;

destructor TSymbolScopeView.Done;
begin
  if assigned(Symbols) then
    begin
       dispose(Symbols,done);
       Symbols:=nil;
    end;
  if Assigned(SymbolsValue) then
    begin
      Dispose(SymbolsValue,Done);
      SymbolsValue:=nil;
    end;
  if Assigned(FilteredSym) then
    begin
      Dispose(FilteredSym,Done);
      FilteredSym:=nil;
    end;
  Inherited Done;
end;

procedure TSymbolScopeView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbBack :
          begin
            LookUp(copy(LookUpStr,1,length(LookUpStr)-1));
            ClearEvent(Event);
          end;
      else
        if Event.CharCode in[#33..#255] then
          begin
            LookUp(LookUpStr+Event.CharCode);
            ClearEvent(Event);
          end;
      end;
  end;
  OldFocus:=Focused;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
    Lookup('');
end;

procedure TSymbolScopeView.Draw;
var DeltaX: sw_integer;
begin
  inherited Draw;
  if Assigned(HScrollBar)=false then DeltaX:=0 else
    DeltaX:=HScrollBar^.Value-HScrollBar^.Min;
  SetCursor(2+SymbolTypLen+length(LookUpStr)-DeltaX,Focused-TopItem);
end;

procedure TSymbolScopeView.LookUp(S: string);
var LookUpS : String;

  function GetFilteredLookUpIdx(Item:Sw_Integer):Sw_Integer;
  var I, Count : Sw_Integer;
      F : PFilteredSym;
      UpS,LeftS : String;
  begin
    GetFilteredLookUpIdx:=-1;
    Count:=FilteredSym^.Count;
    if Count > 0 then
      for I:=0 to Count-1 do
      begin
         F:=FilteredSym^.At(I);
         if F^.ItemSym = Item then   {perfect match}
         begin
           GetFilteredLookUpIdx:=I;
           break;
         end;
         if F^.ItemSym > Item then  { test next item if perfect match is missing}
         begin
           LeftS:=UpcaseStr(F^.Sym^.GetName);
           UpS:=UpcaseStr(LookUpS);
           if copy(LeftS,1,length(UpS))=UpS then  {perfect match}
             GetFilteredLookUpIdx:=I;
           break; {all you get is one second chance, it wont be any better from here}
         end;
      end;
  end;

var Idx,Slength,I: Sw_integer;
    NS: string;
begin
  NS:=LookUpStr;
  Slength:=Length(S);
  LookUpS:=S;
  if (Symbols=nil) or (S='') then NS:='' else
    begin
      S:=Symbols^.LookUp(S,Idx);
      if Idx<>-1 then
        begin
          { Have found, but get filtered list index first
            Some entries might be missing if need then look up agin }
          Idx:=GetFilteredLookUpIdx(Idx);
          if Idx<>-1 then
          begin
            NS:=S;
            FocusItem(Idx);
          end;
        end;
    end;
  LookUpStr:=Copy(NS,1,Slength);
  SetState(sfCursorVis,LookUpStr<>'');
  DrawView;
end;

function TSymbolScopeView.GotoItem(Item: sw_integer): boolean;
var S: PSymbol;
    OK: boolean;
    F : PFilteredSym;
begin
  OK:=Range>0;
  if OK then
  begin
    F:=List^.At(Item);
    S:=F^.Sym;
    OK:=(S^.References<>nil) and (S^.References^.Count>0);
    if OK then
      OK:=GotoReference(S^.References^.At(0));
  end;
  GotoItem:=OK;
end;

function TSymbolScopeView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
var S: PSymbol;
    OK: boolean;
    F: PFilteredSym;
begin
  OK:=Range>0;
  if OK then
  begin
    F:=List^.At(Item);
    S:=F^.Sym;
    OK:=(S^.References<>nil) and (S^.References^.Count>0);
    if OK then
      OK:=TrackReference(S^.References^.At(0),AutoTrack);
  end;
  TrackItem:=OK;
end;

procedure TSymbolScopeView.SetGDBCol;
var S : PSymbol;
    I : sw_integer;
begin
  if assigned(MyBW) and (SymbolsValue^.Count=0) then
    begin
      For i:=0 to Symbols^.Count-1 do
        begin
          S:=Symbols^.At(I);
          SymbolsValue^.Insert(New(PGDBValue,Init(GetStr(MyBW^.Prefix)+S^.GetName,S)));
        end;
    end;
end;

procedure TSymbolScopeView.CopyOrgSymbols;
var S : PSymbol;
    I : sw_integer;
begin
  Symbols^.FreeAll;
  if OrgSymbols^.Count>0 then
    For i:=0 to OrgSymbols^.Count-1 do
      begin
        S:=OrgSymbols^.At(I);
        Symbols^.Insert(new(PHollowSymbol,Init(S,nil)));
      end;
end;

procedure TSymbolScopeView.PullInInheritance; {adds to the list inherited procedures and fields}
var S : PSymbol;
    O : PObjectSymbol;
    InhSymbols : PSymbolCollection;
    I : sw_integer;

  function LookFor (Collection, AItems : PSymbolCollection):PSymbol;
  var I : sw_integer;
      S : PSymbol;
      R : PSymbol;
  begin
    R:=nil;
    for i:=0 to Collection^.count-1 do
    begin
      S:=Collection^.At(I);
      if assigned(S) and assigned(S^.Items) then
      begin
        if S^.Items = AItems then
        begin
          R:=S; break;
        end;
        R:=LookFor(S^.Items,AItems);
        if R<>nil then break;
      end;
    end;
    LookFor:=R;
  end;

begin
  S:=LookFor(Modules,OrgSymbols); { find the owner of OrgSymbols }
  if assigned(S) then
  begin
    ObjSymbol:=S;
    For i:=0 to Symbols^.Count-1 do
      Symbols^.At(I)^.Parent:=S;
    SymbolsValue^.FreeAll;
    O:=SearchObjectForSymbol(S);
    if assigned(O) then
      while assigned(O^.Parent) do
      begin
        O:=O^.Parent;
        S:=O^.Symbol;
        if assigned(S) then
        begin
          {-- add inherited symbols --}
          InhSymbols:=S^.Items;
          if InhSymbols^.Count>0 then
            For i:=0 to InhSymbols^.Count-1 do
              Symbols^.Insert(new(PHollowSymbol,Init(InhSymbols^.At(I),S)));
        end;
    end;
  end;
end;

procedure TSymbolScopeView.FilterSymbols(AFilter:boolean);
var S : PHollowSymbol;
    I : sw_integer;
    Flags : Longint;
    bUni, bLab, bCon, bTyp, bVar, bPrc, bInh, bQua: boolean;
begin
  Flags:=0;
  if assigned(MyBW) then
    Flags:=MyBW^.GetFlags;
  bUni:=(Flags and bfUnits)<>0;
  bLab:=(Flags and bfLabels)<>0;
  bCon:=(Flags and bfConstants)<>0;
  bTyp:=(Flags and bfTypes)<>0;
  bVar:=(Flags and bfVariables)<>0;
  bPrc:=(Flags and bfProcedures)<>0;
  bInh:=(Flags and bfInherited)<>0;
  bQua:=(Flags and bfQualifiedSymbols)<>0;
  FilteredSym^.FreeAll;
  if Symbols^.Count = 0 then exit;
  For i:=0 to Symbols^.Count-1 do
    begin
      S:=Symbols^.At(I);
      if AFilter then begin
        {----------  only selected ones  ----------}
        S^.NeedPrefix:=bQua;
        if Inh then  { we are in object scope view }
          if not bInh then  { Inherite checkbox is not selected }
            if S^.Parent <> ObjSymbol then continue;
        case S^.typ of
          labelsym: if not bLab then continue;
          namespacesym,staticvarsym,localvarsym,paravarsym,
          fieldvarsym,absolutevarsym,programparasym,
          propertysym: if not bVar then continue;
          procsym,syssym : if not bPrc then continue;
          typesym : if not bTyp then continue;
          constsym,enumsym : if not bCon then continue;
          unitsym : if not bUni then continue;
          errorsym,macrosym,undefinedsym: ;  {accepted anyway}
        end;
      end;
      FilteredSym^.Insert(New(PFilteredSym,Init(I,S)));
    end;
end;

function TSymbolScopeView.GetText(Item,MaxLen: Sw_Integer): String;
var S1: string;
    S : PSymbol;
    SG : PGDBValue;
    F : PFilteredSym;
begin
  F:=FilteredSym^.At(Item);
  Item:=F^.ItemSym;
  S:=Symbols^.At(Item);
  //S:=F^.Sym;
  if Assigned(SymbolsValue) and (SymbolsValue^.Count>Item) then
    SG:=SymbolsValue^.At(Item)
  else
    SG:=nil;
  if assigned(SG) then
    S1:=SG^.getText
  else
    S1:=S^.GetText;
  GetText:=copy(S1,1,MaxLen);
end;


{****************************************************************************
                             TSymbolReferenceView
****************************************************************************}

constructor TSymbolReferenceView.Init(var Bounds: TRect; AReferences: PReferenceCollection;
              AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  References:=AReferences;
  NewList(AReferences);
  SetRange(References^.Count);
end;

destructor TSymbolReferenceView.Done;
begin
  Inherited Done;
end;

procedure TSymbolReferenceView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
    DontClear: boolean;
begin
  OldFocus:=Focused;
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            TrackItem(Focused,false);
          kbCtrlEnter :
            GotoItem(Focused);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
   if (MiscOptions and moAutoTrackSource)=0 then
    ClearHighlights;
end;

procedure TSymbolReferenceView.Browse;
begin
  { do nothing here }
end;

function TSymbolReferenceView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
    P: PReference;
begin
  P:=References^.At(Item);
  S:=P^.GetFileName+'('+IntToStr(P^.Position.Y)+','+IntToStr(P^.Position.X)+')';
  GetText:=copy(S,1,MaxLen);
end;

function TSymbolReferenceView.GotoItem(Item: sw_integer): boolean;
var OK: boolean;
begin
  OK:=Range>0;
  if OK then
    OK:=GotoReference(List^.At(Item));
  GotoItem:=OK;
end;

function TSymbolReferenceView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
var OK: boolean;
begin
  OK:=Range>0;
  if OK then
    OK:=TrackReference(List^.At(Item),AutoTrack);
  TrackItem:=OK;
end;

procedure TSymbolReferenceView.SelectItem(Item: Sw_Integer);
begin
  GotoItem(Item);
end;


constructor TSymbolMemInfoView.Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
begin
  inherited Init(Bounds,'');
  Options:=Options or (ofSelectable+ofTopSelect);
  MemInfo:=AMemInfo;
  MyBW:=nil;
end;

destructor TSymbolMemInfoView.Done;
begin
{  if assigned(MemInfo) then
    dispose(MemInfo);}
  Inherited Done;
end;

procedure TSymbolMemInfoView.GetText(var S: String);
function SizeStr(Size: longint): string;
var S: string[40];
begin
  S:=IntToStrL(Size,7);
  S:=S+' byte';
  if Size>1 then S:=S+'s';
  if Size=-1 then
    SizeStr:='variable'
  else
    SizeStr:=S;
end;
function AddrStr(Addr: longint): string;
{ Warning this is endian specific code !! (PM) }
type TLongint = record LoW,HiW: word; end;
begin
  with TLongint(Addr) do
  AddrStr:='$'+hexstr(HiW,4)+hexstr(LoW,4);
end;
begin
  ClearFormatParams;
  AddFormatParamStr(msg_sizeinmemory);
  AddFormatParamStr(msg_sizeonstack);
  S:=
  FormatStrF(
   #13+
{  ' Memory location: '+AddrStr(MemInfo^.Addr)+#13+
  '   Local address: '+AddrStr(MemInfo^.LocalAddr)+#13+}

  { ??? internal linker ??? }

  '%18s: '+SizeStr(MemInfo^.Size)+#13+
  '%18s: '+SizeStr(MemInfo^.PushSize)+#13+
  '',
  FormatParams);
end;

function TSymbolMemInfoView.GetPalette: PPalette;
begin
  GetPalette:=inherited GetPalette;
end;

function TSymbolMemoView.GetPalette: PPalette;
const P: string[length(CFPSymbolMemo)] = CFPSymbolMemo;
begin
  GetPalette:=@P;
end;

{****************************************************************************
                          TSymbolInheritanceView
****************************************************************************}

constructor TSymbolInheritanceView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
begin
{$ifdef HASOUTLINE}
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
{$else not HASOUTLINE}
  inherited Init(Bounds,1,AVScrollBar);
  HScrollBar:=AHScrollBar;
{$endif not HASOUTLINE}
  Options:=Options or (ofSelectable+ofTopSelect);
  Root:=ARoot;
  MyBW:=nil;
  ExpandAll(Root);
{$ifdef HASOUTLINE}
  Update;
{$else not HASOUTLINE}
  SetRange(GetNumChildrenExposed(Root));
{$endif not HASOUTLINE}
end;

destructor TSymbolInheritanceView.Done;
begin
  { do not dispose,
    belongs to a symbolcollection (PM)
  if assigned(Root) then
    dispose(Root,done); }
  Inherited Done;
end;

function TSymbolInheritanceView.GetRoot: Pointer;
begin
  GetRoot:=Root;
end;

function TSymbolInheritanceView.HasChildren(Node: Pointer): Boolean;
begin
  HasChildren:=GetNumChildren(Node)>0;
end;

function TSymbolInheritanceView.GetChild(Node: Pointer; I: sw_Integer): Pointer;
begin
  GetChild:=PObjectSymbol(Node)^.GetDescendant(I);
end;

function TSymbolInheritanceView.GetNumChildren(Node: Pointer): sw_Integer;
begin
  GetNumChildren:=PObjectSymbol(Node)^.GetDescendantCount;
end;

function TSymbolInheritanceView.GetNumChildrenExposed(Node: Pointer) : sw_Integer;
var
  Nb : integer;
  P : PObjectSymbol;
    Procedure AddCount(P : PObjectSymbol);
    var
      i,count : integer;
      D : PObjectSymbol;
    begin
      if not assigned(P) then
        exit;
      Count:=P^.GetDescendantCount;
      Inc(Nb,Count);
      for I:=0 to Count-1 do
        begin
          D:=P^.GetDescendant(I);
          AddCount(D);
        end;
    end;
begin
  Nb:=0;
  AddCount(Node);
  GetNumChildrenExposed:=Nb;
end;


procedure TSymbolInheritanceView.Adjust(Node: Pointer; Expand: Boolean);
begin
  PObjectSymbol(Node)^.Expanded:=Expand;
end;

function TSymbolInheritanceView.IsExpanded(Node: Pointer): Boolean;
begin
  IsExpanded:=PObjectSymbol(Node)^.Expanded;
end;

procedure TSymbolInheritanceView.HandleEvent(var Event: TEvent);
var DontClear: boolean;
{$ifndef HASOUTLINE}
        P: TPoint;
{$endif HASOUTLINE}
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
{$ifndef HASOUTLINE}
          kbEnter:
            NodeSelected(GetLineNode(Cursor.Y-Origin.Y));
{$endif HASOUTLINE}
          kbLeft,kbRight,
          kbCtrlLeft,kbCtrlRight :
            if Assigned(HScrollBar) then
              HScrollBar^.HandleEvent(Event)
            else
              DontClear:=true;
          kbCtrlP :
            Message(MyBw,evCommand,cmSymPrevious,nil);
          kbF2: SaveAs;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouseDown :
      begin
{$ifndef HASOUTLINE}
        MakeLocal(Event.Where,P);
        SetCursor(P.X,P.Y);
{$endif HASOUTLINE}
        if Event.double then
          begin
            Message(@Self,evKeyDown,kbEnter,nil);
            ClearEvent(Event);
          end;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmSymBrowse :
            Message(@Self,evKeyDown,kbEnter,nil);
          cmSymPrevious :
            Message(MyBw,evCommand,cmSymPrevious,nil);
          cmSymSaveAs,cmSaveAs :
            SaveAs;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TSymbolInheritanceView.GetPalette: PPalette;
const P: string[length(CBrowserOutline)] = CBrowserOutline;
begin
  GetPalette:=@P;
end;

function TSymbolInheritanceView.GetLocalMenu: PMenu;
begin
    GetLocalMenu:=NewMenu(
    NewItem(menu_symlocal_browse,'',kbNoKey,cmSymBrowse,hcSymBrowse,
    NewItem(menu_symlocal_previous,'Ctrl+P',kbCtrlP,cmSymPrevious,hcSymPrevious,
    NewLine(
    NewItem(menu_symlocal_saveas,'F2',kbF2,cmSymSaveAs,hcSymSaveAs,
    nil)))));
end;

function TSymbolInheritanceView.NodeCountToFoc(aFoc:Sw_Integer):Sw_Integer;
var P : PObjectSymbol;
    Exp: Sw_Integer;
    ExpandedFoc, NormalFoc : Sw_integer;

    procedure CountSymbolTree(P:PObjectSymbol;Depth:Sw_Integer);
    var
      Q : PObjectSymbol;
      Des,Count : integer;
    begin
      if not assigned(P) then
         exit;
      Count:=GetNumChildren{Exposed}(P);
      if Count=0 then exit;
      Des:=0;
      if not IsExpanded(P) then
        Inc(Exp);
      While Count>Des do
        begin
          Q:=P^.GetDescendant(Des);
          If aFoc=NormalFoc then break; { exit if reached focused node }
          if Exp=0 then Inc(NormalFoc);
          Inc(ExpandedFoc);
          CountSymbolTree(Q,Depth+1);
          Inc(Des);
        end;
      if not IsExpanded(P) then
        Dec(Exp);
    end;

begin
      P:=Root;
      Exp:=0;
      ExpandedFoc:=0;
      NormalFoc:=0;
      CountSymbolTree(P,1);
      NodeCountToFoc:=ExpandedFoc;
end;

function TSymbolInheritanceView.SaveToFile(const AFileName: string): boolean;
var OK: boolean;
    S: PBufStream;
    st : string;
    P : PObjectSymbol;

    procedure WriteSymbolTree(P:PObjectSymbol;Depth:Sw_Integer;grph:string);
    var
      Q : PObjectSymbol;
      Des,Count : integer;
      Space : String;
    begin
      if not assigned(P) then
         exit;
      Des:=0;
      Count:=GetNumChildren{Exposed}(P);
      if Count=0 then exit;
      While Count>Des do
        begin
          if not ok then exit;
          Q:=P^.GetDescendant(Des);
          st:=Q^.GetName;
          if (Des+1)=Count then
            Space:=grph+#32#192#196#196
          else Space:=grph+#32#195#196#196;
          S^.Write(Space[1],Length(Space));
          if not OK then exit;
          S^.Write(St[1],length(St));
          OK:=(S^.Status=stOK);
          if not OK then exit;
          S^.Write(EOL[1],length(EOL));
          OK:=(S^.Status=stOK);
          if not OK then exit;
          if Ok then
          begin
            if (Des+1)=Count then
              Space:=grph+'   ' else Space:=grph+' '#179' ';
            WriteSymbolTree(Q,Depth+1,Space);
          end;
          Inc(Des);
        end;
    end;

begin
  New(S, Init(AFileName,stCreate,4096));
  OK:=Assigned(S) and (S^.Status=stOK);
  if OK then
    begin
      P:=Root;
      st:=#32#192#196#196+P^.GetName;
      S^.Write(St[1],length(St));
      OK:=(S^.Status=stOK);
      if OK then
      begin
        S^.Write(EOL[1],length(EOL));
        OK:=(S^.Status=stOK);
        if OK then
          WriteSymbolTree(P,1,'   ');
      end;
    end;
  if Assigned(S) then Dispose(S, Done);
  SaveToFile:=OK;
end;

function TSymbolInheritanceView.SaveAs: Boolean;
var
  DefExt,Title,Filename : string;
  Re : word;
begin
  SaveAs := False;
  Filename:='list.txt';
  DefExt:='*.txt';
  Title:='Save content';
  Re:=Application^.ExecuteDialog(New(PFPFileDialog, Init(DefExt,
          Title, label_name, fdOkButton, FileId)), @FileName);
  if Re <> cmCancel then
    SaveAs := SaveToFile(FileName);
end;

{$ifdef HASOUTLINE}
function TSymbolInheritanceView.GetText(Node: Pointer): String;
begin
  GetText:=PObjectSymbol(Node)^.GetName;
end;

{$else not HASOUTLINE}
function TSymbolInheritanceView.GetNode(I : sw_Integer) : Pointer;
var
  P : PObjectSymbol;
begin
  P:=Root;
  If Assigned(P) then
    P:=P^.GetDescendant(I);
  GetNode:=Pointer(P);
end;

procedure TSymbolInheritanceView.ExpandAll(Node: Pointer);
var
  i : integer;
  P : Pointer;
begin
  Adjust(Node,true);
  For i:=0 to GetNumChildren(Node)-1 do
    begin
      P:=GetChild(Node,I);
      if Assigned(P) then
        ExpandAll(P);
    end;
end;

function TSymbolInheritanceView.GetLineNode(Item : sw_Integer) : Pointer;
var
  P : PObjectSymbol;
  NT: Integer;
    procedure FindSymbol(var P:PObjectSymbol);
    var
      Q : PObjectSymbol;
      Nc,Des : integer;
    begin
      if not assigned(P) then
         exit;
      Des:=0;
      While (NT<Item) and (Des<GetNumChildren(P)) do
        begin
          Q:=P^.GetDescendant(Des);
          Inc(NT);
          if NT=Item then
            begin
              P:=Q;
              exit;
            end;
          Nc:=GetNumChildrenExposed(Q);
          If NT+Nc<Item then
            Inc(NT,Nc)
          else
            begin
              FindSymbol(Q);
              P:=Q;
              exit;
            end;
          Inc(Des);
        end;
    end;

begin
  P:=Root;
  NT:=0;
  FindSymbol(P);
  GetLineNode:=P;
end;

function TSymbolInheritanceView.GetText(Item,MaxLen: Sw_Integer): String;
var
  P,Ans : PObjectSymbol;
  NC,NT,NumParents : Integer;
  S : String;
    procedure FindSymbol(var P:PObjectSymbol);
    var
      Q : PObjectSymbol;
      Des : integer;
    begin
      if not assigned(P) then
         exit;
      Des:=0;
      While (NT<Item) and (Des<GetNumChildren(P)) do
        begin
          Q:=P^.GetDescendant(Des);
          Inc(NT);
          if NT=Item then
            begin
              P:=Q;
              exit;
            end;
          Nc:=GetNumChildrenExposed(Q);
          If NT+Nc<Item then
            Inc(NT,Nc)
          else
            begin
              FindSymbol(Q);
              P:=Q;
              exit;
            end;
          Inc(Des);
        end;
    end;

begin
  P:=Root;
  NT:=0;
  FindSymbol(P);

  if assigned(P) then
    begin
      S:=P^.GetName;
      Ans:=P^.Parent;
      NumParents:=0;
      While Assigned(Ans) do
        begin
          Inc(NumParents);
          Ans:=Ans^.Parent;
        end;
      S:=CharStr('-',NumParents)+S;
      GetText:=Copy(S,1,MaxLen);
    end
  else
    GetText:='';
end;

{$endif HASOUTLINE}


procedure TSymbolInheritanceView.Selected(I: sw_Integer);
var P: pointer;
begin
  P:=GetNode(I);
  NodeSelected(P);
end;

procedure TSymbolInheritanceView.NodeSelected(P: pointer);
var
    S: PSymbol;
    St : String;
    Anc: PObjectSymbol;
    R, WH :TPoint;
begin
  if P=nil then Exit;

  S:=PObjectSymbol(P)^.Symbol;

  { this happens for the top objects view (PM) }
  if S=nil then exit;

  R.X:=MyBw^.Origin.X-1;WH.X:=0;WH.Y:=0;
{$ifdef HASOUTLINE}
    R.Y:=FOC-Delta.Y+1;
{$else not HASOUTLINE}
    R.Y:=MyBw^.Origin.Y+1;
{$endif not HASOUTLINE}
  if DefaultBrowserSub = ReplaceCurrent then begin
    R.X:=MyBw^.Origin.X;R.Y:=MyBw^.Origin.Y;
    WH.X:=Size.X;WH.Y:=Size.Y;
  end;

  st:=S^.GetName;
  if S^.Ancestor=nil then
    Anc:=ObjectTree
  else
    Anc:=SearchObjectForSymbol(S^.Ancestor);
  OpenSymbolBrowser(R.X,R.Y,WH.X,WH.Y,
    st,
    S^.GetText,S,MyBw,
    S^.Items,S^.References,Anc,S^.MemInfo);
  if DefaultBrowserSub = ReplaceCurrent then
    Message(MyBw,evCommand,cmClose,nil);
end;


{****************************************************************************
                               TBrowserTab
****************************************************************************}

constructor TBrowserTab.Init(var Bounds: TRect; AItems: PBrowserTabItem);
begin
  inherited Init(Bounds);
  Options:=Options or ofPreProcess;
  Items:=AItems;
  SetParams(0,0);
end;

procedure TBrowserTab.SetParams(AFlags: word; ACurrent: Sw_integer);
begin
  Flags:=AFlags;
  SelectItem(ACurrent);
end;

procedure TBrowserTab.SelectItem(Index: Sw_integer);
var P: PBrowserTabItem;
    PrevTab:Sw_Integer;
begin
  PrevTab:=Current;
  Current:=Index;
  if PrevTab<>Current then
  begin
    P:=GetItem(PrevTab);
    if (P<>nil) and (P^.Link<>nil) then
      P^.Link^.SetState(sfVisible,False);
  end;
  P:=GetItem(Current);
  if (P<>nil) and (P^.Link<>nil) then
  begin
    P^.Link^.SetState(sfVisible,True);
    P^.Link^.Focus;
  end;
  DrawView;
end;

function TBrowserTab.GetItemCount: sw_integer;
var Count: integer;
    P: PBrowserTabItem;
begin
  Count:=0; P:=Items;
  while (P<>nil) do
    begin
      Inc(Count);
      P:=P^.Next;
    end;
  GetItemCount:=Count;
end;

function TBrowserTab.GetItem(Index: sw_integer): PBrowserTabItem;
var Counter: integer;
    P: PBrowserTabItem;
begin
  P:=Items;
  Counter:=0;
  while (P<>nil) and (Counter<Index) do
    begin
      P:=P^.Next;
      Inc(Counter);
    end;
  GetItem:=P;
end;

procedure TBrowserTab.Draw;
var B: TDrawBuffer;
    SelColor, NormColor, C: word;
    I,CurX,Count: Sw_integer;
function Names(Idx: integer): AnsiChar;
begin
  Names:=GetItem(Idx)^.Sign;
end;
begin
  NormColor:=GetColor(1); SelColor:=GetColor(2);
  MoveChar(B,#196{-},SelColor,Size.X);
  CurX:=0; Count:=0;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      Inc(Count);
      if Current=I then C:=SelColor
                   else C:=NormColor;
      if Count=1 then MoveChar(B[CurX],#180,SelColor,1)
                 else MoveChar(B[CurX],#179,SelColor,1);
      MoveCStr(B[CurX+1],' '+Names(I)+' ',C);
      Inc(CurX,4);
    end;
  if Count>0 then
    MoveChar(B[CurX],#195,SelColor,1);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

procedure TBrowserTab.HandleEvent(var Event: TEvent);
var I,Idx: integer;
    DontClear: boolean;
    P: TPoint;
function GetItemForCoord(X: integer): integer;
var I,CurX,Idx: integer;
begin
  CurX:=0; Idx:=-1;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      if (CurX+1<=X) and (X<=CurX+3) then
        begin Idx:=I; Break; end;
      Inc(CurX,4);
    end;
  GetItemForCoord:=Idx;
end;
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) then
        begin
          repeat
            MakeLocal(Event.Where,P);
            Idx:=GetItemForCoord(P.X);
            if Idx<>-1 then
              SelectItem(Idx);
          until not MouseEvent(Event, evMouseMove);
          ClearEvent(Event);
        end;
    evKeyDown :
      begin
        DontClear:=false; Idx:=-1;
        for I:=0 to GetItemCount-1 do
          if (GetCtrlCode(GetItem(I)^.Sign)=Event.KeyCode){ or
             (GetItem(I)^.Sign=UpCase(Event.CharCode))}  then
           if (Flags and (1 shl I))<>0 then
            begin
              Idx:=I;
              Break;
            end;
        if Idx=-1 then
          DontClear:=true
        else
          SelectItem(Idx);
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TBrowserTab.GetPalette: PPalette;
const P: string[length(CBrowserTab)] = CBrowserTab;
begin
  GetPalette:=@P;
end;

destructor TBrowserTab.Done;
begin
  if Items<>nil then DisposeBrowserTabList(Items);
  inherited Done;
end;

procedure TUnitInfoPanel.SetState(AState: Word; Enable: Boolean);
var OState: longint;
begin
  OState:=State;
  inherited SetState(AState,Enable);
  if ((OState xor State) and sfVisible)<>0 then
  begin
    if GetState(sfVisible) then
      begin
        { even they are visible already
          we need to make them visible for focus to work }
        if assigned(UnitInfoUsed) then
          UnitInfoUsed^.SetState(sfVisible,true);
        if assigned(UnitInfoDependent) then
          UnitInfoDependent^.SetState(sfVisible,true);
      end;
  end;
end;

procedure TUnitInfoPanel.HandleEvent(var Event: TEvent);
begin
  if (Event.What=evBroadcast) and (Event.Command=cmListItemSelected) and
     (InOwnerCall=false) then
    begin
      InOwnerCall:=true;
      if Assigned(Owner) then
        Owner^.HandleEvent(Event);
      InOwnerCall:=false;
    end else
  if (Event.What=evBroadcast) and (Event.Command=cmSymTabKeyPress) then
  begin
    if Event.InfoPtr = UnitInfoUsed then
      if assigned(UnitInfoDependent) then
        UnitInfoDependent^.Focus;
    if Event.InfoPtr = UnitInfoDependent then
      if assigned(UnitInfoUsed) then
        UnitInfoUsed^.Focus;
    ClearEvent(Event);
  end;
  inherited HandleEvent(Event);
end;

constructor TBrowserWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
             const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
             AInheritance: PObjectSymbol; AMemInfo: PSymbolMemINfo);
var R,R2,R3: TRect;
    HSB,VSB: PScrollBar;
    CST: PColorStaticText;
    I: sw_integer;
function CreateVSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  CreateVSB:=SB;
end;
function CreateHSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  R2.Copy(R); R2.Move(0,1); R2.A.Y:=R2.B.Y-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
  CreateHSB:=SB;
end;
begin
  inherited Init(Bounds, FormatStrStr(dialog_browse,ATitle), ANumber);
  New(BrowserLinked,Init(ATitle,ANumber,ASym,
      AName,APrefix,ASymbols,AReferences,AInheritance,AMemInfo));
  HelpCtx:=hcBrowserWindow;
  Sym:=ASym;
  Prefix:=NewStr(APrefix);
  BrowserFlags:=DefaultDispayFlags shl 30 or DefaultSymbolFlags;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
{$ifndef NODEBUG}
  if {assigned(Debugger) and Debugger^.IsRunning and}
     assigned(Sym) and (Sym^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym]) then
    begin
      New(DebuggerValue,Init(ATitle,Sym));
      New(ST, Init(R, ' '+DebuggerValue^.GetText));
    end
  else
{$endif NODEBUG}
    begin
      New(ST, Init(R, ' '+AName));
      DebuggerValue:=nil;
    end;
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);

  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);
  if assigned(ASymbols) and (ASymbols^.Count>0) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(ScopeView, Init(R, ASymbols, HSB, VSB));
      ScopeView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ScopeView);
      ScopeView^.MyBW:=@Self;
      if assigned(AInheritance) then
      begin
        ScopeView^.Inh:=true;
        ScopeView^.PullInInheritance;
      end;
      ScopeView^.SetGDBCol;
      ScopeView^.FilterSymbols(true);
      ScopeView^.SetRange(ScopeView^.FilteredSym^.Count);
    end;
  if assigned(AReferences) and (AReferences^.Count>0) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(ReferenceView, Init(R, AReferences, HSB, VSB));
      ReferenceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ReferenceView);
      ReferenceView^.MyBW:=@Self;
    end;
  if assigned(AInheritance) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(InheritanceView, Init(R, HSB,VSB, AInheritance));
      InheritanceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(InheritanceView);
      InheritanceView^.MyBW:=@Self;
    end;
  if assigned(AMemInfo) then
    begin
      New(MemInfoView, Init(R, AMemInfo));
      MemInfoView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(MemInfoView);
      MemInfoView^.MyBW:=@Self;
    end;
  if Assigned(Asym) and (TypeOf(ASym^)=TypeOf(TModuleSymbol)) then
  with PModuleSymbol(Sym)^ do
    begin
      New(UnitInfo, Init(R));
      UnitInfo^.GetExtent(R3);

      R2.Copy(R3);
      R2.B.Y:=R2.A.Y+3;
      if (Assigned(UsedUnits) or Assigned(DependentUnits))=false then
        R2.B.Y:=R3.B.Y;
      {HSB:=CreateHSB(R2);} {UnitInfo^.Insert(HSB); HSB:=nil;}
      {VSB:=CreateVSB(R2);}
      HSB:=nil; { It is for the best to not have HSB at all. M }
      VSB:=nil;
      {UnitInfo^.Insert(VSB);
       VSB will be owned by UnitInfoText PM }
      New(UnitInfoText, Init(R2,HSB,VSB, nil));
      with UnitInfoText^ do
      begin
        GrowMode:=gfGrowHiX;
        if Assigned(LoadedFrom) then  {this will be false always because it is not set anymore in browcol unit}
          AddLine(FormatStrStr2('%s : %s',msg_usedfirstin,GetStr(LoadedFrom)));
        if Assigned(MainSource) then
        begin
          AddLine(FormatStrStr('%s : ',msg_mainsource));
          AddLine(FormatStrStr('  %s',GetStr(MainSource)));
        end;
        if Assigned(SourceFiles) and (SourceFiles^.Count>1) then
        begin
          AddLine(FormatStrStr('%s : ',msg_sourcefiles));
          for I:=0 to SourceFiles^.Count-1 do
            AddLine(FormatStrStr('  %s',GetStr(SourceFiles^.At(I))));
        end;
      end;
      UnitInfo^.Insert(UnitInfoText);

      UnitInfo^.UnitInfoUsed:=nil;
      if Assigned(UsedUnits) then
      begin
        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+1;
        New(CST, Init(R2,#180' Used units '#195+CharStr(#196,255),ColorIndex(12),false));
        CST^.GrowMode:=gfGrowHiX;
        UnitInfo^.Insert(CST);

        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+Max(3,Size.Y-12);
        Dec(R2.B.X);  { make space for VSB inside Panel }
        if Assigned(DependentUnits)=false then R2.B.Y:=R3.B.Y;
        {HSB:=CreateHSB(R2); UnitInfo^.Insert(HSB); }
        HSB:=nil;
        VSB:=CreateVSB(R2);
        {UnitInfo^.Insert(VSB);  this created crashes,
        that were difficult to findout PM }
        { Maybe because it was outside Panel?  M }
        UnitInfo^.Insert(VSB); { lets try again with VSB inside Panel area }
        New(UnitInfoUsed, Init(R2,UsedUnits,HSB,VSB));
        Inc(R2.B.X); { restore R2 }
        UnitInfoUsed^.GrowMode:=gfGrowHiY+gfGrowHiX;
        UnitInfoUsed^.MyBW:=@Self;
        UnitInfo^.Insert(UnitInfoUsed);
        UnitInfo^.UnitInfoUsed:=UnitInfoUsed;
        UnitInfo^.UsedVSB:=VSB;
        UnitInfo^.UsedCST:=CST;
      end;

      UnitInfo^.UnitInfoDependent:=nil;
      if Assigned(DependentUnits) then
      begin
        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+1;
        New(CST, Init(R2,#180' Dependent units '#195+CharStr(#196,255),ColorIndex(12),false));
        CST^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
        if not Assigned(UsedUnits) then CST^.GrowMode:=gfGrowHiX;
        UnitInfo^.Insert(CST);

        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R3.B.Y;
        Dec(R2.B.X); { make space for VSB inside Panel }
        {HSB:=CreateHSB(R2); UnitInfo^.Insert(HSB); }
        HSB:=nil;
        VSB:=CreateVSB(R2);
        { UnitInfo^.Insert(VSB);  this created crashes,
        that were difficult to findout PM }
        { Maybe because it was outside Panel?  M }
        UnitInfo^.Insert(VSB); { lets try again with VSB inside Panel area }
        if Assigned(UsedUnits) then
          VSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowLoX+gfGrowHiY;
        New(UnitInfoDependent, Init(R2,DependentUnits,HSB,VSB));
        UnitInfoDependent^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
        if not Assigned(UsedUnits) then
          UnitInfoDependent^.GrowMode:=gfGrowHiY+gfGrowHiX;
        UnitInfoDependent^.MyBW:=@Self;
        UnitInfo^.Insert(UnitInfoDependent);
        UnitInfo^.UnitInfoDependent:=UnitInfoDependent;
        UnitInfo^.DependVSB:=VSB;
        UnitInfo^.DependCST:=CST;
      end;

      if Assigned(UnitInfoText) then
        UnitInfoText^.Select;

      PrevSize.Y:=0;
      PrevSize.X:=0;
      OnResize;
      Insert(UnitInfo);
    end;
  PrevSize:=Size;

  { hide not active pages so that scrollbars do not overlap }
  if assigned(ScopeView) then ScopeView^.SetState(sfVisible,False);
  if assigned(ReferenceView) then ReferenceView^.SetState(sfVisible,False);
  if assigned(InheritanceView) then InheritanceView^.SetState(sfVisible,False);
  if assigned(MemInfoView) then MemInfoView^.SetState(sfVisible,False);
  if assigned(UnitInfo) then UnitInfo^.SetState(sfVisible,False);

  GetExtent(R); R.Grow(-1,-1); R.Move(0,1); R.B.Y:=R.A.Y+1;
  New(PageTab, Init(R,
    NewBrowserTabItem(label_browsertab_scope,ScopeView,
    NewBrowserTabItem(label_browsertab_reference,ReferenceView,
    NewBrowserTabItem(label_browsertab_inheritance,InheritanceView,
    NewBrowserTabItem(label_browsertab_memory,MemInfoView,
    NewBrowserTabItem(label_browsertab_unit,UnitInfo,
    nil)))))));
  PageTab^.GrowMode:=gfGrowHiX;
  Insert(PageTab);

  if assigned(ScopeView) {Scope assinged and chosen to be selected by default}
    and ((DefaultBrowserPane=0) or not assigned(ReferenceView)) then
    SelectTab(btScope)
  else if assigned(ReferenceView) then
    SelectTab(btReferences)
  else if assigned(MemInfoView) then
    SelectTab(btMemInfo)
  else
   if assigned(InheritanceView) then
    SelectTab(btInheritance);
end;

destructor  TBrowserWindow.Done;
begin
  { UnitInfoText needs to be removed first
    to avoid crashes within the UnitInfo destructor PM }
  if Assigned(UnitInfoText) then
    begin
      UnitInfo^.Delete(UnitInfoText);
      Dispose(UnitInfoText,Done);
      UnitInfoText:=nil;
    end;
  if assigned(DebuggerValue) then
    begin
      Dispose(DebuggerValue,Done);
      DebuggerValue:=nil;
    end;
  if assigned(Prefix) then
    begin
      DisposeStr(Prefix);
      Prefix:=nil;
    end;
  inherited Done;
end;

procedure TBrowserWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    S: PHollowSymbol;
    Symbols: PSymbolCollection;
    Anc: PObjectSymbol;
    P,WH: TPoint;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmDebuggerStopped :
          begin
            if Assigned(DebuggerValue) and
               (DebuggerValue^.GDBI<>PtrInt(Event.InfoPtr)) then
              begin
                If Assigned(ST^.Text) then
                  DisposeStr(ST^.Text);
                ST^.Text:=NewStr(DebuggerValue^.GetText);
                ST^.DrawView;
              end;
          end;
        cmSearchWindow :
          ClearEvent(Event);
        cmListItemSelected :
          begin
            S:=nil;
            if (Event.InfoPtr=ScopeView) then
              begin
                S:=PHollowSymbol(ScopeView^.FilteredSym^.At(ScopeView^.Focused)^.Sym);
                MakeGlobal(ScopeView^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,ScopeView^.Focused-ScopeView^.TopItem);
                Inc(P.Y);
              end;
            if (Event.InfoPtr=UnitInfoUsed) then
              begin
                S:=PHollowSymbol(UnitInfoUsed^.FilteredSym^.At(UnitInfoUsed^.Focused)^.Sym);
                MakeGlobal(UnitInfoUsed^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,UnitInfoUsed^.Focused-UnitInfoUsed^.TopItem);
                Inc(P.Y);
              end;
            if (Event.InfoPtr=UnitInfoDependent) then
              begin
                S:=PHollowSymbol(UnitInfoDependent^.FilteredSym^.At(UnitInfoDependent^.Focused)^.Sym);
                MakeGlobal(UnitInfoDependent^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,UnitInfoDependent^.Focused-UnitInfoDependent^.TopItem);
                Inc(P.Y);
              end;
            if Assigned(S) then
              begin
                P.X:=Origin.X-1;WH.X:=0;WH.Y:=0;
                if DefaultBrowserSub = ReplaceCurrent then begin
                  P.X:=Origin.X;P.Y:=Origin.Y;
                  WH.X:=Size.X;WH.Y:=Size.Y;
                end;
                if S^.Ancestor=nil then Anc:=nil else
                  Anc:=SearchObjectForSymbol(S^.Ancestor);
                Symbols:=S^.Items;
                if (not assigned(Symbols)  or (symbols^.count=0)) then
                  if assigned(S^.Ancestor) then
                    Symbols:=S^.Ancestor^.Items;
                if (S^.GetReferenceCount>0) or (assigned(Symbols) and (Symbols^.Count>0)) or (Anc<>nil) then
                 OpenSymbolBrowser(P.X,P.Y,WH.X,WH.Y,
                   S^.GetName,
                   S^.GetText {ScopeView^.GetText(ScopeView^.Focused,255)},
                   S^.Sym,@self,
                   Symbols,S^.References,Anc,S^.MemInfo);
                ClearEvent(Event);
                if DefaultBrowserSub = ReplaceCurrent then
                  if (S^.GetReferenceCount>0) or (assigned(Symbols) and (Symbols^.Count>0)) or (Anc<>nil) then
                    Message(@Self,evCommand,cmClose,nil);
              end;
            end;
      end;
{    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
        cmGotoSymbol :
          if Event.InfoPtr=ScopeView then
           if ReferenceView<>nil then
            if ReferenceView^.Range>0 then
              ReferenceView^.GotoItem(0);
        cmTrackSymbol :
          if Event.InfoPtr=ScopeView then
            if (ScopeView<>nil) and (ScopeView^.Range>0) then
              begin
                S:=ScopeView^.At(ScopeView^.Focused);
                if (S^.References<>nil) and (S^.References^.Count>0) then
                  TrackItem(S^.References^.At(0));
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;}
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEsc :
            Close;
          kbAltI :
            If not Disassemble then
              DontClear:=true;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evCommand :
      begin
        if Event.Command = cmSymPrevious then
        begin
          if assigned (BrowserLinked) then
            BrowserLinked^.PreviousWindow;
          ClearEvent(Event);
        end else
        if Event.Command = cmClose then
        begin
          if assigned (BrowserLinked) then begin
            BrowserLinked^.LeaveTree;
            BrowserLinked:=nil;
          end;
          { do not clear event because actual close will be handled later }
        end;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TBrowserWindow.Disassemble : boolean;
begin
  Disassemble:=false;
  if not assigned(sym) or (sym^.typ<>procsym) then
    exit;
  { We need to load exefile }
{$ifndef NODEBUG}
  InitGDBWindow;
  if not assigned(Debugger) then
    begin
      new(Debugger,Init);
      if assigned(Debugger) then
        Debugger^.SetExe(ExeFile);
    end;
  if not assigned(Debugger) or not Debugger^.HasExe then
    exit;
  { goto source/assembly mixture }
  InitDisassemblyWindow;
  DisassemblyWindow^.LoadFunction(Sym^.GetName);
  DisassemblyWindow^.SelectInDebugSession;
  Disassemble:=true;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

function TBrowserWindow.GetFlags: longint;
begin
  GetFlags:=BrowserFlags;
end;

procedure TBrowserWindow.SetFlags(AFlags: longint);
begin
  BrowserFlags:=AFlags;
  if assigned(ScopeView) then
  begin
    ScopeView^.FilterSymbols(true);
    ScopeView^.SetRange(ScopeView^.FilteredSym^.Count);
    ScopeView^.DrawView;
  end;
end;

procedure TBrowserWindow.UpdateCommands;
var Active, Visible: boolean;
begin
  Visible:=GetState(sfVisible);
  Active:=GetState(sfActive) and Visible;
  SetCmdState([cmSaveAs,cmSymPrevious],Active);
  if Active and assigned(BrowserLinked) then
    if not assigned(BrowserLinked^.Previous) then
      SetCmdState([cmSymPrevious],false)  { parent is unknown yet }
    else if not assigned(BrowserLinked^.Previous^.Previous) then
      SetCmdState([cmSymPrevious],false); { those based in root have no Previous option }
  {Message(Application,evBroadcast,cmCommandSetChanged,nil);}
end;

procedure TBrowserWindow.SizeLimits (Var Min, Max: TPoint);
begin
  Min.X:=20;
  Min.Y:=15; { Scrollbars in unit info page is still usable }
  Max.X:=ScreenWidth;
  Max.Y:=ScreenHeight-2;
  if (PrevSize.X<>Size.X) or (PrevSize.Y<>Size.Y) then
  begin
    OnResize;
    PrevSize:=Size;
  end;
end;

procedure TBrowserWindow.OnResize;
var Y, uL,dL,tL: sw_integer;
    uMi,dMi,tMi: sw_integer;
    T,U,D : sw_integer;
    TotalLinesNeed : sw_integer;
begin
  if (PrevSize.Y<>Size.Y) then
  begin
    {-- unit info page resize manualy --}
    if assigned(UnitInfo) then
    begin
      { get number of lines everyone needs }
      Y:=UnitInfo^.Size.Y;
      tL:=UnitInfoText^.GetLineCount;
      tMi:=Min(tL,3);
      uL:=0;dL:=0;uMi:=0;dMi:=0;
      if assigned(UnitInfoUsed) then
      begin
        uMi:=4;
        uL:=UnitInfoUsed^.FilteredSym^.Count+1;
      end;
      if assigned(UnitInfoDependent) then
      begin
        dMi:=4;
        dL:=UnitInfoDependent^.FilteredSym^.Count+1;
      end;
      { proportional split amongst needy }
      TotalLinesNeed:=Max(tL+uL+dL,1);
      T:=Max(tMi,(tL*Y) div TotalLinesNeed);
      T:=Min(T,Max(tMi,tL)); { don't give more than actual need }
      TotalLinesNeed:=Max(uL+dL,1);
      Y:=Y-T;
      U:=Max(uMi,(uL*Y) div TotalLinesNeed);
      Y:=Y-U;
      D:=Y;
      if D<dMi then
      begin
        U:=U-(dMi-D);
        D:=dMi;
      end;
      { assign newly calculated positions and height for everyone }
      UnitInfoText^.Size.Y:=T;
      if assigned(UnitInfoUsed) then
      begin
        UnitInfo^.UsedCST^.Origin.Y:=T;
        UnitInfoUsed^.Origin.Y:=T+1;
        UnitInfo^.UsedVSB^.Origin.Y:=T+1;
        UnitInfoUsed^.Size.Y:=U-1;
        UnitInfo^.UsedVSB^.Size.Y:=U-1;
      end;
      if assigned(UnitInfoDependent) then
      begin
        UnitInfo^.DependCST^.Origin.Y:=T+U;
        UnitInfoDependent^.Origin.Y:=T+U+1;
        UnitInfo^.DependVSB^.Origin.Y:=T+U+1;
        UnitInfoDependent^.Size.Y:=D-1;
        UnitInfo^.DependVSB^.Size.Y:=D-1;
      end;
    end;
  end;
end;

procedure TBrowserWindow.Close;
begin
  inherited Close;
end;

procedure TBrowserWindow.SelectTab(BrowserTab: Sw_integer);
var Tabs: Sw_integer;
{$ifndef NODEBUG}
    PB : PBreakpoint;
{$endif}
    PS :PString;
    l : longint;
begin
  case BrowserTab of
    btScope :
      if assigned(ScopeView) then
        ScopeView^.Select;
    btReferences :
      if assigned(ReferenceView) then
        ReferenceView^.Select;
    btMemInfo:
      if assigned(MemInfoView) then
        MemInfoView^.Select;
{$ifndef NODEBUG}
    btBreakWatch :
      begin
        if Assigned(Sym) then
          begin
            if Pos('proc',Sym^.GetText)>0 then
          { insert function breakpoint }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointsCollection^.GetType(bt_function,copy(GetStr(PS),1,l-1));
                   If Assigned(PB) then
                     BreakpointsCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(GetStr(PS),1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_function(GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointsCollection^.Insert(PB);
                   BreakpointsCollection^.Update;
                 end;
            end
          else if pos('var',Sym^.GetText)>0 then
            { insert watch point }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointsCollection^.GetType(bt_awatch,copy(PS^,1,l-1));
                   If Assigned(PB) then
                     BreakpointsCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(PS^,1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_type(bt_awatch,GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointsCollection^.Insert(PB);
                   BreakpointsCollection^.Update;
                 end;
            end;
        end;
      end;
{$endif NODEBUG}
  end;
  Tabs:=0;
  if assigned(ScopeView) then
    Tabs:=Tabs or (1 shl btScope);
  if assigned(ReferenceView) then
    Tabs:=Tabs or (1 shl btReferences);
  if assigned(InheritanceView) then
    Tabs:=Tabs or (1 shl btInheritance);
  if assigned(MemInfoView) then
    Tabs:=Tabs or (1 shl btMemInfo);
{$ifndef NODEBUG}
  if Assigned(Sym) then
    if (Pos('proc',Sym^.GetText)>0) or (Pos('var',Sym^.GetText)>0) then
      Tabs:=Tabs or (1 shl btBreakWatch);
{$endif NODEBUG}
  if assigned(UnitInfo) then
    Tabs:=Tabs or (1 shl btUnitInfo);
  if PageTab<>nil then PageTab^.SetParams(Tabs,BrowserTab);
end;

function TBrowserWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

function OpenSymbolBrowser(X,Y,W,H: Sw_integer;const Name,Line: string;S : PSymbol;
            ParentBrowser : PBrowserWindow;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo):PBrowserWindow;
var R: TRect;
    PB : PBrowserWindow;
    St,st2 : string;
begin
  if X=0 then X:=Desktop^.Size.X-35;
  R.A.X:=X; R.A.Y:=Y;
  R.B.X:=R.A.X+35; R.B.Y:=R.A.Y+Max(15,(ScreenHeight * 3 div 5));
  if W<>0 then R.B.X:=R.A.X+W;
  if H<>0 then R.B.Y:=R.A.Y+H;
  while (R.B.Y>Desktop^.Size.Y) do R.Move(0,-1);
  if assigned(ParentBrowser) and assigned(ParentBrowser^.Prefix) and
     assigned(ParentBrowser^.sym) and
     (ParentBrowser^.sym^.typ<>unitsym)
     then
    begin
      st:=GetStr(ParentBrowser^.Prefix)+' '+Name;
    end
  else
    st:=Name;
  st2:=st;
  if assigned(S) and ((S^.Flags and sfPointer)<>0) then
    begin
      st:=st+'^';
      if assigned(S^.Ancestor) and
         ((S^.Ancestor^.Flags and sfRecord)<>0) then
        st:=st+'.';
    end
  else if assigned(S) and ((S^.Flags and sfRecord)<>0) then
    st:=st+'.';

  PB:=New(PBrowserWindow, Init(R,
    st2,SearchFreeWindowNo,S,Line,st,
    Symbols,References,Inheritance,MemInfo));
  if (assigned(S) and (S^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym])) or
     (assigned(ParentBrowser) and ParentBrowser^.IsValid) then
    PB^.IsValid:=true;

  if assigned(ParentBrowser) then
    ParentBrowser^.BrowserLinked^.InsertWindow(PB);

  Desktop^.Insert(PB);
  OpenSymbolBrowser:=PB;
end;


constructor TBrowserLinked.Init(ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
                    AInheritance: PObjectSymbol; AMemInfo: PSymbolMemInfo);
begin
  inherited init;
  Title:=ATitle;
  Number:=ANumber;
  Sym:=ASym;
  Name:=AName;
  Prefix:=APrefix;
  Symbols:= ASymbols;
  References:=AReferences;
  Inheritance:=AInheritance;
  MemInfo:=AMemInfo;
  New(Branches,Init(10,10));
end;

destructor TBrowserLinked.Done;
begin
  if Assigned(Branches) then
    begin
      Dispose(Branches,Done);
      Branches:=nil;
    end;
  Inherited Done;
end;

procedure TBrowserLinked.InsertWindow(BW : PBrowserWindow);
begin
  BW^.BrowserLinked^.Previous:=@self;
  BW^.BrowserLinked^.BrowserWindow:=BW;
  Branches^.Insert(BW^.BrowserLinked);
end;

procedure TBrowserLinked.CreateNewWindow;
var R: TRect;
    BW:PBrowserWindow;
begin
      R.A.X:=Origin.X;
      R.A.Y:=Origin.Y;
      R.B.X:=Origin.X+Size.X;
      R.B.Y:=Origin.Y+Size.Y;
      New(BrowserWindow, Init(R,Title,Number,Sym,
                    Name,Prefix,Symbols,References,
                    Inheritance,MemInfo));
      Dispose(BrowserWindow^.BrowserLinked,Done);
      BrowserWindow^.BrowserLinked:=@self;

      BW:=BrowserWindow;
      BW^.SelectTab(Tab);
      BW^.SetFlags(BrowserFlags);
      if assigned(BW^.ScopeView) then
      begin
        BW^.ScopeView^.SetTopItem(ScopeTop);
        BW^.ScopeView^.FocusItem(ScopeFocused);
      end;
      if assigned(BW^.ReferenceView) then
      begin
        BW^.ReferenceView^.SetTopItem(ReferenceTop);
        BW^.ReferenceView^.FocusItem(ReferenceFocused);
      end;
      if assigned(BW^.InheritanceView) then
      begin
{$ifdef HASOUTLINE}
        BW^.InheritanceView^.Delta.Y:=InheritanceTop;
        BW^.InheritanceView^.Focused(InheritanceFocused);
{$else}
        BW^.InheritanceView^.SetTopItem(InheritanceTop);
        BW^.InheritanceView^.FocusItem(InheritanceFocused);
{$endif}
      end;
      if assigned(BW^.UnitInfoUsed) then
      begin
        BW^.UnitInfoUsed^.SetTopItem(UnitInfoUsedTop);
        BW^.UnitInfoUsed^.FocusItem(UnitInfoUsedFocused);
      end;
      if assigned(BW^.UnitInfoDependent) then
      begin
        BW^.UnitInfoDependent^.SetTopItem(UnitInfoDependentTop);
        BW^.UnitInfoDependent^.FocusItem(UnitInfoDependentFocused);
      end;

      Desktop^.Insert(BrowserWindow);
end;

procedure TBrowserLinked.PreviousWindow;
begin
  if assigned(Previous) then
  begin
    if not assigned(Previous^.Previous) then
      exit; {root has no window to show}
    if not assigned(Previous^.BrowserWindow) then
      {window has been closed - recreate}
      Previous^.CreateNewWindow;
    if assigned(Previous^.BrowserWindow) then
    begin
      Previous^.BrowserWindow^.Show;
      Previous^.BrowserWindow^.Focus;
      if DefaultBrowserSub = ReplaceCurrent then
        if assigned(BrowserWindow) then
          Message(BrowserWindow,evCommand,cmClose,nil);
    end;
  end;
end;

function FreeIfEmpty(bl : PBrowserLinkedCollection):boolean;
var k : sw_integer;
    p : PBrowserLinked;
begin
   FreeIfEmpty:=true;
   if bl^.Count > 0 then
     for k:= bl^.Count-1 downto 0 do
     begin
        p:=bl^.at(k);
        if FreeIfEmpty(p^.Branches) then
          begin
            if assigned(p^.BrowserWindow) then
              FreeIfEmpty:=false
            else
              bl^.Free(P);
          end
        else
          FreeIfEmpty:=false;
     end;
end;


procedure TBrowserLinked.LeaveTree;
var k : sw_integer;
   BW : PBrowserWindow;
begin
  if assigned(BrowserWindow) then
  begin
    BW:=BrowserWindow;
    Origin:=BW^.Origin;
    Size:=BW^.Size;
    Tab:=BW^.PageTab^.Current;
    BrowserFlags:=BW^.GetFlags;
    if assigned(BW^.ScopeView) then
    begin
      ScopeTop:=BW^.ScopeView^.TopItem;
      ScopeFocused:=BW^.ScopeView^.Focused;
    end;
    if assigned(BW^.ReferenceView) then
    begin
      ReferenceTop:=BW^.ReferenceView^.TopItem;
      ReferenceFocused:=BW^.ReferenceView^.Focused;
    end;
    if assigned(BW^.InheritanceView) then
    begin
{$ifdef HASOUTLINE}
      InheritanceTop:=BW^.InheritanceView^.Delta.Y;
      InheritanceFocused:=BW^.InheritanceView^.Foc;
      InheritanceFocused:=BW^.InheritanceView^.NodeCountToFoc(InheritanceFocused);
{$else}
      InheritanceTop:=BW^.InheritanceView^.TopItem;
      InheritanceFocused:=BW^.InheritanceView^.Focused;
{$endif}
    end;
    if assigned(BW^.UnitInfoUsed) then
    begin
      UnitInfoUsedTop:=BW^.UnitInfoUsed^.TopItem;
      UnitInfoUsedFocused:=BW^.UnitInfoUsed^.Focused;
    end;
    if assigned(BW^.UnitInfoDependent) then
    begin
      UnitInfoDependentTop:=BW^.UnitInfoDependent^.TopItem;
      UnitInfoDependentFocused:=BW^.UnitInfoDependent^.Focused;
    end;

    BrowserWindow^.BrowserLinked:=nil; {this is pulling rug under the feet, FreeAndNil style}
    BrowserWindow:=nil;
  end;
  if FreeIfEmpty(Branches) then
  begin
    if assigned(Previous) then
      Previous^.Branches^.Free(@self);
    FreeIfEmpty(BrowserRoot^.Branches);
  end;
end;

function TBrowserLinkedCollection.At(Index: sw_Integer): PBrowserLinked;
begin
  At:=inherited At(Index);
end;

initialization
  New(BrowserRoot,init('',0,nil,'','',nil,nil,nil,nil));
finalization
  Dispose(BrowserRoot,Done);
END.
