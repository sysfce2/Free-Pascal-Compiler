{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

Unit UnixUtils;

Interface

uses
  SysUtils,Libc,Classes;

{ ---------------------------------------------------------------------
    Error handling
  ---------------------------------------------------------------------}


Type
  EUnixOperationFailed = Class(Exception)
  Private
    FErrorCode : Integer;
  Public
    Constructor Create (AnErrorCode : Longint);
    Property ErrorCode: Integer Read FErrorCode;
  end;

Function  StrError(Error:longint):AnsiString;
Function  CheckUnixError (Error : Integer) : Integer;

{ ---------------------------------------------------------------------
    File handling
  ---------------------------------------------------------------------}


Const
  PathSeparator = '/';

Type
  TUnixFileStream = Class(TFileStream)
    Procedure GetInfo(Var StatInfo: TStatBuf);
    Procedure LockRegion(Cmd,LockType,Whence : Integer;
                         Offset,Len : __off_t);
    Procedure ReadLock(Whence : Integer;Offset,Len : __off_t; Wait : Boolean);
    Procedure WriteLock(Whence : Integer;Offset,Len : __off_t; Wait : Boolean);
    Procedure UnLock(Whence : Integer;Offset,Len : __off_t);
  end;


{ Useful constants and structures }

Const
  PermissionBits : Array [1..9] of Integer =
         (S_IRUSR,S_IWUSR,S_IXUSR,
          S_IRGRP,S_IWGRP,S_IXGRP,
          S_IROTH,S_IWOTH,S_IXOTH);
  PermissionChars : Array[1..9] of AnsiChar =
          ('r','w','x','r','w','x','r','w','x');

  SuidBits  : array[1..3] of Integer = (S_ISUID,S_ISGID,S_ISVTX);
  SuidChars : Array[1..3] of AnsiChar = ('s','s','t') ;


{ Utility functions }

Type
 TPermissionString = string[9];

Type
  TGlobFlag = (gfErr,gfMark,gfNoSort,gfNoCheck,gfAppend,gfNoEscape,
               gfPeriod,gfBrace,gfNoMagic,gfTilde,gfOnlyDir,gfTildeCheck);
  TGlobFlags = Set of TGlobFlag;

  TFnmFlag = (fnmNoEscape,fnmPathName,fnmPeriod,fnmLeadingDir,fnmCaseFold);
  TFnmFlags = Set of TFnmFlag;

Procedure Stat  (Const FileName : AnsiString; Var StatInfo : TStatBuf);
Procedure LStat  (Const FileName : AnsiString; Var StatInfo : TStatBuf);
Procedure StatFS  (Const FileName : AnsiString; Var StatInfo : TStatFS);
Procedure UnLink(Const FileName: AnsiString);
Procedure Link  (Const FromName, ToName: AnsiString);
Procedure SymLink  (Const FromName, ToName: AnsiString);
Function  ReadLink (Const FileName : AnsiString) : AnsiString;
Function  FilePermString (Const Mode : __mode_t) : TPermissionString;
Function  PermStringToMask (Const Perm : TPermissionstring) : __mode_t;
Procedure ChMod(Const FileName : AnsiString; Mode : __mode_t);
Procedure ReName(Const OldName,NewName : AnsiString);
Function  Access(Const FileName : AnsiString; Mode :Integer) : Boolean;
Procedure Glob(Const Pattern : AnsiString; Flags : TGlobFlags; List : TStrings);
// Globfree call with correct calling conventions.
Procedure globfree(__pglob: PGlobData);cdecl;external 'libc.so.6' name 'globfree';

Function  OpenDir(Const Dir : AnsiString) : PDirectoryStream;
Function  FNMatch(Const Pattern,Name : AnsiString; Flags : TFnmFlags) : Boolean;
Procedure GetDirectoryListing(Const Dir : AnsiString; List : TStrings);overload;
Procedure GetDirectoryListing(Const Dir,Pattern : AnsiString;
                              Flags : TFnmFlags; List : TStrings);overload;
Procedure GetSubdirectories(Const Dir : AnsiString; List : TStrings);
Function  StripTrailingSeparator(Const Dir : AnsiString) : AnsiString;
Function  AddTraiLingSeparator(Const Dir : AnsiString) : AnsiString;
Function  FileSizeToString(Size: Int64) : AnsiString;
Function  SetMntEnt(FileName,Mode : AnsiString) : PIOFile;
Procedure Mount(Const Device,Directory,FileSystemType : AnsiString; Flags : Cardinal; Data: Pointer);
Procedure Umount(Const FileName);
Function  FSTypeToString(FSType : Integer) : AnsiString;
Procedure fcntl(Handle: Integer; Command: Integer; Var Lock: TFlock);
Procedure  Dup2(Stream1,Stream2 : THandleStream);
Function  Dup(Stream : THandleStream) : THandleStream;


{ ---------------------------------------------------------------------
  Process management routines.
  ---------------------------------------------------------------------}

function SetUID(UID: __uid_t):Boolean;
function SetEUID(UID: __uid_t):Boolean;
function SetGID(GroupID: __gid_t):Boolean;
function SetEGID(GroupID: __gid_t):Boolean;
function SetREUID(RUID: __uid_t; EUID: __uid_t):Boolean;
function SetREGID(RGID: __gid_t; EGID: __gid_t):Boolean;
Function GetGroups(Var A : Array of __gid_t) : Integer;
Function Group_member(GroupID : __gid_t) : Boolean;
Function Fork : __pid_t;
Function wait(var Status : Integer) : pid_t;
Function waitpid(PID : pid_t;var Status : Integer;options : Integer) : pid_t;
Function ConvertStatusToString(Status : Integer) : AnsiString;
Procedure Execve(ProgName : AnsiString; Args,Env : TStrings);
Procedure Execv(ProgName : AnsiString; Args : TStrings);
Procedure Execvp(ProgName : AnsiString; Args : TStrings);
Procedure Execle(ProgName : AnsiString; Args : Array of AnsiString;Env : TStrings);
Procedure Execl(ProgName : AnsiString; Args : Array of AnsiString);
Procedure Execlp(ProgName : AnsiString; Args : Array of AnsiString);

{ ---------------------------------------------------------------------
    User/group management routines
  ---------------------------------------------------------------------}

Type
  EUserLookupError = Class(Exception);
  EGroupLookupError = Class(Exception);
  EShadowLookupError = Class(Exception);

{ User functions }

Function  getpwnam(Const UserName: AnsiString) : PPasswordRecord;
Procedure GetUserData(Const UserName : AnsiString; Var Data : TPasswordRecord); overload;
Procedure GetUserData(Uid : Integer; Var Data : TPasswordRecord); overload;
function  GetUserName(UID : Integer) : AnsiString;
function  GetUserId(Const UserName : AnsiString) : Integer;
function  GetUserGid(Const UserName : AnsiString) : Integer;
function  GetUserDir(Const UserName : AnsiString): AnsiString;
function  GetUserDescription(Const UserName : AnsiString): AnsiString;
Procedure GetUserList(List : Tstrings);overload;
Procedure GetUserList(List : TStrings; WithIDs : Boolean);overload;

{ Group functions }

Function  getgrnam(Const GroupName: AnsiString) : PGroup;
Procedure GetGroupData(Const GroupName : AnsiString; Var Data : TGroup); overload;
Procedure GetGroupData(Gid : Integer; Var Data : TGroup); overload;
function  GetGroupName(GID : Integer) : AnsiString;
function  GetGroupId(Const GroupName : AnsiString) : Integer;
Procedure GetGroupList(List : Tstrings);overload;
Procedure GetGroupList(List : TStrings; WithIDs : Boolean);overload;
Procedure GetGroupMembers(GID : Integer;List : TStrings);overload;
Procedure GetGroupMembers(Const GroupName : AnsiString;List : TStrings);overload;

{ Shadow password functions }

function getspnam(UserName : AnsiString): PPasswordFileEntry;
function sgetspent(Line : AnsiString): PPasswordFileEntry;
Procedure GetUserShadowData(Const UserName : AnsiString; Var Data : TPasswordFileEntry);overload;
Procedure GetUserShadowData(UID : Integer; Var Data : TPasswordFileEntry);overload;

{ Extra functions }

Function GetUserGroup(Const UserName : AnsiString) : AnsiString;




Implementation

ResourceString
  SErrOpeningDir = 'Could not open directory "%s" for reading';
  SUnknownFileSystemType = 'Unknown filesystem (%x)';
  SNormalExitWithErrCode = 'Child exited with error code %d';
  SNormalExit            = 'Child exited normally';
  SSignalExit            = 'Child exited abnormally due to signal %d';
  SStopped               = 'Child stopped due to signal %d';
  SErrUnknowStatusCode   = 'Unknown exit status : %d';
  EnoSuchUserName = 'Unknown username: "%s"';
  EnoSuchUserID = 'Unknown user ID: %d';
  EnoSuchGroupName = 'Unknown groupname: "%s"';
  EnoSuchGroupID = 'Unknown group ID: %d';
  ENoShadowEntry = 'No shadow file entry for "%s"';
  EShadowNotPermitted = 'Not enough permissions to access shadow password file';

{ ---------------------------------------------------------------------
    Error handling
  ---------------------------------------------------------------------}


Function StrError(Error:longint):AnsiString;

begin
  StrError:=strpas(libc.strerror(Error));
end;

Constructor EUnixOperationFailed.Create(AnErrorCode : Longint);

begin
  FErrorCode:=AnErrorCode;
  Inherited Create(StrError(Abs(AnErrorCode)));
end;

Function CheckUnixError (Error : Integer) : Integer;

begin
  If (Error<0) then
    Raise EUnixOperationFailed.Create(Error);
  Result:=Error;
end;


Procedure Stat(Const FileName : AnsiString; Var StatInfo : TStatBuf);

begin
  CheckUnixError(Libc.Stat(PAnsiChar(FileName),StatInfo));
end;

Procedure LStat(Const FileName : AnsiString; Var StatInfo : TStatBuf);

begin
  CheckUnixError(Libc.LStat(PAnsiChar(FileName),StatInfo));
end;

Procedure StatFS  (Const FileName : AnsiString; Var StatInfo : TStatFS);

begin
  CheckUnixError(Libc.statfs(PAnsiChar(FileName),STatinfo));
end;

Procedure UnLink(const FileName: AnsiString);

begin
  CheckUnixError(Libc.unlink(PAnsiChar(FileName)));
end;

Procedure Link  (Const FromName, ToName: AnsiString);

begin
  CheckUnixError(Libc.Link(PAnsiChar(FromName),PAnsiChar(ToName)));
end;

Procedure SymLink  (Const FromName, ToName: AnsiString);

begin
  CheckUnixError(Libc.SymLink(PAnsiChar(FromName),PAnsiChar(ToName)));
end;

Function  ReadLink (Const FileName : AnsiString) : AnsiString;

Const
  NameBufSize = 1024;

begin
  SetLength(Result,NameBufSize);
  Try
    SetLength(Result,CheckUnixError(Libc.readlink(PAnsiChar(FileName),PAnsiChar(Result),NameBufSize)));
  except
    SetLength(Result,0);
    raise
  end;
end;


Function  FilePermString (Const Mode : __mode_t) : TPermissionString;

Var
  i : longint;

    Function ModeToSUIBit (C,RC : AnsiChar) : AnsiChar;

    begin
      If C='x' then
        Result:=RC
      else
        Result:=Upcase(RC);
    end;

begin
  Result:=StringOfChar('-',9);
  For I:=1 to 9 do
    If ((Mode and PermissionBits[i])=PermissionBits[i]) then
      Result[i]:=PermissionChars[i];
  For I:=1 to 3 do
    If ((Mode and SuidBits[i])=SuidBits[i]) then
      If Result[I*3]='x' then
        Result[i*3]:=SuidChars[i]
      else
        Result[i*3]:=UpCase(SuidChars[i]);
end;

Function  PermStringToMask (Const Perm : TPermissionstring) : __mode_t;

Var
  I : integer;

begin
  Result := 0;
  For I:=1 to 9 do
    If Perm[i]=PermissionChars[i] Then
      Result:=Result or PermissionBits[i]
    else
      If (I mod 3)=0 then
        If Perm[i]=suidchars[i] then
          Result:=(Result or PermissionBits[I]) or (SuidBits[I div 3])
        else if (Perm[i]=upcase(SuidChars[I])) then
          Result:=(Result or SuidBits[I div 3])
end;

Procedure ChMod(Const FileName : AnsiString; Mode : __mode_t);

begin
  CheckUnixError(Libc.Chmod(PAnsiChar(FileName),Mode));
end;

Procedure ReName(Const OldName,NewName : AnsiString);

begin
  CheckUnixError(Libc.__rename(PAnsiChar(OldName),PAnsiChar(NewName)));
end;

Function Access(Const FileName : AnsiString; Mode :Integer) : Boolean;

begin
  Result:=Libc.Access(PAnsiChar(FileName),Mode)=0;
end;



Procedure Glob(Const Pattern : AnsiString; Flags : TGlobFlags; List : TStrings);

Const
  // Append and offset are masked to 0, since they're useless.
  GF : Array[TGlobFlag] of Integer
     = (GLOB_ERR,GLOB_MARK,GLOB_NOSORT,GLOB_NOCHECK,0,
        GLOB_NOESCAPE,GLOB_PERIOD,GLOB_BRACE,GLOB_NOMAGIC,
        GLOB_TILDE,GLOB_ONLYDIR, GLOB_TILDE_CHECK);

Type
  TPCharArray = Array[Word] of PAnsiChar;
  PPCharArray = ^TPcharArray;
Var
  gd : TGlobData;
  i  : TGlobFlag;
  f  : Integer;

begin
  FillChar(gd,SizeOf(TGlobData),#0);
  f:=0;
  For i:=gfErr to gfTildeCheck do
    If i in Flags then
      F:=F or GF[i];
  Try
    CheckUnixError(Libc.Glob(PAnsiChar(Pattern),F,Nil,@gd));
    If Not (gfAppend in Flags) then
      List.Clear;
    for f:=0 to gd.gl_pathc-1 do
      List.add(Strpas(PPCharArray(gd.gl_pathv)^[f]));
  finally
    globFree(@gd);
  end;
end;

Function OpenDir(Const Dir : AnsiString) : PDirectoryStream;

begin
  Result:=Libc.OpenDir(PAnsiChar(Dir));
  If (Result=Nil) then
    Raise EUnixOperationFailed.CreateFmt(SErrOpeningDir,[Dir]);
end;


Procedure GetDirectoryListing(Const Dir : AnsiString; List : TStrings);overload;

Var
  P : PDirent;
  D : PDirectoryStream;

begin
  D:=OpenDir(Dir);
  Try
    P:=ReadDir(D);
    List.Clear;
    While P<>Nil do
      begin
      List.Add(StrPas(@p^.d_name[0]));
      P:=ReadDir(D);
      end;
  Finally
    CloseDir(D);
  end;
end;

Function FNtoFNFlags(Flags :TFnmFlags) : Integer;

Const
  FV : Array[TFnmFlag] of integer =
       (FNM_NOESCAPE,FNM_PATHNAME,FNM_PERIOD,FNM_LEADING_DIR,FNM_CASEFOLD);

Var i : TFnmFlag;

begin
  Result:=0;
  For I:=fnmNoEscape to fnmCaseFold do
    If i in Flags then
      Result:=Result or FV[i];
end;

Function FNMatch(Const Pattern,Name : AnsiString; Flags : TFnmFlags) : Boolean;

begin
  Result:=Libc.FNMatch(PAnsiChar(Pattern),PAnsiChar(Name),FNtoFNFlags(Flags))=0;
end;

Procedure GetDirectoryListing(Const Dir,Pattern : AnsiString; Flags : TFnmFlags; List : TStrings);overload;

Var
  P     : PDirent;
  D     : PDirectoryStream;
  PP,PF : PAnsiChar;
  F     : Integer;

begin
  D:=OpenDir(Dir);
  PP:=PAnsiChar(Pattern);
  F:=FNtoFNFlags(Flags);
  Try
    P:=ReadDir(D);
    List.Clear;
    While P<>Nil do
      begin
      PF:=@p^.d_name[0];
      If Libc.FNMatch(PP,PF,F)=0 then
        List.Add(StrPas(PF));
      P:=ReadDir(D);
      end;
  Finally
    CloseDir(D);
  end;
end;

Procedure GetSubdirectories(Const Dir : AnsiString; List : TStrings);

Var
  P : PDirent;
  D : PDirectoryStream;
  S : AnsiString;
  StatInfo : TStatBuf;

begin
  D:=OpenDir(Dir);
  Try
    P:=ReadDir(D);
    List.Clear;
    While P<>Nil do
      begin
      S:=StrPas(@p^.d_name[0]);
      LStat(Dir+'/'+S,StatInfo);
      If S_ISDIR(StatInfo.st_mode) then
        List.Add(S);
      P:=ReadDir(D);
      end;
  Finally
    CloseDir(D);
  end;
end;

Function  StripTrailingSeparator(Const Dir : AnsiString) : AnsiString;

Var
  L : Integer;

begin
  Result:=Dir;
  L:=Length(result);
  If (L>1) and (Result[l]=PathSeparator) then
    SetLength(Result,L-1);
end;

Function  AddTraiLingSeparator(Const Dir : AnsiString) : AnsiString;

Var
  L : Integer;

begin
  Result:=Dir;
  L:=Length(Result);
  If (L>0) and (Result[l]<>PathSeparator) then
    Result:=Result+PathSeparator;
end;

Function  FileSizeToString(Size: Int64) : AnsiString;

Const
  Sizes : Array [0..4] of AnsiString =
     ('Bytes','Kb','Mb','Gb','Tb');
Var
    F : Double;
    I : longint;

begin
  If Size>1024 Then
    begin
    F:=Size;
    I:=0;
    While (F>1024) and (I<4) do
      begin
      F:=F / 1024;
      Inc(i);
      end;
    Result:=Format('%4.2f %s',[F,Sizes[i]]);
    end
  else
    Result:=Format('%d %s',[Size,Sizes[0]]);
end;

Function  SetMntEnt(FileName,Mode : AnsiString) : PIOFile;

begin
  Result:=Libc.setmntent(PAnsiChar(FileName),PAnsiChar(Mode));
end;

Procedure Mount(Const Device,Directory,FileSystemType : AnsiString; Flags : Cardinal; Data: Pointer);

begin
  If Libc.Mount(PAnsiChar(Device),PAnsiChar(Directory),PAnsiChar(FileSystemType),Flags,Data)<>0 then
    CheckUnixError(Libc.errno);
end;

Procedure Umount(Const FileName);

begin
  If Libc.umount(PAnsiChar(FileName))<>0 then
    CheckUnixError(Libc.Errno);
end;

Function  FSTypeToString(FSType : Integer) : AnsiString;

begin
  Case LongWord(FStype) of
    $ADFF : Result:='affs';
    $137D : Result:='ext';
    $EF51,$EF53 : Result:='ext2';
    $F995E849 : Result := 'hpfs';
    $9660 : Result:='iso9660';
    $137F,$138F,$2468,$2478 : Result:='minix';
    $4d44 : Result:='msdos';
    $564c : Result:='ncp';
    $6969 : Result:='nfs';
    $9fa0 : Result:='proc';
    $517B : Result:='smb';
    $012FF7B4,$012FFB5,$012FFB6,$012FFB7 : Result:='xenix';
    $00011954 : Result:='ufs';
    $012FD16D : Result:='xia';
    $1CD1 : Result:='devpts';
    $5346544E : Result:='ntfs';
  else
    Result:=Format(SUnknownFileSystemType,[FStype]);
  end;
end;

Procedure fcntl(Handle: Integer; Command: Integer; Var Lock: TFlock);

begin
  CheckUnixError(Libc.fcntl(Handle,Command,Lock));
end;

Procedure Dup2(Stream1,Stream2 : THandleStream);

begin
  CheckUnixError(Libc.Dup2(Stream1.Handle,Stream2.Handle));
end;

Function Dup(Stream : THandleStream) : THandleStream;

begin
  Result:=ThandleStream.Create(CheckUnixError(Libc.Dup(Stream.Handle)));
end;


{ ---------------------------------------------------------------------
  TUnixFileStream implementation
  ---------------------------------------------------------------------}

Procedure TUnixFileStream.GetInfo(Var StatInfo: TStatBuf);

begin
  CheckUnixError(FStat(Handle,StatInfo));
end;

procedure TUnixFileStream.LockRegion(Cmd, LockType, Whence: Integer;
  Offset, Len: __off_t);

Var
  Lock : TFlock;

begin
  With Lock do
    begin
    L_type:=LockType;
    L_start:=Offset;
    L_Len:=Len;
    L_whence:=Whence;
    end;
  fcntl(Handle,cmd,Lock);
end;

procedure TUnixFileStream.ReadLock(Whence: Integer; Offset, Len: __off_t;
  Wait: Boolean);

begin
  If Wait then
    LockRegion(F_SETLKW,F_RDLCK,whence,offset,len)
  else
    LockRegion(F_SETLK,F_RDLCK,whence,offset,len)
end;

procedure TUnixFileStream.UnLock(Whence: Integer; Offset, Len: __off_t);
begin
  LockRegion(F_SETLK,F_UNLCK,whence,offset,len)
end;

procedure TUnixFileStream.WriteLock(Whence: Integer; Offset, Len: __off_t;
  Wait: Boolean);
begin
  If Wait then
    LockRegion(F_SETLKW,F_WRLCK,whence,offset,len)
  else
    LockRegion(F_SETLK,F_WRLCK,whence,offset,len)
end;

{ ---------------------------------------------------------------------
    Process utilities
  ---------------------------------------------------------------------}

function SetUID(UID: __uid_t):Boolean;

begin
  Result:=LibC.setuid(UID)=0;
end;

function SetEUID(UID: __uid_t):Boolean;

begin
  Result:=LibC.seteuid(UID)=0;
end;

function SetGID(GroupID: __gid_t):Boolean;

begin
  Result:=LibC.setgid(GroupID)=0;
end;

function SetEGID(GroupID: __gid_t):Boolean;

begin
  Result:=LibC.setegid(GroupID)=0;

end;

function SetREUID(RUID: __uid_t; EUID: __uid_t):Boolean;

begin
  Result:=LibC.setreuid(RUID,EUID)=0;
end;

function SetREGID(RGID: __gid_t; EGID: __gid_t):Boolean;

begin
  Result:=LibC.setregid(RGID,EGID)=0;
end;



Function GetGroups(var A : Array of __gid_t) : Integer;

begin
  Result:=LibC.GetGroups(High(A)+1,A);
end;

Function Group_member(GroupID : __gid_t) : Boolean;

begin
  Result:=LibC.group_member(GroupID)<>0;
end;

Function Fork : __pid_t;

begin
  Result:=CheckUnixError(LibC.Fork);
end;

Function wait(var Status : Integer) : pid_t;

begin
  Result:=Libc.wait(@Status);
end;

Function waitpid(PID : pid_t;var Status : Integer;options : Integer) : pid_t;

begin
  Result:=Libc.WaitPid(Pid,@Status,Options);
end;

Function ConvertStatusToString(Status : Integer) : AnsiString;

begin
  If WIfExited(Status) then
    If WExitStatus(Status)=0 then
      Result:=SNormalExit
    else
      Result:=Format(SNormalExitWithErrCode,[WExitStatus(Status)])
  else If WIfSIgnaled(Status) then
    Result:=Format(SSignalExit,[WTermSig(Status)])
  else if WIfStopped(Status) then
    Result:=Format(SStopped,[WStopSig(Status)])
  else
    Result:=Format(SErrUnknowStatusCode,[Status])
end;


Type
  TPCharArray = Array[Word] of PAnsiChar;
  PPCharArray = ^TPcharArray;


Function StringsToPCharList(Arg0 : AnsiString;List : TStrings) : PPAnsiChar;

Var
  I,Org : Integer;
  S : AnsiString;

begin
  I:=(List.Count)+1;
  If Arg0<>'' Then
    begin
    Inc(i);
    Org:=1;
    end
  else
    org:=0;
  GetMem(Result,I*sizeOf(PAnsiChar));
  PPCharArray(Result)^[List.Count+org]:=Nil;
  If Arg0<>'' Then
    PPCharArray(Result)^[0]:=StrNew(PAnsiChar(Arg0));
  For I:=0 to List.Count-1 do
    begin
    S:=List[i];
    PPCharArray(Result)^[i+org]:=StrNew(PAnsiChar(S));
    end;
end;

Procedure FreePCharList(List : PPAnsiChar);

Var
  I : integer;

begin
  I:=0;
  While List[i]<>Nil do
    begin
    StrDispose(List[i]);
    Inc(I);
    end;
  FreeMem(List);
end;

Procedure Execve(ProgName : AnsiString; Args,Env : TStrings);

Var
  ArgP,EnvP : PPAnsiChar;

begin
  ArgP:=StringsToPCharList(ExtractFileName(ProgName),Args);
  try
    EnvP:=StringsToPCharList('',Env);
    try
      CheckUnixError(Libc.execve(PAnsiChar(ProgName),ArgP,EnvP));
    finally
      FreePCharList(EnvP);
    end;
  finally
    FreePCharList(ArgP);
  end;
end;


Procedure Execv(ProgName : AnsiString; Args : TStrings);

Var
  ArgP : PPAnsiChar;

begin
  ArgP:=StringsToPCharList(ExtractFileName(ProgName),Args);
  try
    CheckUnixError(Libc.execv(PAnsiChar(ProgName),ArgP));
  finally
    FreePCharList(ArgP);
  end;
end;

Procedure Execvp(ProgName : AnsiString; Args : TStrings);

Var
  ArgP : PPAnsiChar;

begin
  ArgP:=StringsToPCharList(ExtractFileName(ProgName),Args);
  try
    CheckUnixError(Libc.execvp(PAnsiChar(ProgName),ArgP));
  finally
    FreePCharList(ArgP);
  end;
end;

Function CommandArgsToPCharList(Arg0 :AnsiString;Args : Array of AnsiString) : PPAnsiChar;

Var
  I,Org : Integer;

begin
  I:=High(Args)+2;
  If Arg0<>'' Then
    begin
    Inc(i);
    Org:=1;
    end
  else
    org:=0;
  GetMem(Result,I*sizeOf(PAnsiChar));
  PPCharArray(Result)^[i-1]:=Nil;
  If Arg0<>'' Then
    PPCharArray(Result)^[0]:=StrNew(PAnsiChar(Arg0));
  For I:=0 to High(Args) do
    PPCharArray(Result)^[i+org]:=StrNew(PAnsiChar(Args[i]));
end;

Procedure Execle(ProgName : AnsiString; Args : Array of AnsiString;Env : TStrings);

Var
  ArgP,EnvP : PPAnsiChar;

begin
  ArgP:=CommandArgsToPCharList(ExtractFileName(ProgName),Args);
  try
    EnvP:=StringsToPCharList('',Env);
    try
      CheckUnixError(Libc.execve(PAnsiChar(ProgName),ArgP,EnvP));
    finally
    FreePCharList(EnvP);
    end;
  finally
    FreePCharList(ArgP);
  end;
end;

Procedure Execl(ProgName : AnsiString; Args : Array of AnsiString);

Var
  ArgP : PPAnsiChar;

begin
  ArgP:=CommandArgsToPCharList(ExtractFileName(ProgName),Args);
  try
    CheckUnixError(Libc.execv(PAnsiChar(ProgName),ArgP));
  finally
    FreePCharList(ArgP);
  end;
end;

Procedure Execlp(ProgName : AnsiString; Args : Array of AnsiString);

Var
  ArgP : PPAnsiChar;

begin
  ArgP:=CommandArgsToPCharList(ExtractFileName(ProgName),Args);
  try
    CheckUnixError(Libc.execvp(PAnsiChar(ProgName),ArgP));
  finally
    FreePCharList(ArgP);
  end;
end;

{ ---------------------------------------------------------------------
    User/Group management routines.
  ---------------------------------------------------------------------}


Function getpwnam(Const UserName: AnsiString) : PPasswordRecord;

begin
  Result:=libc.getpwnam(PAnsiChar(UserName));
end;

Procedure GetUserData(Const UserName : AnsiString; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=Getpwnam(UserName);
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserName,[UserName]);
end;

Procedure GetUserData(Uid : Integer; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=Getpwuid(Uid);
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserID,[Uid]);
end;

function GetUserName(UID : Integer) : AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetuserData(UID,UserData);
  Result:=strpas(UserData.pw_Name);
end;

function  GetUserId(Const UserName : AnsiString) : Integer;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_uid;
end;

function  GetUserGId(Const UserName : AnsiString) : Integer;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_gid;
end;

function GetUserDir(Const UserName : AnsiString): AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=strpas(UserData.pw_dir);
end;

function  GetUserDescription(Const UserName : AnsiString): AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=strpas(UserData.pw_gecos);
end;

Procedure GetUserList(List : Tstrings);

begin
  GetUserList(List,False);
end;

Procedure GetUserList(List : TStrings; WithIDs : Boolean);

Var
  P : PPasswordRecord;

begin
  List.Clear;
  setpwent;
  try
    Repeat
      P:=getpwent;
      If P<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[P^.pw_uid,strpas(p^.pw_name)]))
        else
          List.Add(strpas(p^.pw_name));
        end;
    until (P=Nil);
  finally
    endpwent;
  end;
end;

{ ---------------------------------------------------------------------
    Group Functions
  ---------------------------------------------------------------------}


Function  getgrnam(Const GroupName: AnsiString) : PGroup;

begin
  Result:=libc.getgrnam(PAnsiChar(GroupName));
end;

Procedure GetGroupData(Const GroupName : AnsiString; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=Getgrnam(GroupName);
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupName,[GroupName]);
end;

Procedure GetGroupData(Gid : Integer; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=Getgrgid(gid);
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupID,[Gid]);
end;

function GetGroupName(GID : Integer) : AnsiString;

Var
  G : TGroup;

begin
  GetGroupData(Gid,G);
  Result:=StrPas(G.gr_name);
end;

function  GetGroupId(Const GroupName : AnsiString) : Integer;

Var
  G : TGroup;

begin
  GetGroupData(GroupName,G);
  Result:=G.gr_gid;
end;

Procedure GetGroupList(List : Tstrings);overload;

begin
  GetGroupList(List,False);
end;

Procedure GetGroupList(List : TStrings; WithIDs : Boolean);overload;

Var
  G : PGroup;

begin
  List.Clear;
  setgrent;
  try
    Repeat
      G:=getgrent;
      If G<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[G^.gr_gid,strpas(G^.gr_name)]))
        else
          List.Add(strpas(G^.gr_name));
        end;
    until (G=Nil);
  finally
    endgrent;
  end;
end;

Function PCharListToStrings(P : PPAnsiChar; List : TStrings) : Integer;

begin
  List.Clear;
  While P^<>Nil do
    begin
    List.Add(StrPas(P^));
    P:=PPAnsiChar(PAnsiChar(P)+SizeOf(PAnsiChar));
    end;
  Result:=List.Count;
end;


Procedure GetGroupMembers(GID : Integer;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GID,G);
  PCharListToStrings(G.gr_mem,List);
end;

Procedure GetGroupMembers(Const GroupName : AnsiString;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GroupName,G);
  PCharListToStrings(g.gr_mem,List);
end;

{ Shadow password functions }

function getspnam(UserName : AnsiString): PPasswordFileEntry;

begin
  result:=Libc.getspnam(PAnsiChar(UserName));
end;

function sgetspent(Line : AnsiString): PPasswordFileEntry;

begin
  Result:=libc.sgetspent(PAnsiChar(Line));
end;

Procedure GetUserShadowData(Const UserName : AnsiString; Var Data : TPasswordFileEntry);

Var
  P : PPasswordFileEntry;

begin
  P:=getspnam(UserName);
  If P=Nil then
    If (GetUID<>0) and (GetEUID<>0) then
      Raise EShadowLookupError.Create(EShadowNotPermitted)
    else
      Raise EShadowLookupError.CreateFmt(ENoShadowEntry,[UserName])
  else
    Data:=P^;
end;

Procedure GetUserShadowData(UID : Integer; Var Data : TPasswordFileEntry);

begin
  GetUserShadowData(GetUserName(UID),Data);
end;

{ Extra functions }

Function GetUserGroup(Const UserName : AnsiString) : AnsiString;

begin
  Result := GetGroupName(GetUserGid(UserName));
end;





end.
