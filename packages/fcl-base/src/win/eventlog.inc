{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Win32 implementation part of event logging facility.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses windows;
{$ENDIF FPC_DOTTEDUNITS}

const
  SKeyEventLog = 'SYSTEM\CurrentControlSet\Services\EventLog\Application\%s';

Function TEventLog.DefaultFileName : String;

begin
  Result:=ChangeFileExt(Paramstr(0),RTLString('.log'));
end;

Resourcestring
  SErrNoSysLog = 'Could not open system log (error %d)';
  SErrLogFailed = 'Failed to log entry (error %d)';

Procedure TEventLog.ActivateSystemLog;

begin
  CheckIdentification;
  FLogHandle := Pointer(OpenEventLogA(Nil,PAnsiChar(Identification)));
  If (FLogHandle=Nil) and FRaiseExceptionOnError then
    Raise ELogError.CreateFmt(SErrNoSysLog,[GetLastError]);
end;

Procedure TEventLog.DeActivateSystemLog;

begin
  CloseEventLog(Cardinal(FLogHandle));
end;

{
function ReportEvent(hEventLog: THandle; wType, wCategory: Word;
  dwEventID: DWORD; lpUserSid: Pointer; wNumStrings: Word;
  dwDataSize: DWORD; lpStrings, lpRawData: Pointer): BOOL; stdcall;
}

procedure TEventLog.WriteSystemLog(EventType : TEventType; const Msg : String);

Var
  P : PAnsiChar;
  I : Integer;
  FCategory : Word;
  FEventID : DWord;
  FEventType : Word;
  aMsg : AnsiString {$IF SIZEOF(CHAR)=1} absolute Msg{$ENDIF};
  
begin
  FCategory:=MapTypeToCategory(EventType);
  FEventID:=MapTypeToEventID(EventType);
  FEventType:=MapTypeToEvent(EventType);
  {$IF SIZEOF(CHAR)=2}
  aMsg:=Msg;
  {$ENDIF}
  P:=PAnsiChar(aMsg);
  If Not ReportEvent(Cardinal(FLogHandle),FEventType,FCategory,FEventID,Nil,1,0,@P,Nil) and FRaiseExceptionOnError then
    begin
    I:=GetLastError;
    Raise ELogError.CreateFmt(SErrLogFailed,[I]);
    end;
end;

Function TEventLog.RegisterMessageFile(AFileName : String) : Boolean;

Const
  SKeyCategoryCount       = 'CategoryCount';
  SKeyEventMessageFile    = 'EventMessageFile';
  SKeyCategoryMessageFile = 'CategoryMessageFile';
  SKeyTypesSupported      = 'TypesSupported';

Var
  ELKey : String;
  Handle : HKey;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;
  Value,
  Disposition : Dword;
begin
  SecurityAttributes:=nil;
  CheckIdentification;
  ELKey:=Format(SKeyEventLog,[IDentification]);
  Result:=RegCreateKeyExA(HKEY_LOCAL_MACHINE,
                          PAnsiChar(ELKey),0,'',
                          REG_OPTION_NON_VOLATILE,
                          KEY_ALL_ACCESS,
                          SecurityAttributes,Handle,
                          pdword(@Disposition))=ERROR_SUCCESS;
  If Result then
    begin
      If AFileName='' then
        AFileName:=ParamStr(0);
      Value:=4;
      Result:=Result and (RegSetValueExA(Handle,PAnsiChar(SKeyCategoryCount),0,REG_DWORD,@Value,sizeof(DWORD))=ERROR_SUCCESS);
      Value:=7;
      Result:=Result and (RegSetValueExA(Handle,PAnsiChar(SKeyTypesSupported),0,REG_DWORD,@Value,sizeof(DWORD))=ERROR_SUCCESS);
      Result:=Result and (RegSetValueExA(Handle,PAnsiChar(SKeyCategoryMessageFile),0,REG_SZ,@AFileName[1],Length(AFileName))=ERROR_SUCCESS);
      Result:=Result and (RegSetValueExA(Handle,PAnsiChar(SKeyEventMessageFile),0,REG_SZ,@AFileName[1],Length(AFileName))=ERROR_SUCCESS);
    end;
end;

Function TEventLog.UnRegisterMessageFile : Boolean;

Var
  ELKey : String;
begin
  ELKey:=Format(SKeyEventLog,[IDentification]);
  Result:=(RegDeleteKeyA(HKEY_LOCAL_MACHINE,PAnsiChar(ELKey))=ERROR_SUCCESS);
end;

function TEventLog.MapTypeToCategory(EventType: TEventType): Word;
begin
  If (EventType=ETCustom) then
    DoGetCustomEventCategory(Result)
  else
    Result:=Ord(EventType);
  If Result=0 then
    Result:=1;
end;

function TEventLog.MapTypeToEventID(EventType: TEventType): DWord;

begin
  If (EventType=ETCustom) then
    DoGetCustomEventID(Result)
  else
    begin
    If (FEventIDOffset=0) then
      FEventIDOffset:=1000;
    Result:=FEventIDOffset+Ord(EventType);
    end;
end;

function TEventLog.MapTypeToEvent(EventType: TEventType): DWord;



Const
  EVENTLOG_SUCCESS=0;
  WinET : Array[TEventType] of word = (EVENTLOG_SUCCESS,
     EVENTLOG_INFORMATION_TYPE,
     EVENTLOG_WARNING_TYPE,EVENTLOG_ERROR_TYPE,
     EVENTLOG_AUDIT_SUCCESS);

begin
  If EventType=etCustom Then
    begin
    If CustomLogType=0 then
      CustomLogType:=EVENTLOG_SUCCESS;
    Result:=CustomLogType;
    DoGetCustomEvent(Result);
    end
  else
    Result:=WinET[EventType];
end;
