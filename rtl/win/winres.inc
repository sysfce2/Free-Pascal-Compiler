function SysEnumResourceTypes(hModule : TFPResourceHMODULE; lpEnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool; stdcall; external 'kernel32' name 'EnumResourceTypesA';
function SysEnumResourceNames(hModule : TFPResourceHMODULE; lpszType : PAnsiChar; lpEnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool; stdcall; external 'kernel32' name 'EnumResourceNamesA';
function SysEnumResourceLanguages(hModule : TFPResourceHMODULE; lpType : PAnsiChar; lpName : PAnsiChar; lpEnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool; stdcall; external 'kernel32' name 'EnumResourceLanguagesA';
function SysFindResource(hModule:TFPResourceHMODULE; lpName:PAnsiChar; lpType:PAnsiChar):TFPResourceHandle; stdcall; external 'kernel32' name 'FindResourceA';
function SysFindResourceEx(hModule:TFPResourceHMODULE; lpType:PAnsiChar; lpName:PAnsiChar; Language : WORD):TFPResourceHandle; stdcall; external 'kernel32' name 'FindResourceExA';
function SysLoadResource(hModule:TFPResourceHMODULE; hResInfo:TFPResourceHandle):TFPResourceHGLOBAL; stdcall; external 'kernel32' name 'LoadResource';
function SysSizeofResource(hModule:TFPResourceHMODULE; hResInfo:TFPResourceHandle):DWORD; stdcall; external 'kernel32' name 'SizeofResource';
function SysLockResource(hResData:TFPResourceHGLOBAL):Pointer; stdcall; external 'kernel32' name 'LockResource';
function SysFreeResource(hResData:TFPResourceHGLOBAL):Longbool; stdcall; external 'kernel32' name 'FreeResource';

var
  SysInstance : PPtrUInt;external name '_FPC_SysInstance';

Function IntHINSTANCE : TFPResourceHMODULE;
begin
  IntHINSTANCE:=sysinstance^;
end;

Function IntEnumResourceTypes(ModuleHandle : TFPResourceHMODULE; EnumFunc : EnumResTypeProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceTypes:=SysEnumResourceTypes(ModuleHandle,EnumFunc,lParam);
end;

Function IntEnumResourceNames(ModuleHandle : TFPResourceHMODULE; ResourceType : PAnsiChar; EnumFunc : EnumResNameProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceNames:=SysEnumResourceNames(ModuleHandle,ResourceType,EnumFunc,lParam);
end;

Function IntEnumResourceLanguages(ModuleHandle : TFPResourceHMODULE; ResourceType, ResourceName : PAnsiChar; EnumFunc : EnumResLangProc; lParam : PtrInt) : LongBool;
begin
  IntEnumResourceLanguages:=SysEnumResourceLanguages(ModuleHandle,ResourceType,ResourceName,EnumFunc,lParam);
end;

Function IntFindResource(ModuleHandle: TFPResourceHMODULE; ResourceName, ResourceType: PAnsiChar): TFPResourceHandle;
begin
  IntFindResource:=SysFindResource(ModuleHandle,ResourceName,ResourceType);
end;

Function IntFindResourceEx(ModuleHandle: TFPResourceHMODULE; ResourceType, ResourceName: PAnsiChar; Language : word): TFPResourceHandle;
begin
  IntFindResourceEx:=SysFindResourceEx(ModuleHandle,ResourceType,ResourceName,Language);
end;

Function IntLoadResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): TFPResourceHGLOBAL;
begin
  IntLoadResource:=SysLoadresource(ModuleHandle,Reshandle);
end;

Function IntSizeofResource(ModuleHandle: TFPResourceHMODULE; ResHandle: TFPResourceHandle): LongWord;
begin
  IntSizeofResource:=SysSizeofResource(ModuleHandle,Reshandle);
end;

Function IntLockResource(ResData: TFPResourceHGLOBAL): Pointer;
begin
  IntLockResource:=SysLockResource(ResData);
end;

Function IntUnlockResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntUnlockResource:=SysFreeResource(ResData);
end;

Function IntFreeResource(ResData: TFPResourceHGLOBAL): LongBool;
begin
  IntFreeResource:=SysFreeResource(ResData);
end;

const
  InternalResourceManager : TResourceManager =
  (
    HINSTANCEFunc : @IntHINSTANCE;
    EnumResourceTypesFunc : @IntEnumResourceTypes;
    EnumResourceNamesFunc : @IntEnumResourceNames;
    EnumResourceLanguagesFunc : @IntEnumResourceLanguages;
    FindResourceFunc : @IntFindResource;
    FindResourceExFunc : @IntFindResourceEx;
    LoadResourceFunc : @IntLoadResource;
    SizeofResourceFunc : @IntSizeofResource;
    LockResourceFunc : @IntLockResource;
    UnlockResourceFunc : @IntUnlockResource;
    FreeResourceFunc : @IntFreeResource;
  );
