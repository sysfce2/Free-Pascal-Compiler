{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Free Pascal development team

    This file implements parts of the startup code for OpenBSD
    shared object (.so) libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode att}

procedure _init; cdecl; weakexternal name '_init';

procedure _FPC_shared_lib_start; cdecl; public name 'FPC_LIB_START';
  begin
    { todo: figure out if there's any way to obtain these in OpenBSD shared libraries }
    environ:=nil;
    operatingsystem_parameter_envp:=nil;
    operatingsystem_parameter_argc:=0;
    operatingsystem_parameter_argv:=nil;
    if Assigned(@_init) then
      _init;
    PascalMain;
  end;

{ this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called }
procedure _FPC_shared_lib_haltproc; cdecl; public name '_haltproc';
  var
    ExitCode: LongInt;
  begin
    ExitCode:=operatingsystem_result;
    asm
.Lendless:
      pushl   ExitCode
      mov     $1,%eax
      call    .Lactualsyscall
      jmp     .Lendless

.Lactualsyscall:
      int     $0x80
      ret
    end;
  end;
