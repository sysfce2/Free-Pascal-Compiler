{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    RiscV32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Common RiscV stuff }
{$I ../riscv/riscv.inc}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;
  asm
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
  begin
    dec(Target);
    Result:=Target;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
    inc(Target);
    Result:=Target;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    inc(Target,Source);
  end;
{$endif}


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32'];
{$endif VER3_2}
  begin
    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
  end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
{$ifdef FPUFD}
var
  cw: TNativeFPUControlWord;
{$endif}
begin
  softfloat_exception_flags:=[];
  softfloat_exception_mask:=[exPrecision,exUnderflow];
{$ifdef FPUFD}
  cw:=GetNativeFPUControlWord;
  { riscv does not support triggering exceptions when FPU exceptions happen;
    it merely records which exceptions have happened until now -> clear }
  cw.cw:=0;
  { round to nearest }
  cw.rndmode:=0;
  SetNativeFPUControlWord(cw);
{$endif}
end;

