{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Michael Van Canneyt, Peter Vreman,
    & Daniel Mantione, members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 Linux ELF startup code for Free Pascal


 Stack layout at program start:

         nil
         envn
         ....
         ....           ENVIRONMENT VARIABLES
         env1
         env0
         nil
         argn
         ....
         ....           COMMAND LINE OPTIONS
         arg1
         arg0
         argc <--- esp
}

var
  libc_environ: PAnsiChar; external name '__environ';
  libc_fpu_control: word; external name '__fpu_control';
  libc_init_proc: procedure; external name '_init';
  libc_fini_proc: procedure; external name '_fini';

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

Procedure fpc_geteipasebx;[external name 'fpc_geteipasebx'];

{******************************************************************************
                          C library start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }
  popl    %ecx                    { Get argc in ecx }

  {$ifdef FPC_PIC}
        movl    %esp,%ebp               { Points to the arguments }
        movl    %ecx,%esi               
  {$else FPC_PIC}
        movl    %esp,%ebx               { Points to the arguments }
  {$endif FPC_PIC}

  leal    4(%esp,%ecx,4),%eax     { The start of the environment is: esp+4*eax+8 }
  andl    $0xfffffff8,%esp        { Align stack }

  {$ifdef FPC_PIC}
        pushl   %ecx
        call  fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx
        movl  operatingsystem_parameter_envp@GOT(%ebx),%ecx
        movl  %eax,(%ecx)
        movl  libc_environ@GOT(%ebx),%ecx
        movl  %eax,(%ecx)

        pushl %eax
        movl  operatingsystem_parameter_argc@GOT(%ebx),%ecx
        movl  %esi,%eax
        movl  %eax,(%ecx)
        movl  operatingsystem_parameter_argv@GOT(%ebx),%ecx
        movl  %ebp,%eax
        movl  %eax,(%ecx)
	popl  %eax

        popl  %ecx
        movl  %ebp,%ebx
  {$else FPC_PIC}
	movl    %eax,operatingsystem_parameter_envp    { Move the environment pointer }
	movl    %ecx,operatingsystem_parameter_argc    { Move the argument counter    }
	movl    %ebx,operatingsystem_parameter_argv    { Move the argument pointer    }
  	movl    %eax,libc_environ          { libc environ }
  {$endif FPC_PIC}


  pushl   %eax
  pushl   %ebx
  pushl   %ecx

  call    libc_init             { init libc }

  {$ifdef FPC_PIC}
        pushl   %ecx
  	pushl   %ebx
        call  fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx
        movl  libc_init_proc@GOT(%ebx),%ecx
	movl  (%ecx),%edi
  	popl    %ebx
        popl    %ecx
  {$else FPC_PIC}
	movzwl  libc_fpu_control,%eax
  {$endif FPC_PIC}


  pushl   %eax
  call    libc_setfpucw
  popl    %eax
  pushl   $libc_fini_proc
  call    libc_atexit
  popl    %eax

  {$ifdef FPC_PIC}
	call    *%edi
  {$else FPC_PIC}
	call    libc_init_proc
  {$endif FPC_PIC}

  popl    %eax
  popl    %eax

  { Save initial stackpointer }
  {$ifdef FPC_PIC}
        pushl   %ecx
  	pushl   %ebx
        call  fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx
        movl  initialstkptr@GOT(%ebx),%ecx
	movl  %esp,(%ecx)
  	popl    %ebx
        popl    %ecx
  {$else FPC_PIC}
	movl    %esp,initialstkptr
  {$endif FPC_PIC}

  xorl    %ebp,%ebp
  call    PASCALMAIN              { start the program }
end;

procedure _FPC_libc_haltproc(e: longint); cdecl; assembler; public name '_haltproc';
asm
.Lhaltproc:
  pushl   e
  call    libc_exit
  xorl    %eax,%eax
  incl    %eax                    { eax=1, exit call }
  movl    e,%ebx
  int     $0x80
  jmp     .Lhaltproc
end;

