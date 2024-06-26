{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ definition of pthread_mutex_t, because needed in both ptypes.inc and }
{ in sysosh.inc                                                        }

{ use a macro rather than a constant, so this name doesn't get exported
  from the system unit interface; macro's have to be on at this point
  because they're use to propagate the MUTEXTYPENAME here }

{$define USE_PTHREAD_SIZEOF}

{$if defined(cpuaarch64)}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 48}
{$elseif defined(CPU64)}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 40}
{$else CPU64}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 24}
{$endif CPU64}

  MUTEXTYPENAME = record
    case byte of
      0 : (
            __size : array[0..__SIZEOF_PTHREAD_MUTEX_T-1] of AnsiChar;
          );
      1 : (
             __align : sizeint;
          );
      2 : (
             pad1, pad2, pad3 {$ifdef cpu64}, pad4{$endif}: longint;
             { this field is guaranteed to stay at this offset by glibc for
               binary compatibility with static initialisers }
             __m_kind: longint;
          );
  end;

{$ifdef __SIZEOF_PTHREAD_MUTEX_T}
{$undef __SIZEOF_PTHREAD_MUTEX_T}
{$endif __SIZEOF_PTHREAD_MUTEX_T}
{$macro off}
