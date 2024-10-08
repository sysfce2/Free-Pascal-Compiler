{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Low leve file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{****************************************************************************
                        Low level File Routines
 ****************************************************************************}

procedure do_close(handle : thandle);inline;
begin
end;


procedure do_erase(p : PAnsiChar; pchangeable: boolean);inline;
begin
end;


procedure do_rename(p1,p2 : PAnsiChar; p1changeable, p2changeable: boolean);inline;
begin
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
  i: longint;
begin
  case h of
    StdOutputHandle,
    StdErrorHandle:
      begin
        for i:=0 to len-1 do
          _putchar(PChar(addr)[i]);
        do_write:=len;
      end;
    else
      do_write:=len;
  end;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
begin
  do_read:=0;
end;


function do_filepos(handle : thandle) : longint;inline;
begin
end;


procedure do_seek(handle:thandle;pos : longint);inline;
begin
end;



function do_seekend(handle:thandle):longint;inline;
begin
end;


function do_filesize(handle : thandle) : longint;inline;
begin

end;


{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);inline;
begin
end;

procedure do_open(var f;p:PAnsiChar;flags:longint; pchangeable: boolean);inline;
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
begin
end;


function do_isdevice(handle:THandle):boolean;inline;
begin
  do_isdevice:=true;
end;
