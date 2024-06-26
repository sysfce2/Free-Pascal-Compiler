{ Pango
   PangoUtils - Utilities for internal functions and modules

   Copyright (C) 2000 Red Hat Software

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
  }
{$IFNDEF FPC_DOTTEDUNITS}
unit pangoutils;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC}
  {$MODE objfpc}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE KYLIX}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.Glib2, Api.Pango;
{$ELSE FPC_DOTTEDUNITS}
uses
  glib2, pango;
{$ENDIF FPC_DOTTEDUNITS}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
{$IFDEF KYLIX}
  PFile     = ^file;
{$ENDIF}

{$IFNDEF KYLIX}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN 4}
  {$WEAKPACKAGEUNIT}
  {$WARNINGS OFF}
{$ENDIF}

function pango_split_file_list(str:PAnsiChar):PPAnsiChar; cdecl; external pangolib;
function pango_trim_string(str:PAnsiChar):PAnsiChar; cdecl; external pangolib;
function pango_read_line(stream:PFILE; str:PGString):gint; cdecl; external pangolib;
function pango_skip_space(pos:PPAnsiChar):gboolean; cdecl; external pangolib;
function pango_scan_word(pos:PPAnsiChar; OutStr:PGString):gboolean; cdecl; external pangolib;
function pango_scan_string(pos:PPAnsiChar; OutStr:PGString):gboolean; cdecl; external pangolib;
function pango_scan_int(pos:PPAnsiChar; OutInt:Plongint):gboolean; cdecl; external pangolib;

{$ifdef PANGO_ENABLE_BACKEND}
function pango_config_key_get(key:PAnsiChar):PAnsiChar; cdecl; external pangolib;
procedure pango_lookup_aliases(fontname:PAnsiChar; families:PPPAnsiChar; n_families:Plongint); cdecl; external pangolib;
{$endif}
{ PANGO_ENABLE_BACKEND  }

{ Functions for parsing textual representations
   of PangoFontDescription fields. They return TRUE if the input string
   contains a valid value, which then has been assigned to the corresponding
   field in the PangoFontDescription. If the warn parameter is TRUE,
   a warning is printed (with g_warning) if the string does not
   contain a valid value.
  }
function pango_parse_style(str:PAnsiChar; style:PPangoStyle; warn:gboolean):gboolean; cdecl; external pangolib;
function pango_parse_variant(str:PAnsiChar; variant:PPangoVariant; warn:gboolean):gboolean; cdecl; external pangolib;
function pango_parse_weight(str:PAnsiChar; weight:PPangoWeight; warn:gboolean):gboolean; cdecl; external pangolib;
function pango_parse_stretch(str:PAnsiChar; stretch:PPangoStretch; warn:gboolean):gboolean; cdecl; external pangolib;
{$ifdef PANGO_ENABLE_BACKEND}

{ On Unix, return the name of the "pango" subdirectory of SYSCONFDIR
   (which is set at compile time). On Win32, return the Pango
   installation directory (which is set at installation time, and
   stored in the registry). The returned string should not be
   g_free'd.
  }
function pango_get_sysconf_subdirectory:PAnsiChar; cdecl; external pangolib;

{ Ditto for LIBDIR/pango. On Win32, use the same Pango
   installation directory. This returned string should not be
   g_free'd either.
  }
function pango_get_lib_subdirectory:PAnsiChar; cdecl; external pangolib;
{$endif}
{ PANGO_ENABLE_BACKEND  }

{ A couple of routines from fribidi that we either wrap or
   provide ourselves.
  }

function pango_log2vis_get_embedding_levels(str:Pgunichar; len:longint; pbase_dir:PPangoDirection; embedding_level_list:Pguint8):gboolean; cdecl; external pangolib;
function pango_unichar_direction(ch : gunichar) : TPangoDirection; cdecl; external pangolib;
function pango_find_base_dir(text : Pgchar; aLength : gint) : TPangoDirection; cdecl; external pangolib;
function pango_get_mirror_char(ch:gunichar; mirrored_ch:Pgunichar):gboolean; cdecl; external pangolib;
function pango_language_get_sample_string(language:PPangoLanguage):PAnsiChar; cdecl; external pangolib;

implementation

end.
