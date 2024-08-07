{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2022 Michael Van Canneyt

    Import unit for libgmp

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$IFNDEF FPC_DOTTEDUNITS}
unit libgmp;
{$ENDIF FPC_DOTTEDUNITS}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{
  Automatically converted by H2Pas 1.0.0 from libgmp.h
  The following command line parameters were used:
    -D
    -C
    -c
    -l
    libgmp
    -o
    libgmp.pp
    -P
    -T
    -u
    libgmp
    -p
    libgmp.h
}

const
  GMPExternal_library = 'libgmp'; {Setup as you need}

{$ifdef linux}
  GMPlibraryFileName = GMPExternal_library+'.so';
{$else}
{$ifdef windows}
  GMPlibraryFileName = GMPExternal_library+'.dll';
{$else}  
{$ifdef darwin}
  // macOS
  GMPlibraryFileName = GMPExternal_library+'.dylib';
{$else}  
  // Generic unix
  GMPlibraryFileName = GMPExternal_library+'.so';
{$endif}
{$endif}
{$endif}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  Tclonglong = clonglong;
  TDouble = double;
  tculong = culong;
  tclong = clong;
  Tcint = cint;
  Ppcchar = ^PAnsiChar;
  Tcslong = cslong;
  Tcuint = cuint;

{
Type
  P_gmp_randstate_struct  = ^T_gmp_randstate_struct;
  P_mpf_struct  = ^T_mpf_struct;
  P_mpq_struct  = ^T_mpq_struct;
  P_mpz_struct  = ^T_mpz_struct;
  Pgmp_randalg_t  = ^Tgmp_randalg_t;
  Pgmp_randstate_struct  = ^Tgmp_randstate_struct;
  Pgmp_randstate_t  = ^Tgmp_randstate_t;
  PGMP_RESULT  = ^TGMP_RESULT;
  Pmax_align_t  = ^Tmax_align_t;
  Pmp_bitcnt_t  = ^Tmp_bitcnt_t;
  Pmp_exp_t  = ^Tmp_exp_t;
  PMP_INT  = ^TMP_INT;
  Pmp_limb_signed_t  = ^Tmp_limb_signed_t;
  Pmp_limb_t  = ^Tmp_limb_t;
  Pmp_ptr  = ^Tmp_ptr;
  PMP_RAT  = ^TMP_RAT;
  Pmp_size_t  = ^Tmp_size_t;
  Pmp_srcptr  = ^Tmp_srcptr;
  Pmpf_ptr  = ^Tmpf_ptr;
  Pmpf_srcptr  = ^Tmpf_srcptr;
  Pmpf_struct  = ^Tmpf_struct;
  Pmpf_t  = ^Tmpf_t;
  Pmpq_ptr  = ^Tmpq_ptr;
  Pmpq_srcptr  = ^Tmpq_srcptr;
  Pmpq_struct  = ^Tmpq_struct;
  Pmpq_t  = ^Tmpq_t;
  Pmpz_ptr  = ^Tmpz_ptr;
  Pmpz_srcptr  = ^Tmpz_srcptr;
  Pmpz_struct  = ^Tmpz_struct;
  Pmpz_t  = ^Tmpz_t;
  Psize_t  = ^Tsize_t;}
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
  Pmax_align_t = ^Tmax_align_t;
  Tmax_align_t = record
      __max_align_ll : Tclonglong;
      __max_align_ld : Tdouble;
    end;

  Pmp_limb_t = ^Tmp_limb_t;
  Tmp_limb_t = Tculong;

  Pmp_limb_signed_t = ^Tmp_limb_signed_t;
  Tmp_limb_signed_t = Tclong;

  Pmp_bitcnt_t = ^Tmp_bitcnt_t;
  Tmp_bitcnt_t = Tculong;

  P_mpz_struct = ^T_mpz_struct;
  T_mpz_struct = record
      _mp_alloc : Tcint;
      _mp_size : Tcint;
      _mp_d : Pmp_limb_t;
    end;
  Tmpz_struct = T_mpz_struct;
  Pmpz_struct= ^Tmpz_struct;

  PMP_INT = ^TMP_INT;
  TMP_INT = T_mpz_struct;

  Pmpz_t = ^Tmpz_t;
  Tmpz_t = array[0..0] of Tmpz_struct;

  Pmp_ptr = ^Tmp_ptr;
  Tmp_ptr = Pmp_limb_t;
(* Const before type ignored *)

  Pmp_srcptr = ^Tmp_srcptr;
  Tmp_srcptr = Pmp_limb_t;

  Pmp_size_t = ^Tmp_size_t;
  Tmp_size_t = Tclong;

  Pmp_exp_t = ^Tmp_exp_t;
  Tmp_exp_t = Tclong;

  P_mpq_struct = ^T_mpq_struct;
  T_mpq_struct = record
      _mp_num : Tmpz_struct;
      _mp_den : Tmpz_struct;
    end;
  Tmpq_struct = T_mpq_struct;
  pmpq_struct = ^tmpq_struct;

  PMP_RAT = ^TMP_RAT;
  TMP_RAT = T_mpq_struct;

  Pmpq_t = ^Tmpq_t;
  Tmpq_t = array[0..0] of T_mpq_struct;

  P_mpf_struct = ^T_mpf_struct;
  T_mpf_struct = record
      _mp_prec : Tcint;
      _mp_size : Tcint;
      _mp_exp : Tmp_exp_t;
      _mp_d : Pmp_limb_t;
    end;
  tmpf_struct = T_mpf_struct;
  pmpf_struct = ^tmpf_struct;

  Pmpf_t = ^Tmpf_t;
  Tmpf_t = array[0..0] of Tmpf_struct;

  Pgmp_randalg_t = ^Tgmp_randalg_t;
  Tgmp_randalg_t = (GMP_RAND_ALG_DEFAULT := 0,GMP_RAND_ALG_LC := GMP_RAND_ALG_DEFAULT
    );

  P_gmp_randstate_struct = ^T_gmp_randstate_struct;
  T_gmp_randstate_struct = record
      _mp_seed : Tmpz_t;
      _mp_alg : Tgmp_randalg_t;
      _mp_algdata : record
          case longint of
            0 : ( _mp_lc : pointer );
          end;
    end;
  Tgmp_randstate_struct = T_gmp_randstate_struct;
  Pgmp_randstate_struct = P_gmp_randstate_struct;

  Pgmp_randstate_t = ^Tgmp_randstate_t;
  Tgmp_randstate_t = array[0..0] of Tgmp_randstate_struct;
(* Const before type ignored *)

  Pmpz_srcptr = ^Tmpz_srcptr;
  Tmpz_srcptr = Pmpz_struct;

  Pmpz_ptr = ^Tmpz_ptr;
  Tmpz_ptr = Pmpz_struct;
(* Const before type ignored *)

  Pmpf_srcptr = ^Tmpf_srcptr;
  Tmpf_srcptr = Pmpf_struct;

  Pmpf_ptr = ^Tmpf_ptr;
  Tmpf_ptr = Pmpf_struct;
(* Const before type ignored *)

  Pmpq_srcptr = ^Tmpq_srcptr;
  Tmpq_srcptr = Pmpq_struct;

  Pmpq_ptr = ^Tmpq_ptr;
  Tmpq_ptr = Pmpq_struct;
  Tsize_t = longint;
  Psize_t = ^TSize_t;

  TSizeCallBack = procedure (para1:Tsize_t); cdecl;
  TReSizeCallBack = procedure (para1:pointer; para2:Tsize_t; para3:Tsize_t); cdecl;
  TReSize2CallBack = procedure (para1:pointer; para2:Tsize_t); cdecl;

var
  mp_set_memory_functions : procedure(para1:TSizeCallBack; para2:TReSizeCallBack; para3:TReSize2CallBack);cdecl;
  mp_get_memory_functions : procedure(para1:TSizeCallBack; para2:TReSizeCallBack; para3:TReSize2CallBack);cdecl;
  mp_bits_per_limb : Tcint;cvar;external;
  mp_errno : Tcint;cvar;external;
  mp_version : pcchar;cvar;external;
  mp_randinit : procedure(para1:Tgmp_randstate_t; para2:Tgmp_randalg_t; args:array of const);cdecl;
  mp_randinit_default : procedure(para1:Tgmp_randstate_t);cdecl;
  mp_randinit_lc_2exp : procedure(para1:Tgmp_randstate_t; para2:Tmpz_srcptr; para3:Tculong; para4:Tmp_bitcnt_t);cdecl;
  mp_randinit_lc_2exp_size : function(para1:Tgmp_randstate_t; para2:Tmp_bitcnt_t):Tcint;cdecl;
  mp_randinit_mt : procedure(para1:Tgmp_randstate_t);cdecl;
  mp_randinit_set : procedure(para1:Tgmp_randstate_t; para2:Pgmp_randstate_struct);cdecl;
  mp_randseed : procedure(para1:Tgmp_randstate_t; para2:Tmpz_srcptr);cdecl;
  mp_randseed_ui : procedure(para1:Tgmp_randstate_t; para2:Tculong);cdecl;
  mp_randclear : procedure(para1:Tgmp_randstate_t);cdecl;
  mp_urandomb_ui : function(para1:Tgmp_randstate_t; para2:Tculong):Tculong;cdecl;
  mp_urandomm_ui : function(para1:Tgmp_randstate_t; para2:Tculong):Tculong;cdecl;
  mp_asprintf : function(para1:Ppcchar; para2:pcchar):Tcint;varargs; cdecl;
  mp_printf : function(para1:pcchar):Tcint;varargs;cdecl;
  mp_snprintf : function(para1:pcchar; para2:Tsize_t; para3:pcchar):Tcint;varargs;cdecl;
  mp_sprintf : function(para1:pcchar; para2:pcchar):Tcint;varargs; cdecl;
  mp_scanf : function(para1:pcchar):Tcint;varargs; cdecl;
  mp_sscanf : function(para1:pcchar; para2:pcchar):Tcint;varargs; cdecl;
  mpz_realloc : function(para1:Tmpz_ptr; para2:Tmp_size_t):pointer;cdecl;
  mpz_abs : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_add : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_add_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_addmul : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_addmul_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_and : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_array_init : procedure(para1:Tmpz_ptr; para2:Tmp_size_t; para3:Tmp_size_t);cdecl;
  mpz_bin_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_bin_uiui : procedure(para1:Tmpz_ptr; para2:Tculong; para3:Tculong);cdecl;
  mpz_cdiv_q : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_cdiv_q_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_cdiv_q_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_cdiv_qr : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tmpz_srcptr);cdecl;
  mpz_cdiv_qr_ui : function(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tculong):Tculong;cdecl;
  mpz_cdiv_r : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_cdiv_r_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_cdiv_r_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_cdiv_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tculong;cdecl;
  mpz_clear : procedure(para1:Tmpz_ptr);cdecl;
  mpz_clears : procedure(para1:Tmpz_ptr);varargs; cdecl;
  mpz_clrbit : procedure(para1:Tmpz_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpz_cmp : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_cmp_d : function(para1:Tmpz_srcptr; para2:Tdouble):Tcint;cdecl;
  mpz_cmp_si : function(para1:Tmpz_srcptr; para2:Tcslong):Tcint;cdecl;
  mpz_cmp_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tcint;cdecl;
  mpz_cmpabs : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_cmpabs_d : function(para1:Tmpz_srcptr; para2:Tdouble):Tcint;cdecl;
  mpz_cmpabs_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tcint;cdecl;
  mpz_com : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_combit : procedure(para1:Tmpz_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpz_congruent_p : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr):Tcint;cdecl;
  mpz_congruent_2exp_p : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t):Tcint;cdecl;
  mpz_congruent_ui_p : function(para1:Tmpz_srcptr; para2:Tculong; para3:Tculong):Tcint;cdecl;
  mpz_divexact : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_divexact_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_divisible_p : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_divisible_ui_p : function(para1:Tmpz_srcptr; para2:Tculong):Tcint;cdecl;
  mpz_divisible_2exp_p : function(para1:Tmpz_srcptr; para2:Tmp_bitcnt_t):Tcint;cdecl;
  mpz_dump : procedure(para1:Tmpz_srcptr);cdecl;
  mpz_export : function(para1:pointer; para2:Psize_t; para3:Tcint; para4:Tsize_t; para5:Tcint; para6:Tsize_t; para7:Tmpz_srcptr):pointer;cdecl;
  mpz_fac_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_2fac_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_mfac_uiui : procedure(para1:Tmpz_ptr; para2:Tculong; para3:Tculong);cdecl;
  mpz_primorial_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_fdiv_q : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_fdiv_q_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_fdiv_q_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_fdiv_qr : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tmpz_srcptr);cdecl;
  mpz_fdiv_qr_ui : function(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tculong):Tculong;cdecl;
  mpz_fdiv_r : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_fdiv_r_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_fdiv_r_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_fdiv_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tculong;cdecl;
  mpz_fib_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_fib2_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tculong);cdecl;
  mpz_fits_sint_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_fits_slong_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_fits_sshort_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_fits_uint_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_fits_ulong_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_fits_ushort_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_gcd : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_gcd_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_gcdext : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_ptr; para4:Tmpz_srcptr; para5:Tmpz_srcptr);cdecl;
  mpz_get_d : function(para1:Tmpz_srcptr):Tdouble;cdecl;
  mpz_get_d_2exp : function(para1:pcslong; para2:Tmpz_srcptr):Tdouble;cdecl;
  mpz_get_si : function(para1:Tmpz_srcptr):Tclong;cdecl;
  mpz_get_str : function(para1:pcchar; para2:Tcint; para3:Tmpz_srcptr):pcchar;cdecl;
  mpz_get_ui : function(para1:Tmpz_srcptr):Tculong;cdecl;
  mpz_getlimbn : function(para1:Tmpz_srcptr; para2:Tmp_size_t):Tmp_limb_t;cdecl;
  mpz_hamdist : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr):Tmp_bitcnt_t;cdecl;
  mpz_import : procedure(para1:Tmpz_ptr; para2:Tsize_t; para3:Tcint; para4:Tsize_t; para5:Tcint; para6:Tsize_t; para7:pointer);cdecl;
  mpz_init : procedure(para1:Tmpz_ptr);cdecl;
  mpz_init2 : procedure(para1:Tmpz_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpz_inits : procedure(para1:Tmpz_ptr);varargs; cdecl;
  mpz_init_set : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_init_set_d : procedure(para1:Tmpz_ptr; para2:Tdouble);cdecl;
  mpz_init_set_si : procedure(para1:Tmpz_ptr; para2:Tcslong);cdecl;
  mpz_init_set_str : function(para1:Tmpz_ptr; para2:pcchar; para3:Tcint):Tcint;cdecl;
  mpz_init_set_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_invert : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr):Tcint;cdecl;
  mpz_ior : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_jacobi : function(para1:Tmpz_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_kronecker_si : function(para1:Tmpz_srcptr; para2:Tclong):Tcint;cdecl;
  mpz_kronecker_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tcint;cdecl;
  mpz_si_kronecker : function(para1:Tclong; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_ui_kronecker : function(para1:Tculong; para2:Tmpz_srcptr):Tcint;cdecl;
  mpz_lcm : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_lcm_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_lucnum_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_lucnum2_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tculong);cdecl;
  mpz_millerrabin : function(para1:Tmpz_srcptr; para2:Tcint):Tcint;cdecl;
  mpz_mod : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_mul : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_mul_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_mul_si : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tclong);cdecl;
  mpz_mul_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_neg : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_nextprime : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_perfect_power_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_perfect_square_p : function(para1:Tmpz_srcptr):Tcint;cdecl;
  mpz_popcount : function(para1:Tmpz_srcptr):Tmp_bitcnt_t;cdecl;
  mpz_pow_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_powm : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr; para4:Tmpz_srcptr);cdecl;
  mpz_powm_sec : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr; para4:Tmpz_srcptr);cdecl;
  mpz_powm_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong; para4:Tmpz_srcptr);cdecl;
  mpz_probab_prime_p : function(para1:Tmpz_srcptr; para2:Tcint):Tcint;cdecl;
  mpz_random : procedure(para1:Tmpz_ptr; para2:Tmp_size_t);cdecl;
  mpz_random2 : procedure(para1:Tmpz_ptr; para2:Tmp_size_t);cdecl;
  mpz_realloc2 : procedure(para1:Tmpz_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpz_remove : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr):Tmp_bitcnt_t;cdecl;
  mpz_root : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tcint;cdecl;
  mpz_rootrem : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tculong);cdecl;
  mpz_rrandomb : procedure(para1:Tmpz_ptr; para2:Tgmp_randstate_t; para3:Tmp_bitcnt_t);cdecl;
  mpz_scan0 : function(para1:Tmpz_srcptr; para2:Tmp_bitcnt_t):Tmp_bitcnt_t;cdecl;
  mpz_scan1 : function(para1:Tmpz_srcptr; para2:Tmp_bitcnt_t):Tmp_bitcnt_t;cdecl;
  mpz_set : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_set_d : procedure(para1:Tmpz_ptr; para2:Tdouble);cdecl;
  mpz_set_f : procedure(para1:Tmpz_ptr; para2:Tmpf_srcptr);cdecl;
  mpz_set_q : procedure(para1:Tmpz_ptr; para2:Tmpq_srcptr);cdecl;
  mpz_set_si : procedure(para1:Tmpz_ptr; para2:Tcslong);cdecl;
  mpz_set_str : function(para1:Tmpz_ptr; para2:pcchar; para3:Tcint):Tcint;cdecl;
  mpz_set_ui : procedure(para1:Tmpz_ptr; para2:Tculong);cdecl;
  mpz_setbit : procedure(para1:Tmpz_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpz_size : function(para1:Tmpz_srcptr):Tsize_t;cdecl;
  mpz_sizeinbase : function(para1:Tmpz_srcptr; para2:Tcint):Tsize_t;cdecl;
  mpz_sqrt : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr);cdecl;
  mpz_sqrtrem : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr);cdecl;
  mpz_sub : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_sub_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_ui_sub : procedure(para1:Tmpz_ptr; para2:Tculong; para3:Tmpz_srcptr);cdecl;
  mpz_submul : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_submul_ui : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong);cdecl;
  mpz_swap : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr);cdecl;
  mpz_tdiv_ui : function(para1:Tmpz_srcptr; para2:Tculong):Tculong;cdecl;
  mpz_tdiv_q : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_tdiv_q_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_tdiv_q_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_tdiv_qr : procedure(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tmpz_srcptr);cdecl;
  mpz_tdiv_qr_ui : function(para1:Tmpz_ptr; para2:Tmpz_ptr; para3:Tmpz_srcptr; para4:Tculong):Tculong;cdecl;
  mpz_tdiv_r : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_tdiv_r_2exp : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpz_tdiv_r_ui : function(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tculong):Tculong;cdecl;
  mpz_tstbit : function(para1:Tmpz_srcptr; para2:Tmp_bitcnt_t):Tcint;cdecl;
  mpz_ui_pow_ui : procedure(para1:Tmpz_ptr; para2:Tculong; para3:Tculong);cdecl;
  mpz_urandomb : procedure(para1:Tmpz_ptr; para2:Tgmp_randstate_t; para3:Tmp_bitcnt_t);cdecl;
  mpz_urandomm : procedure(para1:Tmpz_ptr; para2:Tgmp_randstate_t; para3:Tmpz_srcptr);cdecl;
  mpz_xor : procedure(para1:Tmpz_ptr; para2:Tmpz_srcptr; para3:Tmpz_srcptr);cdecl;
  mpz_limbs_read : function(para1:Tmpz_srcptr):Tmp_srcptr;cdecl;
  mpz_limbs_write : function(para1:Tmpz_ptr; para2:Tmp_size_t):Tmp_ptr;cdecl;
  mpz_limbs_modify : function(para1:Tmpz_ptr; para2:Tmp_size_t):Tmp_ptr;cdecl;
  mpz_limbs_finish : procedure(para1:Tmpz_ptr; para2:Tmp_size_t);cdecl;
  mpz_roinit_n : function(para1:Tmpz_ptr; para2:Tmp_srcptr; para3:Tmp_size_t):Tmpz_srcptr;cdecl;
  mpq_abs : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_add : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmpq_srcptr);cdecl;
  mpq_canonicalize : procedure(para1:Tmpq_ptr);cdecl;
  mpq_clear : procedure(para1:Tmpq_ptr);cdecl;
  mpq_clears : procedure(para1:Tmpq_ptr); varargs;cdecl;
  mpq_cmp : function(para1:Tmpq_srcptr; para2:Tmpq_srcptr):Tcint;cdecl;
  mpq_cmp_si : function(para1:Tmpq_srcptr; para2:Tclong; para3:Tculong):Tcint;cdecl;
  mpq_cmp_ui : function(para1:Tmpq_srcptr; para2:Tculong; para3:Tculong):Tcint;cdecl;
  mpq_cmp_z : function(para1:Tmpq_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpq_div : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmpq_srcptr);cdecl;
  mpq_div_2exp : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpq_equal : function(para1:Tmpq_srcptr; para2:Tmpq_srcptr):Tcint;cdecl;
  mpq_get_num : procedure(para1:Tmpz_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_get_den : procedure(para1:Tmpz_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_get_d : function(para1:Tmpq_srcptr):Tdouble;cdecl;
  mpq_get_str : function(para1:pcchar; para2:Tcint; para3:Tmpq_srcptr):pcchar;cdecl;
  mpq_init : procedure(para1:Tmpq_ptr);cdecl;
  mpq_inits : procedure(para1:Tmpq_ptr);varargs; cdecl;
  mpq_inv : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_mul : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmpq_srcptr);cdecl;
  mpq_mul_2exp : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpq_neg : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_set : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr);cdecl;
  mpq_set_d : procedure(para1:Tmpq_ptr; para2:Tdouble);cdecl;
  mpq_set_den : procedure(para1:Tmpq_ptr; para2:Tmpz_srcptr);cdecl;
  mpq_set_f : procedure(para1:Tmpq_ptr; para2:Tmpf_srcptr);cdecl;
  mpq_set_num : procedure(para1:Tmpq_ptr; para2:Tmpz_srcptr);cdecl;
  mpq_set_si : procedure(para1:Tmpq_ptr; para2:Tcslong; para3:Tculong);cdecl;
  mpq_set_str : function(para1:Tmpq_ptr; para2:pcchar; para3:Tcint):Tcint;cdecl;
  mpq_set_ui : procedure(para1:Tmpq_ptr; para2:Tculong; para3:Tculong);cdecl;
  mpq_set_z : procedure(para1:Tmpq_ptr; para2:Tmpz_srcptr);cdecl;
  mpq_sub : procedure(para1:Tmpq_ptr; para2:Tmpq_srcptr; para3:Tmpq_srcptr);cdecl;
  mpq_swap : procedure(para1:Tmpq_ptr; para2:Tmpq_ptr);cdecl;
  mpf_abs : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_add : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmpf_srcptr);cdecl;
  mpf_add_ui : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tculong);cdecl;
  mpf_ceil : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_clear : procedure(para1:Tmpf_ptr);cdecl;
  mpf_clears : procedure(para1:Tmpf_ptr); varargs; cdecl;
  mpf_cmp : function(para1:Tmpf_srcptr; para2:Tmpf_srcptr):Tcint;cdecl;
  mpf_cmp_z : function(para1:Tmpf_srcptr; para2:Tmpz_srcptr):Tcint;cdecl;
  mpf_cmp_d : function(para1:Tmpf_srcptr; para2:Tdouble):Tcint;cdecl;
  mpf_cmp_si : function(para1:Tmpf_srcptr; para2:Tcslong):Tcint;cdecl;
  mpf_cmp_ui : function(para1:Tmpf_srcptr; para2:Tculong):Tcint;cdecl;
  mpf_div : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmpf_srcptr);cdecl;
  mpf_div_2exp : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpf_div_ui : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tculong);cdecl;
  mpf_dump : procedure(para1:Tmpf_srcptr);cdecl;
  mpf_eq : function(para1:Tmpf_srcptr; para2:Tmpf_srcptr; para3:Tmp_bitcnt_t):Tcint;cdecl;
  mpf_fits_sint_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_fits_slong_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_fits_sshort_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_fits_uint_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_fits_ulong_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_fits_ushort_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_floor : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_get_d : function(para1:Tmpf_srcptr):Tdouble;cdecl;
  mpf_get_d_2exp : function(para1:pcslong; para2:Tmpf_srcptr):Tdouble;cdecl;
  mpf_get_default_prec : function:Tmp_bitcnt_t;cdecl;
  mpf_get_prec : function(para1:Tmpf_srcptr):Tmp_bitcnt_t;cdecl;
  mpf_get_si : function(para1:Tmpf_srcptr):Tclong;cdecl;
  mpf_get_str : function(para1:pcchar; para2:Pmp_exp_t; para3:Tcint; para4:Tsize_t; para5:Tmpf_srcptr):pcchar;cdecl;
  mpf_get_ui : function(para1:Tmpf_srcptr):Tculong;cdecl;
  mpf_init : procedure(para1:Tmpf_ptr);cdecl;
  mpf_init2 : procedure(para1:Tmpf_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpf_inits : procedure(para1:Tmpf_ptr); varargs;cdecl;
  mpf_init_set : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_init_set_d : procedure(para1:Tmpf_ptr; para2:Tdouble);cdecl;
  mpf_init_set_si : procedure(para1:Tmpf_ptr; para2:Tcslong);cdecl;
  mpf_init_set_str : function(para1:Tmpf_ptr; para2:pcchar; para3:Tcint):Tcint;cdecl;
  mpf_init_set_ui : procedure(para1:Tmpf_ptr; para2:Tculong);cdecl;
  mpf_integer_p : function(para1:Tmpf_srcptr):Tcint;cdecl;
  mpf_mul : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmpf_srcptr);cdecl;
  mpf_mul_2exp : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmp_bitcnt_t);cdecl;
  mpf_mul_ui : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tculong);cdecl;
  mpf_neg : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_pow_ui : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tculong);cdecl;
  mpf_random2 : procedure(para1:Tmpf_ptr; para2:Tmp_size_t; para3:Tmp_exp_t);cdecl;
  mpf_reldiff : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmpf_srcptr);cdecl;
  mpf_set : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_set_d : procedure(para1:Tmpf_ptr; para2:Tdouble);cdecl;
  mpf_set_default_prec : procedure(para1:Tmp_bitcnt_t);cdecl;
  mpf_set_prec : procedure(para1:Tmpf_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpf_set_prec_raw : procedure(para1:Tmpf_ptr; para2:Tmp_bitcnt_t);cdecl;
  mpf_set_q : procedure(para1:Tmpf_ptr; para2:Tmpq_srcptr);cdecl;
  mpf_set_si : procedure(para1:Tmpf_ptr; para2:Tcslong);cdecl;
  mpf_set_str : function(para1:Tmpf_ptr; para2:pcchar; para3:Tcint):Tcint;cdecl;
  mpf_set_ui : procedure(para1:Tmpf_ptr; para2:Tculong);cdecl;
  mpf_set_z : procedure(para1:Tmpf_ptr; para2:Tmpz_srcptr);cdecl;
  mpf_size : function(para1:Tmpf_srcptr):Tsize_t;cdecl;
  mpf_sqrt : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_sqrt_ui : procedure(para1:Tmpf_ptr; para2:Tculong);cdecl;
  mpf_sub : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tmpf_srcptr);cdecl;
  mpf_sub_ui : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr; para3:Tculong);cdecl;
  mpf_swap : procedure(para1:Tmpf_ptr; para2:Tmpf_ptr);cdecl;
  mpf_trunc : procedure(para1:Tmpf_ptr; para2:Tmpf_srcptr);cdecl;
  mpf_ui_div : procedure(para1:Tmpf_ptr; para2:Tculong; para3:Tmpf_srcptr);cdecl;
  mpf_ui_sub : procedure(para1:Tmpf_ptr; para2:Tculong; para3:Tmpf_srcptr);cdecl;
  mpf_urandomb : procedure(para1:Tmpf_t; para2:Tgmp_randstate_t; para3:Tmp_bitcnt_t);cdecl;
  mpn_add : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_add_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_add_n : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_addmul_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_cmp : function(para1:Tmp_srcptr; para2:Tmp_srcptr; para3:Tmp_size_t):Tcint;cdecl;
  mpn_zero_p : function(para1:Tmp_srcptr; para2:Tmp_size_t):Tcint;cdecl;
  mpn_divexact_1 : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t);cdecl;
  mpn_divexact_by3c : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_divrem : function(para1:Tmp_ptr; para2:Tmp_size_t; para3:Tmp_ptr; para4:Tmp_size_t; para5:Tmp_srcptr;  para6:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_divrem_1 : function(para1:Tmp_ptr; para2:Tmp_size_t; para3:Tmp_srcptr; para4:Tmp_size_t; para5:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_divrem_2 : function(para1:Tmp_ptr; para2:Tmp_size_t; para3:Tmp_ptr; para4:Tmp_size_t; para5:Tmp_srcptr):Tmp_limb_t;cdecl;
  mpn_div_qr_1 : function(para1:Tmp_ptr; para2:Pmp_limb_t; para3:Tmp_srcptr; para4:Tmp_size_t; para5:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_div_qr_2 : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_srcptr; para4:Tmp_size_t; para5:Tmp_srcptr):Tmp_limb_t;cdecl;
  mpn_gcd : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_size_t; para4:Tmp_ptr; para5:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_gcd_11 : function(para1:Tmp_limb_t; para2:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_gcd_1 : function(para1:Tmp_srcptr; para2:Tmp_size_t; para3:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_gcdext_1 : function(para1:Pmp_limb_signed_t; para2:Pmp_limb_signed_t; para3:Tmp_limb_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_gcdext : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Pmp_size_t; para4:Tmp_ptr; para5:Tmp_size_t;  para6:Tmp_ptr; para7:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_get_str : function(para1:pcuchar; para2:Tcint; para3:Tmp_ptr; para4:Tmp_size_t):Tsize_t;cdecl;
  mpn_hamdist : function(para1:Tmp_srcptr; para2:Tmp_srcptr; para3:Tmp_size_t):Tmp_bitcnt_t;cdecl;
  mpn_lshift : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tcuint):Tmp_limb_t;cdecl;
  mpn_mod_1 : function(para1:Tmp_srcptr; para2:Tmp_size_t; para3:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_mul : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_mul_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_mul_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_sqr : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t);cdecl;
  mpn_neg : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_com : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t);cdecl;
  mpn_perfect_square_p : function(para1:Tmp_srcptr; para2:Tmp_size_t):Tcint;cdecl;
  mpn_perfect_power_p : function(para1:Tmp_srcptr; para2:Tmp_size_t):Tcint;cdecl;
  mpn_popcount : function(para1:Tmp_srcptr; para2:Tmp_size_t):Tmp_bitcnt_t;cdecl;
  mpn_pow_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t; para5:Tmp_ptr):Tmp_size_t;cdecl;
  mpn_preinv_mod_1 : function(para1:Tmp_srcptr; para2:Tmp_size_t; para3:Tmp_limb_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_random : procedure(para1:Tmp_ptr; para2:Tmp_size_t);cdecl;
  mpn_random2 : procedure(para1:Tmp_ptr; para2:Tmp_size_t);cdecl;
  mpn_rshift : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tcuint):Tmp_limb_t;cdecl;
  mpn_scan0 : function(para1:Tmp_srcptr; para2:Tmp_bitcnt_t):Tmp_bitcnt_t;cdecl;
  mpn_scan1 : function(para1:Tmp_srcptr; para2:Tmp_bitcnt_t):Tmp_bitcnt_t;cdecl;
  mpn_set_str : function(para1:Tmp_ptr; para2:pcuchar; para3:Tsize_t; para4:Tcint):Tmp_size_t;cdecl;
  mpn_sizeinbase : function(para1:Tmp_srcptr; para2:Tmp_size_t; para3:Tcint):Tsize_t;cdecl;
  mpn_sqrtrem : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_srcptr; para4:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sub : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_sub_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_sub_n : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_submul_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t):Tmp_limb_t;cdecl;
  mpn_tdiv_qr : procedure(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t;  para6:Tmp_srcptr; para7:Tmp_size_t);cdecl;
  mpn_and_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_andn_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_nand_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_ior_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_iorn_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_nior_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_xor_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_xnor_n : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_srcptr; para4:Tmp_size_t);cdecl;
  mpn_copyi : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t);cdecl;
  mpn_copyd : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t);cdecl;
  mpn_zero : procedure(para1:Tmp_ptr; para2:Tmp_size_t);cdecl;
  mpn_cnd_add_n : function(para1:Tmp_limb_t; para2:Tmp_ptr; para3:Tmp_srcptr; para4:Tmp_srcptr; para5:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_cnd_sub_n : function(para1:Tmp_limb_t; para2:Tmp_ptr; para3:Tmp_srcptr; para4:Tmp_srcptr; para5:Tmp_size_t):Tmp_limb_t;cdecl;
  mpn_sec_add_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t; para5:Tmp_ptr):Tmp_limb_t;cdecl;
  mpn_sec_add_1_itch : function(para1:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_sub_1 : function(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_limb_t; para5:Tmp_ptr):Tmp_limb_t;cdecl;
  mpn_sec_sub_1_itch : function(para1:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_cnd_swap : procedure(para1:Tmp_limb_t; para2:Pmp_limb_t; para3:Pmp_limb_t; para4:Tmp_size_t);cdecl;
  mpn_sec_mul : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t;  para6:Tmp_ptr);cdecl;
  mpn_sec_mul_itch : function(para1:Tmp_size_t; para2:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_sqr : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_ptr);cdecl;
  mpn_sec_sqr_itch : function(para1:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_powm : procedure(para1:Tmp_ptr; para2:Tmp_srcptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_bitcnt_t;  para6:Tmp_srcptr; para7:Tmp_size_t; para8:Tmp_ptr);cdecl;
  mpn_sec_powm_itch : function(para1:Tmp_size_t; para2:Tmp_bitcnt_t; para3:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_tabselect : procedure(para1:Pmp_limb_t; para2:Pmp_limb_t; para3:Tmp_size_t; para4:Tmp_size_t; para5:Tmp_size_t);cdecl;
  mpn_sec_div_qr : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_size_t; para4:Tmp_srcptr; para5:Tmp_size_t; para6:Tmp_ptr):Tmp_limb_t;cdecl;
  mpn_sec_div_qr_itch : function(para1:Tmp_size_t; para2:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_div_r : procedure(para1:Tmp_ptr; para2:Tmp_size_t; para3:Tmp_srcptr; para4:Tmp_size_t; para5:Tmp_ptr);cdecl;
  mpn_sec_div_r_itch : function(para1:Tmp_size_t; para2:Tmp_size_t):Tmp_size_t;cdecl;
  mpn_sec_invert : function(para1:Tmp_ptr; para2:Tmp_ptr; para3:Tmp_srcptr; para4:Tmp_size_t; para5:Tmp_bitcnt_t; para6:Tmp_ptr):Tcint;cdecl;
  mpn_sec_invert_itch : function(para1:Tmp_size_t):Tmp_size_t;cdecl;

Const
    GMP_ERROR_NONE = 0;
    GMP_ERROR_UNSUPPORTED_ARGUMENT = 1;
    GMP_ERROR_DIVISION_BY_ZERO = 2;
    GMP_ERROR_SQRT_OF_NEGATIVE = 4;
    GMP_ERROR_INVALID_ARGUMENT = 8;

procedure Loadlibgmp(const lib : AnsiString);
procedure Loadlibgmp;
procedure Freelibgmp;
function libgmpLoaded: boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.DynLibs;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, dynlibs;
{$ENDIF FPC_DOTTEDUNITS}

var
  hlib : tlibhandle;

function libgmpLoaded: boolean;
begin
  Result:=hLib<>NilHandle;
end;

procedure Freelibgmp;

begin
  FreeLibrary(hlib);
  mp_set_memory_functions:=nil;
  mp_get_memory_functions:=nil;
  mp_randinit:=nil;
  mp_randinit:=nil;
  mp_randinit_default:=nil;
  mp_randinit_lc_2exp:=nil;
  mp_randinit_lc_2exp_size:=nil;
  mp_randinit_mt:=nil;
  mp_randinit_set:=nil;
  mp_randseed:=nil;
  mp_randseed_ui:=nil;
  mp_randclear:=nil;
  mp_urandomb_ui:=nil;
  mp_urandomm_ui:=nil;
  mp_asprintf:=nil;
  mp_asprintf:=nil;
  mp_printf:=nil;
  mp_printf:=nil;
  mp_snprintf:=nil;
  mp_snprintf:=nil;
  mp_sprintf:=nil;
  mp_sprintf:=nil;
  mp_scanf:=nil;
  mp_scanf:=nil;
  mp_sscanf:=nil;
  mp_sscanf:=nil;
  mpz_realloc:=nil;
  mpz_abs:=nil;
  mpz_add:=nil;
  mpz_add_ui:=nil;
  mpz_addmul:=nil;
  mpz_addmul_ui:=nil;
  mpz_and:=nil;
  mpz_array_init:=nil;
  mpz_bin_ui:=nil;
  mpz_bin_uiui:=nil;
  mpz_cdiv_q:=nil;
  mpz_cdiv_q_2exp:=nil;
  mpz_cdiv_q_ui:=nil;
  mpz_cdiv_qr:=nil;
  mpz_cdiv_qr_ui:=nil;
  mpz_cdiv_r:=nil;
  mpz_cdiv_r_2exp:=nil;
  mpz_cdiv_r_ui:=nil;
  mpz_cdiv_ui:=nil;
  mpz_clear:=nil;
  mpz_clears:=nil;
  mpz_clears:=nil;
  mpz_clrbit:=nil;
  mpz_cmp:=nil;
  mpz_cmp_d:=nil;
  mpz_cmp_si:=nil;
  mpz_cmp_ui:=nil;
  mpz_cmpabs:=nil;
  mpz_cmpabs_d:=nil;
  mpz_cmpabs_ui:=nil;
  mpz_com:=nil;
  mpz_combit:=nil;
  mpz_congruent_p:=nil;
  mpz_congruent_2exp_p:=nil;
  mpz_congruent_ui_p:=nil;
  mpz_divexact:=nil;
  mpz_divexact_ui:=nil;
  mpz_divisible_p:=nil;
  mpz_divisible_ui_p:=nil;
  mpz_divisible_2exp_p:=nil;
  mpz_dump:=nil;
  mpz_export:=nil;
  mpz_fac_ui:=nil;
  mpz_2fac_ui:=nil;
  mpz_mfac_uiui:=nil;
  mpz_primorial_ui:=nil;
  mpz_fdiv_q:=nil;
  mpz_fdiv_q_2exp:=nil;
  mpz_fdiv_q_ui:=nil;
  mpz_fdiv_qr:=nil;
  mpz_fdiv_qr_ui:=nil;
  mpz_fdiv_r:=nil;
  mpz_fdiv_r_2exp:=nil;
  mpz_fdiv_r_ui:=nil;
  mpz_fdiv_ui:=nil;
  mpz_fib_ui:=nil;
  mpz_fib2_ui:=nil;
  mpz_fits_sint_p:=nil;
  mpz_fits_slong_p:=nil;
  mpz_fits_sshort_p:=nil;
  mpz_fits_uint_p:=nil;
  mpz_fits_ulong_p:=nil;
  mpz_fits_ushort_p:=nil;
  mpz_gcd:=nil;
  mpz_gcd_ui:=nil;
  mpz_gcdext:=nil;
  mpz_get_d:=nil;
  mpz_get_d_2exp:=nil;
  mpz_get_si:=nil;
  mpz_get_str:=nil;
  mpz_get_ui:=nil;
  mpz_getlimbn:=nil;
  mpz_hamdist:=nil;
  mpz_import:=nil;
  mpz_init:=nil;
  mpz_init2:=nil;
  mpz_inits:=nil;
  mpz_inits:=nil;
  mpz_init_set:=nil;
  mpz_init_set_d:=nil;
  mpz_init_set_si:=nil;
  mpz_init_set_str:=nil;
  mpz_init_set_ui:=nil;
  mpz_invert:=nil;
  mpz_ior:=nil;
  mpz_jacobi:=nil;
  mpz_kronecker_si:=nil;
  mpz_kronecker_ui:=nil;
  mpz_si_kronecker:=nil;
  mpz_ui_kronecker:=nil;
  mpz_lcm:=nil;
  mpz_lcm_ui:=nil;
  mpz_lucnum_ui:=nil;
  mpz_lucnum2_ui:=nil;
  mpz_millerrabin:=nil;
  mpz_mod:=nil;
  mpz_mul:=nil;
  mpz_mul_2exp:=nil;
  mpz_mul_si:=nil;
  mpz_mul_ui:=nil;
  mpz_neg:=nil;
  mpz_nextprime:=nil;
  mpz_perfect_power_p:=nil;
  mpz_perfect_square_p:=nil;
  mpz_popcount:=nil;
  mpz_pow_ui:=nil;
  mpz_powm:=nil;
  mpz_powm_sec:=nil;
  mpz_powm_ui:=nil;
  mpz_probab_prime_p:=nil;
  mpz_random:=nil;
  mpz_random2:=nil;
  mpz_realloc2:=nil;
  mpz_remove:=nil;
  mpz_root:=nil;
  mpz_rootrem:=nil;
  mpz_rrandomb:=nil;
  mpz_scan0:=nil;
  mpz_scan1:=nil;
  mpz_set:=nil;
  mpz_set_d:=nil;
  mpz_set_f:=nil;
  mpz_set_q:=nil;
  mpz_set_si:=nil;
  mpz_set_str:=nil;
  mpz_set_ui:=nil;
  mpz_setbit:=nil;
  mpz_size:=nil;
  mpz_sizeinbase:=nil;
  mpz_sqrt:=nil;
  mpz_sqrtrem:=nil;
  mpz_sub:=nil;
  mpz_sub_ui:=nil;
  mpz_ui_sub:=nil;
  mpz_submul:=nil;
  mpz_submul_ui:=nil;
  mpz_swap:=nil;
  mpz_tdiv_ui:=nil;
  mpz_tdiv_q:=nil;
  mpz_tdiv_q_2exp:=nil;
  mpz_tdiv_q_ui:=nil;
  mpz_tdiv_qr:=nil;
  mpz_tdiv_qr_ui:=nil;
  mpz_tdiv_r:=nil;
  mpz_tdiv_r_2exp:=nil;
  mpz_tdiv_r_ui:=nil;
  mpz_tstbit:=nil;
  mpz_ui_pow_ui:=nil;
  mpz_urandomb:=nil;
  mpz_urandomm:=nil;
  mpz_xor:=nil;
  mpz_limbs_read:=nil;
  mpz_limbs_write:=nil;
  mpz_limbs_modify:=nil;
  mpz_limbs_finish:=nil;
  mpz_roinit_n:=nil;
  mpq_abs:=nil;
  mpq_add:=nil;
  mpq_canonicalize:=nil;
  mpq_clear:=nil;
  mpq_clears:=nil;
  mpq_clears:=nil;
  mpq_cmp:=nil;
  mpq_cmp_si:=nil;
  mpq_cmp_ui:=nil;
  mpq_cmp_z:=nil;
  mpq_div:=nil;
  mpq_div_2exp:=nil;
  mpq_equal:=nil;
  mpq_get_num:=nil;
  mpq_get_den:=nil;
  mpq_get_d:=nil;
  mpq_get_str:=nil;
  mpq_init:=nil;
  mpq_inits:=nil;
  mpq_inits:=nil;
  mpq_inv:=nil;
  mpq_mul:=nil;
  mpq_mul_2exp:=nil;
  mpq_neg:=nil;
  mpq_set:=nil;
  mpq_set_d:=nil;
  mpq_set_den:=nil;
  mpq_set_f:=nil;
  mpq_set_num:=nil;
  mpq_set_si:=nil;
  mpq_set_str:=nil;
  mpq_set_ui:=nil;
  mpq_set_z:=nil;
  mpq_sub:=nil;
  mpq_swap:=nil;
  mpf_abs:=nil;
  mpf_add:=nil;
  mpf_add_ui:=nil;
  mpf_ceil:=nil;
  mpf_clear:=nil;
  mpf_clears:=nil;
  mpf_clears:=nil;
  mpf_cmp:=nil;
  mpf_cmp_z:=nil;
  mpf_cmp_d:=nil;
  mpf_cmp_si:=nil;
  mpf_cmp_ui:=nil;
  mpf_div:=nil;
  mpf_div_2exp:=nil;
  mpf_div_ui:=nil;
  mpf_dump:=nil;
  mpf_eq:=nil;
  mpf_fits_sint_p:=nil;
  mpf_fits_slong_p:=nil;
  mpf_fits_sshort_p:=nil;
  mpf_fits_uint_p:=nil;
  mpf_fits_ulong_p:=nil;
  mpf_fits_ushort_p:=nil;
  mpf_floor:=nil;
  mpf_get_d:=nil;
  mpf_get_d_2exp:=nil;
  mpf_get_default_prec:=nil;
  mpf_get_prec:=nil;
  mpf_get_si:=nil;
  mpf_get_str:=nil;
  mpf_get_ui:=nil;
  mpf_init:=nil;
  mpf_init2:=nil;
  mpf_inits:=nil;
  mpf_inits:=nil;
  mpf_init_set:=nil;
  mpf_init_set_d:=nil;
  mpf_init_set_si:=nil;
  mpf_init_set_str:=nil;
  mpf_init_set_ui:=nil;
  mpf_integer_p:=nil;
  mpf_mul:=nil;
  mpf_mul_2exp:=nil;
  mpf_mul_ui:=nil;
  mpf_neg:=nil;
  mpf_pow_ui:=nil;
  mpf_random2:=nil;
  mpf_reldiff:=nil;
  mpf_set:=nil;
  mpf_set_d:=nil;
  mpf_set_default_prec:=nil;
  mpf_set_prec:=nil;
  mpf_set_prec_raw:=nil;
  mpf_set_q:=nil;
  mpf_set_si:=nil;
  mpf_set_str:=nil;
  mpf_set_ui:=nil;
  mpf_set_z:=nil;
  mpf_size:=nil;
  mpf_sqrt:=nil;
  mpf_sqrt_ui:=nil;
  mpf_sub:=nil;
  mpf_sub_ui:=nil;
  mpf_swap:=nil;
  mpf_trunc:=nil;
  mpf_ui_div:=nil;
  mpf_ui_sub:=nil;
  mpf_urandomb:=nil;
  mpn_add:=nil;
  mpn_add_1:=nil;
  mpn_add_n:=nil;
  mpn_addmul_1:=nil;
  mpn_cmp:=nil;
  mpn_zero_p:=nil;
  mpn_divexact_1:=nil;
  mpn_divexact_by3c:=nil;
  mpn_divrem:=nil;
  mpn_divrem_1:=nil;
  mpn_divrem_2:=nil;
  mpn_div_qr_1:=nil;
  mpn_div_qr_2:=nil;
  mpn_gcd:=nil;
  mpn_gcd_11:=nil;
  mpn_gcd_1:=nil;
  mpn_gcdext_1:=nil;
  mpn_gcdext:=nil;
  mpn_get_str:=nil;
  mpn_hamdist:=nil;
  mpn_lshift:=nil;
  mpn_mod_1:=nil;
  mpn_mul:=nil;
  mpn_mul_1:=nil;
  mpn_mul_n:=nil;
  mpn_sqr:=nil;
  mpn_neg:=nil;
  mpn_com:=nil;
  mpn_perfect_square_p:=nil;
  mpn_perfect_power_p:=nil;
  mpn_popcount:=nil;
  mpn_pow_1:=nil;
  mpn_preinv_mod_1:=nil;
  mpn_random:=nil;
  mpn_random2:=nil;
  mpn_rshift:=nil;
  mpn_scan0:=nil;
  mpn_scan1:=nil;
  mpn_set_str:=nil;
  mpn_sizeinbase:=nil;
  mpn_sqrtrem:=nil;
  mpn_sub:=nil;
  mpn_sub_1:=nil;
  mpn_sub_n:=nil;
  mpn_submul_1:=nil;
  mpn_tdiv_qr:=nil;
  mpn_and_n:=nil;
  mpn_andn_n:=nil;
  mpn_nand_n:=nil;
  mpn_ior_n:=nil;
  mpn_iorn_n:=nil;
  mpn_nior_n:=nil;
  mpn_xor_n:=nil;
  mpn_xnor_n:=nil;
  mpn_copyi:=nil;
  mpn_copyd:=nil;
  mpn_zero:=nil;
  mpn_cnd_add_n:=nil;
  mpn_cnd_sub_n:=nil;
  mpn_sec_add_1:=nil;
  mpn_sec_add_1_itch:=nil;
  mpn_sec_sub_1:=nil;
  mpn_sec_sub_1_itch:=nil;
  mpn_cnd_swap:=nil;
  mpn_sec_mul:=nil;
  mpn_sec_mul_itch:=nil;
  mpn_sec_sqr:=nil;
  mpn_sec_sqr_itch:=nil;
  mpn_sec_powm:=nil;
  mpn_sec_powm_itch:=nil;
  mpn_sec_tabselect:=nil;
  mpn_sec_div_qr:=nil;
  mpn_sec_div_qr_itch:=nil;
  mpn_sec_div_r:=nil;
  mpn_sec_div_r_itch:=nil;
  mpn_sec_invert:=nil;
  mpn_sec_invert_itch:=nil;
end;


procedure Loadlibgmp;

begin
  LoadLibGMP(GMPlibraryFileName);
end;

procedure Loadlibgmp(const lib : AnsiString);

begin
  if libgmpLoaded then
    Freelibgmp;
  hlib:=LoadLibrary(lib);
  if hlib=0 then
    raise Exception.Create(format('Could not load library: %s',[lib]));

  pointer(mp_set_memory_functions):=GetProcAddress(hlib,'__gmp_set_memory_functions');
  pointer(mp_get_memory_functions):=GetProcAddress(hlib,'__gmp_get_memory_functions');
  pointer(mp_randinit):=GetProcAddress(hlib,'__gmp_randinit');
  pointer(mp_randinit):=GetProcAddress(hlib,'__gmp_randinit');
  pointer(mp_randinit_default):=GetProcAddress(hlib,'__gmp_randinit_default');
  pointer(mp_randinit_lc_2exp):=GetProcAddress(hlib,'__gmp_randinit_lc_2exp');
  pointer(mp_randinit_lc_2exp_size):=GetProcAddress(hlib,'__gmp_randinit_lc_2exp_size');
  pointer(mp_randinit_mt):=GetProcAddress(hlib,'__gmp_randinit_mt');
  pointer(mp_randinit_set):=GetProcAddress(hlib,'__gmp_randinit_set');
  pointer(mp_randseed):=GetProcAddress(hlib,'__gmp_randseed');
  pointer(mp_randseed_ui):=GetProcAddress(hlib,'__gmp_randseed_ui');
  pointer(mp_randclear):=GetProcAddress(hlib,'__gmp_randclear');
  pointer(mp_urandomb_ui):=GetProcAddress(hlib,'__gmp_urandomb_ui');
  pointer(mp_urandomm_ui):=GetProcAddress(hlib,'__gmp_urandomm_ui');
  pointer(mp_asprintf):=GetProcAddress(hlib,'__gmp_asprintf');
  pointer(mp_asprintf):=GetProcAddress(hlib,'__gmp_asprintf');
  pointer(mp_printf):=GetProcAddress(hlib,'__gmp_printf');
  pointer(mp_printf):=GetProcAddress(hlib,'__gmp_printf');
  pointer(mp_snprintf):=GetProcAddress(hlib,'__gmp_snprintf');
  pointer(mp_snprintf):=GetProcAddress(hlib,'__gmp_snprintf');
  pointer(mp_sprintf):=GetProcAddress(hlib,'__gmp_sprintf');
  pointer(mp_sprintf):=GetProcAddress(hlib,'__gmp_sprintf');
  pointer(mp_scanf):=GetProcAddress(hlib,'__gmp_scanf');
  pointer(mp_scanf):=GetProcAddress(hlib,'__gmp_scanf');
  pointer(mp_sscanf):=GetProcAddress(hlib,'__gmp_sscanf');
  pointer(mp_sscanf):=GetProcAddress(hlib,'__gmp_sscanf');
  pointer(mpz_realloc):=GetProcAddress(hlib,'__gmpz_realloc');
  pointer(mpz_abs):=GetProcAddress(hlib,'__gmpz_abs');
  pointer(mpz_add):=GetProcAddress(hlib,'__gmpz_add');
  pointer(mpz_add_ui):=GetProcAddress(hlib,'__gmpz_add_ui');
  pointer(mpz_addmul):=GetProcAddress(hlib,'__gmpz_addmul');
  pointer(mpz_addmul_ui):=GetProcAddress(hlib,'__gmpz_addmul_ui');
  pointer(mpz_and):=GetProcAddress(hlib,'__gmpz_and');
  pointer(mpz_array_init):=GetProcAddress(hlib,'__gmpz_array_init');
  pointer(mpz_bin_ui):=GetProcAddress(hlib,'__gmpz_bin_ui');
  pointer(mpz_bin_uiui):=GetProcAddress(hlib,'__gmpz_bin_uiui');
  pointer(mpz_cdiv_q):=GetProcAddress(hlib,'__gmpz_cdiv_q');
  pointer(mpz_cdiv_q_2exp):=GetProcAddress(hlib,'__gmpz_cdiv_q_2exp');
  pointer(mpz_cdiv_q_ui):=GetProcAddress(hlib,'__gmpz_cdiv_q_ui');
  pointer(mpz_cdiv_qr):=GetProcAddress(hlib,'__gmpz_cdiv_qr');
  pointer(mpz_cdiv_qr_ui):=GetProcAddress(hlib,'__gmpz_cdiv_qr_ui');
  pointer(mpz_cdiv_r):=GetProcAddress(hlib,'__gmpz_cdiv_r');
  pointer(mpz_cdiv_r_2exp):=GetProcAddress(hlib,'__gmpz_cdiv_r_2exp');
  pointer(mpz_cdiv_r_ui):=GetProcAddress(hlib,'__gmpz_cdiv_r_ui');
  pointer(mpz_cdiv_ui):=GetProcAddress(hlib,'__gmpz_cdiv_ui');
  pointer(mpz_clear):=GetProcAddress(hlib,'__gmpz_clear');
  pointer(mpz_clears):=GetProcAddress(hlib,'__gmpz_clears');
  pointer(mpz_clears):=GetProcAddress(hlib,'__gmpz_clears');
  pointer(mpz_clrbit):=GetProcAddress(hlib,'__gmpz_clrbit');
  pointer(mpz_cmp):=GetProcAddress(hlib,'__gmpz_cmp');
  pointer(mpz_cmp_d):=GetProcAddress(hlib,'__gmpz_cmp_d');
  pointer(mpz_cmp_si):=GetProcAddress(hlib,'__gmpz_cmp_si');
  pointer(mpz_cmp_ui):=GetProcAddress(hlib,'__gmpz_cmp_ui');
  pointer(mpz_cmpabs):=GetProcAddress(hlib,'__gmpz_cmpabs');
  pointer(mpz_cmpabs_d):=GetProcAddress(hlib,'__gmpz_cmpabs_d');
  pointer(mpz_cmpabs_ui):=GetProcAddress(hlib,'__gmpz_cmpabs_ui');
  pointer(mpz_com):=GetProcAddress(hlib,'__gmpz_com');
  pointer(mpz_combit):=GetProcAddress(hlib,'__gmpz_combit');
  pointer(mpz_congruent_p):=GetProcAddress(hlib,'__gmpz_congruent_p');
  pointer(mpz_congruent_2exp_p):=GetProcAddress(hlib,'__gmpz_congruent_2exp_p');
  pointer(mpz_congruent_ui_p):=GetProcAddress(hlib,'__gmpz_congruent_ui_p');
  pointer(mpz_divexact):=GetProcAddress(hlib,'__gmpz_divexact');
  pointer(mpz_divexact_ui):=GetProcAddress(hlib,'__gmpz_divexact_ui');
  pointer(mpz_divisible_p):=GetProcAddress(hlib,'__gmpz_divisible_p');
  pointer(mpz_divisible_ui_p):=GetProcAddress(hlib,'__gmpz_divisible_ui_p');
  pointer(mpz_divisible_2exp_p):=GetProcAddress(hlib,'__gmpz_divisible_2exp_p');
  pointer(mpz_dump):=GetProcAddress(hlib,'__gmpz_dump');
  pointer(mpz_export):=GetProcAddress(hlib,'__gmpz_export');
  pointer(mpz_fac_ui):=GetProcAddress(hlib,'__gmpz_fac_ui');
  pointer(mpz_2fac_ui):=GetProcAddress(hlib,'__gmpz_2fac_ui');
  pointer(mpz_mfac_uiui):=GetProcAddress(hlib,'__gmpz_mfac_uiui');
  pointer(mpz_primorial_ui):=GetProcAddress(hlib,'__gmpz_primorial_ui');
  pointer(mpz_fdiv_q):=GetProcAddress(hlib,'__gmpz_fdiv_q');
  pointer(mpz_fdiv_q_2exp):=GetProcAddress(hlib,'__gmpz_fdiv_q_2exp');
  pointer(mpz_fdiv_q_ui):=GetProcAddress(hlib,'__gmpz_fdiv_q_ui');
  pointer(mpz_fdiv_qr):=GetProcAddress(hlib,'__gmpz_fdiv_qr');
  pointer(mpz_fdiv_qr_ui):=GetProcAddress(hlib,'__gmpz_fdiv_qr_ui');
  pointer(mpz_fdiv_r):=GetProcAddress(hlib,'__gmpz_fdiv_r');
  pointer(mpz_fdiv_r_2exp):=GetProcAddress(hlib,'__gmpz_fdiv_r_2exp');
  pointer(mpz_fdiv_r_ui):=GetProcAddress(hlib,'__gmpz_fdiv_r_ui');
  pointer(mpz_fdiv_ui):=GetProcAddress(hlib,'__gmpz_fdiv_ui');
  pointer(mpz_fib_ui):=GetProcAddress(hlib,'__gmpz_fib_ui');
  pointer(mpz_fib2_ui):=GetProcAddress(hlib,'__gmpz_fib2_ui');
  pointer(mpz_fits_sint_p):=GetProcAddress(hlib,'__gmpz_fits_sint_p');
  pointer(mpz_fits_slong_p):=GetProcAddress(hlib,'__gmpz_fits_slong_p');
  pointer(mpz_fits_sshort_p):=GetProcAddress(hlib,'__gmpz_fits_sshort_p');
  pointer(mpz_fits_uint_p):=GetProcAddress(hlib,'__gmpz_fits_uint_p');
  pointer(mpz_fits_ulong_p):=GetProcAddress(hlib,'__gmpz_fits_ulong_p');
  pointer(mpz_fits_ushort_p):=GetProcAddress(hlib,'__gmpz_fits_ushort_p');
  pointer(mpz_gcd):=GetProcAddress(hlib,'__gmpz_gcd');
  pointer(mpz_gcd_ui):=GetProcAddress(hlib,'__gmpz_gcd_ui');
  pointer(mpz_gcdext):=GetProcAddress(hlib,'__gmpz_gcdext');
  pointer(mpz_get_d):=GetProcAddress(hlib,'__gmpz_get_d');
  pointer(mpz_get_d_2exp):=GetProcAddress(hlib,'__gmpz_get_d_2exp');
  pointer(mpz_get_si):=GetProcAddress(hlib,'__gmpz_get_si');
  pointer(mpz_get_str):=GetProcAddress(hlib,'__gmpz_get_str');
  pointer(mpz_get_ui):=GetProcAddress(hlib,'__gmpz_get_ui');
  pointer(mpz_getlimbn):=GetProcAddress(hlib,'__gmpz_getlimbn');
  pointer(mpz_hamdist):=GetProcAddress(hlib,'__gmpz_hamdist');
  pointer(mpz_import):=GetProcAddress(hlib,'__gmpz_import');
  pointer(mpz_init):=GetProcAddress(hlib,'__gmpz_init');
  pointer(mpz_init2):=GetProcAddress(hlib,'__gmpz_init2');
  pointer(mpz_inits):=GetProcAddress(hlib,'__gmpz_inits');
  pointer(mpz_inits):=GetProcAddress(hlib,'__gmpz_inits');
  pointer(mpz_init_set):=GetProcAddress(hlib,'__gmpz_init_set');
  pointer(mpz_init_set_d):=GetProcAddress(hlib,'__gmpz_init_set_d');
  pointer(mpz_init_set_si):=GetProcAddress(hlib,'__gmpz_init_set_si');
  pointer(mpz_init_set_str):=GetProcAddress(hlib,'__gmpz_init_set_str');
  pointer(mpz_init_set_ui):=GetProcAddress(hlib,'__gmpz_init_set_ui');
  pointer(mpz_invert):=GetProcAddress(hlib,'__gmpz_invert');
  pointer(mpz_ior):=GetProcAddress(hlib,'__gmpz_ior');
  pointer(mpz_jacobi):=GetProcAddress(hlib,'__gmpz_jacobi');
  pointer(mpz_kronecker_si):=GetProcAddress(hlib,'__gmpz_kronecker_si');
  pointer(mpz_kronecker_ui):=GetProcAddress(hlib,'__gmpz_kronecker_ui');
  pointer(mpz_si_kronecker):=GetProcAddress(hlib,'__gmpz_si_kronecker');
  pointer(mpz_ui_kronecker):=GetProcAddress(hlib,'__gmpz_ui_kronecker');
  pointer(mpz_lcm):=GetProcAddress(hlib,'__gmpz_lcm');
  pointer(mpz_lcm_ui):=GetProcAddress(hlib,'__gmpz_lcm_ui');
  pointer(mpz_lucnum_ui):=GetProcAddress(hlib,'__gmpz_lucnum_ui');
  pointer(mpz_lucnum2_ui):=GetProcAddress(hlib,'__gmpz_lucnum2_ui');
  pointer(mpz_millerrabin):=GetProcAddress(hlib,'__gmpz_millerrabin');
  pointer(mpz_mod):=GetProcAddress(hlib,'__gmpz_mod');
  pointer(mpz_mul):=GetProcAddress(hlib,'__gmpz_mul');
  pointer(mpz_mul_2exp):=GetProcAddress(hlib,'__gmpz_mul_2exp');
  pointer(mpz_mul_si):=GetProcAddress(hlib,'__gmpz_mul_si');
  pointer(mpz_mul_ui):=GetProcAddress(hlib,'__gmpz_mul_ui');
  pointer(mpz_neg):=GetProcAddress(hlib,'__gmpz_neg');
  pointer(mpz_nextprime):=GetProcAddress(hlib,'__gmpz_nextprime');
  pointer(mpz_perfect_power_p):=GetProcAddress(hlib,'__gmpz_perfect_power_p');
  pointer(mpz_perfect_square_p):=GetProcAddress(hlib,'__gmpz_perfect_square_p');
  pointer(mpz_popcount):=GetProcAddress(hlib,'__gmpz_popcount');
  pointer(mpz_pow_ui):=GetProcAddress(hlib,'__gmpz_pow_ui');
  pointer(mpz_powm):=GetProcAddress(hlib,'__gmpz_powm');
  pointer(mpz_powm_sec):=GetProcAddress(hlib,'__gmpz_powm_sec');
  pointer(mpz_powm_ui):=GetProcAddress(hlib,'__gmpz_powm_ui');
  pointer(mpz_probab_prime_p):=GetProcAddress(hlib,'__gmpz_probab_prime_p');
  pointer(mpz_random):=GetProcAddress(hlib,'__gmpz_random');
  pointer(mpz_random2):=GetProcAddress(hlib,'__gmpz_random2');
  pointer(mpz_realloc2):=GetProcAddress(hlib,'__gmpz_realloc2');
  pointer(mpz_remove):=GetProcAddress(hlib,'__gmpz_remove');
  pointer(mpz_root):=GetProcAddress(hlib,'__gmpz_root');
  pointer(mpz_rootrem):=GetProcAddress(hlib,'__gmpz_rootrem');
  pointer(mpz_rrandomb):=GetProcAddress(hlib,'__gmpz_rrandomb');
  pointer(mpz_scan0):=GetProcAddress(hlib,'__gmpz_scan0');
  pointer(mpz_scan1):=GetProcAddress(hlib,'__gmpz_scan1');
  pointer(mpz_set):=GetProcAddress(hlib,'__gmpz_set');
  pointer(mpz_set_d):=GetProcAddress(hlib,'__gmpz_set_d');
  pointer(mpz_set_f):=GetProcAddress(hlib,'__gmpz_set_f');
  pointer(mpz_set_q):=GetProcAddress(hlib,'__gmpz_set_q');
  pointer(mpz_set_si):=GetProcAddress(hlib,'__gmpz_set_si');
  pointer(mpz_set_str):=GetProcAddress(hlib,'__gmpz_set_str');
  pointer(mpz_set_ui):=GetProcAddress(hlib,'__gmpz_set_ui');
  pointer(mpz_setbit):=GetProcAddress(hlib,'__gmpz_setbit');
  pointer(mpz_size):=GetProcAddress(hlib,'__gmpz_size');
  pointer(mpz_sizeinbase):=GetProcAddress(hlib,'__gmpz_sizeinbase');
  pointer(mpz_sqrt):=GetProcAddress(hlib,'__gmpz_sqrt');
  pointer(mpz_sqrtrem):=GetProcAddress(hlib,'__gmpz_sqrtrem');
  pointer(mpz_sub):=GetProcAddress(hlib,'__gmpz_sub');
  pointer(mpz_sub_ui):=GetProcAddress(hlib,'__gmpz_sub_ui');
  pointer(mpz_ui_sub):=GetProcAddress(hlib,'__gmpz_ui_sub');
  pointer(mpz_submul):=GetProcAddress(hlib,'__gmpz_submul');
  pointer(mpz_submul_ui):=GetProcAddress(hlib,'__gmpz_submul_ui');
  pointer(mpz_swap):=GetProcAddress(hlib,'__gmpz_swap');
  pointer(mpz_tdiv_ui):=GetProcAddress(hlib,'__gmpz_tdiv_ui');
  pointer(mpz_tdiv_q):=GetProcAddress(hlib,'__gmpz_tdiv_q');
  pointer(mpz_tdiv_q_2exp):=GetProcAddress(hlib,'__gmpz_tdiv_q_2exp');
  pointer(mpz_tdiv_q_ui):=GetProcAddress(hlib,'__gmpz_tdiv_q_ui');
  pointer(mpz_tdiv_qr):=GetProcAddress(hlib,'__gmpz_tdiv_qr');
  pointer(mpz_tdiv_qr_ui):=GetProcAddress(hlib,'__gmpz_tdiv_qr_ui');
  pointer(mpz_tdiv_r):=GetProcAddress(hlib,'__gmpz_tdiv_r');
  pointer(mpz_tdiv_r_2exp):=GetProcAddress(hlib,'__gmpz_tdiv_r_2exp');
  pointer(mpz_tdiv_r_ui):=GetProcAddress(hlib,'__gmpz_tdiv_r_ui');
  pointer(mpz_tstbit):=GetProcAddress(hlib,'__gmpz_tstbit');
  pointer(mpz_ui_pow_ui):=GetProcAddress(hlib,'__gmpz_ui_pow_ui');
  pointer(mpz_urandomb):=GetProcAddress(hlib,'__gmpz_urandomb');
  pointer(mpz_urandomm):=GetProcAddress(hlib,'__gmpz_urandomm');
  pointer(mpz_xor):=GetProcAddress(hlib,'__gmpz_xor');
  pointer(mpz_limbs_read):=GetProcAddress(hlib,'__gmpz_limbs_read');
  pointer(mpz_limbs_write):=GetProcAddress(hlib,'__gmpz_limbs_write');
  pointer(mpz_limbs_modify):=GetProcAddress(hlib,'__gmpz_limbs_modify');
  pointer(mpz_limbs_finish):=GetProcAddress(hlib,'__gmpz_limbs_finish');
  pointer(mpz_roinit_n):=GetProcAddress(hlib,'__gmpz_roinit_n');
  pointer(mpq_abs):=GetProcAddress(hlib,'__gmpq_abs');
  pointer(mpq_add):=GetProcAddress(hlib,'__gmpq_add');
  pointer(mpq_canonicalize):=GetProcAddress(hlib,'__gmpq_canonicalize');
  pointer(mpq_clear):=GetProcAddress(hlib,'__gmpq_clear');
  pointer(mpq_clears):=GetProcAddress(hlib,'__gmpq_clears');
  pointer(mpq_clears):=GetProcAddress(hlib,'__gmpq_clears');
  pointer(mpq_cmp):=GetProcAddress(hlib,'__gmpq_cmp');
  pointer(mpq_cmp_si):=GetProcAddress(hlib,'__gmpq_cmp_si');
  pointer(mpq_cmp_ui):=GetProcAddress(hlib,'__gmpq_cmp_ui');
  pointer(mpq_cmp_z):=GetProcAddress(hlib,'__gmpq_cmp_z');
  pointer(mpq_div):=GetProcAddress(hlib,'__gmpq_div');
  pointer(mpq_div_2exp):=GetProcAddress(hlib,'__gmpq_div_2exp');
  pointer(mpq_equal):=GetProcAddress(hlib,'__gmpq_equal');
  pointer(mpq_get_num):=GetProcAddress(hlib,'__gmpq_get_num');
  pointer(mpq_get_den):=GetProcAddress(hlib,'__gmpq_get_den');
  pointer(mpq_get_d):=GetProcAddress(hlib,'__gmpq_get_d');
  pointer(mpq_get_str):=GetProcAddress(hlib,'__gmpq_get_str');
  pointer(mpq_init):=GetProcAddress(hlib,'__gmpq_init');
  pointer(mpq_inits):=GetProcAddress(hlib,'__gmpq_inits');
  pointer(mpq_inits):=GetProcAddress(hlib,'__gmpq_inits');
  pointer(mpq_inv):=GetProcAddress(hlib,'__gmpq_inv');
  pointer(mpq_mul):=GetProcAddress(hlib,'__gmpq_mul');
  pointer(mpq_mul_2exp):=GetProcAddress(hlib,'__gmpq_mul_2exp');
  pointer(mpq_neg):=GetProcAddress(hlib,'__gmpq_neg');
  pointer(mpq_set):=GetProcAddress(hlib,'__gmpq_set');
  pointer(mpq_set_d):=GetProcAddress(hlib,'__gmpq_set_d');
  pointer(mpq_set_den):=GetProcAddress(hlib,'__gmpq_set_den');
  pointer(mpq_set_f):=GetProcAddress(hlib,'__gmpq_set_f');
  pointer(mpq_set_num):=GetProcAddress(hlib,'__gmpq_set_num');
  pointer(mpq_set_si):=GetProcAddress(hlib,'__gmpq_set_si');
  pointer(mpq_set_str):=GetProcAddress(hlib,'__gmpq_set_str');
  pointer(mpq_set_ui):=GetProcAddress(hlib,'__gmpq_set_ui');
  pointer(mpq_set_z):=GetProcAddress(hlib,'__gmpq_set_z');
  pointer(mpq_sub):=GetProcAddress(hlib,'__gmpq_sub');
  pointer(mpq_swap):=GetProcAddress(hlib,'__gmpq_swap');
  pointer(mpf_abs):=GetProcAddress(hlib,'__gmpf_abs');
  pointer(mpf_add):=GetProcAddress(hlib,'__gmpf_add');
  pointer(mpf_add_ui):=GetProcAddress(hlib,'__gmpf_add_ui');
  pointer(mpf_ceil):=GetProcAddress(hlib,'__gmpf_ceil');
  pointer(mpf_clear):=GetProcAddress(hlib,'__gmpf_clear');
  pointer(mpf_clears):=GetProcAddress(hlib,'__gmpf_clears');
  pointer(mpf_clears):=GetProcAddress(hlib,'__gmpf_clears');
  pointer(mpf_cmp):=GetProcAddress(hlib,'__gmpf_cmp');
  pointer(mpf_cmp_z):=GetProcAddress(hlib,'__gmpf_cmp_z');
  pointer(mpf_cmp_d):=GetProcAddress(hlib,'__gmpf_cmp_d');
  pointer(mpf_cmp_si):=GetProcAddress(hlib,'__gmpf_cmp_si');
  pointer(mpf_cmp_ui):=GetProcAddress(hlib,'__gmpf_cmp_ui');
  pointer(mpf_div):=GetProcAddress(hlib,'__gmpf_div');
  pointer(mpf_div_2exp):=GetProcAddress(hlib,'__gmpf_div_2exp');
  pointer(mpf_div_ui):=GetProcAddress(hlib,'__gmpf_div_ui');
  pointer(mpf_dump):=GetProcAddress(hlib,'__gmpf_dump');
  pointer(mpf_eq):=GetProcAddress(hlib,'__gmpf_eq');
  pointer(mpf_fits_sint_p):=GetProcAddress(hlib,'__gmpf_fits_sint_p');
  pointer(mpf_fits_slong_p):=GetProcAddress(hlib,'__gmpf_fits_slong_p');
  pointer(mpf_fits_sshort_p):=GetProcAddress(hlib,'__gmpf_fits_sshort_p');
  pointer(mpf_fits_uint_p):=GetProcAddress(hlib,'__gmpf_fits_uint_p');
  pointer(mpf_fits_ulong_p):=GetProcAddress(hlib,'__gmpf_fits_ulong_p');
  pointer(mpf_fits_ushort_p):=GetProcAddress(hlib,'__gmpf_fits_ushort_p');
  pointer(mpf_floor):=GetProcAddress(hlib,'__gmpf_floor');
  pointer(mpf_get_d):=GetProcAddress(hlib,'__gmpf_get_d');
  pointer(mpf_get_d_2exp):=GetProcAddress(hlib,'__gmpf_get_d_2exp');
  pointer(mpf_get_default_prec):=GetProcAddress(hlib,'__gmpf_get_default_prec');
  pointer(mpf_get_prec):=GetProcAddress(hlib,'__gmpf_get_prec');
  pointer(mpf_get_si):=GetProcAddress(hlib,'__gmpf_get_si');
  pointer(mpf_get_str):=GetProcAddress(hlib,'__gmpf_get_str');
  pointer(mpf_get_ui):=GetProcAddress(hlib,'__gmpf_get_ui');
  pointer(mpf_init):=GetProcAddress(hlib,'__gmpf_init');
  pointer(mpf_init2):=GetProcAddress(hlib,'__gmpf_init2');
  pointer(mpf_inits):=GetProcAddress(hlib,'__gmpf_inits');
  pointer(mpf_inits):=GetProcAddress(hlib,'__gmpf_inits');
  pointer(mpf_init_set):=GetProcAddress(hlib,'__gmpf_init_set');
  pointer(mpf_init_set_d):=GetProcAddress(hlib,'__gmpf_init_set_d');
  pointer(mpf_init_set_si):=GetProcAddress(hlib,'__gmpf_init_set_si');
  pointer(mpf_init_set_str):=GetProcAddress(hlib,'__gmpf_init_set_str');
  pointer(mpf_init_set_ui):=GetProcAddress(hlib,'__gmpf_init_set_ui');
  pointer(mpf_integer_p):=GetProcAddress(hlib,'__gmpf_integer_p');
  pointer(mpf_mul):=GetProcAddress(hlib,'__gmpf_mul');
  pointer(mpf_mul_2exp):=GetProcAddress(hlib,'__gmpf_mul_2exp');
  pointer(mpf_mul_ui):=GetProcAddress(hlib,'__gmpf_mul_ui');
  pointer(mpf_neg):=GetProcAddress(hlib,'__gmpf_neg');
  pointer(mpf_pow_ui):=GetProcAddress(hlib,'__gmpf_pow_ui');
  pointer(mpf_random2):=GetProcAddress(hlib,'__gmpf_random2');
  pointer(mpf_reldiff):=GetProcAddress(hlib,'__gmpf_reldiff');
  pointer(mpf_set):=GetProcAddress(hlib,'__gmpf_set');
  pointer(mpf_set_d):=GetProcAddress(hlib,'__gmpf_set_d');
  pointer(mpf_set_default_prec):=GetProcAddress(hlib,'__gmpf_set_default_prec');
  pointer(mpf_set_prec):=GetProcAddress(hlib,'__gmpf_set_prec');
  pointer(mpf_set_prec_raw):=GetProcAddress(hlib,'__gmpf_set_prec_raw');
  pointer(mpf_set_q):=GetProcAddress(hlib,'__gmpf_set_q');
  pointer(mpf_set_si):=GetProcAddress(hlib,'__gmpf_set_si');
  pointer(mpf_set_str):=GetProcAddress(hlib,'__gmpf_set_str');
  pointer(mpf_set_ui):=GetProcAddress(hlib,'__gmpf_set_ui');
  pointer(mpf_set_z):=GetProcAddress(hlib,'__gmpf_set_z');
  pointer(mpf_size):=GetProcAddress(hlib,'__gmpf_size');
  pointer(mpf_sqrt):=GetProcAddress(hlib,'__gmpf_sqrt');
  pointer(mpf_sqrt_ui):=GetProcAddress(hlib,'__gmpf_sqrt_ui');
  pointer(mpf_sub):=GetProcAddress(hlib,'__gmpf_sub');
  pointer(mpf_sub_ui):=GetProcAddress(hlib,'__gmpf_sub_ui');
  pointer(mpf_swap):=GetProcAddress(hlib,'__gmpf_swap');
  pointer(mpf_trunc):=GetProcAddress(hlib,'__gmpf_trunc');
  pointer(mpf_ui_div):=GetProcAddress(hlib,'__gmpf_ui_div');
  pointer(mpf_ui_sub):=GetProcAddress(hlib,'__gmpf_ui_sub');
  pointer(mpf_urandomb):=GetProcAddress(hlib,'__gmpf_urandomb');
  pointer(mpn_add):=GetProcAddress(hlib,'__gmpn_add');
  pointer(mpn_add_1):=GetProcAddress(hlib,'__gmpn_add_1');
  pointer(mpn_add_n):=GetProcAddress(hlib,'__gmpn_add_n');
  pointer(mpn_addmul_1):=GetProcAddress(hlib,'__gmpn_addmul_1');
  pointer(mpn_cmp):=GetProcAddress(hlib,'__gmpn_cmp');
  pointer(mpn_zero_p):=GetProcAddress(hlib,'__gmpn_zero_p');
  pointer(mpn_divexact_1):=GetProcAddress(hlib,'__gmpn_divexact_1');
  pointer(mpn_divexact_by3c):=GetProcAddress(hlib,'__gmpn_divexact_by3c');
  pointer(mpn_divrem):=GetProcAddress(hlib,'__gmpn_divrem');
  pointer(mpn_divrem_1):=GetProcAddress(hlib,'__gmpn_divrem_1');
  pointer(mpn_divrem_2):=GetProcAddress(hlib,'__gmpn_divrem_2');
  pointer(mpn_div_qr_1):=GetProcAddress(hlib,'__gmpn_div_qr_1');
  pointer(mpn_div_qr_2):=GetProcAddress(hlib,'__gmpn_div_qr_2');
  pointer(mpn_gcd):=GetProcAddress(hlib,'__gmpn_gcd');
  pointer(mpn_gcd_11):=GetProcAddress(hlib,'__gmpn_gcd_11');
  pointer(mpn_gcd_1):=GetProcAddress(hlib,'__gmpn_gcd_1');
  pointer(mpn_gcdext_1):=GetProcAddress(hlib,'__gmpn_gcdext_1');
  pointer(mpn_gcdext):=GetProcAddress(hlib,'__gmpn_gcdext');
  pointer(mpn_get_str):=GetProcAddress(hlib,'__gmpn_get_str');
  pointer(mpn_hamdist):=GetProcAddress(hlib,'__gmpn_hamdist');
  pointer(mpn_lshift):=GetProcAddress(hlib,'__gmpn_lshift');
  pointer(mpn_mod_1):=GetProcAddress(hlib,'__gmpn_mod_1');
  pointer(mpn_mul):=GetProcAddress(hlib,'__gmpn_mul');
  pointer(mpn_mul_1):=GetProcAddress(hlib,'__gmpn_mul_1');
  pointer(mpn_mul_n):=GetProcAddress(hlib,'__gmpn_mul_n');
  pointer(mpn_sqr):=GetProcAddress(hlib,'__gmpn_sqr');
  pointer(mpn_neg):=GetProcAddress(hlib,'__gmpn_neg');
  pointer(mpn_com):=GetProcAddress(hlib,'__gmpn_com');
  pointer(mpn_perfect_square_p):=GetProcAddress(hlib,'__gmpn_perfect_square_p');
  pointer(mpn_perfect_power_p):=GetProcAddress(hlib,'__gmpn_perfect_power_p');
  pointer(mpn_popcount):=GetProcAddress(hlib,'__gmpn_popcount');
  pointer(mpn_pow_1):=GetProcAddress(hlib,'__gmpn_pow_1');
  pointer(mpn_preinv_mod_1):=GetProcAddress(hlib,'__gmpn_preinv_mod_1');
  pointer(mpn_random):=GetProcAddress(hlib,'__gmpn_random');
  pointer(mpn_random2):=GetProcAddress(hlib,'__gmpn_random2');
  pointer(mpn_rshift):=GetProcAddress(hlib,'__gmpn_rshift');
  pointer(mpn_scan0):=GetProcAddress(hlib,'__gmpn_scan0');
  pointer(mpn_scan1):=GetProcAddress(hlib,'__gmpn_scan1');
  pointer(mpn_set_str):=GetProcAddress(hlib,'__gmpn_set_str');
  pointer(mpn_sizeinbase):=GetProcAddress(hlib,'__gmpn_sizeinbase');
  pointer(mpn_sqrtrem):=GetProcAddress(hlib,'__gmpn_sqrtrem');
  pointer(mpn_sub):=GetProcAddress(hlib,'__gmpn_sub');
  pointer(mpn_sub_1):=GetProcAddress(hlib,'__gmpn_sub_1');
  pointer(mpn_sub_n):=GetProcAddress(hlib,'__gmpn_sub_n');
  pointer(mpn_submul_1):=GetProcAddress(hlib,'__gmpn_submul_1');
  pointer(mpn_tdiv_qr):=GetProcAddress(hlib,'__gmpn_tdiv_qr');
  pointer(mpn_and_n):=GetProcAddress(hlib,'__gmpn_and_n');
  pointer(mpn_andn_n):=GetProcAddress(hlib,'__gmpn_andn_n');
  pointer(mpn_nand_n):=GetProcAddress(hlib,'__gmpn_nand_n');
  pointer(mpn_ior_n):=GetProcAddress(hlib,'__gmpn_ior_n');
  pointer(mpn_iorn_n):=GetProcAddress(hlib,'__gmpn_iorn_n');
  pointer(mpn_nior_n):=GetProcAddress(hlib,'__gmpn_nior_n');
  pointer(mpn_xor_n):=GetProcAddress(hlib,'__gmpn_xor_n');
  pointer(mpn_xnor_n):=GetProcAddress(hlib,'__gmpn_xnor_n');
  pointer(mpn_copyi):=GetProcAddress(hlib,'__gmpn_copyi');
  pointer(mpn_copyd):=GetProcAddress(hlib,'__gmpn_copyd');
  pointer(mpn_zero):=GetProcAddress(hlib,'__gmpn_zero');
  pointer(mpn_cnd_add_n):=GetProcAddress(hlib,'__gmpn_cnd_add_n');
  pointer(mpn_cnd_sub_n):=GetProcAddress(hlib,'__gmpn_cnd_sub_n');
  pointer(mpn_sec_add_1):=GetProcAddress(hlib,'__gmpn_sec_add_1');
  pointer(mpn_sec_add_1_itch):=GetProcAddress(hlib,'__gmpn_sec_add_1_itch');
  pointer(mpn_sec_sub_1):=GetProcAddress(hlib,'__gmpn_sec_sub_1');
  pointer(mpn_sec_sub_1_itch):=GetProcAddress(hlib,'__gmpn_sec_sub_1_itch');
  pointer(mpn_cnd_swap):=GetProcAddress(hlib,'__gmpn_cnd_swap');
  pointer(mpn_sec_mul):=GetProcAddress(hlib,'__gmpn_sec_mul');
  pointer(mpn_sec_mul_itch):=GetProcAddress(hlib,'__gmpn_sec_mul_itch');
  pointer(mpn_sec_sqr):=GetProcAddress(hlib,'__gmpn_sec_sqr');
  pointer(mpn_sec_sqr_itch):=GetProcAddress(hlib,'__gmpn_sec_sqr_itch');
  pointer(mpn_sec_powm):=GetProcAddress(hlib,'__gmpn_sec_powm');
  pointer(mpn_sec_powm_itch):=GetProcAddress(hlib,'__gmpn_sec_powm_itch');
  pointer(mpn_sec_tabselect):=GetProcAddress(hlib,'__gmpn_sec_tabselect');
  pointer(mpn_sec_div_qr):=GetProcAddress(hlib,'__gmpn_sec_div_qr');
  pointer(mpn_sec_div_qr_itch):=GetProcAddress(hlib,'__gmpn_sec_div_qr_itch');
  pointer(mpn_sec_div_r):=GetProcAddress(hlib,'__gmpn_sec_div_r');
  pointer(mpn_sec_div_r_itch):=GetProcAddress(hlib,'__gmpn_sec_div_r_itch');
  pointer(mpn_sec_invert):=GetProcAddress(hlib,'__gmpn_sec_invert');
  pointer(mpn_sec_invert_itch):=GetProcAddress(hlib,'__gmpn_sec_invert_itch');
end;

end.
