

const
   _PATH_HEQUIV = '/etc/hosts.equiv';
   _PATH_HOSTS = '/etc/hosts';
   _PATH_NETWORKS = '/etc/networks';
   _PATH_NSSWITCH_CONF = '/etc/nsswitch.conf';
   _PATH_PROTOCOLS = '/etc/protocols';
   _PATH_SERVICES = '/etc/services';

var
   h_errno : longint;cvar;external;

function __h_errno_location:Plongint;cdecl;external clib name '__h_errno_location';

const
  NETDB_INTERNAL = -(1);
  NETDB_SUCCESS = 0;
  HOST_NOT_FOUND = 1;
  TRY_AGAIN = 2;
  NO_RECOVERY = 3;
  NO_DATA = 4;
  NO_ADDRESS = NO_DATA;
  SCOPE_DELIMITER = '%';

procedure herror(__str:PAnsiChar);cdecl;external clib name 'herror';
function hstrerror(__err_num:longint):PAnsiChar;cdecl;external clib name 'hstrerror';

type
  Phostent = ^hostent;
  hostent = record
    h_name: PAnsiChar;
    h_aliases: PPAnsiChar;
    h_addrtype: Integer;
    h_length: socklen_t;
    case Byte of
      0: (h_addr_list: PPAnsiChar);
      1: (h_addr: PPAnsiChar);
  end;
  PPhostent = ^Phostent;

procedure sethostent(__stay_open:longint);cdecl;external clib name 'sethostent';
procedure endhostent;cdecl;external clib name 'endhostent';
function gethostent:Phostent;cdecl;external clib name 'gethostent';
function gethostbyaddr(__addr:pointer; __len:__socklen_t; __type:longint):Phostent;cdecl;external clib name 'gethostbyaddr';
function gethostbyname(__name:PAnsiChar):Phostent;cdecl;external clib name 'gethostbyname';
function gethostbyname2(__name:PAnsiChar; __af:longint):Phostent;cdecl;external clib name 'gethostbyname2';
function gethostent_r(__result_buf:Phostent; __buf:PAnsiChar; __buflen:size_t; __result:PPhostent; __h_errnop:Plongint):longint;cdecl;external clib name 'gethostent_r';
function gethostbyaddr_r(__addr:pointer; __len:__socklen_t; __type:longint; __result_buf:Phostent; __buf:PAnsiChar;
               __buflen:size_t; __result:PPhostent; __h_errnop:Plongint):longint;cdecl;external clib name 'gethostbyaddr_r';
function gethostbyname_r(__name:PAnsiChar; __result_buf:Phostent; __buf:PAnsiChar; __buflen:size_t; __result:PPhostent;
               __h_errnop:Plongint):longint;cdecl;external clib name 'gethostbyname_r';
function gethostbyname2_r(__name:PAnsiChar; __af:longint; __result_buf:Phostent; __buf:PAnsiChar; __buflen:size_t;
               __result:PPhostent; __h_errnop:Plongint):longint;cdecl;external clib name 'gethostbyname2_r';
procedure setnetent(__stay_open:longint);cdecl;external clib name 'setnetent';
procedure endnetent;cdecl;external clib name 'endnetent';
function getnetent:Pnetent;cdecl;external clib name 'getnetent';
function getnetbyaddr(__net:uint32_t; __type:longint):Pnetent;cdecl;external clib name 'getnetbyaddr';
function getnetbyname(__name:PAnsiChar):Pnetent;cdecl;external clib name 'getnetbyname';
function getnetent_r(__result_buf:Pnetent; __buf:PAnsiChar; __buflen:size_t; __result:PPnetent; __h_errnop:Plongint):longint;cdecl;external clib name 'getnetent_r';
function getnetbyaddr_r(__net:uint32_t; __type:longint; __result_buf:Pnetent; __buf:PAnsiChar; __buflen:size_t;
               __result:PPnetent; __h_errnop:Plongint):longint;cdecl;external clib name 'getnetbyaddr_r';
function getnetbyname_r(__name:PAnsiChar; __result_buf:Pnetent; __buf:PAnsiChar; __buflen:size_t; __result:PPnetent;
               __h_errnop:Plongint):longint;cdecl;external clib name 'getnetbyname_r';
type
  Pservent = ^servent;
  servent = record
       s_name : PAnsiChar;
       s_aliases : ^PAnsiChar;
       s_port : longint;
       s_proto : PAnsiChar;
    end;
  PPservent = ^Pservent;

procedure setservent(__stay_open:longint);cdecl;external clib name 'setservent';
procedure endservent;cdecl;external clib name 'endservent';
function getservent:Pservent;cdecl;external clib name 'getservent';
function getservbyname(__name:PAnsiChar; __proto:PAnsiChar):Pservent;cdecl;external clib name 'getservbyname';
function getservbyport(__port:longint; __proto:PAnsiChar):Pservent;cdecl;external clib name 'getservbyport';
function getservent_r(__result_buf:Pservent; __buf:PAnsiChar; __buflen:size_t; __result:PPservent):longint;cdecl;external clib name 'getservent_r';
function getservbyname_r(__name:PAnsiChar; __proto:PAnsiChar; __result_buf:Pservent; __buf:PAnsiChar; __buflen:size_t;
               __result:PPservent):longint;cdecl;external clib name 'getservbyname_r';
function getservbyport_r(__port:longint; __proto:PAnsiChar; __result_buf:Pservent; __buf:PAnsiChar; __buflen:size_t;
               __result:PPservent):longint;cdecl;external clib name 'getservbyport_r';
type
  Pprotoent = ^protoent;
  protoent = record
       p_name : PAnsiChar;
       p_aliases : ^PAnsiChar;
       p_proto : longint;
    end;
  PPprotoent =  ^Pprotoent;

procedure setprotoent(__stay_open:longint);cdecl;external clib name 'setprotoent';
procedure endprotoent;cdecl;external clib name 'endprotoent';
function getprotoent:Pprotoent;cdecl;external clib name 'getprotoent';
function getprotobyname(__name:PAnsiChar):Pprotoent;cdecl;external clib name 'getprotobyname';
function getprotobynumber(__proto:longint):Pprotoent;cdecl;external clib name 'getprotobynumber';
function getprotoent_r(__result_buf:Pprotoent; __buf:PAnsiChar; __buflen:size_t; __result:PPprotoent):longint;cdecl;external clib name 'getprotoent_r';
function getprotobyname_r(__name:PAnsiChar; __result_buf:Pprotoent; __buf:PAnsiChar; __buflen:size_t; __result:PPprotoent):longint;cdecl;external clib name 'getprotobyname_r';
function getprotobynumber_r(__proto:longint; __result_buf:Pprotoent; __buf:PAnsiChar; __buflen:size_t; __result:PPprotoent):longint;cdecl;external clib name 'getprotobynumber_r';
function setnetgrent(__netgroup:PAnsiChar):longint;cdecl;external clib name 'setnetgrent';
procedure endnetgrent;cdecl;external clib name 'endnetgrent';
function getnetgrent(__hostp:PPAnsiChar; __userp:PPAnsiChar; __domainp:PPAnsiChar):longint;cdecl;external clib name 'getnetgrent';
function innetgr(__netgroup:PAnsiChar; __host:PAnsiChar; __user:PAnsiChar; domain:PAnsiChar):longint;cdecl;external clib name 'innetgr';
function getnetgrent_r(__hostp:PPAnsiChar; __userp:PPAnsiChar; __domainp:PPAnsiChar; __buffer:PAnsiChar; __buflen:size_t):longint;cdecl;external clib name 'getnetgrent_r';

function rcmd(__ahost:PPAnsiChar; __rport:word; __locuser:PAnsiChar; __remuser:PAnsiChar; __cmd:PAnsiChar;
               __fd2p:Plongint):longint;cdecl;external clib name 'rcmd';
function rcmd_af(__ahost:PPAnsiChar; __rport:word; __locuser:PAnsiChar; __remuser:PAnsiChar; __cmd:PAnsiChar;
               __fd2p:Plongint; __af:sa_family_t):longint;cdecl;external clib name 'rcmd_af';
function rexec(__ahost:PPAnsiChar; __rport:longint; __name:PAnsiChar; __pass:PAnsiChar; __cmd:PAnsiChar;
               __fd2p:Plongint):longint;cdecl;external clib name 'rexec';
function rexec_af(__ahost:PPAnsiChar; __rport:longint; __name:PAnsiChar; __pass:PAnsiChar; __cmd:PAnsiChar;
               __fd2p:Plongint; __af:sa_family_t):longint;cdecl;external clib name 'rexec_af';
function ruserok(__rhost:PAnsiChar; __suser:longint; __remuser:PAnsiChar; __locuser:PAnsiChar):longint;cdecl;external clib name 'ruserok';
function ruserok_af(__rhost:PAnsiChar; __suser:longint; __remuser:PAnsiChar; __locuser:PAnsiChar; __af:sa_family_t):longint;cdecl;external clib name 'ruserok_af';
function rresvport(__alport:Plongint):longint;cdecl;external clib name 'rresvport';
function rresvport_af(__alport:Plongint; __af:sa_family_t):longint;cdecl;external clib name 'rresvport_af';

type
  Paddrinfo = ^addrinfo;
  addrinfo = record
       ai_flags : longint;
       ai_family : longint;
       ai_socktype : longint;
       ai_protocol : longint;
       ai_addrlen : socklen_t;
       ai_addr : Psockaddr;
       ai_canonname : PAnsiChar;
       ai_next : Paddrinfo;
    end;
  PPaddrinfo = ^Paddrinfo;

  Pgaicb = ^gaicb;
  gaicb = record
       ar_name : PAnsiChar;
       ar_service : PAnsiChar;
       ar_request : Paddrinfo;
       ar_result : Paddrinfo;
       __return : longint;
       __unused : array[0..4] of longint;
    end;
  PPgaicb = ^Pgaicb;


const
  GAI_WAIT = 0;
  GAI_NOWAIT = 1;

  AI_PASSIVE = $0001;
  AI_CANONNAME = $0002;
  AI_NUMERICHOST = $0004;
  EAI_BADFLAGS = -(1);
  EAI_NONAME = -(2);
  EAI_AGAIN = -(3);
  EAI_FAIL = -(4);
  EAI_NODATA = -(5);
  EAI_FAMILY = -(6);
  EAI_SOCKTYPE = -(7);
  EAI_SERVICE = -(8);
  EAI_ADDRFAMILY = -(9);
  EAI_MEMORY = -(10);
  EAI_SYSTEM = -(11);

  EAI_INPROGRESS = -(100);
  EAI_CANCELED = -(101);
  EAI_NOTCANCELED = -(102);
  EAI_ALLDONE = -(103);
  EAI_INTR = -(104);

  NI_MAXHOST = 1025;
  NI_MAXSERV = 32;
  NI_NUMERICHOST = 1;
  NI_NUMERICSERV = 2;
  NI_NOFQDN = 4;
  NI_NAMEREQD = 8;
  NI_DGRAM = 16;

function getaddrinfo(__name:PAnsiChar; __service:PAnsiChar; __req:Paddrinfo; __pai:PPaddrinfo):longint;cdecl;external clib name 'getaddrinfo';
procedure freeaddrinfo(__ai:Paddrinfo);cdecl;external clib name 'freeaddrinfo';
function gai_strerror(__ecode:longint):PAnsiChar;cdecl;external clib name 'gai_strerror';
function getnameinfo(__sa:Psockaddr; __salen:socklen_t; __host:PAnsiChar; __hostlen:socklen_t; __serv:PAnsiChar;
               __servlen:socklen_t; __flags:dword):longint;cdecl;external clib name 'getnameinfo';
function getaddrinfo_a(__mode:longint; __list: PPgaicb; __ent:longint; __sig:Psigevent):longint;cdecl;external clib name 'getaddrinfo_a';
function gai_suspend(__list:array of Pgaicb; __ent:longint; __timeout:Ptimespec):longint;cdecl;external clib name 'gai_suspend';
function gai_error(__req:Pgaicb):longint;cdecl;external clib name 'gai_error';
function gai_cancel(__gaicbp:Pgaicb):longint;cdecl;external clib name 'gai_cancel';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  THostEnt = hostent;
  TServEnt = servent;
  TProtoEnt = protoent;
  TAddressInfo = addrinfo;

