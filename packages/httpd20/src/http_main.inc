{ Copyright 1999-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
}

//#include "apr_optional.h"

{ AP_SERVER_BASEARGS is the command argument list parsed by http_main.c
 * in apr_getopt() format.  Use this for default'ing args that the MPM
 * can safely ignore and pass on from its rewrite_args() handler.
 }
const
  AP_SERVER_BASEARGS = 'C:c:D:d:E:e:f:vVlLtSh?X';

{
 * @package Command line options
 }

{ The name of the Apache executable }
//AP_DECLARE_DATA extern const AnsiChar *ap_server_argv0;
{ The global server's ServerRoot }
//AP_DECLARE_DATA extern const AnsiChar *ap_server_root;

{ for -C, -c and -D switches }
{ An array of all -C directives.  These are processed before the server's
 *  config file }
//AP_DECLARE_DATA extern apr_array_header_t *ap_server_pre_read_config;
{ An array of all -c directives.  These are processed after the server's
 *  config file }
//AP_DECLARE_DATA extern apr_array_header_t *ap_server_post_read_config;
{ An array of all -D defines on the command line.  This allows people to
 *  effect the server based on command line options }
//AP_DECLARE_DATA extern apr_array_header_t *ap_server_config_defines;

//APR_DECLARE_OPTIONAL_FN(int, ap_signal_server, (int *, apr_pool_t *));

