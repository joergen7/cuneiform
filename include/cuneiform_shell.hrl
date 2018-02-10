%% -*- erlang -*-
%%
%% A functional workflow language for large-scale scientific data analysis.
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 3.0.0
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-record( shell_state, {line       = 1,
                       string_buf = "",
                       token_buf  = [],
                       token_lst  = [],
                       import_buf = [],
                       import_lst = [],
                       def_buf    = [],
                       def_lst    = [],
                       query_lst  = [],
                       reply_lst  = []} ).