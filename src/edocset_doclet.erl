%% Copyright 2014 mtgto <hogerappa@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0       
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(edocset_doclet).

-export([run/2]).

-include_lib("edoc/include/edoc_doclet.hrl").

run(#doclet_gen{}=Cmd, Ctxt) ->
    ok = edoc_doclet:run(Cmd, Ctxt),
    ContentsDir = lists:flatten(io_lib:format("~s.docset/Contents", [Cmd#doclet_gen.app])),
    DocumentDir = lists:flatten(io_lib:format("~s/Resources/Documents/", [ContentsDir])),
    case filelib:ensure_dir(DocumentDir) of
        {error, Reason} ->
            io:format("error:~p~n", [Reason]),
            exit(error);
        ok ->
            ok = copy_dir("doc", DocumentDir),
            _InfoPlistFile = lists:flatten(io_lib:format("~s/Info.plist", [ContentsDir]))
    end,
    ok.

copy_dir(Src, Dst) ->
    Fun = fun(Path, Acc) ->
                  case Acc of
                      ok ->
                          NewPath = filename:join(Dst, Path),
                          ok = filelib:ensure_dir(NewPath),
                          case file:copy(Path, NewPath) of
                              {ok, _} -> ok;
                              {error, _Reason} -> error
                          end;
                      Other -> Other
                  end
          end,
    ok = filelib:fold_files(Src, "^[^.]", true, Fun, ok).
