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

-export([run/2, create_info_plist/2]).

-include_lib("edoc/include/edoc_doclet.hrl").
-include_lib("xmerl/include/xmerl.hrl").

run(#doclet_gen{}=Cmd, Ctxt) ->
    ok = edoc_doclet:run(Cmd, Ctxt),
    App = Cmd#doclet_gen.app,
    ContentsDir = lists:flatten(io_lib:format("~s.docset/Contents", [App])),
    DocumentDir = lists:flatten(io_lib:format("~s/Resources/Documents/", [ContentsDir])),
    case filelib:ensure_dir(DocumentDir) of
        {error, Reason} ->
            io:format("error:~p~n", [Reason]),
            exit(error);
        ok ->
            ok = copy_dir("doc", DocumentDir),
            InfoPlistFile = lists:flatten(io_lib:format("~s/Info.plist", [ContentsDir])),
            create_info_plist(InfoPlistFile, App),
            SQLiteFile = lists:flatten(io_lib:format("~s/Resources/docSet.dsidx", [ContentsDir])),
            ok = create_table(SQLiteFile)
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

create_info_plist(Path, App) ->
    %Decl = #xmlDecl{vsn="1.0", encoding="UTF-8"}
    Prolog = ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
"],
    Elem = #xmlElement{name=plist,
                      attributes=[#xmlAttribute{name=version, value="1.0"}],
                      content=[{dict,
                                [
                                 {key, ["CFBundleIdentifier"]}, {string, [App]},
                                 {key, ["CFBundleName"]}, {string, [App]},
                                 {key, ["DocSetPlatformFamily"]}, {string, [App]},
                                 {key, ["isDashDocset"]}, {true, []}
                                ]}]},
    Xml = xmerl:export_simple([Elem], xmerl_xml, [{prolog, Prolog}]),
    file:write_file(Path, Xml).

create_table(SQLiteFile) ->
    case sqlite3:open(db, [{file, SQLiteFile}]) of
        {ok, _Pid} ->
            ok = sqlite3:create_table(db, "searchIndex", [{id, integer, [primary_key]}, {name, text}, {type, text}, {path, text}]),
            ok = sqlite3:sql_exec(db, "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);"),
            ok;
        {error, Error} ->
            io:format("error ~p~n", [Error]),
            error(error)
    end.
