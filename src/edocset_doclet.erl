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
-include_lib("xmerl/include/xmerl.hrl").

-define(TABLE_NAME, "searchIndex").
-define(DOC_DIR, "doc").

-export_type([assoc_list/2]).
-type assoc_list(Key, Value) :: [{Key, Value}].

%% @doc entry point of doclet. Create a docset after generated edoc.
run(#doclet_gen{}=Cmd, Ctxt) ->
    ok = edoc_doclet:run(Cmd, Ctxt),
    App = Cmd#doclet_gen.app,
    ContentsDir = lists:flatten(io_lib:format("~s.docset/Contents", [App])),
    DocumentDir = lists:flatten(io_lib:format("~s/Resources/Documents/", [ContentsDir])),
    case filelib:ensure_dir(DocumentDir) of
        {error, _Reason} ->
            exit(error);
        ok ->
            ok = copy_dir(?DOC_DIR, DocumentDir),
            InfoPlistFile = lists:flatten(io_lib:format("~s/Info.plist", [ContentsDir])),
            create_info_plist(InfoPlistFile, App),
            SQLiteFile = lists:flatten(io_lib:format("~s/Resources/docSet.dsidx", [ContentsDir])),
            ok = create_table(SQLiteFile),
            ok = create_database(?DOC_DIR)
    end,
    ok.

%% @doc copy all files with the directory.
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

%% @doc create Info.plist for Dash.
create_info_plist(Path, App) ->
    AppName = atom_to_list(App),
    Prolog = ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
"],
    Elem = #xmlElement{name=plist,
                      attributes=[#xmlAttribute{name=version, value="1.0"}],
                      content=[{dict,
                                [
                                 {key, ["CFBundleIdentifier"]}, {string, [AppName]},
                                 {key, ["CFBundleName"]}, {string, [AppName]},
                                 {key, ["DocSetPlatformFamily"]}, {string, [AppName]},
                                 {key, ["isDashDocset"]}, {true, []}
                                ]}]},
    Xml = xmerl:export_simple([Elem], xmerl_xml, [{prolog, Prolog}]),
    file:write_file(Path, Xml).

%% @doc create sqlite3 database and table.
create_table(SQLiteFile) ->
    case sqlite3:open(db, [{file, SQLiteFile}]) of
        {ok, _Pid} ->
            ok = sqlite3:create_table(db, ?TABLE_NAME, [{id, integer, [primary_key]}, {name, text}, {type, text}, {path, text}]),
            ok = sqlite3:sql_exec(db, "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);"),
            ok;
        {error, _Error} ->
            error(error)
    end.

%% @doc create the database containing modules, types, functions.
create_database(Dir) ->
    Fun = fun(Path, Acc) ->
        case Acc of
            ok ->
                try xmerl_scan:file(Path, [{quiet, true}]) of
                    {Xml, _} ->
                        ModuleName = module_name(Xml),
                        {rowid, _} = sqlite3:write(db, ?TABLE_NAME, [{name, ModuleName}, {type, "Module"}, {path, Path}]),
                        Types = types(Xml),
                        ok = write_types(Types, ModuleName, Path),
                        Functions = functions(Xml),
                        ok = write_functions(Functions, ModuleName, Path),
                        ok
                catch
                    exit:_Reason ->
                        ok;
                    _:_ ->
                        error(error)
                end;
            Other -> Other
        end
    end,
    ok = filelib:fold_files(Dir, "\\.html$", true, Fun, ok).

%% @doc create types database
write_types(Types, Module, FilePath) ->
    write_types_or_functions(Types, Module, "Type", FilePath).

%% @doc create functions database
write_functions(Functions, Module, FilePath) ->
    write_types_or_functions(Functions, Module, "Function", FilePath).

write_types_or_functions([], _, _, _) -> ok;
write_types_or_functions([{Anchor, Name} | Rest], Module, Type, FilePath) ->
    Path = string:join([FilePath, "#", Anchor], ""),
    DisplayName = string:join([Module, ":", Name], ""),
    {rowid, _} = sqlite3:write(db, ?TABLE_NAME, [{name, DisplayName}, {type, Type}, {path, Path}]),
    write_types_or_functions(Rest, Module, Type, FilePath).

module_name(Elem) ->
    [#xmlElement{content = [Content]}] = xmerl_xpath:string("//html/body/h1", Elem),
    Text = Content#xmlText.value,
    [_, ModuleName] = string:tokens(Text, " "),
    ModuleName.

types(Elem) ->
    Types = xmerl_xpath:string("//html/body/h3[@class='typedecl']/a", Elem),
    collect_anchors(Types).

functions(Elem) ->
    Functions = xmerl_xpath:string("//html/body/h3[@class='function']/a", Elem),
    collect_anchors(Functions).

collect_anchors(Anchors) ->
    lists:usort(lists:map(fun (Anchor) ->
        [#xmlText{value = Name}] = Anchor#xmlElement.content,
        Attributes = Anchor#xmlElement.attributes,
        AnchorName = anchor_name(Attributes),
        {AnchorName, Name}
    end, Anchors)).

anchor_name([]) -> throw(not_found);
anchor_name([#xmlAttribute{name=name, value=Value} | _]) -> Value;
anchor_name([_ | Rest]) -> anchor_name(Rest).
