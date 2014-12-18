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
