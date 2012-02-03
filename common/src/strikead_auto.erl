-module(strikead_auto).

-export([using/3]).

-spec using(Module, Params, Callback) -> term() when
    Module :: module(),
    Params :: term(),
    Callback :: fun().

using(Module, Params, F) ->
    case Module:open(Params) of
		{ok, Descriptor} ->
			try
                {ok, F(Descriptor)}
			after
				Module:close(Descriptor)
			end;
		X -> X
	end.