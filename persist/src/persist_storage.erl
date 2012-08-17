-module(persist_storage).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
    {load, 0},
    {store, 2},
    {delete, 1}
];
%behaviour_info(callbacks) -> [{api, 1}];
behaviour_info(_Other) -> undefined.


