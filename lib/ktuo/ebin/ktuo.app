%% -*- mode:Erlang; fill-column:79 -*-
{application, ktuo,
 [{description, "Json/Tuple parser encoder for Erlang"},
  {vsn, "0.5.0.0"},
  {modules, [ktj_encode,
             ktj_decode,
             ktj_parse,
             ktt_decode,
             ktuo_parse_utils]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
