%% Copyright
{application, xl_stdlib,
    [{description, "common libraries"},
        {vsn, "0.1.0"},
        {applications, [kernel, stdlib, et_lang]},
        {registered, [
            xl_ets_server
        ]},
        {mod, {xl_stdlib_app, []}}
    ]}.


