%% Copyright
-author("volodymyr.kyrychenko@strikead.com").
-record(persist_data, {
    module :: atom(),
    identify :: fun((term()) -> term()),
    data :: any()
}).