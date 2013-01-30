%% Copyright
-author("volodymyr.kyrychenko@strikead.com").

-define(FORMAT_RECORD(R, Name), xl_string:format_record(R, record_info(fields, Name))).
-define(RECORD_TO_PROPLIST(R, Name), xl_lang:record_to_proplist(R, record_info(fields, Name))).
