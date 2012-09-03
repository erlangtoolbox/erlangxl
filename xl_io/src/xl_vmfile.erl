-module(xl_vmfile).

-include_lib("kernel/include/file.hrl").

-export([auto_open/1, open/2, auto_close/1, close/1, pread/3, parse_lines/1]).

-record(handle, {segment_size, segments, data_size}).

-behaviour(xl_autoresource).

auto_open([Path, Mode]) -> open(Path, Mode).
auto_close(Fd) -> close(Fd).

open(Path, [{segment, SegmentSize}]) ->
    xl_file:using(Path, [read, raw, binary], fun(File) ->
        {Segments, DataSize} = read_segments([], File, SegmentSize, 0),
        #file_descriptor{module = ?MODULE, data = #handle{segment_size = SegmentSize, segments = Segments, data_size = DataSize}}
    end);
open(_, _) -> {error, badarg}.

read_segments(Segments, File, SegmentSize, DataSize) ->
    case file:read(File, SegmentSize) of
        {ok, Data} -> read_segments([Data | Segments], File, SegmentSize, DataSize + byte_size(Data));
        eof -> {lists:reverse(Segments), DataSize}
    end.

close(#file_descriptor{module = ?MODULE}) -> ok;
close(_) -> {error, badarg}.


%% todo handle possible posision specifications

pread(#file_descriptor{module = ?MODULE, data = Handle}, Position, Number) when is_number(Position) ->
    case pread(Handle, Position, Number, <<>>) of
        eof -> eof;
        L -> {ok, L}
    end;

pread(_, _, _) -> {error, badarg}.

pread(Handle = #handle{segment_size = SegmentSize, segments = Segments}, Position, Number, Acc) ->
    Offset = Position rem SegmentSize,
    Index = Position div SegmentSize + 1,
    Count = min(SegmentSize - Offset, Number),
    if
            Index > length(Segments), byte_size(Acc) == 0 -> eof;
            Index > length(Segments) -> Acc;
            true ->
            Segment = lists:nth(Index, Segments),
            IndexedSegmentLength = byte_size(Segment),
            if
                    Offset >= IndexedSegmentLength, byte_size(Acc) == 0 -> eof;
                    Number == 0 -> Acc;
                    true ->
                    Part = binary_part(Segment, Offset, min(Count, IndexedSegmentLength - Offset)),
                    pread(Handle, Position + Count, Number - Count, <<Acc/binary, Part/binary>>)
            end
    end.

parse_lines(#file_descriptor{module = ?MODULE, data = #handle{segments = []}}) -> xl_stream:empty();
parse_lines(Fd = #file_descriptor{module = ?MODULE}) ->
    xl_stream:stream(0, fun(Position) ->
        case get_line(Fd, Position) of
            eof -> empty;
            {Line, NextPosition} -> {{Line, Position}, NextPosition}
        end
    end).


get_line(Fd, Position) -> get_line(Fd, Position, []).
get_line(Fd, Position, Acc) ->
    case pread(Fd, Position, 1) of
        eof when Acc == [] -> eof;
        eof -> {Acc, Position};
        {ok, Eol = <<"\n">>} -> {Acc ++ binary_to_list(Eol), Position + 1};
        {ok, X} -> get_line(Fd, Position + 1, Acc ++ binary_to_list(X))
    end.
