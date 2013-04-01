%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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


get_line(Fd, Position) -> get_line(Fd, Position, <<>>).
get_line(Fd, Position, Acc) ->
    case pread(Fd, Position, 1) of
        eof when Acc == <<>> -> eof;
        eof -> {Acc, Position};
        {ok, Eol = <<"\n">>} -> {<<Acc/binary, Eol/binary>>, Position + 1};
        {ok, X} -> get_line(Fd, Position + 1, <<Acc/binary, X/binary>>)
    end.

