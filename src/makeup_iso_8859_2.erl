%%% File    : makeup_iso_8859_2.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : ISO-8859-2 processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_iso_8859_2).

-export([input/1, output/1]).

-include("makeup_utf.hrl").
%%
%% Convert from iso-8859-2 to unicode
%%
%% return
%%     {Valid, [code()], Continuation/1}
%%
%%
input(Bin) when is_binary(Bin) ->
    input(binary_to_list(Bin),[],true);
input(List) when is_list(List) ->
    input(List,[],true).

input([C|Cs], Acc, Valid) when C < 16#7F ->
    input(Cs, [C|Acc], Valid);
input([C|Cs], Acc, Valid) ->
    case C of
	16#A0 -> input(Cs,[16#00A0|Acc], Valid); %% NO-BREAK SPACE
	16#A1 -> input(Cs,[16#0104|Acc], Valid); %% LATIN CAPITAL LETTER A WITH OGONEK
	16#A2 -> input(Cs,[16#02D8|Acc], Valid); %% BREVE
	16#A3 -> input(Cs,[16#0141|Acc], Valid); %% LATIN CAPITAL LETTER L WITH STROKE
	16#A4 -> input(Cs,[16#00A4|Acc], Valid); %% CURRENCY SIGN
	16#A5 -> input(Cs,[16#013D|Acc], Valid); %% LATIN CAPITAL LETTER L WITH CARON
	16#A6 -> input(Cs,[16#015A|Acc], Valid); %% LATIN CAPITAL LETTER S WITH ACUTE
	16#A7 -> input(Cs,[16#00A7|Acc], Valid); %% SECTION SIGN
	16#A8 -> input(Cs,[16#00A8|Acc], Valid); %% DIAERESIS
	16#A9 -> input(Cs,[16#0160|Acc], Valid); %% LATIN CAPITAL LETTER S WITH CARON
	16#AA -> input(Cs,[16#015E|Acc], Valid); %% LATIN CAPITAL LETTER S WITH CEDILLA
	16#AB -> input(Cs,[16#0164|Acc], Valid); %% LATIN CAPITAL LETTER T WITH CARON
	16#AC -> input(Cs,[16#0179|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH ACUTE
	16#AD -> input(Cs,[16#00AD|Acc], Valid); %% SOFT HYPHEN
	16#AE -> input(Cs,[16#017D|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH CARON
	16#AF -> input(Cs,[16#017B|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH DOT ABOVE
	16#B0 -> input(Cs,[16#00B0|Acc], Valid); %% DEGREE SIGN
	16#B1 -> input(Cs,[16#0105|Acc], Valid); %% LATIN SMALL LETTER A WITH OGONEK
	16#B2 -> input(Cs,[16#02DB|Acc], Valid); %% OGONEK
	16#B3 -> input(Cs,[16#0142|Acc], Valid); %% LATIN SMALL LETTER L WITH STROKE
	16#B4 -> input(Cs,[16#00B4|Acc], Valid); %% ACUTE ACCENT
	16#B5 -> input(Cs,[16#013E|Acc], Valid); %% LATIN SMALL LETTER L WITH CARON
	16#B6 -> input(Cs,[16#015B|Acc], Valid); %% LATIN SMALL LETTER S WITH ACUTE
	16#B7 -> input(Cs,[16#02C7|Acc], Valid); %% CARON (Mandarin Chinese third tone)
	16#B8 -> input(Cs,[16#00B8|Acc], Valid); %% CEDILLA
	16#B9 -> input(Cs,[16#0161|Acc], Valid); %% LATIN SMALL LETTER S WITH CARON
	16#BA -> input(Cs,[16#015F|Acc], Valid); %% LATIN SMALL LETTER S WITH CEDILLA
	16#BB -> input(Cs,[16#0165|Acc], Valid); %% LATIN SMALL LETTER T WITH CARON
	16#BC -> input(Cs,[16#017A|Acc], Valid); %% LATIN SMALL LETTER Z WITH ACUTE
	16#BD -> input(Cs,[16#02DD|Acc], Valid); %% DOUBLE ACUTE ACCENT
	16#BE -> input(Cs,[16#017E|Acc], Valid); %% LATIN SMALL LETTER Z WITH CARON
	16#BF -> input(Cs,[16#017C|Acc], Valid); %% LATIN SMALL LETTER Z WITH DOT ABOVE
	16#C0 -> input(Cs,[16#0154|Acc], Valid); %% LATIN CAPITAL LETTER R WITH ACUTE
	16#C1 -> input(Cs,[16#00C1|Acc], Valid); %% LATIN CAPITAL LETTER A WITH ACUTE
	16#C2 -> input(Cs,[16#00C2|Acc], Valid); %% LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	16#C3 -> input(Cs,[16#0102|Acc], Valid); %% LATIN CAPITAL LETTER A WITH BREVE
	16#C4 -> input(Cs,[16#00C4|Acc], Valid); %% LATIN CAPITAL LETTER A WITH DIAERESIS
	16#C5 -> input(Cs,[16#0139|Acc], Valid); %% LATIN CAPITAL LETTER L WITH ACUTE
	16#C6 -> input(Cs,[16#0106|Acc], Valid); %% LATIN CAPITAL LETTER C WITH ACUTE
	16#C7 -> input(Cs,[16#00C7|Acc], Valid); %% LATIN CAPITAL LETTER C WITH CEDILLA
	16#C8 -> input(Cs,[16#010C|Acc], Valid); %% LATIN CAPITAL LETTER C WITH CARON
	16#C9 -> input(Cs,[16#00C9|Acc], Valid); %% LATIN CAPITAL LETTER E WITH ACUTE
	16#CA -> input(Cs,[16#0118|Acc], Valid); %% LATIN CAPITAL LETTER E WITH OGONEK
	16#CB -> input(Cs,[16#00CB|Acc], Valid); %% LATIN CAPITAL LETTER E WITH DIAERESIS
	16#CC -> input(Cs,[16#011A|Acc], Valid); %% LATIN CAPITAL LETTER E WITH CARON
	16#CD -> input(Cs,[16#00CD|Acc], Valid); %% LATIN CAPITAL LETTER I WITH ACUTE
	16#CE -> input(Cs,[16#00CE|Acc], Valid); %% LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	16#CF -> input(Cs,[16#010E|Acc], Valid); %% LATIN CAPITAL LETTER D WITH CARON
	16#D0 -> input(Cs,[16#0110|Acc], Valid); %% LATIN CAPITAL LETTER D WITH STROKE
	16#D1 -> input(Cs,[16#0143|Acc], Valid); %% LATIN CAPITAL LETTER N WITH ACUTE
	16#D2 -> input(Cs,[16#0147|Acc], Valid); %% LATIN CAPITAL LETTER N WITH CARON
	16#D3 -> input(Cs,[16#00D3|Acc], Valid); %% LATIN CAPITAL LETTER O WITH ACUTE
	16#D4 -> input(Cs,[16#00D4|Acc], Valid); %% LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	16#D5 -> input(Cs,[16#0150|Acc], Valid); %% LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
	16#D6 -> input(Cs,[16#00D6|Acc], Valid); %% LATIN CAPITAL LETTER O WITH DIAERESIS
	16#D7 -> input(Cs,[16#00D7|Acc], Valid); %% MULTIPLICATION SIGN
	16#D8 -> input(Cs,[16#0158|Acc], Valid); %% LATIN CAPITAL LETTER R WITH CARON
	16#D9 -> input(Cs,[16#016E|Acc], Valid); %% LATIN CAPITAL LETTER U WITH RING ABOVE
	16#DA -> input(Cs,[16#00DA|Acc], Valid); %% LATIN CAPITAL LETTER U WITH ACUTE
	16#DB -> input(Cs,[16#0170|Acc], Valid); %% LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
	16#DC -> input(Cs,[16#00DC|Acc], Valid); %% LATIN CAPITAL LETTER U WITH DIAERESIS
	16#DD -> input(Cs,[16#00DD|Acc], Valid); %% LATIN CAPITAL LETTER Y WITH ACUTE
	16#DE -> input(Cs,[16#0162|Acc], Valid); %% LATIN CAPITAL LETTER T WITH CEDILLA
	16#DF -> input(Cs,[16#00DF|Acc], Valid); %% LATIN SMALL LETTER SHARP S (German)
	16#E0 -> input(Cs,[16#0155|Acc], Valid); %% LATIN SMALL LETTER R WITH ACUTE
	16#E1 -> input(Cs,[16#00E1|Acc], Valid); %% LATIN SMALL LETTER A WITH ACUTE
	16#E2 -> input(Cs,[16#00E2|Acc], Valid); %% LATIN SMALL LETTER A WITH CIRCUMFLEX
	16#E3 -> input(Cs,[16#0103|Acc], Valid); %% LATIN SMALL LETTER A WITH BREVE
	16#E4 -> input(Cs,[16#00E4|Acc], Valid); %% LATIN SMALL LETTER A WITH DIAERESIS
	16#E5 -> input(Cs,[16#013A|Acc], Valid); %% LATIN SMALL LETTER L WITH ACUTE
	16#E6 -> input(Cs,[16#0107|Acc], Valid); %% LATIN SMALL LETTER C WITH ACUTE
	16#E7 -> input(Cs,[16#00E7|Acc], Valid); %% LATIN SMALL LETTER C WITH CEDILLA
	16#E8 -> input(Cs,[16#010D|Acc], Valid); %% LATIN SMALL LETTER C WITH CARON
	16#E9 -> input(Cs,[16#00E9|Acc], Valid); %% LATIN SMALL LETTER E WITH ACUTE
	16#EA -> input(Cs,[16#0119|Acc], Valid); %% LATIN SMALL LETTER E WITH OGONEK
	16#EB -> input(Cs,[16#00EB|Acc], Valid); %% LATIN SMALL LETTER E WITH DIAERESIS
	16#EC -> input(Cs,[16#011B|Acc], Valid); %% LATIN SMALL LETTER E WITH CARON
	16#ED -> input(Cs,[16#00ED|Acc], Valid); %% LATIN SMALL LETTER I WITH ACUTE
	16#EE -> input(Cs,[16#00EE|Acc], Valid); %% LATIN SMALL LETTER I WITH CIRCUMFLEX
	16#EF -> input(Cs,[16#010F|Acc], Valid); %% LATIN SMALL LETTER D WITH CARON
	16#F0 -> input(Cs,[16#0111|Acc], Valid); %% LATIN SMALL LETTER D WITH STROKE
	16#F1 -> input(Cs,[16#0144|Acc], Valid); %% LATIN SMALL LETTER N WITH ACUTE
	16#F2 -> input(Cs,[16#0148|Acc], Valid); %% LATIN SMALL LETTER N WITH CARON
	16#F3 -> input(Cs,[16#00F3|Acc], Valid); %% LATIN SMALL LETTER O WITH ACUTE
	16#F4 -> input(Cs,[16#00F4|Acc], Valid); %% LATIN SMALL LETTER O WITH CIRCUMFLEX
	16#F5 -> input(Cs,[16#0151|Acc], Valid); %% LATIN SMALL LETTER O WITH DOUBLE ACUTE
	16#F6 -> input(Cs,[16#00F6|Acc], Valid); %% LATIN SMALL LETTER O WITH DIAERESIS
	16#F7 -> input(Cs,[16#00F7|Acc], Valid); %% DIVISION SIGN
	16#F8 -> input(Cs,[16#0159|Acc], Valid); %% LATIN SMALL LETTER R WITH CARON
	16#F9 -> input(Cs,[16#016F|Acc], Valid); %% LATIN SMALL LETTER U WITH RING ABOVE
	16#FA -> input(Cs,[16#00FA|Acc], Valid); %% LATIN SMALL LETTER U WITH ACUTE
	16#FB -> input(Cs,[16#0171|Acc], Valid); %% LATIN SMALL LETTER U WITH DOUBLE ACUTE
	16#FC -> input(Cs,[16#00FC|Acc], Valid); %% LATIN SMALL LETTER U WITH DIAERESIS
	16#FD -> input(Cs,[16#00FD|Acc], Valid); %% LATIN SMALL LETTER Y WITH ACUTE
	16#FE -> input(Cs,[16#0163|Acc], Valid); %% LATIN SMALL LETTER T WITH CEDILLA
	16#FF -> input(Cs,[16#02D9|Acc], Valid); %% DOT ABOVE (Mandarin Chinese light tone)
	_ -> 
	    if C > 16#FF ->
		    input(Cs,[?UNI_REPLACEMENT_CHAR|Acc], false);
	       true ->
		    input(Cs,[C|Acc],Valid)
	    end
    end;
input([],Acc,Valid) ->
    {Valid, lists:reverse(Acc), fun input/1}.
	    
    
output(Us) ->
    output(Us, [], true).

output([U|Us], Acc, Valid) when U =< 127 ->
    output(Us, [U|Acc],Valid);
output([U|Us], Acc, Valid) ->
    case U of
	16#00A0 -> output(Us,[16#A0|Acc], Valid); %% NO-BREAK SPACE
	16#0104 -> output(Us,[16#A1|Acc], Valid); %% LATIN CAPITAL LETTER A WITH OGONEK
	16#02D8 -> output(Us,[16#A2|Acc], Valid); %% BREVE
	16#0141 -> output(Us,[16#A3|Acc], Valid); %% LATIN CAPITAL LETTER L WITH STROKE
	16#00A4 -> output(Us,[16#A4|Acc], Valid); %% CURRENCY SIGN
	16#013D -> output(Us,[16#A5|Acc], Valid); %% LATIN CAPITAL LETTER L WITH CARON
	16#015A -> output(Us,[16#A6|Acc], Valid); %% LATIN CAPITAL LETTER S WITH ACUTE
	16#00A7 -> output(Us,[16#A7|Acc], Valid); %% SECTION SIGN
	16#00A8 -> output(Us,[16#A8|Acc], Valid); %% DIAERESIS
	16#0160 -> output(Us,[16#A9|Acc], Valid); %% LATIN CAPITAL LETTER S WITH CARON
	16#015E -> output(Us,[16#AA|Acc], Valid); %% LATIN CAPITAL LETTER S WITH CEDILLA
	16#0164 -> output(Us,[16#AB|Acc], Valid); %% LATIN CAPITAL LETTER T WITH CARON
	16#0179 -> output(Us,[16#AC|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH ACUTE
	16#00AD -> output(Us,[16#AD|Acc], Valid); %% SOFT HYPHEN
	16#017D -> output(Us,[16#AE|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH CARON
	16#017B -> output(Us,[16#AF|Acc], Valid); %% LATIN CAPITAL LETTER Z WITH DOT ABOVE
	16#00B0 -> output(Us,[16#B0|Acc], Valid); %% DEGREE SIGN
	16#0105 -> output(Us,[16#B1|Acc], Valid); %% LATIN SMALL LETTER A WITH OGONEK
	16#02DB -> output(Us,[16#B2|Acc], Valid); %% OGONEK
	16#0142 -> output(Us,[16#B3|Acc], Valid); %% LATIN SMALL LETTER L WITH STROKE
	16#00B4 -> output(Us,[16#B4|Acc], Valid); %% ACUTE ACCENT
	16#013E -> output(Us,[16#B5|Acc], Valid); %% LATIN SMALL LETTER L WITH CARON
	16#015B -> output(Us,[16#B6|Acc], Valid); %% LATIN SMALL LETTER S WITH ACUTE
	16#02C7 -> output(Us,[16#B7|Acc], Valid); %% CARON (Mandarin Chinese third tone)
	16#00B8 -> output(Us,[16#B8|Acc], Valid); %% CEDILLA
	16#0161 -> output(Us,[16#B9|Acc], Valid); %% LATIN SMALL LETTER S WITH CARON
	16#015F -> output(Us,[16#BA|Acc], Valid); %% LATIN SMALL LETTER S WITH CEDILLA
	16#0165 -> output(Us,[16#BB|Acc], Valid); %% LATIN SMALL LETTER T WITH CARON
	16#017A -> output(Us,[16#BC|Acc], Valid); %% LATIN SMALL LETTER Z WITH ACUTE
	16#02DD -> output(Us,[16#BD|Acc], Valid); %% DOUBLE ACUTE ACCENT
	16#017E -> output(Us,[16#BE|Acc], Valid); %% LATIN SMALL LETTER Z WITH CARON
	16#017C -> output(Us,[16#BF|Acc], Valid); %% LATIN SMALL LETTER Z WITH DOT ABOVE
	16#0154 -> output(Us,[16#C0|Acc], Valid); %% LATIN CAPITAL LETTER R WITH ACUTE
	16#00C1 -> output(Us,[16#C1|Acc], Valid); %% LATIN CAPITAL LETTER A WITH ACUTE
	16#00C2 -> output(Us,[16#C2|Acc], Valid); %% LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	16#0102 -> output(Us,[16#C3|Acc], Valid); %% LATIN CAPITAL LETTER A WITH BREVE
	16#00C4 -> output(Us,[16#C4|Acc], Valid); %% LATIN CAPITAL LETTER A WITH DIAERESIS
	16#0139 -> output(Us,[16#C5|Acc], Valid); %% LATIN CAPITAL LETTER L WITH ACUTE
	16#0106 -> output(Us,[16#C6|Acc], Valid); %% LATIN CAPITAL LETTER C WITH ACUTE
	16#00C7 -> output(Us,[16#C7|Acc], Valid); %% LATIN CAPITAL LETTER C WITH CEDILLA
	16#010C -> output(Us,[16#C8|Acc], Valid); %% LATIN CAPITAL LETTER C WITH CARON
	16#00C9 -> output(Us,[16#C9|Acc], Valid); %% LATIN CAPITAL LETTER E WITH ACUTE
	16#0118 -> output(Us,[16#CA|Acc], Valid); %% LATIN CAPITAL LETTER E WITH OGONEK
	16#00CB -> output(Us,[16#CB|Acc], Valid); %% LATIN CAPITAL LETTER E WITH DIAERESIS
	16#011A -> output(Us,[16#CC|Acc], Valid); %% LATIN CAPITAL LETTER E WITH CARON
	16#00CD -> output(Us,[16#CD|Acc], Valid); %% LATIN CAPITAL LETTER I WITH ACUTE
	16#00CE -> output(Us,[16#CE|Acc], Valid); %% LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	16#010E -> output(Us,[16#CF|Acc], Valid); %% LATIN CAPITAL LETTER D WITH CARON
	16#0110 -> output(Us,[16#D0|Acc], Valid); %% LATIN CAPITAL LETTER D WITH STROKE
	16#0143 -> output(Us,[16#D1|Acc], Valid); %% LATIN CAPITAL LETTER N WITH ACUTE
	16#0147 -> output(Us,[16#D2|Acc], Valid); %% LATIN CAPITAL LETTER N WITH CARON
	16#00D3 -> output(Us,[16#D3|Acc], Valid); %% LATIN CAPITAL LETTER O WITH ACUTE
	16#00D4 -> output(Us,[16#D4|Acc], Valid); %% LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	16#0150 -> output(Us,[16#D5|Acc], Valid); %% LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
	16#00D6 -> output(Us,[16#D6|Acc], Valid); %% LATIN CAPITAL LETTER O WITH DIAERESIS
	16#00D7 -> output(Us,[16#D7|Acc], Valid); %% MULTIPLICATION SIGN
	16#0158 -> output(Us,[16#D8|Acc], Valid); %% LATIN CAPITAL LETTER R WITH CARON
	16#016E -> output(Us,[16#D9|Acc], Valid); %% LATIN CAPITAL LETTER U WITH RING ABOVE
	16#00DA -> output(Us,[16#DA|Acc], Valid); %% LATIN CAPITAL LETTER U WITH ACUTE
	16#0170 -> output(Us,[16#DB|Acc], Valid); %% LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
	16#00DC -> output(Us,[16#DC|Acc], Valid); %% LATIN CAPITAL LETTER U WITH DIAERESIS
	16#00DD -> output(Us,[16#DD|Acc], Valid); %% LATIN CAPITAL LETTER Y WITH ACUTE
	16#0162 -> output(Us,[16#DE|Acc], Valid); %% LATIN CAPITAL LETTER T WITH CEDILLA
	16#00DF -> output(Us,[16#DF|Acc], Valid); %% LATIN SMALL LETTER SHARP S (German)
	16#0155 -> output(Us,[16#E0|Acc], Valid); %% LATIN SMALL LETTER R WITH ACUTE
	16#00E1 -> output(Us,[16#E1|Acc], Valid); %% LATIN SMALL LETTER A WITH ACUTE
	16#00E2 -> output(Us,[16#E2|Acc], Valid); %% LATIN SMALL LETTER A WITH CIRCUMFLEX
	16#0103 -> output(Us,[16#E3|Acc], Valid); %% LATIN SMALL LETTER A WITH BREVE
	16#00E4 -> output(Us,[16#E4|Acc], Valid); %% LATIN SMALL LETTER A WITH DIAERESIS
	16#013A -> output(Us,[16#E5|Acc], Valid); %% LATIN SMALL LETTER L WITH ACUTE
	16#0107 -> output(Us,[16#E6|Acc], Valid); %% LATIN SMALL LETTER C WITH ACUTE
	16#00E7 -> output(Us,[16#E7|Acc], Valid); %% LATIN SMALL LETTER C WITH CEDILLA
	16#010D -> output(Us,[16#E8|Acc], Valid); %% LATIN SMALL LETTER C WITH CARON
	16#00E9 -> output(Us,[16#E9|Acc], Valid); %% LATIN SMALL LETTER E WITH ACUTE
	16#0119 -> output(Us,[16#EA|Acc], Valid); %% LATIN SMALL LETTER E WITH OGONEK
	16#00EB -> output(Us,[16#EB|Acc], Valid); %% LATIN SMALL LETTER E WITH DIAERESIS
	16#011B -> output(Us,[16#EC|Acc], Valid); %% LATIN SMALL LETTER E WITH CARON
	16#00ED -> output(Us,[16#ED|Acc], Valid); %% LATIN SMALL LETTER I WITH ACUTE
	16#00EE -> output(Us,[16#EE|Acc], Valid); %% LATIN SMALL LETTER I WITH CIRCUMFLEX
	16#010F -> output(Us,[16#EF|Acc], Valid); %% LATIN SMALL LETTER D WITH CARON
	16#0111 -> output(Us,[16#F0|Acc], Valid); %% LATIN SMALL LETTER D WITH STROKE
	16#0144 -> output(Us,[16#F1|Acc], Valid); %% LATIN SMALL LETTER N WITH ACUTE
	16#0148 -> output(Us,[16#F2|Acc], Valid); %% LATIN SMALL LETTER N WITH CARON
	16#00F3 -> output(Us,[16#F3|Acc], Valid); %% LATIN SMALL LETTER O WITH ACUTE
	16#00F4 -> output(Us,[16#F4|Acc], Valid); %% LATIN SMALL LETTER O WITH CIRCUMFLEX
	16#0151 -> output(Us,[16#F5|Acc], Valid); %% LATIN SMALL LETTER O WITH DOUBLE ACUTE
	16#00F6 -> output(Us,[16#F6|Acc], Valid); %% LATIN SMALL LETTER O WITH DIAERESIS
	16#00F7 -> output(Us,[16#F7|Acc], Valid); %% DIVISION SIGN
	16#0159 -> output(Us,[16#F8|Acc], Valid); %% LATIN SMALL LETTER R WITH CARON
	16#016F -> output(Us,[16#F9|Acc], Valid); %% LATIN SMALL LETTER U WITH RING ABOVE
	16#00FA -> output(Us,[16#FA|Acc], Valid); %% LATIN SMALL LETTER U WITH ACUTE
	16#0171 -> output(Us,[16#FB|Acc], Valid); %% LATIN SMALL LETTER U WITH DOUBLE ACUTE
	16#00FC -> output(Us,[16#FC|Acc], Valid); %% LATIN SMALL LETTER U WITH DIAERESIS
	16#00FD -> output(Us,[16#FD|Acc], Valid); %% LATIN SMALL LETTER Y WITH ACUTE
	16#0163 -> output(Us,[16#FE|Acc], Valid); %% LATIN SMALL LETTER T WITH CEDILLA
	16#02D9 -> output(Us,[16#FF|Acc], Valid); %% DOT ABOVE (Mandarin Chinese light tone)
	_ when U > 255 ->
	    output(Us, [U band 16#ff | Acc], false);
	true ->
	    output(Us, [U | Acc], Valid)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.
