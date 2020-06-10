%%
%% Some UTF constants needed
%%
-define(UNI_REPLACEMENT_CHAR, 16#0000FFFD).
-define(UNI_MAX_BMP,          16#0000FFFF).
-define(UNI_MAX_UTF16,        16#0010FFFF).
-define(UNI_MAX_UTF32,        16#7FFFFFFF).
-define(UNI_MAX_LEGAL_UTF32,  16#0010FFFF).

-define(UNI_SUR_HIGH_START,   16#D800).
-define(UNI_SUR_HIGH_END,     16#DBFF).
-define(UNI_SUR_LOW_START,    16#DC00).
-define(UNI_SUR_LOW_END,      16#DFFF).
