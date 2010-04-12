{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2005 National Technical University of Athens      }
{                                                                  }
{******************************************************************}

{** Locale functions.
}
unit ilocales;

interface

{** Returns a encoding string suitable for set the client encoding of
    PostgreSQL.
    GetEncodingStr reads the user loacale by calling the system function
    GetUserDefaultLangID.
    If the locale is not implemented within the function, it returns a
    ISO_8859_1 encoding.
    See
    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/intl/nls_2lgk.asp
}
function GetEncodingStr: string;

type
  TDBEncoding = (DBENLocaleDefault = 0, DBENGreekISO = 1,
    DBENLatinISO = 2, DBENCyrillicISO = 3);

const DatabaseEncodingsDescriptions: array[TDBEncoding] of string =
  ('Default encoding defined by the locale', 'Greek ISO encoding (ISO-8859-7)',
     'Latin ISO encoding (ISO-8859-1)', 'Cyrillic ISO encoding (ISO-8859-5)');

const DatabaseEncodings: array[TDBEncoding] of string =
  ('UTF-8', 'ISO_8859_7', 'ISO_8859_1', 'ISO_8859_5');

implementation

uses Windows;

{$IFNDEF LANG_MACEDONIAN}
  {$DEFINE LANG_MACEDONIAN}
  const LANG_MACEDONIAN = $2f;
{$ENDIF}

function GetEncodingStr: string;
var
  ALan: Word;
begin
  ALan := (GetUserDefaultLangID and $03FF);
  case ALan of
    LANG_ENGLISH: Result := 'ISO_8859_1';
    LANG_GREEK: Result := 'ISO_8859_7';
    LANG_RUSSIAN, LANG_SERBIAN, LANG_UKRAINIAN, LANG_BELARUSIAN, LANG_BULGARIAN,
      LANG_MACEDONIAN:
      Result := 'ISO_8859_5';
  else
    Result := 'ISO_8859_1';
  end;
end;

end.
