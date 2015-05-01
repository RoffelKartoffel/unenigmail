unit util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function encodeAsQuotedPrintable(const s: string): string;
function decodeQuotedPrintable(const s: string): string;
function programID(): string;
procedure addUnenigmailSignature(var meta: TStringList);

implementation


function fillLine(input: string; filler: char; count: integer): string;
var
  missing, i: integer;
begin
  result := input;
  missing := count - Length(input);
  for i := 1 to missing do
    result := result + filler;
end;

function programID(): string;
begin
  result := 'unenigmail ' + FloatToStrF({$I VERSION},ffGeneral, 2, 0);
end;

procedure addUnenigmailSignature(var meta: TStringList);
var
  tmp: TStringList;
  line: String;
begin
  tmp := meta;
  meta := TStringList.Create;

  meta.Add( fillLine('', '-', 90) );
  meta.Add( fillLine('--------- ' + programID() + ' ', '-', 90) );
  meta.Add( fillLine('', '-', 90) );

  for line in tmp do
      meta.Add(line);

  meta.Add( fillLine('', '-', 90) );

  FreeAndNil(tmp);
end;

function encodeAsQuotedPrintable(const s: string): string;
var
  i: integer;
begin
  Result := '';
  i := 0;
  while i < Length(s) do
  begin
    inc(i);

    // is no ascii character
    if (ord(s[i]) >= 128) or (s[i] = '=') then
    begin
        Result := Result + '=' + IntToHex(ord(s[i]), 2) + '=' + IntToHex(ord(s[i+1]), 2);
        inc(i);
    end
    else
        Result := Result + s[i];
  end;
end;

function decodeQuotedPrintable(const s: string): string;
begin
  // was to lazy to code a real implementation
  Result := StringReplace(s, '=3D', '=', [rfReplaceAll]);
end;


end.

