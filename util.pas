unit util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function encodeAsQuotedPrintable(const s: string): string;
function programID(): string;
procedure addUnenigmailSignature(var meta: TStringList);

implementation


function programID(): string;
begin
  result := 'unenigmail v0.01';
end;


procedure addUnenigmailSignature(var meta: TStringList);
var
  tmp: TStringList;
  line: String;
begin
  tmp := meta;
  meta := TStringList.Create;

  meta.Add('------------------------------------------------------------------------------');
  meta.Add('--------- unenigmail v0.01 ---------------------------------------------------');
  meta.Add('------------------------------------------------------------------------------');

  for line in tmp do
      meta.Add(line);

  meta.Add('------------------------------------------------------------------------------');

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


end.

