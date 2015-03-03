unit mbox;

{$mode objfpc}{$H+}

interface

uses
  contnrs,
  mail,
  Classes, SysUtils;


Type
  T_mbox = class
  private
    fFileIn, fFileOut : TextFile;

    dataBuffered: boolean;
    getNextBuffer: string;
    head: TStringList;
    body: TStringList;

    procedure readNextToBuffer();
  protected
  public

    Constructor Create(fileIn: string; fileOut: string);
    Destructor Destroy(); override;
    function getNext(): TMail;
    function isEof(): boolean;
    procedure writeNext(mail: TMail);
    procedure closeAndFlush();

  End;


implementation

Destructor T_mbox.Destroy();
begin
  closeAndFlush();
  FreeAndNil(head);
  FreeAndNil(body);
  inherited;
end;

Constructor T_mbox.Create(fileIn: string; fileOut: string);
begin
  AssignFile(fFileIn, fileIn);
  Reset(fFileIn);

  AssignFile(fFileOut, fileOut);
  ReWrite(fFileOut);

  dataBuffered := false;
end;

procedure T_mbox.closeAndFlush();
begin
  CloseFile(fFileIn);
  CloseFile(fFileOut);
end;

procedure T_mbox.readNextToBuffer();
var
   tmp: string;
begin
  if dataBuffered = True then exit;

  head := TStringList.Create;
  if getNextBuffer <> '' then head.Add(getNextBuffer);
  while not Eof(fFileIn) do
  begin
    ReadLn(fFileIn, tmp);
    if tmp = '' then break;
    head.Add(tmp);
  end;

  if head.Count = 0 then
    raise EExternal.Create('mbox is malformed.');
  if (copy(head[0], 1, 5) <> 'From ') then
    raise EExternal.Create('mbox is malformed.');

  body := TStringList.Create;
  tmp := '';
  while not Eof(fFileIn) do
  begin
    ReadLn(fFileIn, tmp);
    if (copy(tmp, 1, 5) = 'From ') then break;
    body.Add(tmp);
  end;
  getNextBuffer := tmp;

  if body.Count = 0 then
    raise EExternal.Create('mbox is malformed.');
end;

function T_mbox.getNext(): TMail;
begin
  if not dataBuffered then readNextToBuffer();

  Result := TMail.Create;
  Result.head := head;
  Result.body := body;
  dataBuffered := false;
end;

procedure T_mbox.writeNext(mail: TMail);
var
   line: string;
begin
  for line in mail.head do
      WriteLn(fFileOut, line);
  Write(fFileOut, #10);
  for line in mail.body do
    WriteLn(fFileOut, line);
end;

function T_mbox.isEof(): boolean;
begin
  Result := Eof(fFileIn);
end;






end.

