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

    fFileIsBinary: Boolean;
    dataBuffered: boolean;
    getNextBuffer: string;
    head: TStringList;
    body: TStringList;

    procedure readNextToBuffer();
    function fileStartsWithBinary(path: string): Boolean;
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
  fFileIsBinary := fileStartsWithBinary(fileIn);

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

function T_mbox.fileStartsWithBinary(path: string): Boolean;
var
   handle: file;
   buf : Array[1..2048] of byte;
   total : smallint;
   i: integer;
begin
   AssignFile(handle, path);
   reset(handle, 1);
   BlockRead(handle ,buf, sizeof(buf), total);
   close(handle);

   for i := 1 to total do
   begin
      if (buf[i] < 32) then
      begin
        // \r \n \t
        if (buf[i] = 10) or (buf[i] = 13) or (buf[i] = 9) then continue;
        result := true;
        exit;
      end;
   end;

   result := false;
end;

procedure T_mbox.readNextToBuffer();
var
   tmp: string;
   len: integer;
begin
  if dataBuffered = True then exit;

  head := TStringList.Create;
  if getNextBuffer <> '' then head.Add(getNextBuffer);
  len := 0;
  while not Eof(fFileIn) do
  begin
    ReadLn(fFileIn, tmp);
    if tmp = '' then break;
    head.Add(tmp);

    inc(len, Length(tmp));
    if (len > 100 * 1024 * 1024) then
      raise EExternal.Create('mail exceeded 100mb.');
  end;

  if head.Count = 0 then
    raise EExternal.Create('mbox is malformed.');
  if (copy(head[0], 1, 5) <> 'From ') then
    raise EExternal.Create('mbox is malformed.');

  body := TStringList.Create;
  tmp := '';
  len := 0;
  while not Eof(fFileIn) do
  begin
    ReadLn(fFileIn, tmp);
    if (copy(tmp, 1, 5) = 'From ') then break;
    body.Add(tmp);

    inc(len, Length(tmp));
    if (len > 100 * 1024 * 1024) then
      raise EExternal.Create('mail exceeded 100mb.');
  end;
  getNextBuffer := tmp;

  dataBuffered := True;
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
  if fFileIsBinary then
  begin
    result := true;
    exit;
  end;

  Result := Eof(fFileIn);
end;






end.

