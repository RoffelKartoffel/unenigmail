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
   tmp, lastLine: string;
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

  body := TStringList.Create;
  tmp := '';
  lastLine := 'foobar'; // != empty line
  while not Eof(fFileIn) do
  begin
    ReadLn(fFileIn, tmp);
    //if (lastLine = '') and (copy(tmp, 1, 5) = 'From ') then break;
    if (copy(tmp, 1, 5) = 'From ') then break;
    body.Add(tmp);
    lastLine := tmp;
  end;
  getNextBuffer := tmp;

  dataBuffered := true;
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

