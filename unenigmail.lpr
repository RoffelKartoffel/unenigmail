program unenig;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  baseunix,
  mbox, gpg,
  Classes, SysUtils, CustApp, util, mail
  { you can add units after this };

type


  unenigmail = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    procedure processMailFile(file1: string);
    procedure filter(var mail: TMail);
  end;

{ unenigmail }


Procedure SignalHandler(SigNo: cint); cdecl;
Begin
    //If SigNo = SIGPIPE Then WriteLn('Received SIGPIPE.');
End;


procedure unenigmail.DoRun;
var
  ErrorMsg: String;
begin
  // SIGPIPE-Handler registrieren, damit wir nicht bei abgebrochenen Pipes sterben
  fpSignal(SIGPIPE, @SignalHandler);

  if not isGPGInstalled then
  begin
    WriteLn('gpg not found!');
    halt(1);
  end;

  // quick check parameters
  ErrorMsg:=CheckOptions('hf:', '');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h') or (not HasOption('f')) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;


  WriteLn('Processing file: ', GetOptionValue('f') );
  processMailFile( GetOptionValue('f') );


  // stop program loop
  Terminate;
end;

procedure unenigmail.processMailFile(file1: string);
var
  mbox: T_mbox;
  mail: TMail;
  file2: string;
begin
  file2 := file1 + '.unenigmail';
  mbox := T_mbox.Create(file1, file2);

  while not mbox.isEof() do
  begin
    mail := mbox.getNext();

    // only consider multipart mails
    if mail.isGPGMultipart() then
       filter(mail);

    mbox.writeNext(mail);
    FreeAndNil(mail);
  end;
  mbox.closeAndFlush();

  DeleteFile(file1);
  RenameFile(file2, file1);
end;

procedure unenigmail.filter(var mail: TMail);
const
  PGP_START_MESSAGE = '-----BEGIN PGP MESSAGE-----';
  PGP_END_MESSAGE = '-----END PGP MESSAGE-----';
var
   posStart, posEnd: integer;
   gpg, plain: TStringList;
   line: string;
   headerDone: boolean;
begin
  posStart := 0;
  posEnd := 0;
  gpg := TStringList.Create;

  // locate pgp message
  for line in mail.body do
  begin
    if posStart = 0 then
      posStart := pos(PGP_START_MESSAGE, line);

    if (posStart > 0) and (posEnd = 0) then
      gpg.Add(line);

    if posEnd = 0 then
       posEnd := pos(PGP_END_MESSAGE, line);
  end;

  if gpg.Count > 0 then
  begin
    // try to decrypt
    plain := nil;
    if GPGDecrypt(gpg, plain) then
    begin
        mail.removeXEnigmailHeader();
        mail.removeGPGMultipartHeader();

        // merge plaintext header with old header, copy plaintext body to mail body
        mail.body.Clear;
        headerDone := false;
        for line in plain do
            if not headerDone then
            begin
                 if line = '' then headerDone := true
                              else mail.head.Add(line);
            end
            else
            begin
                 mail.body.Add(line);
            end;

        FreeAndNil(plain);
    end;
  end;

  FreeAndNil(gpg);
end;

constructor unenigmail.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor unenigmail.Destroy;
begin
  inherited Destroy;
end;

procedure unenigmail.WriteHelp;
begin
  { add your help code here }
  WriteLn(programID);
  WriteLn();
  WriteLn('Unenigmail strips Enigmails PGP encryption from emails within a supplied mbox file.');
  WriteLn();
  writeln('Usage: ');
  WriteLn('  -h                this screen');
  WriteLn('  -f <file name>    file to strip');
  WriteLn();
  WriteLn('Example:');
  WriteLn('  unenigmail "~/.thunderbird/.default/Mail/Local Folders/Inbox.sbd/friends"');
end;

var
  Application: unenigmail;
begin
  Application:=unenigmail.Create(nil);
  Application.Run;
  Application.Free;
end.

