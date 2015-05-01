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

TProcessedStats = record
  total, gpg, gpg_success : integer;
end;


  unenigmail = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    procedure processFileOrFolder(pathIn: string; var stats: TProcessedStats);
    procedure processMailFile(file1: string; var stats: TProcessedStats);
    procedure filter(var mail: TMail; var stats: TProcessedStats);
  end;

{ unenigmail }


Procedure SignalHandler(SigNo: cint); cdecl;
Begin
    //If SigNo = SIGPIPE Then WriteLn('Received SIGPIPE.');
End;


procedure unenigmail.DoRun;
var
  ErrorMsg: String;
  stats: TProcessedStats;
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

  stats.total := 0;
  stats.gpg := 0;
  stats.gpg_success := 0;
  processFileOrFolder( GetOptionValue('f'), stats );

  WriteLn();
  WriteLn('summary:');
  WriteLn('    mails: ':7 ,  IntToStr(stats.total):7,
    '    with gpg:':7,IntToStr(stats.gpg):7,
    '    decrypted:':7 ,IntToStr(stats.gpg_success):7 );

  // stop program loop
  Terminate;
end;

procedure unenigmail.processFileOrFolder(pathIn: string; var stats: TProcessedStats);
var
  statsTmp: TProcessedStats;
  Info : TSearchRec;
  path: string;
  files: array of string;
  len: integer;
begin
  if (FileGetAttr(pathIn) and faDirectory) <> 0 then
  begin // it's a folder
    // search all files and folders whithin given folder
    SetLength(files, 0);
    If FindFirst ( pathIn + PathDelim + '*',faAnyFile and faDirectory,Info)=0 then
    begin
      Repeat
        With Info do
        begin
          If (Attr and faDirectory) = faDirectory then
            if (Name = '.') or (Name = '..') then
               continue;

          len := Length(files);
          SetLength(files, len+1);
          files[len] := Name;
        end;
      Until FindNext(info)<>0;
    end;
    FindClose(Info);

    for path in files do
      processFileOrFolder(pathIn + PathDelim + path, stats);
  end
  else
  begin  // it's a file
    processMailFile( pathIn, statsTmp );

    if statsTmp.total > 0 then
    begin
      WriteLn( pathIn );
      WriteLn('    mails: ':7 ,  IntToStr(statsTmp.total):7,
          '    with gpg:':7,IntToStr(statsTmp.gpg):7,
          '    decrypted:':7 ,IntToStr(statsTmp.gpg_success):7 );
    end;

    Inc(stats.total, statsTmp.total);
    Inc(stats.gpg, statsTmp.gpg);
    Inc(stats.gpg_success, statsTmp.gpg_success);
  end;

end;

procedure unenigmail.processMailFile(file1: string; var stats: TProcessedStats);
var
  mbox: T_mbox;
  mail: TMail;
  file2: string;
begin
  stats.total := 0;
  stats.gpg := 0;
  stats.gpg_success := 0;

  file2 := file1 + '.unenigmail';
  mbox := T_mbox.Create(file1, file2);

  try
    while not mbox.isEof() do
    begin
      mail := mbox.getNext();
      Inc(stats.total);

      filter(mail, stats);

      mbox.writeNext(mail);
      FreeAndNil(mail);
    end;
  except

  end;

  mbox.closeAndFlush();

  if stats.gpg_success > 0 then
  begin
    DeleteFile(file1);
    RenameFile(file2, file1);
  end
  else
  begin
    DeleteFile(file2);
  end;
end;

procedure unenigmail.filter(var mail: TMail; var stats: TProcessedStats);
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
      gpg.Add( decodeQuotedPrintable(line) );

    if posEnd = 0 then
       posEnd := pos(PGP_END_MESSAGE, line);
  end;

  if gpg.Count > 0 then
  begin
    // try to decrypt
    Inc(stats.gpg);
    plain := nil;
    if GPGDecrypt(gpg, plain) then
    begin
        Inc(stats.gpg_success);

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
  WriteLn('Note: This is only recommended if you store your emails in a secure environment. For example an mbox on your local disk with disk encrypion.');

  WriteLn();
  writeln('Usage: ');
  WriteLn('  -h                            this screen');
  WriteLn('  -f <file name/folder name>    file or folder with files to strip');
  WriteLn();
  WriteLn('Example (single File):');
  WriteLn('  unenigmail -f "/home/me/.thunderbird/.default/Mail/Local Folders/Inbox.sbd/friends"');
  WriteLn('Example (folder):');
  WriteLn('  unenigmail -f "/home/me/.thunderbird/.default/Mail/Local Folders/"');

end;

var
  Application: unenigmail;
begin
  Application:=unenigmail.Create(nil);
  Application.Run;
  Application.Free;
end.

