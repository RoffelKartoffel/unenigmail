unit gpg;

{$mode objfpc}{$H+}



interface
uses
  process,
  util,
  Classes, SysUtils;


  function isGPGInstalled(): Boolean;
  function GPGDecrypt(const input: TStringList; var output: TStringList): boolean;


implementation



function isGPGInstalled(): Boolean;
var
  P: TProcess;
  AStringList: TStringList;
begin
  P := TProcess.Create(nil);
  P.CommandLine := 'which gpg';
  P.Options := [poWaitOnExit, poUsePipes];
  P.Execute;

  AStringList := TStringList.Create;
  AStringList.LoadFromStream(P.Output);

  if AStringList.Count > 0 then Result := true
                           else Result := false;

  AStringList.Free;
  P.Free;
end;




function readStdOutErr(P: TProcess;
                        var MOut: TMemoryStream;
                        var MErr: TMemoryStream;
                        var BytesReadOut: LongInt;
                        var BytesReadErr: LongInt;
                        var nOut: LongInt;
                        var nErr: LongInt;
                        const READ_BYTES: LongInt): boolean;
begin
  result := false;

  // stellt sicher, dass wir Platz haben
  MOut.SetSize(BytesReadOut + READ_BYTES);
  MErr.SetSize(BytesReadErr + READ_BYTES);

  // versuche, es zu lesen
  if (P.Output.NumBytesAvailable >0) then
  begin
    nOut := P.Output.Read((MOut.Memory + BytesReadOut)^, READ_BYTES);
    if nOut > 0 then Inc(BytesReadOut, nOut);
    result := true;
  end;

  if (P.Stderr.NumBytesAvailable >0) then
  begin
    nErr := P.Stderr.Read((MErr.Memory + BytesReadErr)^, READ_BYTES);
    if nErr > 0 then Inc(BytesReadErr, nErr);
    result := true;
  end;
end;






function GPGDecrypt(const input: TStringList; var output: TStringList): boolean;
const
  READ_BYTES = 2048;
  BOUNDARY = 'boundary="';
  BOUNDARY_LEN = Length(BOUNDARY);
var
  MOut, MErr: TMemoryStream;
  P: TProcess;
  nOut, nErr: LongInt;
  BytesReadOut, BytesReadErr: LongInt;
  stdOut, stdErr: TStringList;
  n, funde: integer;
  str, tmpStr, str2, bound: string;
  headerDone: boolean;
begin
  P := TProcess.Create(nil);
  P.CommandLine := 'gpg --decrypt';
  P.Options := [poUsePipes];
  P.Execute;

  MOut := TMemoryStream.Create;
  MErr := TMemoryStream.Create;
  BytesReadOut := 0;
  BytesReadErr := 0;
  nOut := 0;
  nErr := 0;

  // Eingabe an gpg pipen
  for str in input do
  begin
    // wenn wir große Daten schreiben, muessen wir auch immer lesen
    readStdOutErr(P, MOut, MErr, BytesReadOut, BytesReadErr, nOut, nErr, READ_BYTES );

    tmpStr := str + #10;
    P.Input.Write(tmpStr[1], Length(tmpStr));
  end;

  // gpg signalisieren, zum Ende zu kommen
  P.CloseInput;

  // Ausgabe lesen

  // Wir können poWaitOnExit hier nicht nutzen, weil wir die
  // Größe des Outputs nicht kennen. In Linux ist die Größe der
  // Pipe 2kB. Wenn die Ausgabe größer ist, müssen die Daten
  // zwischenzeitlich ausgelesen werden. Dies ist nicht möglich,
  // wenn auf das Ende gewartet wird - ein Deadlock tritt auf.
  //
  // Ein temporärer Memorystream wird verwendet, um den output zu puffern.
  repeat
  begin
    if readStdOutErr(P, MOut, MErr, BytesReadOut, BytesReadErr, nOut, nErr, READ_BYTES ) then
      Sleep(1);
  end
  until (not P.Running) and
        (P.Output.NumBytesAvailable = 0) and
        (P.Stderr.NumBytesAvailable = 0);


  // entferne ueberfluessigen Platz
  MOut.SetSize(BytesReadOut);
  MErr.SetSize(BytesReadErr);

  // Memorystream -> StringList
  stdOut := TStringList.Create;
  stdOut.LoadFromStream(MOut);
  stdErr := TStringList.Create;
  stdErr.LoadFromStream(Merr);

  FreeAndNil(P);
  FreeAndNil(MOut);
  FreeAndNil(MErr);

  // Entschluesseln fehlgeschlagen?
  if stdOut.Count = 0 then
  begin
    Result := false;
    FreeAndNil(stdOut);
    FreeAndNil(stdErr);
    exit;
  end;


  addUnenigmailSignature(stdErr);
  // meta data is not properly encoded yet
  for n := 0 to stdErr.Count -1 do
      stdErr[n] := encodeAsQuotedPrintable(stdErr[n]);


  // Ist der Klartext multipart? Nach Multipart-Boundary suchen
  n := 0;
  for str in stdOut do
  begin
     // Header zuende?
     if str = '' then break;

     n := pos(BOUNDARY, str);
     if n > 0 then
     begin
       // Boundary ausschneiden
       bound := str;
       Delete(bound, 1, n -1 + BOUNDARY_LEN );
       Delete(bound, Length(bound), 1);
       bound := bound;
     end;
  end;

  if n > 0 then
  begin
    // Multipart...

    // Metadaten und Klartext zusammenbauen
    output := TStringList.Create;
    funde := 0;
    headerDone := false;
    for str in stdOut do
    begin
        if not headerDone then
        begin
             if str = '' then headerDone := true;
        end
        else
        begin
            if pos(bound, str) > 0 then inc(funde);

            // Metadaten am Ende des ersten Multiparts einfuegen
            if funde = 2 then
            begin
              inc(funde);

              // Metadaten einfuegen
              output.Add('');
              output.Add('');
              output.Add('');
              for str2 in stdErr do
                  output.Add(str2);

              // Newline zum Abschluss des Multiparts
              output.Add('');
            end;


        end;
        output.Add(str);
    end;
  end
  else
  begin
      // kein Multipart, also Konkatinieren wir einfach nur

      output := TStringList.Create;
      for str in stdOut do
          output.Add(str);

      // Metadaten einfuegen
      output.Add('');
      output.Add('');
      output.Add('');
      for str2 in stdErr do
          output.Add(str2);

      // Newline zum Abschluss des Multiparts
      output.Add('');
  end;


  FreeAndNil(stdOut);
  FreeAndNil(stdErr);
  Result := true;

   //WriteLn('+');
end;




end.

