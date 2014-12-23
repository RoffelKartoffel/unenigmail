unit mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TMail = class
  private
    fhead : TStringList;
    fbody : TStringList;
  protected
  public


    Destructor Destroy(); override;

    property head: TStringList read fhead write fhead;
    property body: TStringList read fbody write fbody;

    function multipartBoundary(): string;
    function isGPGMultipart(): boolean;
    procedure removeGPGMultipartHeader();
    procedure removeXEnigmailHeader();

  End;

implementation


Destructor TMail.Destroy();
begin
  FreeAndNil(fhead);
  FreeAndNil(fbody);
end;

function TMail.multipartBoundary(): string;
const
   BOUNDARY = ' boundary="';
   BOUNDARY_LEN = Length(BOUNDARY);
var
   i: integer;
   str: string;
begin
   Result := '';
   for i := 0 to head.Count -1 -2 do
     if (head[i] = 'Content-Type: multipart/encrypted;') and
        (head[i+1] = ' protocol="application/pgp-encrypted";') and
        (copy(head[i+2], 1, BOUNDARY_LEN) = BOUNDARY) then
     begin
       str := head[i+2];
       Delete(str, 1, BOUNDARY_LEN);
       Delete(str, Length(str), 1);
       Result := str;
       break;
     end;
end;



procedure TMail.removeXEnigmailHeader();
var
   i: integer;
begin
  for i := 0 to head.Count -1 do
    if pos('X-Enigmail', head[i]) >0 then
    begin
      head.Delete(i);
      break;
    end;

end;

procedure TMail.removeGPGMultipartHeader();
var
   line: string;
   i,j,n: integer;
begin
  for i := 0 to head.Count -1 do
    if (head[i] = 'Content-Type: multipart/encrypted;') then
    begin
      // locate last line belonging to multipart entry
      for j := i+1 to head.Count -1 do
         if head[j][1] <> ' ' then break;
      // delete all multipart lines
      for n := j-1 downto i do
         head.Delete(n);
      break;
    end;
end;


function Tmail.isGPGMultipart(): boolean;
begin
  if Length(multipartBoundary()) > 0 then Result := true
                                     else Result := false;
end;




end.

