unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TErrorObject = class(Exception);

function ExtractFilePathAndNameWithoutExt(Filename:String):String;
function UnQuoteString(s:string):string;
function getASTfileOfPHPfile(phpFilename:string):string;
Function IsQuotedString(s:string):boolean;
procedure SplitStringAt(s:string; at:string; var left,right:string);
procedure debug(s:string; _append:boolean);
procedure logging(category:string; s:string; _append:boolean);
function ZeroPad(s:string; len:integer):string;
procedure RaiseError(const msg: string);

implementation

function ZeroPad(s:string; len:integer):string;
begin
  while length(s)<len do
    s:='0'+s;

  ZeroPad:=s;
end;

Function ExtractFilePathAndNameWithoutExt(Filename:String):String;
Begin
   ExtractFilePathAndNameWithoutExt := copy(Filename,1,pos(ExtractFileExt(Filename),Filename)-1);
End;

Function UnQuoteString(s:string):string;
Begin
  result:=s;
  if (s[1]='"') and (s[length(s)]='"') then
   result:=copy(s,2,length(s)-2);
End;

Function IsQuotedString(s:string):boolean;
Begin
  result:=False;
  if (s[1]='"') and (s[length(s)]='"') then
   result:=True;


end;

function getASTfileOfPHPfile(phpFilename:string):string;
begin
  getASTfileOfPHPfile := ExtractFilePathAndNameWithoutExt(phpfilename) + '.ast';
end;

procedure SplitStringAt(s:string; at:string; var left,right:string);
var //sa:TStringArray;
    //sa:array of string;
    p:integer;
begin
  p:=pos(at,s);

  if p>0 then begin
    left:=trim(copy(s,1,p-1));
    right:=trim(copy(s,p+1,length(s)-p));
    end
  else begin
    left:=trim(s);
    right:='';

  end;

  // there appears to be a bug in Split

  //if pos(at,s)>0 then begin
  //  sa:=s.Split(at);
  //  left:=trim(sa[0]);
  //  right:=trim(sa[1]);
  //  end
  //else begin
  //  left:=trim(s);
  //  right:='';
  //
  //end;


end;

procedure debug(s:string; _append:boolean);
var f:text;
    filename:string;
begin
  filename:='debug.txt';
  assign(f,filename);
  if _append and (fileexists(filename)) then
    append(f)
  else
    rewrite(f);

  writeln(f,s);
  closefile(f);
end;

procedure logging(category:string; s:string; _append:boolean);
var f:text;
    filename:string;
begin
  filename:=category+'.txt';
  assign(f,filename);
  if _append and (fileexists(filename)) then
    append(f)
  else
    rewrite(f);

  writeln(f,s);
  closefile(f);
end;

procedure RaiseError(const msg: string);
begin
  raise TErrorObject.Create(msg);
end;

end.

