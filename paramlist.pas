unit ParamList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, Dialogs;

type
  TParamList = class
  private
    Fparams : TStringList;
    Tbuildin : boolean;
    function getCount:integer;
  public
    constructor Create;
    constructor Create(_ParamListAsString:string);
    procedure add( _paramname:string);
    function toString:string;
    function fromString(_ParamListAsString:string):string;
    function getParamByIndex( _n:integer):string;

    property Count: integer read GetCount;
  end;


implementation

constructor TParamList.Create;
begin
  Fparams:=TStringList.Create;
  Fparams.Clear;
  Fparams.Delimiter       := ',';
  Fparams.StrictDelimiter := True;
end;

constructor TParamList.Create(_ParamListAsString:string);
begin
  Fparams:=TStringList.Create;
  Fparams.Clear;
  Fparams.Delimiter       := ',';
  Fparams.StrictDelimiter := True;
  Fparams.DelimitedText   := _ParamListAsString;
  //showmessage( inttostr(fparams.count));
End;


function TParamList.getCount:integer;
Begin
  result:=Fparams.count;
end;

procedure TParamList.add( _paramname:string);
begin
  Fparams.add( _paramname);
end;

function TParamList.toString:string;
begin
  tostring:=Fparams.DelimitedText;
end;

function TParamList.fromString(_ParamListAsString:string):string;
begin
  Fparams.DelimitedText   := _ParamListAsString;
end;

function TParamList.getParamByIndex( _n:integer):string;
begin
  if (_n>=0) and (_n<Fparams.Count) Then
    result := Fparams[ _n  ]
  Else
    RaiseError('ERROR (TParamList.getParamByNumber): Index not 0<=index<count');
end;



end.

