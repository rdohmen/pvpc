unit FunctionList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  utils,
  ParamList;

type
  TFunction = class
  private
    Ffunctionname: string;
    //Fctx,
    Fparamlist: TParamList;
    Fbuildin: boolean;
  public
    constructor Create(_functionname: string; _paramlist: string; _buildin: boolean);
    //property ctx:string read Fctx;
    property functionname: string read Ffunctionname;
    property paramlist: Tparamlist read Fparamlist;


  end;


  TFunctionList = class
  private
    FFunctionList: TList;
    procedure add(_functionname, _paramlist: string; _buildin: boolean);
  public
    constructor Create;

    procedure addUserFunction(_functionname, _paramlist: string);
    procedure addBuildinFunction(_functionname, _paramlist: string);

    function indexOf(_functionname: string): integer;
    function getParamListOfFunction(_functionname: string): TParamList;

    // get paramterlist with context put in front of each parameter
    function get_CTX_ParamListOfFunction(_functionname: string): Tparamlist;

    procedure addBuildInPHPfunction;
    //constructor Create;
    //procedure addConnection(n: TCFGNode);
  end;



implementation

constructor TFunction.Create(_functionname: string; _paramlist: string;
  _buildin: boolean);
begin
  Ffunctionname := _functionname;
  //Fctx,
  Fparamlist := TParamList.Create(_paramlist);
  Fbuildin := _buildin;
end;

constructor TFunctionList.Create;
begin
  FFunctionList := TList.Create;

  addBuildInPHPfunction;
end;

procedure TFunctionList.addBuildInPHPfunction;
begin
  addBuildinFunction('max','a,b');
  addBuildinFunction('min','a,b');

end;

procedure TFunctionList.add(_functionname, _paramlist: string; _buildin: boolean);
var
  f: TFunction;
begin
  if indexOf(_functionname) = -1 then
  begin
    f := TFunction.Create(_functionname, _paramlist, _buildin);
    FFunctionList.add(f);
  end
  else
    //raiseError('ERROR (TFunctionList.add): trying to add a function with the name '+_functionname+', but this function was all ready in the list. Multiple functions with the same name are not allowed in PHP.');
    ShowMessage('ERROR (TFunctionList.add): trying to add a function with the name ' +
      _functionname +
      ', but this function was all ready in the list. Multiple functions with the same name are not allowed in PHP. Only the first occurence was added to the CFG-world');
end;

procedure TFunctionList.addUserFunction(_functionname, _paramlist: string);
begin
  add(_functionname, _paramlist, False);
end;

procedure TFunctionList.addBuildinFunction(_functionname, _paramlist: string);
begin
  add(_functionname, _paramlist, True);
end;




function TFunctionList.indexOf(_functionname: string): integer;
var
  c, i, found: integer;
  f: TFunction;
begin
  found := -1;
  c := FFunctionList.Count;
  for i := 0 to c - 1 do
  begin
    f := TFunction(FFunctionList[i]);
    if {(v.ctx=CurrentCtx) and} (f.FFunctionname = _functionname) then
      found := i;
  end;
  IndexOf := found;
end;

function TFunctionList.getParamListOfFunction(_functionname: string): Tparamlist;
var
  i: integer;
  PL : TParamList;
begin
  i := IndexOf(_functionname);
  if i <> -1 then
    Result := TFunction(FFunctionList[i]).Fparamlist
  else begin
    PL:=TParamList.Create;
    Result:=PL;

    logging('missing_functions', _functionname, true);

    //RaiseError(
    //  'ERROR (TFunctionList.getParamListOfFunction): Trying to retrieve the paramlist of a function ('
    //  +
    //  _functionname + ') that is not in the Functionlist');
    end;

end;

function TFunctionList.get_CTX_ParamListOfFunction(_functionname: string): Tparamlist;
var
  i, j, c: integer;
  PL,paramlist_copy: TParamlist;
  paramlist_as_stringlist: TStringList;

begin
  i := IndexOf(_functionname);
  if i <> -1 then
  begin
    // create a copy of the parameterlist, put the context (functionname) in front of the parameter

    paramlist_as_stringlist := TStringList.Create;
    paramlist_as_stringlist.DelimitedText :=
      getParamListOfFunction(_functionname).toString;

    // put functioname in front of all parameters
    c := paramlist_as_stringlist.Count;
    for j := 0 to c - 1 do
      paramlist_as_stringlist[j] := _functionname + paramlist_as_stringlist[j];

    paramlist_copy := TParamList.Create;
    paramlist_copy.fromString(paramlist_as_stringlist.DelimitedText);

    Result := paramlist_copy;
  end
  else begin
    PL:=TParamList.Create;
    Result:=PL;

    logging('missing_functions', _functionname, true);
    //RaiseError(
    //  'ERROR (TFunctionList.get_CTX_ParamListOfFunction): Trying to retrieve the paramlist of a function ('
    //  +
    //  _functionname + ') that is not in the Functionlist');

  end;
end;


end.
