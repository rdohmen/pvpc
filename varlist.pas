unit varlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TreeClass, Dialogs,
  utils,
  ContNrs
  ;


type
  TVar = class
  private
    Fvarctx,
    Fvarname : string;
    TAllowedType : string;
  public
    constructor Create(_ctx,_varname:string);
    property ctx:string read Fvarctx;
    property name:string read Fvarname;

  end;


  TVarList = class
  private
    FctxStack : TStringList;
    FVarList: TList;
    FcurrentCtx : string;
    procedure add( _v:Tvar);
  public
    constructor Create(_ctx:string);
    procedure pushContext(_ctx:string);
    procedure popContext;
    procedure introduceVar(_varname:string);
    function introduceTempVar:string;
    function getCtxVarName(_varname:string):string;
    function IndexOf(_varname:string):integer;
    property CurrentCTX: string read FcurrentCtx;
  end;

implementation


constructor TVar.Create(_ctx,_varname:string);
begin
  Fvarctx :=_ctx;
  Fvarname := _varname;
  TAllowedType := '';
end;

constructor TVarList.Create(_ctx:string);
begin
  FctxStack:=TStringList.Create;
  FvarList:=TList.Create;
end;

procedure TVarList.add( _v : Tvar);
begin
  FVarList.add( _v );
end;

procedure TVarList.pushContext(_ctx:string);
begin
  FctxStack.add(_ctx);
  FcurrentCtx:=_ctx;
end;

procedure TVarList.popContext;
var c:integer;
begin
  c:=FctxStack.count;
  FctxStack.delete(c-1);
  FcurrentCtx:=FctxStack[c-2];
end;

procedure TVarList.introduceVar(_varname:string);
var v : Tvar;
begin
  v:=TVar.Create(FCurrentCtx,_varname);
  add(v);
end;

function TVarList.introduceTempVar:string;
var counter:integer;
    tempNumber,
    strCounter,
    prefix,
    varName:string;
    v:TVar;
begin
  // temporary variables are named 'temp001' were 001 is
  // increased for each new temporary variable

  counter:=1;
  tempNumber:='1';
  prefix:='res';
  strCounter:=ZeroPad(tempNumber,3);
  varname:=prefix+strCounter;
  // note
  // using random numbers is faster when the set too choose from is (much) larger
  // than the actual number is tempVars
  // downside is that there is no  sense of logical order is this case

  while Indexof(varName)<>-1 do begin
    inc(counter);
    tempNumber:=intToStr(counter);
    strCounter:=ZeroPad(tempNumber,3);
    varName:=prefix+strCounter;
    end;

  v:=TVar.Create(FCurrentCtx,varname);
  add(v);
  introduceTempVar:=varname
end;


function TVarList.IndexOf(_varname:string):integer;
var c,i,found:integer;
    v:TVar;
begin
  found:=-1;
  c:=FVarList.count;
  for i:=0 to c-1 do begin
    v:= TVar(FvarList[i]);
    if (v.ctx=CurrentCtx) and (v.name=_varname) then
       found:=i;

  end;
  IndexOf:=found;
end;

function TVarList.getCtxVarName(_varname:string):string;
var found:integer;
begin
  // search for _varname in the current ctx
  found:=IndexOf(_varname);

  if found<>-1 Then
     result:=Currentctx+'_'+_varname
  else
    RaiseError('ERROR (TVarList.getCtxVarName) '+_varname+' not found in current context');
end;


end.

