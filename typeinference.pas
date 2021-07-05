unit TypeInference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, utils
  ;

type
  TTypeOperator = string;
  TTypeClass = class;
  TTypeSet = class;

  TTypeTree = class
  private
    FList : TList;
  public
    constructor create;
    procedure add( T:TTypeClass );
  end;



  TTypeClass = class
  private
    FTypeName : String;
    parent:TTypeClass;
    Fchildren : TList;
  public
    constructor Create(TypeName:string; childof:TTypeClass);
    function elementOf( TS:TTypeSet ):boolean;
    function is_a( T:TTypeClass ):boolean;
    function is_identical( T : TTypeClass):boolean;
    procedure add_child( T:TTypeClass);
    function is_child( T: TTypeClass):boolean;
    function children:TTypeSet;
    //procedure remove( T:TTypeClass );

    property TypeName: String read FTypename;
  end;

  TTypeSet = class
  private
    Flist : TList;
  public
    constructor Create;
    procedure add( T : TTypeClass );
    function Contains( T:TTypeClass ):boolean;
    function has_a(T:TtypeClass):boolean;
    function isSubSetOf( TS:TTypeSet):boolean;

    procedure Union( TS:TTypeSet);

    procedure Intersection( TS:TTypeSet);

    function getListOfTypes:TList;
    procedure remove( T:TTypeClass );
    procedure RemoveSet( TS:TTypeSet );


    procedure promote;
    function toString:string;
  end;

  TTypeInferenceRule = class
  private
    // operator
    FOperator: TTypeOperator;
    // typesets voor operands E1 (and for binary operators E2 (and for ternary operators E3))
    FE1_pre,
    FE2_pre,
    FE3_pre,
    FE1_post,
    FE2_post,
    FE3_post: TTypeSet;

  public
    constructor Create(_to: TTypeOperator; _pre1, _pre2, _pre3, _post1, _post2, _post3:TTypeSet);
    function getOperator:TTypeOperator;

  end;

  TInferenceRuleList = class
  private
    FList : Tlist;
    function IndexOf(op:TTypeOperator): integer;
  public
    constructor Create;
    procedure add( R : TTypeInferenceRule );
    function InferUnary(op:TTypeOperator; E1:TTypeSet):TTypeSet;
    function InferBinary(op:TTypeOperator; E1,E2:TTypeSet):TTypeSet;
    function InferTernary(op:TTypeOperator; E1,E2,E3:TTypeSet):TTypeSet;


  end;

implementation


constructor TTypeClass.Create(Typename:string; childof:TTypeClass);
begin
  FTypeName := TypeName;
  Fchildren:=TList.Create;
  if assigned( childof ) then begin
    parent:=childOf;
    childOf.add_child( self );
    end;
  end;

function TTypeClass.elementOf( TS:TTypeSet ):boolean;
begin
  result:=TS.has_a( self );
  end;

function TTypeClass.is_a( T:TTypeClass ):boolean;
var c,i:integer;
begin
  result:=is_identical( T ) or T.elementOf( Self.Children );
  end;

function TTypeClass.is_identical( T : TTypeClass):boolean;
begin
  result := (Self.Typename = T.Typename);
  end;


constructor TTypeTree.Create;
begin
  FList := TList.Create;
end;

procedure TTypeTree.add( T : TTypeClass );
begin
  FList.add( T );
end;

constructor TTypeSet.Create;
begin
  Flist:=TList.create;
  end;

procedure TTypeClass.add_child( T:TTypeClass);
Begin
  FChildren.add( T );
end;

function TTypeClass.is_child( T: TTypeClass):boolean;
var e: TTypeClass;
    c,i:integer;
Begin
  result:=false;
  c:=FChildren.count;
  for i:=0 to c-1 do begin
    e:=TTypeClass( FChildren[i] );
    if e.is_identical( T ) then
      result:=True;
    end;
end;

function TTypeClass.children:TTypeSet;
var TS:TTypeSet;
    c,i:integer;
Begin
  TS:=TTypeSet.Create;
  c:=FChildren.Count;
  For i:=0 to c-1 do
    TS.add(TTypeClass(FChildren[i]));
  result:=TS;
end;



procedure TTypeSet.add( T : TTypeClass );
begin
  Flist.add( T );
  end;

function TTypeSet.Contains( T:TTypeClass ):boolean;
Var e:TTypeClass;
    c,i:integer;
Begin
  Result:=False;
  c:=FList.count;
  for i:=0 to c-1 do begin
    e:=TTypeClass(Flist[i]);
    If e.is_identical( T ) {or e.elementOf(T.children)} Then
      Result := True;
    end;
  End;

function TTypeset.has_a( T:TTypeClass ) : boolean;
var e:TTypeClass;
    c,i:integer;
begin
  result:=false;
  c:=FList.count;
  for i:=0 to c-1 do begin
    e:=TTypeClass(FList[i]);
    if T.is_identical(e) or T.elementOf(e.children) then
      result:=True;
    end;
  end;

function TTypeSet.isSubSetOf( TS:TTypeSet):boolean;
var c,i:integer;
Begin
  result:=true;
  c:=FList.Count;
  for i:=0 to c-1 do
    if not( TTypeClass(FList[i]).elementOf( TS ) ) then
      result:=false;
End;

procedure TTypeSet.Union( TS:TTypeSet);
var c,i:integer;
    e:TTypeClass;
begin
  c:=FList.Count;
  for i:=0 to c-1 do begin
    e:=TTypeClass(FList[i]);
    if not TS.contains( e )  then
      TS.add( TTypeClass(FList[i]) );

  end;

  FList.assign(TS.getListOfTypes);
end;


procedure TTypeSet.Intersection( TS:TTypeSet);
var c,i:integer;
    e:TTypeClass;
begin
  c:=FList.Count;
  for i:=c-1 downto 0 do begin
    e:=TTypeClass(FList[i]);
    if not( e.elementOf( TS ) ) then
      remove( e );

  end;
end;

procedure TTypeSet.remove( T:TTypeClass );
var c,i:integer;
begin
  c:=Flist.count;
  FList.Remove( T );
end;

procedure TTypeSet.RemoveSet( TS:TTypeSet );
var c,i:integer;
    e:TTypeClass;
Begin
  c:=FList.count;
  for i:=c-1 downto 0 do begin
    e:=TTypeClass(FList[i]);
    if TS.contains(e) then
      remove(e);
    end;

end;

function TTypeSet.getListOfTypes:TList;
begin
  result:=FList;
end;

function TTypeSet.toString:string;
var e,p:TTypeClass;
    c,i:integer;
    s:string;
begin
  s:='';
  c:=FList.count;
  for i:=0 to c-1 do begin
    e:=TTypeClass(FList[i]);
    if s<>'' then s:=s+',';
    s:=s+e.TypeName;
  end;

  result:=s;
end;

procedure TTypeSet.promote;
var e,p:TTypeClass;
    c,i:integer;
begin
  //showmessage(tostring);
  c:=FList.count;
  for i:=0 to c-1 do begin
    e:=TTypeClass(FList[i]);
    p:=e.parent;
    if IsSubSetOf( p.children ) then begin
      // add the parent
      add( p );
      end;
  end;
  // remove the children of p
  if assigned(p) then RemoveSet( p.children );
  //showmessage(tostring);
end;

constructor TTypeInferenceRule.Create(_to: TTypeOperator; _pre1, _pre2, _pre3, _post1, _post2, _post3:TTypeSet);
begin
  FOperator := _to;
  FE1_pre := _pre1;
  FE2_pre := _pre2;
  FE3_pre := _pre3;
  FE1_post := _post1;
  FE2_post := _post2;
  FE3_post := _post3;
  end;

function TTypeInferenceRule.getOperator:TTypeOperator;
begin
  result:=FOperator;
end;

constructor TInferenceRuleList.Create;
begin
  FList:=TList.Create;
end;

function TInferenceRuleList.IndexOf(op:TTypeOperator):integer;
var c,i:integer;

    // we check if there are multiple Rules for this Operator
    // if so, we Raise an error
    // todo
    // if we get into context were multiple Infernce Rules are needed we can adapt de code here
    // to return a list in indexes
    total:integer;
begin
  Result:=-1;
  c:=FList.count;
  total:=0;
  for i:=0 to c-1 do begin
    if TTypeInferenceRule(FList[i]).getOperator = op then begin
      Result:=i;
      inc(total);
    end;
  end;
  if total>1 then
    RaiseError('ERROR (TInferenceRuleList.IndexOf): More than 1 index for operator '+op);
end;

procedure TInferenceRuleList.add( R : TTypeInferenceRule );
begin
  // no checking for previouly added Rules for this operator
  Flist.add( R );
end;

function TInferenceRuleList.InferUnary(op:TTypeOperator; E1:TTypeSet):TTypeSet;
var i:integer;
    Rule:TTypeInferenceRule;
begin
  //i:=IndexOf(op);
  //Rule:=TTypeInferenceRule(FList[i]);
  //// check if current TypeSet for E1 matches premiss
  //if Rule.FE1_pre
  //
end;

function TInferenceRuleList.InferBinary(op:TTypeOperator; E1,E2:TTypeSet):TTypeSet;
begin

end;

function TInferenceRuleList.InferTernary(op:TTypeOperator; E1,E2,E3:TTypeSet):TTypeSet;
begin

end;


end.

