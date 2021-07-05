unit TreeClass;

interface

uses
  Classes, SysUtils, LazFileUtils, utils;

type
  TTreeNode = class;
  TErrorObject = class(Exception);

  TTree = class
  private
    fremoveall: boolean;
    findex: integer;
    fRootnode: TTreeNode;
    fnodes: TList;
    function GetNodeFromIndex(index: integer): TTreeNode;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNode(parentnode: TTreeNode): TTreeNode;
    function getLCA(node1, node2: TTreeNode): TTreeNode;
    function GetChildOfLCALeadingToNode(lcanode,targetnode:TTreeNode):TTreeNode;
    function NodeIsInTree(n: TTreeNode): boolean;
    function NodesAreInTree(n1, n2: TTreeNode): boolean;
    procedure RemoveNode(anode: TTreeNode);
    procedure Clear;
    procedure getAST(astfilename:string);
    function getNodeByLinenumber(_linenumber: string): TTreeNode;
    function print: TStringList;
    function BuildLocalSimplePath(entryNode,exitNode:TTreeNode):TStringList;
    function getFunctionCalls: TList;


    property Count: integer read GetCount;
    property RootNode: TTreeNode read fRootnode;
    property Item[index: integer]: TTreeNode read GetNodeFromIndex;

  end;

  TTreeNode = class
  private
    ptree: TTree;
    findex: integer;
    frelindex: integer;
    flevel: integer;
    fdata: pointer;
    fchildren: TList;
    fparent: TTreeNode;

    function GetNode(index: integer): TTreeNode;
    function GetCount: integer;
    function GetNextSibling: TTreeNode;
    function GetFirstSibling: TTreeNode;
  public
    key, Value, linenumbers: string;
    constructor Create(atree: TTree);
    destructor Destroy; override;
    procedure Clear;

    //function getChildByKey( _key:string):TTreeNode;
    //function getChildByValue( _value:string):TTreeNode;
    function getChildByKey(var child: TTreeNode; _key: string): boolean;
    function getChildByValue(var child: TTreeNode; _value: string): boolean;

    property Tree: TTree read ptree;
    property AbsoluteIndex: integer read findex;
    property RelativeIndex: integer read frelindex;
    property Level: integer read flevel;
    property Count: integer read GetCount;
    property Data: pointer read fdata write fdata;
    property Children[Index: integer]: TTreeNode read GetNode;
    property NextSibling: TTreeNode read GetNextSibling;
    property FirstSibling: TTreeNode read GetFirstSibling;
    property Parent: TTreeNode read fparent;
    function equals( node : TTreeNode):boolean;

    function getFunctionName: string;
  end;

function UnparseConditionToInfix(n:TTreeNode):string;
function UnparseConditionToPrefix(n:TTreeNode):string;
function GetFunctionOfNode(phpfilename:string; n:TTreeNode):string;


procedure RaiseError(const msg: string);

implementation

function TTree.GetCount: integer;
begin
  Result := fnodes.Count;
end;

function TTree.GetNodeFromIndex(index: integer): TTreeNode;
begin
  Result := nil;
  if (index >= 0) and (index < Count) then
    Result := TTreeNode(fnodes[index])
  else
    RaiseError('Range Out of Bounds');
end;

procedure TTree.Clear;
var
  i: integer;
  tmp: TTreeNode;
begin
  fremoveall := True;
  findex := 1;
  for i := 1 to fnodes.Count - 1 do
  begin
    tmp := TTreeNode(fnodes[i]);
    FreeAndNil(tmp);
  end;
  fnodes.Clear;
  fRootnode.fchildren.Clear;
  fnodes.add(fRootnode);
  fremoveall := False;
end;


function TTree.print: TStringList;
var
  node: TTreeNode;
  sl: TStringList;

  procedure ProcesNode(n: TTreeNode);
  var
    i: integer;
    indent: string;

  begin
    indent := '';
    for i := 0 to n.level do
      indent := indent + ' ';


    sl.add(indent + n.key + ' ' + n.Value);
    for i := 0 to n.Count - 1 do
      ProcesNode(n.children[i]);

  end;

begin
  sl := TStringList.Create;
  node := self.RootNode;
  procesNode(node);
  Result := sl;
end;

function TTree.getLCA(node1, node2: TTreeNode): TTreeNode;
begin
  while node1.level > node2.level do
    node1 := node1.parent;
  while node2.level > node1.level do
    node2 := node2.parent;
  while node1 <> node2 do
  begin
    node1 := node1.parent;
    node2 := node2.parent;
  end;
  getLCA := node1;
end;

function TTree.NodeIsInTree(n: TTreeNode): boolean;

  function procesnode(anode: TTreeNode): boolean;
  var
    i: integer;
  begin
    if aNode = n then
      procesnode := True
    else
    begin
      for i := 0 to anode.Count - 1 do
      begin
        procesnode := procesnode(aNode.children[i]);
      end;
    end;
  end;

begin
  NodeIsInTree := procesNode(self.rootnode);
end;

function TTree.NodesAreInTree(n1, n2: TTreeNode): boolean;

  function procesnode(anode: TTreeNode): boolean;
  var
    i: integer;
  begin
    if (aNode = n1) or (aNode = n2) then
      procesnode := True
    else
    begin
      for i := 0 to anode.Count - 1 do
      begin
        procesnode := procesnode(aNode.children[i]);
      end;
    end;
  end;

begin
  NodesAreInTree := procesNode(self.rootnode);

end;


constructor TTreeNode.Create(atree: TTree);
begin
  ptree := atree;
  findex := ptree.findex;
  Inc(ptree.findex);
  fchildren := TList.Create;
  fdata := nil;
  fparent := atree.fRootnode;
end;

destructor TTreeNode.Destroy;
var
  i: integer;
  tmp: TTreeNode;
begin
  if ptree.fremoveall = False then
  begin
    ptree.fnodes.remove(self);
    for i := 0 to fchildren.Count - 1 do
    begin
      tmp := TTreeNode(fchildren[i]);
      FreeAndNil(tmp);
    end;
    FreeAndNil(fchildren);
  end;
  ptree := nil;
  fparent := nil;
  fdata := nil;
  inherited;
end;

function TTreeNode.GetCount: integer;
begin
  Result := fchildren.Count;
end;

function TTreeNode.GetNode(index: integer): TTreeNode;
begin
  Result := nil;
  if (index >= 0) and (index < Count) then
    Result := TTreeNode(fchildren[index])
  else
    RaiseError('Range Out of Bounds');
end;

function TTreeNode.GetNextSibling: TTreeNode;
var
  i, tmp1: integer;
begin
  Result := nil;
  tmp1 := -1;
  if fparent <> nil then
  begin
    for i := 0 to fparent.Count - 1 do
      if fparent.Children[i] = self then
      begin
        tmp1 := i;
        break;
      end;
    if tmp1 <> -1 then
      if (tmp1 >= 0) then
        if (tmp1 + 1 < fparent.Count) then
          Result := TTreeNode(fparent.fchildren[tmp1 + 1])
        else
        begin
          if assigned(fparent.fparent) then
            Result := fparent.GetNextSibling
          else
            Result := nil;
        end;

  end;
end;

function TTreeNode.GetFirstSibling: TTreeNode;
begin
  if fparent <> nil then
    Result := TTreeNode(fparent.fchildren[0])
  else
    Result := self;
end;

function TTreeNode.equals( node : TTreeNode):boolean;
begin
  result:=false;
  if assigned(node) then begin
    if (self.key=node.key) and (self.value=node.value) and (self.linenumbers=node.linenumbers) then
      result:=true;
    end;
end;

function TTreeNode.getChildByKey(var child: TTreeNode; _key: string): boolean;
var
  c,i: integer;
  //key:string;
begin
  Result := False;
  c:=self.count;
  for i := 0 to c - 1 do begin
    //key:=
    //debug('GetChildrenByKey: '+self.children[i].key+'=?='+_key,true);
    //debug('GetChildrenByKey: key='+self.children[i].key+', value='+self.children[i].value,true);
    if self.children[i].key = _key then
    begin
      child := self.children[i];
      Result := True;
    end;

  end;
end;

function TTreeNode.getChildByValue(var child: TTreeNode; _value: string): boolean;
var
  c,i: integer;
begin
  Result := False;
  c:=self.count;
  for i := 0 to c - 1 do
    if self.children[i].Value = _value then
    begin
      child := self.children[i];
      Result := True;
    end;
end;

procedure TTreeNode.Clear;
var
  i: integer;
  tmp: TTreeNode;
begin
  for i := 0 to fchildren.Count - 1 do
  begin
    tmp := TTreeNode(fchildren[i]);
    FreeAndNil(tmp);
  end;
  fchildren.Clear;
  frelindex := 1;
end;

constructor TTree.Create;
begin
  findex := 0;
  fremoveall := False;
  fRootnode := TTreeNode.Create(self);
  fRootnode.fparent := nil;
  fRootnode.flevel := 0;
  fnodes := TList.Create;
  fnodes.add(fRootnode);
end;

destructor TTree.Destroy;
begin
  Clear;
  fremoveall := True;
  FreeAndNil(fRootnode);
  FreeAndNil(fnodes);
  inherited;
end;

function TTree.AddNode(parentnode: TTreeNode): TTreeNode;
begin
  if parentnode = nil then
    RaiseError('Invalid Parent Node');
  Result := TTreeNode.Create(self);
  Result.ptree := self;
  Result.fparent := parentnode;
  Result.fdata := nil;
  parentnode.fchildren.add(Result);
  Result.frelindex := parentnode.fchildren.indexof(Result);
  Result.flevel := parentnode.flevel + 1;
  fnodes.add(Result);
end;

procedure TTree.RemoveNode(anode: TTreeNode);
var
  i: integer;
begin
  if anode = fRootnode then
    RaiseError('Cannot remove root node');
  anode.fparent.fchildren.remove(anode);
  fnodes.remove(anode);
  for i := 0 to anode.fchildren.Count - 1 do
  begin
    anode.Children[i].fparent := anode.fparent;
    anode.fparent.fchildren.add(anode.fchildren[i]);
    anode.Children[i].frelindex := anode.fparent.fchildren.indexof(anode.fchildren[i]);
    anode.Children[i].flevel := anode.fparent.flevel + 1;
  end;
  FreeAndNil(anode);
end;


procedure TTree.getAST(astfilename: string);
var
  i, j: integer;
  code: TStringList;
  level: integer;
  line, leftpart, key, Value, linenumbers: string;
  CurrentNode: TTreeNode;


  function leadingspaces(s: string): integer;
  begin
    leadingspaces := length(s) - length(trimleft(s));
  end;

begin
  code := TStringList.Create;
  code.LoadFromFile(astfilename);
  //astTree := TTree.Create;
  currentNode := self.RootNode;
  currentNode.key := astfilename;
  currentNode.Value := 'main';

  for i := 0 to code.Count - 1 do
  begin

    // in de echte boom beginnen we bij level 1
    level := leadingspaces(code[i]) div 4 + 1;

    // splits de regel in regelnummer(s), sleutel en waarde

    // als er een @ in zit splitsen we het regelnummer/de regelnummers af
    line := code[i];
    SplitStringAt(line,'@',leftpart, linenumbers);

    // als er : in zit splitsen we de key en de waarde hier af
    SplitStringAt(leftpart,':',key, value);

    if level > currentNode.level then
    begin
      currentNode := addNode(currentNode);
    end
    else if level = currentNode.level then
    begin
      currentNode := addNode(CurrentNode.parent);
    end
    else
    begin // level<lastlevel
      //debug(true,' level='+inttostr(level)+' currentlevel='+inttostr(currentNode.level)+' < '+code[i]);
      for j := 0 to currentNode.level - level do
      begin
        currentNode := currentNode.parent;
      end;

      currentNode := addnode(CurrentNode);
    end;

    currentNode.key := key;
    currentNode.Value := Value;
    currentNode.linenumbers := linenumbers;

  end;
  //getAST := asttree;
end;


function TTree.getNodeByLinenumber(_linenumber: string): TTreeNode;
var
  //i, c: integer;
  res, found: TTreeNode;

  function procesnode(node : tTreeNode; _linenumber:string):TTreeNode;
  var i,c:integer;
      found:TTreeNode;
  begin
    if node.linenumbers = _linenumber then
    begin
      Result := node;
    end
    else
    begin
      c := node.Count;
      found := nil;
      for i := 0 to c - 1 do
      begin

        res := procesnode(node.children[i], _linenumber);
        if res <> nil then
          found := res;
      end;

      Result := found;
    end;

  end;

begin
  result := procesnode(rootnode, _linenumber);
end;

function UnparseConditionToInfix(n:TTreeNode):string;
begin
  UnparseConditionToInfix:='<todo>';
end;

function UnparseConditionToPrefix(n:TTreeNode):string;

  function unparsenode(n:TTreenode):string;
  var v,s:string;
      child, child2:TTreeNode;
      p:integer;
  begin

    case n.value of

    'AST_BINARY_OP' :
    begin
      // flags give type of operator
      if n.getChildByKey(child,'flags') then begin
        v:=child.value;
        // scrap (...) from value first
        p:=pos(' ',v);
        v:=copy(v,1,p-1);

        case v of
        'BINARY_IS_SMALLER_OR_EQUAL':
        begin
          s:=' '+'<=';
        end;
        'BINARY_IS_SMALLER':
        begin
          s:=' '+'<';
        end;
        'BINARY_BITWISE_OR':
        begin
          s:=' '+'|';
        end;
        'BINARY_BITWISE_AND':
        begin
          s:=' '+'&';
        end;
        'BINARY_SHIFT_LEFT':
        begin
          s:=' '+'<<';
        end;
        'BINARY_SHIFT_RIGHT':
        begin
          s:=' '+'>>';
        end;
        'BINARY_BITWISE_XOR':
        begin
          s:=' '+^';
        end;
        'BINARY_IS_EQUAL':
        begin
          s:=' '+'==';
        end;
        'BINARY_IS_IDENTICAL':
        begin
          s:=' '+'===';
        end;
        'BINARY_IS_GREATER_OR_EQUAL':
        begin
          s:=' '+'>=';
        end;
        'BINARY_IS_GREATER':
        begin
          s:=' '+'>';
        end;
        'BINARY_IS_NOT_IDENTICAL':
        begin
          s:=' '+'<>';
        end;
        'BINARY_SPACESHIP':
        begin
          s:=' '+'<=>';
        end;
        'BINARY_BOOL_AND':
        begin
          s:=' '+'and';
        end;
        'BINARY_BOOL_OR':
        begin
          s:=' '+'or';
        end;
        'BINARY_BOOL_XOR':
        begin
          s:=' '+'xor';
        end;
        'BINARY_ADD':
        begin
          s:=' '+'+';
        end;
        'BINARY_SUB':
        begin
          s:=' '+'-';
        end;
        'BINARY_MUL':
        begin
          s:=' '+'*';
        end;
        'BINARY_DIV':
        begin
          s:=' '+'/';
        end;
        'BINARY_POW':
        begin
          s:=' '+'**';
        end;
        'BINARY_MOD':
        begin
          s:=' '+'%';
        end
        else begin
          debug('unknown binary operator',true);
          debug(child.value,true);

          //RaiseError('ERROR: unknown type of binary operator '+v);
          end;

        end;

      if n.getChildByKey(child,'left') then begin
        s:=s+unparsenode(child);
        end;
      if n.getChildByKey(child,'right') then begin
        s:=s+unparsenode(child);
        end;
      end;
    end;
    'AST_VAR':
    begin
      if n.getChildByKey(child,'name') then begin
        s:=' '+UnQuoteString(child.value);
        end;
    end;
    'AST_INSTANCE_OF':
    begin
      debug('ast_instance_of not implemented', true);
    end;
    'AST_UNARY_OP':
    begin
      if n.getChildByKey(child,'flags') then begin
        v:=child.value;
        // scrap (...) from value first
        p:=pos(' ',v);
        v:=copy(v,1,p-1);

        case v of
        'UNARY_BITWISE_NOT':
        begin
          s:=' '+'~';
        end;
        'UNARY_MINUS':
        begin
          s:=' '+'-';
        end
        else
        begin
          debug('unknown ast_unary_op:',true);
          debug(v,true);
        end;
        end;


        if n.getChildByKey(child,'left') then begin
          if child.getChildByKey(child2,'name') then begin
            s:=s+unParseNode(child2);
            end;
          end;
      end;
    end
    else
    begin
      // a literal
      s:=' '+n.value;

    end;

    end;
    unparsenode:=s;
  end;

begin
  //if n.key='cond' then
  UnparseConditionToPrefix:=UnparseNode(n)
  //else
  //  RaiseError('ERROR while unparsing a subtree of an condition: node does not contain a condition');
end;

function GetFunctionOfNode(phpfilename:string; n:TTreeNode):string;
var res:string;

function procesnode(node : tTreeNode):string;
var functionname,
    variablename:string;
    child,
    parent:TTreeNode;
    i,c:integer;
    found:TTreeNode;
begin
  functionname:='';

  case node.Value of
  'AST_CLOSURE':
  begin
    // vraag de naam van de variable waaraan de anonieme functie toegewezen werd
    // todo!! klopt dit??
    parent:=node.parent;
    // vraag de naam van de functie op (is in dit geval altijd {closure})
    if node.getChildByKey(child, 'name') then
    begin
      functionname:=UnQuoteString(child.Value);
    end;
  end;
  'AST_FUNC_DECL':
  begin
    if node.getChildByKey(child, 'name') then
    begin
      functionname:=UnQuoteString(child.Value);
    end;
  end;
  end;

  if (functionname<>'') or (node.parent=nil) then
    result:=functionname
  else
    result:=procesnode(node.parent)

end;

begin
  // set the default 'function' name (=> the main() function, labeled with the filename without extension

  result:='main_'+ExtractFileNameWithoutExt(phpFilename);

  // traverse BACK until we reach the rootnode,
  res:=procesnode( n  );
  if res<>'' then
    result:=res;

  end;

function TTree.GetChildOfLCALeadingToNode(lcanode,targetnode:TTreeNode):TTreeNode;
var node:TTreeNode;
    found:TTreeNode;
begin
  // we lopen van de exitnode terug, tot er geen parent meer is
  // als we onderweg een nod vinden met node.parent=lcanode, dan nemen we result:=node (anders result:=nil)
  node:=targetnode;
  if (node.parent=lcanode) then
    found:=node
  else
    found:=nil;

  while (node.parent<>nil) and (found=nil) do begin
    node:=node.parent;
    if (node.parent=lcanode) then
      found:=node
    else
      found:=nil;
    end;

  result:=found;

end;

function TTree.BuildLocalSimplePath(entryNode,exitNode:TTreeNode):TStringList;
const SKIP=0;
      TRAVERSE=1;
      DO_NOT_TRAVERSE=2;

var path:TStringList;
    lca:TTreeNode;
    state:boolean;
    c,i,
    mode:integer;
    firstnode_in_stmtlist,
    lastnode_in_stmtlist : TTreenode;

  function  procesNode(var _inPath:boolean; _mode:integer; _currentnode,_entryNode,_exitNode:TTreeNode):TStringList;
  var i,c:integer;
  var subpath:TStringList;
  Begin
    subpath:=TStringList.Create;

    if _currentnode=_entrynode then
      _inPath:=true;

    //if _inPath then begin
    //  if (_currentNode.Value ='AST_ASSIGN')
    //  or (_currentNode.Value ='AST_SWITCH')
    //  or (_currentNode.Value ='AST_IF')
    //  or (_currentNode.Value ='AST_WHILE')
    //  or (_currentNode.Value ='AST_ECHO') then begin
    //    subpath.add(_currentNode.Value);
    //  end;
    //end;

    if _inPath then begin
      if (_currentNode.Value ='AST_ASSIGN')
      or (_currentNode.Value ='AST_SWITCH')
      or (_currentNode.Value ='AST_IF')
      or (_currentNode.Value ='AST_POST_DEC')
      or (_currentNode.Value ='AST_POST_INC')
      or (_currentNode.Value ='AST_PRE_DEC')
      or (_currentNode.Value ='AST_PRE_INC')
      or (_currentNode.Value ='AST_WHILE')
      or (_currentNode.Value ='AST_ECHO') then begin
        subpath.add(_currentNode.Value);
      end;
    end;


    //if (_mode=TRAVERSE) and (
    //   (_currentNode.Value ='AST_SWITCH')
    //or (_currentNode.Value ='AST_IF')
    //or (_currentNode.Value ='AST_WHILE') ) then begin
    //  c:=_currentNode.count;
    //  for i:=0 to c-1 do begin
    //    subpath.addstrings(procesnode(_inPath,_mode,_currentNode.children[i],_entryNode,_exitNode));
    //    end;
    //  end;

    if (_mode=TRAVERSE)
    and (
       (_currentNode.Value ='AST_SWITCH')
    or (_currentNode.Value ='AST_IF')
    or (_currentNode.Value ='AST_STMT_LIST')
    or (_currentNode.Value ='AST_WHILE') )
    then begin
      c:=_currentNode.count;
      for i:=0 to c-1 do begin
        subpath.addstrings(procesnode(_inPath,_mode,_currentNode.children[i],_entryNode,_exitNode));
        end;
      end;


    if _currentnode=_exitnode then
      _inPath:=false;

    result:=subpath;
  end;

Begin
  path:=TStringList.Create;

  // we travese the smallest subtree containing the entry point e and exit point x

  lca:=getLCA(entrynode,exitnode);

  // the LCA is statementList
  // one of its children is the node which is the root of the subtree leading to e
  // this is the first child which is relevant
  firstnode_in_stmtlist:=GetChildOfLCALeadingToNode(lca,entrynode);
  // the last relevant child contains the subtree containing x
  lastnode_in_stmtlist:=GetChildOfLCALeadingToNode(lca,exitnode);

  // we have 3 modes for travering the subtree of a child
  // SKIP (if index(child)<index(firstnode) or index(child)>index(last)=> node is negleted
  // TRAVERSE (if index(child)=index(firstnode) or index(child)=index(last) => all nodes are travered, depth first
  // DO_NOT_TRAVERSE (if index(first)<index(child)<index(last) => only the child is taken into account

  // !! => for concrete paths the DO_NOT_TRAVERSE-mode is replace by TRAVERSE as well
  // in the tree nodes are only taken into accoun starting from the entry node upto and including the exitnode



  state:=false;
  mode:=SKIP;

  c:=lca.Count;
  for i:=0 to c-1 do begin
    // do we need to change the mode before processing the node?
    case mode of
    SKIP:
    begin
      if (lca.children[i]=firstnode_in_stmtlist) then begin
        mode:=TRAVERSE;
        end;
    end;
    TRAVERSE:
    begin
      // we change the mode to DO_NOT_TRAVERSE after traversing
    end;
    DO_NOT_TRAVERSE:
    begin
      if (lca.children[i]=lastnode_in_stmtlist) then begin
        mode:=TRAVERSE;
      end;
    end;

    end;

    // proces the node according the mode
    case mode of
    SKIP:
    begin
      // do nothing
    end;
    TRAVERSE:
    begin
      // proces the tree
      path.addstrings(procesNode(state,mode,lca.children[i],entryNode,exitNode));
    end;
    DO_NOT_TRAVERSE:
    begin
      // proces the current node only (not the subtree)
      path.addstrings(procesNode(state,mode,lca.children[i],entryNode,exitNode));

    end;


    end;

    // do we need to change the mode AFTER processing the node?
    case mode of
    SKIP:
    begin
      // nothing changes
    end;
    TRAVERSE:
    begin
      // after firstmode
      if (lca.children[i]=firstnode_in_stmtlist) then
        mode:=DO_NOT_TRAVERSE
      else
        // after lastnode_in_stmt
        mode:=SKIP;
    end;
    DO_NOT_TRAVERSE:
    begin
      // no changes
    end;

    end;

  end;
  result:=path;
end;

function TTreeNode.getFunctionName: string;
var functioncalls: TList;
    child : TTreeNode;
begin
  if (self.Value = 'AST_FUNC_DECL') then begin
    if self.getChildByKey(child,'name') then
      result :=UnQuoteString(child.value)
    else
      RaiseError('ERROR (TTree.getFunctionName): missing name-node in AST_FUNC_DECL');

    end
  else
    RaiseError('ERROR (TTree.getFunctionName): trying to retrieve a functionname of a node that is not a AST_FUNC_DECL.');

end;

// search for AST_FUNC_DECL nodes
// resturn the nodes in a TLIST
function TTree.getFunctionCalls: TList;
var functioncalls: TList;
    n : TTreeNode;

procedure searchNode(node : TTreeNode);
var
  i, c: integer;
begin
  //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);

  if (node.Value = 'AST_FUNC_DECL') then
  begin
    functioncalls.add( node );
  end;

  // search for other AST_FUNC_DECL's
  // even in the subtree of this node (inner functions)
  c := node.Count;
  for i := 0 to c - 1 do
    searchnode (node.children[i]);
end;

begin
  functioncalls := TList.Create;

  // search for declaration in the child-nodes
  n:=rootNode;
  SearchNode( n );
  result:=functioncalls;
end;


procedure RaiseError(const msg: string);
begin
  raise TErrorObject.Create(msg);
end;

end.
