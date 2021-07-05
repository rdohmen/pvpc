unit cfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TreeClass, Dialogs,
  utils,
  VarList,
  typeTreeClass,
  TypeInference,
  FunctionList,
  ParamList,
  paths
  ;

type
  TCFGNode = class
  private
    Flabel: string;
    FAstNode: TTreeNode;
    FEdgeList: TList;
    FsmtlibString,
    FinfixString : String;
  public
    constructor Create(_n: TTreeNode);
    constructor Create;

    function getConnection:TCFGNode;
    procedure setConnection(n:TCFGnode);
    procedure addConnection(n: TCFGNode);
    function getCount:integer;
    function copyNode:TCFGNode;

    procedure Assign(cn : TCFGNode);

    property theLabel:string read Flabel write Flabel ;
    property astNode:TTreeNode read FAstNode write FAstNode ;
    property SMTLibString:string read FSMTLIBString write FSMTLIBString ;
    property InfixString:string read FinfixString write FinfixString  ;
    property count:integer read GetCount;


  end;

  TCFGEdge = class
  private
    // we use a stringlist for constraints for now
    Fnode: TCFGNode;
    FconstraintList: TStringList;
  public
    constructor Create(n: TCFGNode);
    procedure addConstraint(c: string);
    procedure Assign(e : TCFGEdge);
    function copyEdge:TCFGEdge;

    property node:TCFGNode read Fnode write Fnode;
  end;

  TCFG = class
  private
    Fname: string;
    Froot: TCFGNode;
    FCurrentlevel: integer;
    FVarList : TVarList;
    //FFunctionList : TFunctionList;
    FLoopStack,
    FopenConnections: TList;
    //cfgFunctionNames:TStringList;
    //FOpenConnectionsStack: TList;
  public
    constructor Create(_name: string);
    procedure addNode(n: TCFGNode);
    //procedure BuildCFGFromAST(t: TTree);
    procedure ClearLabels;
    procedure addNumberLabels;
    procedure addASTLabels;

    function ExportAsDotty(_prefix:string): TStringList;
    function AltDotty(_prefix:string):TStringList;
    function AltDotty2(_prefix:string):TStringList;

    procedure incCurrentLevel;
    procedure decCurrentLevel;
    function getCurrentLevel: integer;

    // add a connection to the current list of openconnections
    procedure AddOpenConnection(level: integer; n: TCFGNode);
    // clear then openconnections
    procedure ClearOpenConnections;

    procedure PushLoopStack(n:TCFGNode);
    function getLoopstack(depth:integer):TCFGNode;
    procedure PopLoopStack;

    function newResultVar:string;

    function proces_stmtlist(n: TTreeNode; fl:TFunctionList):TCFGnode;
    function proces_exprlist(n: TTreeNode; fl:TFunctionList):string;
    function proces_statement(n: TTreeNode; fl:TFunctionList):TCFGnode;
    function proces_ExprNode(n:TTreeNode; fl:TfunctionList):string;

    function getCFGNodeWithASTNode( astNode : TTreeNode) : TCFGNode;
    function contains( cfgNode : TCFGnode ):boolean;

    procedure BuildCFGFromAST( n: TTreeNode; FL: TFunctionList);

    function getCyclomaticComplexity:integer;

    function find_all_simple_paths(_from,_to:TCFGNode; var path:TPath):TPathList;

    function find_all_paths(_from,_to:TCFGNode; max_passes:integer; var path:TPath):TPathList;

    function toStringList(pathlist:TPathList):TStringList;
    function exportPathAsSMTLIB(path:TPath):TStringList;

    function getExitNodes:TList;

    function copyCfg(_name:string):TCFG;
    procedure insertCFG( after: TCFGnode; cfg : TCFG);

    function NodeCount:integer;
  end;

  TCFGWorld = class
  private
    FFunctionList : TFunctionList;
    world: TList;
    cfgFunctionNames:TStringList;
    function getCount:integer;
  public
    constructor Create;
    procedure addCFG(c: tCFG; _name:string);
    procedure LoadFromFile( path: string; phpFile : string );
    procedure LoadFromFiles(path: string; filelist : TStrings);
    //procedure BuildCFGFromAST(n: TTreeNode);


    function getCFGNodeWithASTNode( astNode : TTreeNode) : TCFGNode;
    function getCFGWithCFGnode( cn : TCFGnode ) : TCFG;

    procedure BuildSuperCFG( max_recursion : integer );
    //function SearchPath(startNode, endNode:TCFGNode; min_loop_visit,max_loop_visit:integer;):TList;
    //function CyclomaticComplexity:integer;

    function exportAsDotty:TStringList;
    function MergeCFGs:TCFG;
    function getCFGbyName(_name:string):TCFG;

    procedure insertCFG2( after: TCFGnode; cfg : TCFG);

    property Count: Integer read getCount;

  end;


implementation

constructor TCFGNode.Create(_n: TTreeNode);
begin
  Flabel := '';
  FastNode := _n;
  // list of edges, each edge containts the node pointed to and the constraints.
  FEdgeList := TList.Create;

  FsmtlibString := '';
  FinfixString := '';

end;

constructor TCFGNode.Create;
begin
  Flabel := '';
  FastNode := nil;
  FEdgeList := TList.Create;
  FsmtlibString := '';
  FinfixString := '';

end;


procedure TCFGNode.addConnection(n: TCFGNode);
var
  e: TCFGEdge;
begin
  e := TCFGEdge.Create(n);
  FEdgeList.add(e);
end;

function TCFGNode.getConnection:TCFGNode;
var c:integer;
begin
  c:=FEdgeList.count;
  if c=1 then
    result:=TCFGEdge(FEdgeList[0]).Fnode
  else
    RaiseError('ERROR (TCFGNode.getConnection): Expecting exactly 1 connection, '+inttostr(c)+' connections found.');
end;

procedure TCFGNode.setConnection(n:TCFGnode);
var c:integer;
    e:TCFGEdge;
begin
  c:=FEdgeList.count;
  if c=0 then begin
    e := TCFGEdge.Create(n);
    FEdgeList.add(e);
    end
  else if c=1 then
      TCFGEdge(FEdgeList[0]).Fnode:=n
  else
    RaiseError('ERROR (TCFGNode.setConnection): Expecting 0 or 1 connection, '+inttostr(FEdgeList.count)+' connections found.');

end;

function TCFGnode.getCount:integer;
begin
  result := FEdgeList.count;
end;

procedure TCFGNode.Assign(cn : TCFGNode);
var
  Source: TCFGNode;
begin
  if cn is TCFGNode then
  begin
    Source := TCFGNode(cn);
    FastNode := Source.FastNode;
    FLabel := Source.FLabel;
    FsmtlibString := Source.FsmtlibString;
    FinfixString := Source.FinfixString;

  end else
    { Since TMyClass is a direct TPersistent descendant,
      it calls inherited ONLY when it cannot handle Source class.
      See comments below. }
    //inherited AssignTo(cn);
end;

function TCFGNode.copyNode:TCFGNode;
var NodeCopy : TCFGNode;
    c,i:integer;
begin
  NodeCopy := TCFGNode.Create;
  //NodeCopy.Assign( self );
  NodeCopy.Flabel:= Flabel;

  NodeCopy.FastNode:=FAstNode;
  NodeCopy.FLabel := FLabel;
  NodeCopy.FsmtlibString := FsmtlibString;
  NodeCopy.FinfixString := FinfixString;

  c:=FEdgeList.count;
  for i:=0 to c-1 do begin
    // do not use copies for the edges
    //NodeCopy.FEdgeList.Add(TCFGEdge(Self.FEdgeList[i]).Copy);
    NodeCopy.FEdgeList.Add( TCFGEdge(Self.FEdgeList[i]) );
    end;

  result:=NodeCopy;
end;

constructor TCFGEdge.Create(n: TCFGNode);
begin
  Fnode := n;
  FconstraintList := TStringList.Create;
end;

procedure TCFGEdge.Assign(e : TCFGEdge);
var
  Source: TCFGEdge;
  c,i:integer;
begin
  if e is TCFGEdge then
  begin
    Source := TCFGEdge(e);
    Fnode := Source.Fnode;
    c:=Source.FconstraintList.count;
    for i:=0 to c-1 do begin
      FconstraintList.add( Source.FconstraintList[i] );

    end;


  end else
    { Since TMyClass is a direct TPersistent descendant,
      it calls inherited ONLY when it cannot handle Source class.
      See comments below. }
    //inherited AssignTo(cn);
end;


procedure TCFGEdge.addConstraint(c: string);
begin
  FconstraintList.add(c);
end;


function TCFGEdge.copyEdge:TCFGedge;
var res:TCFGEdge;
    c,i:integer;
begin
  res:=TCFGEdge.Create( self.Fnode );
  c:=self.FconstraintList.count;
  for i:=0 to c-1 do begin
    res.FconstraintList.add( self.FconstraintList[i] );
    end;

  result:=res;
end;

constructor TCFG.Create(_name: string);
var
  L : TList;
begin
  Fname := _name;
  Froot := TCFGNode.Create;
  Froot.Flabel := '<start>';
  FCurrentLevel := 0;

  FVarList := TVarList.Create('main'+_name);

  FLoopStack:=TList.Create;

  FOpenConnections:=TList.Create;
  //cfgFunctionNames:=TStringList.Create;

  L:=TList.Create;
  FOpenConnections.add( L );

  AddOpenConnection(FCurrentLevel, Froot);
end;


procedure TCFG.addNode(n: TCFGNode);
var
  c, i: integer;
  cfgNode: TCFGNode;
  _from, _to: string;
  L: TList;
begin
  // connect node n to all nodes in the OpenConnections list
  // of the current level
  L := TList(FOpenConnections[FcurrentLevel]);
  c:=L.count;

  for i := 0 to c - 1 do
  begin
    cfgNode := TCFGNode( L[i] );
    //debug('adding node: '+TCFGNode(Fopenconnections[i]).FAstNode.Value+'->'+n.FAstNode.Value, true);
    cfgNode.addConnection(n);
    if assigned(cfgNode.FastNode) then
      _from := cfgNode.FAstNode.Value
    else
      _from := cfgNode.Flabel;
    if assigned(n.FastNode) then
      _to := n.FAstNode.Value
    else
      _to := n.Flabel;

    debug('adding node: ' + _from + '->' + _to, True);

  end;

  ClearOpenConnections;
end;

procedure TCFG.AddOpenConnection(level: integer; n: TCFGNode);
var L:TList;
begin
  L:=TList(FOpenConnections[level]);
  L.add(n);
end;

procedure TCFG.ClearOpenConnections;
begin
  TList(FOpenConnections[FcurrentLevel]).Clear;
end;


procedure TCFG.PushLoopStack(n:TCFGNode);
begin
  FLoopStack.add(n);
end;

function TCFG.getLoopstack(depth:integer):TCFGNode;
var c:integer;
    n:TCFGNode;
begin
  c:=FLoopStack.count;
  if c-depth>-1 then begin
    n:=TCFGNode(FLoopstack[c-depth]);
    result:=n;
  end
  else
    RaiseError('ERROR (getLoopStack): the depth results in a negative index.');
end;

Procedure TCFG.PopLoopStack;
var c:integer;
begin
  c:=FLoopStack.Count;
  if c>0 then begin
    //n:=TCFGNode(FLoopStack[c-1]);
    FLoopStack.Delete(c-1);
    end
  else
    RaiseError('ERROR (poploopstack): Popping an element from the stack while the stack is empty.');
end;



function TCFG.NodeCount:integer;
var
  count:integer;
  processednodes: TList;

  procedure procesNode(n: TCFGNode);
  var c,i:integer;
  begin
    if processednodes.IndexOf(n) = -1 then
    begin
      processedNodes.Add(n);
      inc(count);

      c:=n.FEdgeList.count;
      for i := 0 to c - 1 do
      begin
        procesNode(TCFGEdge(n.FEdgeList[i]).Fnode);
      end;

    end;
  end;

begin
  count:=0;
  processednodes := TList.Create;

  procesnode(Froot);

  Result := count;
end;


function TCFG.AltDotty(_prefix:string):TStringList;
var
  SL:TStringList;
  processednodes: TList;
  visitedNodes:TList;

  function getNodeNumber(n:Tcfgnode):integer;
  var i:integer;
  begin
    i:=visitedNodes.indexOf(n);
    if i=-1 then begin
      visitednodes.add(n);
      result:=visitednodes.count;
    end
    else begin
      result:=i+1;
    end;
  end;

  procedure procesNode(n: TCFGNode);
  var c,i:integer;
      _from,_to:string;
  begin
    if processednodes.IndexOf(n) = -1 then
    begin
      processedNodes.Add(n);
      _from:=inttostr(getNodeNumber(n));

      c:=n.FEdgeList.count;
      //if c>1 then
      //  showmessage('c='+inttostr(c));
      for i := 0 to c - 1 do
      begin
        _to:=inttostr(getNodeNumber(TCFGEdge(n.FEdgeList[i]).Fnode));
        sl.add(_from+'->'+_to);
        procesNode(TCFGEdge(n.FEdgeList[i]).Fnode);
      end;

    end;
  end;

begin
  SL:=TStringList.Create;

  processednodes := TList.Create;
  visitedNodes:=TList.Create;

  sl.add('subgraph '+_prefix+' {');

  procesnode(Froot);
  sl.add('}');

  Result := SL;

end;

function TCFG.AltDotty2(_prefix:string):TStringList;
var
  SL:TStringList;
  processednodes: TList;
  visitedNodes:TList;
  nodeLabels:TStringList;

  function getNodeNumber(n:Tcfgnode):integer;
  var i:integer;
  begin
    i:=visitedNodes.indexOf(n);
    if i=-1 then begin
      visitednodes.add(n);
      result:=visitednodes.count;
    end
    else begin
      result:=i+1;
    end;
  end;

  function getNodeLabel(n:Tcfgnode):string;
  var i, counter:integer;
      L, L2:string;
  begin
    i:=visitedNodes.indexOf(n);
    if i=-1 then begin
      visitednodes.add(n);

      if assigned(n.Fastnode) then
        L:=n.FastNode.Value
      else
        L:=n.FLabel;


      if (L<>'<start>') and (L<>'<end>') then begin
        counter:=1;
        L2:=L+'1';
        while nodeLabels.IndexOf(L2)>-1 do begin
          inc(counter);
          L2:=L+inttostr(counter);
          end;
        L:=L2;
        end;
        nodeLabels.add(L);
        result:=L;
      end
    else begin
      result:=nodeLabels[i];
    end;
  end;


  procedure procesNode(n: TCFGNode);
  var c,i:integer;
      _from,_to:string;
  begin
    if processednodes.IndexOf(n) = -1 then
    begin
      processedNodes.Add(n);
      _from:=getNodeLabel(n);

      c:=n.FEdgeList.count;
      //if c>1 then
      //  showmessage('c='+inttostr(c));
      for i := 0 to c - 1 do
      begin
        _to:=getNodeLabel(TCFGEdge(n.FEdgeList[i]).Fnode);
        sl.add(_from+'->'+_to);
        procesNode(TCFGEdge(n.FEdgeList[i]).Fnode);
      end;

    end;
  end;

begin
  SL:=TStringList.Create;

  processednodes := TList.Create;
  visitedNodes:=TList.Create;
  nodeLabels:=TStringList.Create;

  sl.add('subgraph '+_prefix+' {');

  procesnode(Froot);
  sl.add('}');

  Result := SL;

end;



function TCFG.ExportAsDotty(_prefix:string): TStringList;
var
  sl: TStringList;
  processednodes: TList;

  procedure procesNode(n: TCFGNode);
  var
    p, c, i: integer;
    s, _from, _to: string;
  begin
    if processednodes.IndexOf(n) = -1 then
    begin
      processedNodes.Add(n);
      _from := n.Flabel;
      // _from == <start>
      if _from='<start>' then begin
        _from:='<'+_prefix+'_start>';
        end
      else begin

      // remove AST_
        p:=pos('AST_',_from);
        if p>0 then begin
          _from:=copy(_from,p+4,length(_from)-4);
          end;

        // add prefix
        _from:=_prefix+'_'+_from;
        end;

        c := n.FEdgeList.Count;
        for i := 0 to c - 1 do
        begin
          _to := TCFGEdge(n.Fedgelist[i]).node.Flabel;
          // if _to==<end>
          if _to='<end>' then begin
            _to:='<'+_prefix+'_end>';
            end
          else begin

          // remove AST_
            p:=pos('AST_',_to);
            if p>0 then begin
              _to:=copy(_to,p+4,length(_to)-4);
              end;
            // add prefix
            _to:=_prefix+'_'+_to;
            end;
          s := _from + '->' + _to + '[label="'+n.SMTLibString +'"];';
          sl.add(s);
          procesNode(TCFGEdge(n.Fedgelist[i]).node);
        end;
    end;
  end;

begin
  addNumberLabels;
  //addASTLabels;

  processednodes := TList.Create;

  sl := TStringList.Create;
  sl.add('subgraph '+_prefix+' {');
  procesnode(Froot);
  sl.add('}');

  Result := sl;
end;

procedure TCFG.incCurrentLevel;
var
  L : TList;
begin
  debug('inc current level',true);
  Inc(FcurrentLevel);
  L := TList.Create;
  FOpenConnections.add(L);
end;

procedure TCFG.decCurrentLevel;
var
  L : TList;
  c, i: integer;
  elem : TCFGNode;
begin
  debug('dec current level',true);
  // connect all items in the current list to the level below it
  L := TList( FOpenConnections[FCurrentLevel] );
  c := L.Count;

  //if TList(TList(FOpenConnectionsStack[FCurrentLevel - 1])[0]).Count > 1 then
  //  ShowMessage('ERROR (deccurrentlevel): the numbers of nodes in the currentlist of next level must be 1');

  for i := 0 to c - 1 do
  begin

      elem := TCFGNode( L[i] );
      TList( FOpenConnections[ FCurrentLevel -1 ]).add( elem );
      end;

  // delete and decrease the current level
  FOpenConnections.Delete(FCurrentLevel);
  Dec(FcurrentLevel);
end;

function TCFG.getCurrentLevel: integer;
begin
  Result := FCurrentLevel;
end;

procedure TCFG.ClearLabels;
var
  n: TCFGNode;
  processednodes: TList;

  procedure procesNode(n: TCFGNode);
  var
    c, i: integer;
    nextnode: TCFGNode;
  begin
    if processednodes.indexof(n) > -1 then
    begin
      processednodes.Add(n);

      n.Flabel := '';
      c := n.FEdgeList.Count;
      for i := 0 to c - 1 do
      begin
        nextNode := TCFGEdge(n.FEdgeList[i]).node;
        procesNode(nextnode);
      end;
    end;
  end;

begin
  processednodes := TList.Create;
  n := Froot;
  procesNode(n);
end;


procedure TCFG.addNumberLabels;
var
  counter: integer;
  processednodes: TList;
  n: TCFGNode;

  procedure procesNode(n: TCFGNode);
  var
    c, i: integer;
  begin
    if processedNodes.indexof(n)=-1 then begin
      processednodes.add(n);

      if n.Flabel = '' then
      begin
        n.FLabel := IntToStr(counter);
      //if assigned(n.FastNode) then begin
      //  n.FLabel:=n.FLabel+':'+n.FastNode.Value;
      //  end;
        Inc(counter);
      end;

      c := n.FEdgeList.Count;
      for i := 0 to c - 1 do
        procesNode(TCFGEdge(n.FEdgeList[i]).node);
      end;
    end;

begin
  ClearLabels;
  processednodes:=TList.Create;

  counter := 1;
  n := Froot;
  procesNode(n);
end;

function TCFG.copyCFG(_name:string):TCFG;
var cfg:TCfg;
    visitedNodes : TList;
    copiedNodes : TList;
    copyOfNodes : TList;

  function proces_node( n : TCFGnode ) : TCFGnode; forward;

  function getCopyOfNode( n: TCFGnode ): TCFGnode;
  var index:integer;
  begin
    // if we copied the node before, use the same copy
    index:=copiedNodes.IndexOf( n );
    If index = -1 then begin
      result := n.copyNode;
      copiedNodes.add( n );
      copyOfNodes.add(result);
      end
    else begin
      result:=TCFGnode( copyOfNodes[index] );
    end;

  end;

  function getCopyOfEdge( e: TCFGEdge): TCFGEdge;
  var edgeCopy : TCFGEdge;
  Begin
    edgeCopy :=e.copyEdge;
    edgeCopy.Fnode:=proces_node( edgeCopy.Fnode );
    //edgeCopy.Fnode:=getCopyOfNode( edgeCopy.Fnode );
    result := edgeCopy;
  end;


  function proces_node( n : TCFGnode ) : TCFGnode;
  var c,i: integer;
      newnode : TCFGNode;
  begin
    newNode:=getCopyOfNode( n );

    if visitedNodes.IndexOf( n ) = -1 then begin
      visitedNodes.add( n );

      newNode:=getCopyOfNode( n );

    // replace the edge with copies as well
    // look for existing copies first

      c:=n.FEdgeList.count;
      for i:=0 to c-1 do begin
//      newNode.FEdgeList[i]:=getCopyOfNode( TCFGEdge( n.FEdgeList[i]).Fnode );
        newNode.FEdgeList[i]:=getCopyOfEdge( TCFGEdge( n.FEdgeList[i]) );

      end;
    end;
    result := newNode;
  end;


begin
  cfg:=TCFG.create(_name);
  cfg.FCurrentlevel:=self.FCurrentLevel;
  // loopstack, the list with open connection are only used when building an cfg from an ast

  // copy the varlist
  //todo

  visitedNodes := TList.Create;
  copiedNodes := TList.Create;
  copyOfNodes := TList.Create;

  // copy the cfg structure
  cfg.Froot:=proces_node( self.Froot );

  result:=cfg;
end;

procedure TCFG.insertCFG( after: TCFGnode; cfg : TCFG);
var insertcounter:integer;
    dest : TCFGnode;
    exitnodes : TList;
    c,i:integer;


begin
  dest:=after.getconnection;
  altDotty('A').saveToFile('AAA.dot');
  after.setConnection(cfg.Froot.getconnection);
  altDotty('A').saveToFile('BBB.dot');
  exitnodes:=cfg.getexitnodes;
  c:=exitnodes.count;
  for i:=0 to c-1 do begin
    TCFGNode(exitnodes[i]).setconnection(dest)
    end;
  altDotty('A').saveToFile('CCC.dot');


end;



// insert the cfg c after node n
procedure TCFGWorld.insertCFG2( after: TCFGnode; cfg : TCFG);
var insertcounter:integer;
    dest : TCFGnode;
    exitnodes : TList;
    c,i:integer;

  procedure proces_node( last, dest, cn:TCFGnode );
  var c,i:integer;
      newNode:TCFGNode;
  begin

    // skip <start> and <end> node

    if cn.Flabel='<start>' then begin
    //if assigned(cn.Fastnode) and (cn.Fastnode.Value='<start>') then begin
      // skip this node
      // proces the children of this node
      // dest:=last;
      cn:=cn.getconnection;
      proces_node( last, dest, cn );

      //c:=cn.count;
      //for i:=0 to c-1 do begin
      //  last:=TCFGEdge(cn.FEdgeList[i]).fnode;
      //  proces_node( last, dest );
      //  end;
      end
    else if cn.Flabel='<end>' then begin
    //else if assigned(cn.Fastnode) and (cn.Fastnode.Value='<end>') then begin
      // this is the last node
      // the source node points to the destination
      //last:=dest;
      // last should = to dest
      // this is already the case -> do nothing

      newNode:=last.CopyNode;
      newNode.setconnection(dest);
      end
    else begin
      // proces the children of this node
      //dest:=last;


      newNode:=last.CopyNode;
      newNode.setconnection(cn);
      //last:=cn;
      //TCFG(world[i]).exportAsDotty('insert').saveToFile('insert'+inttostr(insertcounter)+'.dot');
      exportAsDotty.saveToFile('insert-'+inttostr(insertcounter)+'.dot');
      inc(insertcounter);
      c:=cn.count;
      //last:=cn;
      for i:=0 to c-1 do begin
        //cn:=TCFGEdge(cn.FEdgeList[i]).node;
        proces_node( cn , dest, TCFGEdge(cn.FEdgeList[i]).node);
        end;
      end;
  end;

begin
  //insertcounter:=0;
  //
  //proces_node( last, last.getconnection, _cn );

  // the node 'after' points to the next node (dest)
  // instead, we let 'after' point to the first node of the cfg
  // all exitnode of the cfg will point to the dest-node

  dest:=after.getconnection;
  exportAsDotty.saveToFile('AAA.dot');
  after.setConnection(cfg.Froot.getconnection);
  exportAsDotty.saveToFile('BBB.dot');
  exitnodes:=cfg.getexitnodes;
  c:=exitnodes.count;
  for i:=0 to c-1 do begin
    TCFGNode(exitnodes[i]).setconnection(dest)
    end;
  exportAsDotty.saveToFile('CCC.dot');


end;


function TCFG.getExitNodes:TList;
var exitnodes:TList;

  procedure proces_node( cn:TCFGnode);
  var c,i:integer;
      isReturn, isLastNodeBeforeEnd: Boolean;
      child:TCFGNode;
  begin
    // check if this node is an AST_RETURN
    // or if theis node point to the last node in the CFG
    isReturn := (assigned(cn.FAstnode)) and (cn.FAstNode.Value='AST_RETURN');


    isLastNodeBeforeEnd:=false;
    if cn.FEdgeList.count=1 then begin
      child:=TCFGEdge(cn.FEdgeList[0]).Fnode;
      isLastNodeBeforeEnd := (child.FLabel='<end>');
      end;

    if isReturn or isLastNodeBeforeEnd then begin
      // return nodes are allways exitnodes
      exitnodes.add(cn);
      end
    else begin
      c:=cn.FEdgeList.count;
      if c=0 then begin
        exitnodes.add(cn)
        end
      else begin
        for i:=0 to c-1 do begin
          proces_node(TCFGEdge(cn.Fedgelist[i]).Fnode);
          end;
        end;
      end;
  end;

begin
  exitnodes:=TList.Create;
  proces_node( Froot );
  result:=exitnodes;
end;

procedure TCFG.addASTLabels;
var
  n: TCFGNode;
  processednodes: TList;
  usedLabels: TStringList;

  procedure procesNode(n: TCFGNode);
  var
    counter, c, i: integer;
    s, t: string;
  begin
    if processednodes.indexOf(n) = -1 then
    begin
      processednodes.add(n);

      if n.Flabel = '' then
      begin

        s := '';
        if assigned(n.FastNode) then
          s := n.FastNode.Value;

        counter := 1;
        t := s + IntToStr(counter);
        while usedLabels.Indexof(t) > -1 do
        begin
          counter := counter + 1;
          t := s + IntToStr(counter);
        end;
        usedlabels.add(t);
        n.FLabel := t;
      end
      else
      begin
        usedlabels.add(n.Flabel);
      end;

      c := n.FEdgeList.Count;
      for i := 0 to c - 1 do
        procesNode(TCFGEdge(n.FEdgeList[i]).node);

    end;
  end;

begin
  ClearLabels;
  processednodes := TList.Create;
  usedLabels := TStringList.Create;

  n := Froot;
  procesNode(n);
end;

function TCFG.newResultVar:string;
Begin
  result:=FVarList.IntroduceTempVar;
end;


function TCFG.proces_ExprNode(n:TTreeNode; fl:TfunctionList):string;
var v,s:string;
    lastNode,
    cfgNode:TCfgNode;
    child,
    child2:TTreeNode;
    nextnode,
    p:integer;
    varname,
    functionname,
    paramname,
    smtExpr,
    infixExpr,
    LHS, RHS: string; // left and right hand side of operator
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
        s:=NewResultVar;
        if n.getChildByKey(child,'left') then begin
          LHS:=proces_ExprNode(child, fl);
          end;
        if n.getChildByKey(child,'right') then begin
          RHS:=proces_ExprNode(child, fl);
          end;

          cfgNode := TCFGNode.Create(n);
          cfgNode.smtlibString:='(assert(<= '+s+' (+'+LHS+' ' +RHS+')))';
          cfgNode.infixString:=LHS+'<='+RHS;
          lastNode:=cfgNode;
          addNode(cfgNode);
          AddOpenConnection(getCurrentLevel, cfgNode);

      end;
      'BINARY_IS_SMALLER':
      begin
        s:=NewResultVar;
        if n.getChildByKey(child,'left') then begin
          LHS:=proces_ExprNode(child, fl);
          end;
        if n.getChildByKey(child,'right') then begin
          RHS:=proces_ExprNode(child, fl);
          end;

          cfgNode := TCFGNode.Create(n);
          cfgNode.smtlibString:='(assert(< '+s+' (+'+LHS+' ' +RHS+')))';
          cfgNode.infixString:=LHS+'<'+RHS;
          lastNode:=cfgNode;
          addNode(cfgNode);
          AddOpenConnection(getCurrentLevel, cfgNode);

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
      s:=NewResultVar;
      if n.getChildByKey(child,'left') then begin
        LHS:=proces_ExprNode(child, fl);
        end;
      if n.getChildByKey(child,'right') then begin
        RHS:=proces_ExprNode(child, fl);
        end;

      smtExpr:='(assert(='+s+'(-'+LHS+' ' +RHS+')))';
      infixExpr:=s+'='+LHS+'-'+RHS;
      end;


      'BINARY_IS_IDENTICAL':
      begin
        s:=' '+'===';
      end;
      'BINARY_IS_GREATER_OR_EQUAL':
      begin
      s:=NewResultVar;
      if n.getChildByKey(child,'left') then begin
        LHS:=proces_ExprNode(child, fl);
        end;
      if n.getChildByKey(child,'right') then begin
        RHS:=proces_ExprNode(child, fl);
        end;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(>= '+LHS+' ' +RHS+')))';
        cfgNode.infixString:=LHS+'+>'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);

      end;
      'BINARY_IS_GREATER':
      begin
      s:=NewResultVar;
      if n.getChildByKey(child,'left') then begin
        LHS:=proces_ExprNode(child, fl);
        end;
      if n.getChildByKey(child,'right') then begin
        RHS:=proces_ExprNode(child, fl);
        end;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(> '+s+' (+'+LHS+' ' +RHS+')))';
        cfgNode.infixString:=LHS+'>'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);

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
      s:=NewResultVar;
      if n.getChildByKey(child,'left') then begin
        LHS:=proces_ExprNode(child, fl);
        end;
      if n.getChildByKey(child,'right') then begin
        RHS:=proces_ExprNode(child, fl);
        end;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(= '+s+' (+'+LHS+' ' +RHS+')))';
        cfgNode.infixString:=s+'='+LHS+'+'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);
      end;

      'BINARY_SUB':
      begin
        s:=NewResultVar;
        if n.getChildByKey(child,'left') then begin
          LHS:=proces_ExprNode(child, fl);
          end;
        if n.getChildByKey(child,'right') then begin
          RHS:=proces_ExprNode(child, fl);
          end;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(= '+s+' (-'+LHS+' ' +RHS+')))';
        cfgNode.infixString:=s+'='+LHS+'-'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);

      end;
      'BINARY_MUL':
      begin
        s:=NewResultVar;
        if n.getChildByKey(child,'left') then begin
          LHS:=proces_ExprNode(child, fl);
          end;
        if n.getChildByKey(child,'right') then begin
          RHS:=proces_ExprNode(child, fl);
          end;

        //varname:=FVarList.introduceTempVar;
        //smtExpr:=;
        //infixExpr:=;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(= '+s+' (* '+LHS+' ' +RHS+')))';
        cfgNode.infixString:=s+'='+LHS+'*'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);


      end;
      'BINARY_DIV':
      begin
      s:=NewResultVar;
      if n.getChildByKey(child,'left') then begin
        LHS:=proces_ExprNode(child, fl);
        end;
      if n.getChildByKey(child,'right') then begin
        RHS:=proces_ExprNode(child, fl);
        end;

      //smtExpr:='(assert(='+s+'(/'+LHS+' ' +RHS+')))';
      //infixExpr:=s+'='+LHS+'/'+RHS;

      cfgNode := TCFGNode.Create(n);
      cfgNode.smtlibString:='(assert(= '+s+' (/'+LHS+' ' +RHS+')))';
      cfgNode.infixString:=s+'='+LHS+'/'+RHS;
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);

      end;
      'BINARY_POW':
      begin
        s:=NewResultVar;
        if n.getChildByKey(child,'left') then begin
          LHS:=proces_ExprNode(child, fl);
          end;
        if n.getChildByKey(child,'right') then begin
          RHS:=proces_ExprNode(child, fl);
          end;

        cfgNode := TCFGNode.Create(n);
        cfgNode.smtlibString:='(assert(= '+s+' (^'+LHS+' ' +RHS+')))';
        cfgNode.infixString:=s+'='+LHS+'^'+RHS;
        lastNode:=cfgNode;
        addNode(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);

        //s:=' '+'**';
      end;
      'BINARY_MOD':
      begin
        s:=' '+'%';
      end
      else begin
        logging('unknown_operator',child.value,true);

        //RaiseError('ERROR: unknown type of binary operator '+v);
        end;

      end;

// weghalen:

//
//      if n.getChildByKey(child,'left') then begin
//        LHS:=evalExprNode(child);
//        end;
//      if n.getChildByKey(child,'right') then begin
//        RHS:=evalExprNode(child);
//        end;
//
//      varname:=FVarList.introduceTempVar;
//      smtExpr:=s+LHS+' ' +RHS;
//      infixExpr:=LHS+s+' '+RHS;
    end;
  end;
  'AST_VAR':
  begin
    if n.getChildByKey(child,'name') then begin
      varname:=' '+UnQuoteString(child.value);
      s:=varname;
      end;
  end;
  'AST_ARRAY':
  begin
    showmessage('AST_ARRAY');
  end;


  'AST_CAST':
  begin
    showmessage('AST_CAST');
  end;
  'AST_DIM':
  begin
    showmessage('AST_DIM');
  end;
  'AST_CALL':
  begin
    if n.getChildByKey(child,'expr') then begin
      case child.value of
      'AST_NAME':
      begin
        if child.getChildByKey(child2,'name') then begin
           functionname := UnquoteString( child2.value );
           // todo
           // if the function is not a standardfunction
           // add this variable to the varlist
           //FFunctionList.getParamListOfFunction(_functionname);
           s:=functionname;
         //
         // todo: create nodes for complete function, including the parameter subsitution
        if child.getChildByKey(child2,'args') then begin

          // traverse the children for the parameters to be passed
          nextnode:=0;
          //arglist:='';
          while child.getChildByKey(child2,inttostr(nextnode)) do begin
            // get the fullname of the parameter
            // i.e. the parametername preceded by the context
            paramname := TParamList(FL.get_CTX_ParamListOfFunction(functionname)).getParamByIndex( nextnode );

            inc(nextnode);
            end;
          end;
          //insert the cfg of the function here

          // todo: meteen de juist parameter substitutie(s) als cfgNode inserten?
          // scheelt werkt bij het mergen van de cfg's
          cfgNode := TCFGNode.Create(n);
          lastNode:=cfgNode;
          addNode(cfgNode);
          AddOpenConnection(getCurrentLevel, cfgNode);

        end
        else
          RaiseError('ERROR (evalexprnode): expecting a function name when evaluating AST_CALL in expression');

      end;
      'AST_VAR':
      begin
        showmessage('TODO: AST_VAR');
      end
      else
      begin
        RaiseError('ERROR (evalexprnode): unknown Value ('+child.value+') when evaluating AST_CALL in expression');
      end;
      varname:=' '+UnQuoteString(child.value);



      end;
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

      // todo: wat deed dit ook alweer?
      if n.getChildByKey(child,'left') then begin
        if child.getChildByKey(child2,'name') then begin
          s:=s+proces_ExprNode(child2, fl);
          end;
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
  proces_ExprNode:=s;
end;


function TCFG.proces_statement(n: TTreeNode; fl:Tfunctionlist):TCFGnode;
var
  child, child2, child3: TTreeNode;

  continue_depth,
  break_depth, i, nextnode: integer;
  LoopNode,
  ConditionNode,
  lastnode,
  cfgNode: TCFGNode;
  conditions: TStringList;
  else_branch_found: boolean;

  paramList : TParamList;

  functionname,
  paramname,
  LHS, RHS,
  condition_expression, condition_value: string;

  conditionList,
  OpenConnectionsStack : TList;

  procedure pushOpenConnections;
  var c, i : integer;
  begin
    c:=TList( FOpenConnections[ FCurrentLevel ]).Count;
    for i:=0 to c-1 do begin
      OpenConnectionsStack.add(TList(FOpenConnections[ FCurrentLevel ])[i]);
      end;
    TList(FOpenConnections[ FCurrentLevel ]).Clear;
  end;

  procedure PopAllOpenConnections;
  var c, i : integer;
  begin
    c:=OpenConnectionsStack.Count;
    for i:=0 to c-1 do begin
      TList(FOpenConnections[ FCurrentLevel]).add(OpenConnectionsStack[i]);
      end;
    OpenConnectionsStack.Clear;
  end;


  function evalExpression(n:TTreeNode; fl:TFunctionlist):string;
  Begin

    result:=proces_ExprNode(n, fl);

  end;

begin
  //LastStatementWasBreak := False;
  conditions := TStringList.Create;
  OpenConnectionsStack:=TList.create;


  case n.Value of
    'AST_ASSIGN':
    begin
      // LHS get variable name
      if n.getChildByKey(child, 'var') then
        if child.getChildByKey(child2,'name') then
          LHS := UnquoteString(child2.Value);

      // RHS evaluate expression
      if n.getChildByKey(child, 'expr') then begin
        // is it a literal, functioncall, ... ?
          RHS:=evalExpression(child, fl);

        end;

      // TODO:
      // insert a node with
      // - smt: (assert (= LHS RHS))
      // - infix: LHS=RHS


      //smtExpr:='(assert(='+s+'( +'+LHS+' ' +RHS+')))';
      //infixExpr:=s+'='+LHS+'*'+RHS;


      cfgNode := TCFGNode.Create(n);
      cfgNode.smtlibString:='(assert(= '+LHS+' '+RHS+'))';
      cfgNode.infixString:=LHS+'='+RHS;
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_ASSIGN_OP':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_IF':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      conditionList:=TList.Create;

      // handle the stmt lists in the AST_IF_ELEM
      // the number of AST_IF_ELEM's is to be determined
      // node.value='AST_IF_ELEM'
      // -> node.key='cond'
      // --> this is the condition for this stmt_list
      // --> we need the negation for a possible ELSE-branch, or in case there is no ELSE-branch
      // => node.key='stmts'
      // --> these are the statements
      nextnode := 0;

      else_branch_found := False;


      while n.getChildByKey(child, IntToStr(nextnode)) do
      begin
        //debug('child '+inttostr(nextnode)+' found',true);
        if child.getChildByKey(child2, 'cond') then
        begin
          // add the condition to the edge
          // if the cond==null, an ELSE branch is found
          // remember the condition for an else clause
          if child2.Value = 'null' then
          begin
            // todo
            // add the negatiation of the or-ed conditions in the list

            // evaluate these expressions and or the results

            conditions.add('null => this is the ELSE-branch');
            debug(conditions[conditions.Count - 1], True);
            else_branch_found := True;
          end
          else
          begin
            // add this condition to the list
            conditionList.add( child2 );
            evalExpression( child2, fl );
            conditions.add(UnparseConditionToPrefix(child2));
            debug(conditions[conditions.Count - 1], True);
          end;
        end;

        if child.getChildByKey(child2, 'stmts') then
        begin
          AddOpenConnection(getCurrentLevel, cfgNode);

          lastNode:=proces_stmtlist(child2, fl);
          pushOpenConnections;
        end;

        Inc(nextnode);
      end;
      // check is an ELSE branch is found
      // if not, we add a 'virtual' ELSE branch
      if not else_branch_found then
      begin
        AddOpenConnection(getCurrentLevel, cfgNode);
        debug('condition is the negation of the disjunction of the conditions<>null',
          True);
        for i := 0 to conditions.Count - 1 do
          debug('=> ' + conditions[i], True);
      end;
      PopAllOpenConnections;
    end;
    'AST_WHILE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      if n.getChildByKey(child, 'cond') then
      begin
        // add the condition to the edge
        conditions.add(UnparseConditionToPrefix(child));
        proces_exprlist(child, fl);
        //showmessage(conditions[0]);
        debug('condition=' + conditions[0], True);

        if n.getChildByKey(child, 'stmts') then
        begin
          incCurrentLevel;
          PushLoopStack(cfgNode);
          AddOpenConnection(getCurrentLevel, cfgNode);

          lastNode:=proces_stmtlist(child, fl);
          debug('condition is negation of:' + conditions[0], True);
          addNode(cfgNode);
          // just throw away the node on the Loopstack
          PopLoopStack;
          decCurrentLevel;

          AddOpenConnection(getCurrentLevel, cfgNode);
        end;

      end;

    end;
    'AST_SWITCH':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      if n.getChildByKey(child, 'cond') then
      begin
        // in this case the 'condition' here is the first part of the total condition
        // --> the expression that has to be checked.
        // todo: the expression has to be parsed..
        debug('we can not handle all kinds of expressions in a SWITCH yet', True);
        condition_expression := UnparseConditionToPrefix(child);
        // todo: proces_exprlist
        // this the result that each case must be compared with
      end;

      if n.getChildByKey(child, 'stmts') then
      begin
        incCurrentLevel;
        PushLoopStack(cfgNode);
        nextnode := 0;
        while child.getChildByKey(child2, IntToStr(nextnode)) do
        begin
          if child2.getChildByKey(child3, 'cond') then
          begin
            // this is the second part of the condition
            // --> the value that the expression has to equal to
            condition_value := UnparseConditionToPrefix(child3);
            // todo:
            // this is the second part of the equallity
            // the result hereof must me equal to te result of the result in the switch
          end;
          if child2.getChildByKey(child3, 'stmts') then
          begin
            //ClearOpenConnections;

            if condition_value = 'null' then
              debug('condition: this is the ELSE condition', True)
            else
              debug('condition:' + condition_expression + '==' +
                condition_value, True);

            AddOpenConnection(getCurrentLevel, cfgNode);
            lastNode:=proces_stmtlist(child3, fl);

            PushOpenconnections;

          end;

          Inc(nextnode);
        end;
        PopAllOpenConnections;
        PopLoopStack;
        decCurrentLevel;
        //AddOpenConnection(getCurrentLevel, lastNode);
      end;
      //AddOpenConnection( getCurrentLevel, cfgNode )
    end;
    'AST_DO_WHILE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      if n.getChildByKey(child, 'cond') then
      begin
        // add the condition to the edge
        // if the cond==null, a ELSE branch is fount
        // remember the condition for an else clause
        conditions.add(UnparseConditionToPrefix(child));
        //showmessage(conditions[0]);
        debug('condition:' + conditions[0], True);
      end;


      if n.getChildByKey(child, 'stmts') then
      begin
        incCurrentLevel;
        PushLoopStack(cfgNode);
        AddOpenConnection(getCurrentLevel, cfgNode);

        lastNode:=proces_stmtlist(child, fl);


        lastNode.AddConnection(cfgNode);
        PopLoopStack;
        decCurrentLevel;
      end;
      debug('condition is negation of:' + conditions[0], True);

    end;
    'AST_FOR':
    begin
      // a FOR loop consists of 4 steps
      // - initialisation
      // - checking conditions
      // - execution statements
      // - prepare for next loop
      // Since these steps are executed in 4 sepereate places, we split te AST_FOR node in
      // 3 nodes + a stmtlist:
      // "AST_FOR_INIT": initialisation
      // "AST_FOR_COND": checking conditions
      // the statement to be executed (AST_STMT_LIST)
      // preparartion for next loop: "AST_FOR_LOOP"


      // the for node is inserted in the cfg AFTER the statements (expression) that
      // that initialiser the for loop
      //cfgNode := TCFGNode.Create(n);
      //lastNode:=cfgNode;
      //addNode(cfgNode);

      if n.getChildByKey(child, 'init') then
      begin
        // handle all expressions in the list
        //incCurrentLevel;
        //AddOpenConnection(getCurrentLevel, cfgNode);
        lastNode:=proces_stmtlist(child, fl);
        //AddOpenConnection(getCurrentLevel, lastNode);
        //lastNode.addConnection(cfgNode);
        //decCurrentLevel;
      end;

      // this is the AST_FOR node
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      PushLoopStack(cfgNode); // in case of a çontinue statement, we have to jump here


      // all subexpressions in the 'condition' are evaluated
      // but these subexpressions do not steer the continuation
      // except for the last condition,

      if n.getChildByKey(child, 'cond') then
      begin
        // add the condition to the edge
        // if the cond==null, a ELSE branch is fount

        // handle all expressions in the list
        //#incCurrentLevel;


        // since this expression list is handled differently we
        // do not use proces_exprlist, but we sift thought it here

        // n.count is the number of childeren
        // the first n-1 (possibly 0) nodes are inserted as expression-node
        // the last is used as condition

        //if child.count>1 then begin
          for i:=0 to child.count-2 do begin // -2, was off-byone-errorrrr
            child.getChildByKey(child2, IntToStr(i));
            conditionNode := TCFGNode.Create(child2);
            lastNode := TCFGNode.Create(child2);
            //lastNode:=conditionNode;
            addNode(lastNode);
            AddOpenConnection(getCurrentLevel, lastNode);
            end;
          //end;


        if child.getChildByKey(child2, IntToStr(child.count-1)) then begin
        // child contains the condition
          conditions.add(UnparseConditionToPrefix(child2));
        //showmessage(conditions[0]);
          debug('condition:' + conditions[0], True);
        end;

        //AddOpenConnection(getCurrentLevel, lastNode);
      end;

      //AddOpenConnection(getCurrentLevel, lastNode);
      incCurrentLevel;
      if n.getChildByKey(child, 'stmts') then
      begin
        // uit AST_WHILE
        //incCurrentLevel;
        //PushLoopStack(cfgNode);
        //AddOpenConnection(getCurrentLevel, cfgNode);
        //
        //lastNode:=proces_stmtlist(child);
        //
        //
        //lastNode.AddConnection(cfgNode);
        //PopLoopStack;
        //decCurrentLevel;
        //
        //==


        AddOpenConnection(getCurrentLevel, lastNode);
        lastNode:=proces_stmtlist(child, fl);
        //AddOpenConnection(getCurrentLevel, lastNode);
        //lastNode.addConnection(cfgNode);


      end;

      if n.getChildByKey(child, 'loop') then
      begin
        //AddOpenConnection(getCurrentLevel, cfgNode);
        lastNode:=proces_stmtlist(child, fl);


        //lastNode.AddConnection(cfgNode);
        //addNode(cfgNode);
        //lastNode.addConnection(cfgNode);
        addNode(cfgNode)
      end
      // even if there are no loop statements, the cfg-graph returns to the AS_FOR node
      else
      // goed bekijken
        lastNode.AddConnection(cfgNode);

      decCurrentLevel;
      popLoopStack;

      // just throw away the node on the Loopstack

      //AddOpenConnection(getCurrentLevel, cfgNode);
      //AddOpenConnection(getCurrentLevel, cfgNode);


    end;
    'AST_FOREACH':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      IncCurrentLevel;
      PushLoopStack(cfgNode);
      // todo
      PopLoopStack;
      decCurrentLevel;
    end;
    'AST_INCLUDE_OR_EVAL':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_TRY':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_BINARY_OP':
    begin
      // this node type is not handled here, but by handling each operator node in the expression tree separate

      //cfgNode := TCFGNode.Create(n);
      //lastNode:=cfgNode;
      //addNode(cfgNode);
      //AddOpenConnection(getCurrentLevel, cfgNode);
      //logging('unsupported_statements', n.Value, True);
    end;

    'AST_CLASS':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_NAMESPACE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_CONST_DECL':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_STATIC_CALL':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_EXIT':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_METHOD_CALL':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_USE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_ASSIGN_REF':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_DECLARE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
      logging('unsupported_statements', n.Value, True);
    end;
    'AST_FUNC_DECL':
    begin
      // create a new cfg
      // construct the cfg from the stmt_list
      // todo: mechanism for passing parameters

      // put this node on the functionlist
      // when we encounter a FUNC_CALL, we can search for a matching function

      // note
      //'flags' are ignored

      if n.getchildByKey(child, 'name') then begin
        functionname := UnQuoteString(child.value);
        //showmessage('functionname='+functionname);
      // get paramlist
        if n.getchildByKey(child, 'params') then begin
          nextnode:=0;
          paramList := TParamList.Create;
          while child.getchildByKey(child2, inttostr(nextnode)) do begin
            // note
            // flags,type and default are ignored
            // thought there are no many opputunities for setting the type in PGP,
            // ignoring the type might not be to smart...
            if child2.getchildByKey(child3, 'name') then begin
              paramname := UnquoteString(child3.Value);
              paramList.add( paramname );
              //showmessage('param='+paramname);
              end;
            inc(nextnode);
            end;
          FL.addUserFunction( functionname, paramList.toString);
      // add function to functionlist


          end;
       if n.getchildByKey(child, 'stmts') then
       begin

         // todo: create a separate cfg for this function

       end;


      end;
    end;
    'AST_CALL':
    begin
      // function calls are handled when mergeing cfg's
      // all function call are replaced by a copy of the function's graph
      // parameter and variable names are make unique

       // insert the cfg
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);

      AddOpenConnection(getCurrentLevel, cfgNode);
      //logging('unsupported_functions',n.value,true);
      //if n.getChildByKey(child, 'expr') then
      //begin
      //  if child.getChildByKey(child2, 'name') then
      //  // create a separate cfg for each function
      //  //logging('unsupported_functions', UnQuoteString(child.Value), True);
      //  AddOpenConnection(getCurrentLevel, cfgNode);
      //end;
    end;

    'AST_ECHO':
    begin
      LHS:=NewResultVar;
      if n.getChildByKey(child,'expr') then begin
        RHS:=proces_ExprNode(child, fl);


        end;


      cfgNode := TCFGNode.Create(n);
      cfgNode.smtlibString:='(assert(= '+LHS+' '+RHS+'))';
      cfgNode.infixString:=LHS+'='+RHS;
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_PRINT':
    begin
     LHS:=NewResultVar;
     if n.getChildByKey(child,'expr') then begin
       RHS:=proces_ExprNode(child, fl);
       end;

     cfgNode := TCFGNode.Create(n);
     cfgNode.smtlibString:='(assert(= '+LHS+' '+RHS+'))';
     cfgNode.infixString:=LHS+'='+RHS;
     lastNode:=cfgNode;
     addNode(cfgNode);
     AddOpenConnection(getCurrentLevel, cfgNode);
    end;


    'AST_PRE_INC':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_POST_INC':
    begin
    //1: AST_POST_DEC @ 10
    //    var: AST_VAR @ 10
    //        name: "x"

      if n.getChildByKey(child,'var') then begin
        if child.value='AST_VAR' then begin
            if child.getChildByKey(child2,'name') then begin
            RHS:=UnQuoteString(child2.value);
            end;
          end;
        end;

      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_PRE_DEC':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_POST_DEC':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      AddOpenConnection(getCurrentLevel, cfgNode);
    end;
    'AST_CONTINUE':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      // handle Continue for SWITCH, DO-WHILE,WHILE, FOR
      if n.getChildByKey(child, 'depth') then
        if child.Value = 'null' then
          continue_depth := 1
        else
          continue_depth := StrToInt(child.Value);

      LoopNode:=getLoopStack(continue_depth);

      if (continue_depth=1) and (LoopNode.FAstNode.Value='AST_SWITCH') then begin
        AddOpenConnection(getCurrentLevel - break_depth, cfgNode);
        debug('break to level ' + IntToStr(getCurrentLevel - break_depth), True);
        end
      else
        cfgNode.addConnection(LoopNode);
    end;
    'AST_RETURN':
    begin
    LHS:=NewResultVar;
    if n.getChildByKey(child,'expr') then begin
      RHS:=proces_ExprNode(child, fl);
      end;

    cfgNode := TCFGNode.Create(n);

    cfgNode.SMTLibString:='(assert(= '+LHS+' '+RHS+'))';
    cfgNode.infixString:=LHS+'='+RHS;

    lastNode:=cfgNode;
    addNode(cfgNode);

    //LastStatementWasBreak := True;
    AddOpenConnection(getCurrentLevel, cfgNode);
    debug('return', True);

    end;
    'AST_BREAK':
    begin
      cfgNode := TCFGNode.Create(n);
      lastNode:=cfgNode;
      addNode(cfgNode);
      // handle Break for SWITCH, DO-WHILE, WHILE, FOR
      if n.getChildByKey(child, 'depth') then
        if child.Value = 'null' then
          break_depth := 1
        else
          break_depth := StrToInt(child.Value);

      //LastStatementWasBreak := True;
      AddOpenConnection(getCurrentLevel - break_depth, cfgNode);
      debug('break to level ' + IntToStr(getCurrentLevel - break_depth), True);
    end;
    else
    begin
      // TODO
      // this is the place where we can detect/filter for
      // unsupported internal functions

      //showmessage('Unsupported statement:'+n.value);
      logging('unsupported_statements', n.Value, True);
    end;
  end;
  result:=lastNode;
end;


function  TCFG.proces_stmtlist(n: TTreeNode; FL: TFunctionList):TCFGnode;
var
  c, i: integer;
  lastNode: TCFGNode;
begin
  c := n.Count;
  for i := 0 to c - 1 do
  begin
    // add the CFG-node to the CFG
    lastNode:=Proces_statement(n.children[i], fl);
  end;
  result:=lastNode;
end;


function TCFG.proces_exprlist(n: TTreeNode; FL: TFunctionList):string;
var
  c, i: integer;
  //lastNode: TCFGNode;
  res:string;

begin
  c := n.Count;
  for i := 0 to c - 1 do
  begin
    // add the CFG-node to the CFG
    res:=Proces_exprnode(n.children[i], fl);
  end;
  result:=res;

end;



procedure TCFG.BuildCFGFromAST( n: TTreeNode; FL: TFunctionList);
var
  AnotherCfgNode: TCFGNode;
  child : tTreeNode;
begin
  // create a cfg-node
  AnotherCfgNode := TCFGNode.Create(n);

  // the first (and only) child is the statement list
  //showmessage('TODO zorg dat we de stmtlist gebruiken');

  // is this the node of a statementment list, we proces each
  // statement S and put a node for S in de cfg
  // in case of an AST_FUNC_DECL we add a separate CFG to the world

  if n.Children[0].Key = 'AST_STMT_LIST' then
  begin
    n:=n.Children[0];
    // in the main routine
    proces_stmtlist(n, fl);
    //showmessage(n.value);

  end
  else
  begin
    If n.GetChildByKey(child,'stmts') then begin
      // in a function declaration

      //showmessage(n.value);

      proces_stmtlist(child, fl);
      end;
    end;

  AnotherCfgNode := AnotherCfgNode.Create;
  AnotherCfgNode.Flabel := '<end>';
  AddNode(AnotherCfgNode);
end;

function TCFG.find_all_simple_paths(_from,_to:TCFGNode; var path:TPath):TPathList;
var //connections     : TList; // of TCFGNode
    connection      : TCFGNode;
    pathcopy        : TPath;
    //newpath         : TPath;

    paths, newpaths : TPathlist;

    i,j,k: integer;

    found : boolean;

begin
  // create an empty list of paths
  paths:=TPathList.Create;

  // local copy of the path (that is to be extended)
  pathcopy := TPath.Create;
  //showmessage( inttostr( Path.Count ));
  for i := 0 to Path.Count - 1 do begin
    PathCopy.Add(Path.items[i]);
    end;

   // _from is the first vertex in the path
   pathcopy.add(_from);

  // when _from=_to we reached the endpoint
  if _from.Flabel =_to.Flabel then begin
    paths.add(pathcopy);
    end
  else begin
    // the connection of the _from node reside in the edgelist

    // for alle connections
    for i := 0 to _from.FEdgeList.Count - 1 do begin
      connection := TCFGEdge(_from.FEdgeList[i]).node;
      // check wether the connection is in the path allready

      found:=false;
      for j:=0 to path.count-1 do begin
        if TCfgNode(path.Items[j]).Flabel = connection.Flabel then
          found:=true
        end;

      if not found then begin

        newpaths := TPathList.Create;
        newPaths := find_all_simple_paths( connection, _to, pathcopy);

        for k:=0 to newpaths.count-1 do begin
          paths.add(  TPath( newpaths.items[k]) );
          end;
        End;
      End;
  end;
  result := paths;
end;

function TCFG.find_all_paths(_from,_to:TCFGNode; max_passes:integer; var path:TPath):TPathList;
var //connections     : TList; // of TCFGNode
    connection      : TCFGNode;
    pathcopy        : TPath;
    //newpath         : TPath;

    paths, newpaths : TPathlist;

    i,j,k: integer;

    count : integer;

begin
  // create an empty list of paths
  paths:=TPathList.Create;

  // local copy of the path (that is to be extended)
  pathcopy := TPath.Create;
  //showmessage( inttostr( Path.Count ));
  for i := 0 to Path.Count - 1 do begin
    PathCopy.Add(Path.items[i]);
    end;

   // _from is the first vertex in the path
   pathcopy.add(_from);

  // when _from=_to we reached the endpoint
  //if _from.Flabel =_to.Flabel then begin
  if _from =_to then begin
    paths.add(pathcopy);
    end
  else begin
    // the connection of the _from node reside in the edgelist

    // for alle connections
    for i := 0 to _from.FEdgeList.Count - 1 do begin
      connection := TCFGEdge(_from.FEdgeList[i]).node;
      // check wether the connection is in the path allready

      count:=0;
      for j:=0 to path.count-1 do begin
        //if TCfgNode(path.Items[j]).Flabel = connection.Flabel then
        if TCfgNode(path.Items[j])= connection then

          inc(count)
        end;

      if count<max_passes then begin

        newpaths := TPathList.Create;
        newPaths := find_all_paths( connection, _to, max_passes, pathcopy);

        for k:=0 to newpaths.count-1 do begin
          paths.add(  TPath( newpaths.items[k]) );
          end;
        End;
      End;
  end;
  result := paths;
end;



function TCFG.contains( cfgNode : TCFGnode ):boolean;
var copiedNodes : TList;

  function process_cfg_node ( _cn , _cfgNode:TCFGNode  ) : boolean;
  var c,i: integer;
      child : TCFGNode;
      res:boolean;
  begin
    result:=false;
    if _cn = _cfgNode then
      res := true
    else begin
      c:=_cn.FEdgeList.count;
      for i:=0 to c-1 do begin
        child := TCFGEdge( _cn.FEdgeList[i] ).node;
        if copiedNodes.indexOf( child ) =-1 then begin
          copiedNodes.add( child );
          if process_cfg_node( child, _cfgNode) then
            res:=true
          end;
        end;
    end;
    result:=res;
  end;

begin
// start at the root
// walk repeatedly through all children
// put all nodes in a 'visited' list, to prevent multiple visits
  copiedNodes := TList.Create;

  result := process_cfg_node( Froot, cfgNode );

end;


function TCFG.getCFGNodeWithASTNode( astnode : TTreeNode) :TCFGNode;
var ProcedNodes : TList;

  function process_cfg_node ( cfgNode:TCFGnode; _astNode:TTreeNode ) : TCFGnode;
  var c,i   : integer;
      child,
      res   : TCFGNode;
  begin
    res:=nil;
    if assigned(cfgNode.FAstNode) and _astnode.equals(cfgNode.FAstNode) then
      res := cfgNode
    else begin
      c:=cfgNode.FEdgeList.count;
      i:=0;
      while (i<c) and (res=nil) do begin
        child := TCFGEdge( cfgNode.FEdgeList[i] ).node;
        if ProcedNodes.IndexOf( child ) = -1 then begin
          ProcedNodes.add( child );
          res := process_cfg_node ( child, _astnode );
          end;

        inc(i);
      end;
    end;

    result:=res;
  end;

begin
  // start at the root
  // walk repeatedly through all children
  // put all nodes in a 'visited' list, to prevent multiple visits
  ProcedNodes := TList.Create;

  result := process_cfg_node( Froot, astNode );

end;

function TCFG.toStringList(pathlist:TPathList):TStringList;
var i,j:integer;
    path:TPath;
    SL :TStringList;
begin
  SL := TStringList.Create;

  showmessage('number of paths: '+inttostr(pathlist.count));
  for i:=0 to pathlist.count-1 do begin

    path := pathlist.items[i];

    //showmessage('number of nodes in path: '+inttostr(path.count));
    SL.add('== path '+inttostr(i+1)+ '== nodes in path:' + inttostr(path.count)+ ' ===');
    for j:=0 to path.count-1 do begin

      //SL.add( TCfgNode(path.items[j]).SMTLibString );
      SL.add( TCfgNode(path.items[j]).Flabel );
      end;
    SL.add('');

    end;
  result := SL
end;

//=== TCFGWorld
// The CFGWorld reads cfgs from a filelist
// each cfg is from either the 'main'-code or a function declaration


constructor TCFGWorld.Create;
begin
  world := TList.Create;
  FFunctionList:=TFunctionList.Create;
  cfgFunctionNames:=TStringList.Create;
end;

procedure TCFGWorld.addCFG(c: tCFG; _name:string);
begin
  world.add(c);
  cfgFunctionNames.add(_name);;
end;

function TCFGWorld.getCFGbyName(_name:string):TCFG;
var i:integer;
begin
  i:=cfgFunctionNames.indexOf(_name);
  if i<>-1 then begin
    result:=TCFG(world[i]);
    end
  else begin
    //RaiseError('ERROR (TCFGWorld.getCFGbyName): no functionname name '+_name+' in CFG-world.');
    result:=nil;
    end;

end;


function TCFGWorld.GetCount:integer;
begin
  result:=World.count;
end;



procedure TCFGWorld.LoadFromFile( path: string; phpFile : string );
var ListOfNodesWithFunctionDeclarations : TList;
    node:TTreeNode;
    ast : TTree;
    functionname,
    astFile : String;
    cfg : TCFG;
    i,c:integer;
begin
  debug('new run: '+phpFile , false);

  // get AST file for PHP file
  astFile := getASTfileOfPHPfile( phpFile );
  // load the AST
  ast := TTree.Create;
  ast.getAST( path+ astFile );

  // cfg's are build from the root and all nodes in ListOfNodesWithFunctionDeclarations

  // we start with 'main'
  cfg:=TCFG.Create(getASTfileOfPHPfile( phpFile ));
  cfg.BuildCFGFromAST( ast.rootnode , FfunctionList);
  addCFG(cfg,'phpfile_main');
  //cfg.ExportAsDotty.savetofile(path+'main.dot');

  // scan for AST_FUNC_DECL
  ListOfNodesWithFunctionDeclarations := ast.getFunctionCalls;
  c:=ListOfNodesWithFunctionDeclarations.count;
  for i:=0 to c-1 do begin
    node := TTreeNode(ListOfNodesWithFunctionDeclarations[i]);
    functionname:=node.getFunctionname;
    cfg:=TCFG.Create(getASTfileOfPHPfile( phpFile ));
    cfg.BuildCFGFromAST( node, FFunctionList );
    addCFG(cfg, functionname);
    // todo:
    // retrieve the function name
    // --set the name in the cfg and
    // --use the name in the dotty export
    //cfg.ExportAsDotty.savetofile(path+'function'+inttostr(i)+'.dot');
    end;

  //showmessage(inttostr(ListOfNodesWithFunctionDeclarations.count));
end;

procedure TCFGWorld.LoadFromFiles(path: string; filelist : TStrings);
var i,c:integer;
begin
  // for all files
  //   scan the ast for function declarations (AST_FUNC_DECL)
  //   build an cfg for main (starting from the root node of the ast)
  //   and for all function declarations
  // finally this results in a set of cfg's
  // the cfg's kan be connected with BuildSuperCfg

  c:=filelist.count;
  for i:=0 to c-1 do begin

    LoadFromFile( path, filelist[i] );

    end;

end;

function TCFGWorld.exportAsDotty:TStringList;
var c,i: integer;
    graph,subgraph:TStringList;
begin
  graph:=TStringList.Create;
  graph.add('digraph G {');
  c:=count;
  for i:=0 to c-1 do begin
    subgraph:=TStringList.Create;
    subgraph:=TCFG(world[i]).ExportAsDotty('G'+inttostr(i+1)); // some unique name
    graph.addstrings(subgraph);

    end;
  graph.Add('}');
  result:=graph;
  //graph.savetofile('super_cfg.dot');
end;

procedure TCFGWorld.BuildSuperCFG( max_recursion : integer );
begin
  // This connects all cfg's in the cfg-world
  // In order to prevent builing an infite sized cfg
  // we restrict the number of recurive call's and maximise the number of loops
end;

function TCFGWorld.getCFGWithCFGnode( cn : TCFGnode ) : TCFG;
var i:integer;
    cfg:TCFG;
Begin
  result:=nil;
  for i:=0 to world.count-1 do begin
    cfg:=TCFG(world[i]);
    if cfg.contains( cn ) then
      result:=cfg;
    end;
end;

function TCFGWorld.getCFGNodeWithASTNode( astNode : TTreeNode) : TCFGNode;
var c,i:integer;
    cfgNode :TCFGNode;
Begin
  cfgNode:=nil;
  c:=world.count;
  i:=0;
  repeat
    cfgNode := TCFG(world[i]).getCFGNodeWithASTNode( astNode );
    inc(i);
  until (i>=c) or (cfgNode<>nil);
  result:=cfgNode;
end;

function TCFGWorld.MergeCFGs:TCFG;
var c,i:integer;
    globalcounter:integer;
    cfg:TCFG;


  procedure proces_cfg( _cfg : TCFG);
  var copiedNodes:TList;

    procedure proces_node( cn : TCFGnode);
    var index,c,i,j:integer;
        n : TCFGnode;
        child, child2, child3:TTreeNode;

        LHS, RHS,
        variablename, functionname : string;
        arglist, paramlist : Tparamlist;

        cfg2, cfgCopy : TCFG;

        exitnodes : TList;

        dest,
        lastNode,
        cfgnode : TCFGnode;
    begin
      //exportAsDotty.saveToFile('step-by-step'+inttostr(globalcounter)+'.dot');
      //inc(globalcounter);
      // did we visit this node already?
      if copiedNodes.IndexOf( cn ) = -1 then begin
        // add it to the list of visited nodes
        copiedNodes.add( cn );
        //c:=copiedNodes.count;
        //showmessage(inttostr(c));
        // if the node is a function call
        if ( assigned(cn.FAstNode) ) and ( cn.FAstNode.Value = 'AST_CALL' ) then begin
          // proces the node

          //expr: AST_CALL @ 13
          //    expr: AST_NAME @ 13
          //        flags: NAME_NOT_FQ (1)
          //        name: "add"
          //    args: AST_ARG_LIST @ 13
          //        0: AST_VAR @ 13
          //            name: "x"
          //        1: AST_VAR @ 13
          //            name: "y"

          // get the function name
          if cn.Fastnode.getchildByKey(child, 'expr') then begin
            if child.value='AST_NAME' then begin
              if child.getchildByKey(child2, 'name') then begin
                functionname:=UnquoteString(child2.value);
                //showmessage('function:'+functionname);
                end;
              end
            else begin
              if child.value='AST_VAR' then begin
                 if child.getchildByKey(child2, 'name') then begin
                   functionname:=UnquoteString(child2.value);
                   showmessage('functionvar:'+functionname);
                   end;
                 end
              else begin
                debug('ERROR: (MergeCFG>proces_cfg) We encountered a function call without an AST_NAME- or AST_VAR-node ('+child.value+') in the AST; unable to resolve a functionname...', true);
                raiseError('ERROR: (MergeCFG>proces_cfg) We encountered a function call without an AST_NAME- or AST_VAR-node ('+child.value+') in the AST; unable to resolve a functionname...');

                end;
            end;
          end;

           // get the arguments
          arglist := TparamList.create;
          if cn.Fastnode.getchildByKey(child, 'args') then begin
            if child.value='AST_ARG_LIST' then begin
              index:=0;
              while child.getchildByKey(child2, inttostr(index) ) do begin

                variablename:=_cfg.proces_ExprNode(child2, FFunctionList );

                arglist.add(variablename);
                //showmessage('argument:'+variablename);

                inc(index);
                end;
              end
            else begin
              // no arguments found
              // todo

              end
            end;

          // if A->B (this node)->C
          // - remove B
          // - connect/ insert parameter substitions if neccesary
          // - copy all nodes of f
          //   - alter variable naming
          //   - count this number of times that f is called: variable a is relabeled to f_001_a
          // - connect return nodes to c
          //   AND/OR connect the last node to c



          paramlist := FFunctionList.get_CTX_ParamListOfFunction(functionname);

          // are the number of parameters of the declaration and the number in the call equal
          // to do: ... operator in PHP 8 not supported
          //showmessage(arglist.tostring+' - '+paramlist.tostring);


          if arglist.count=paramlist.count then begin

            // for each pair (decl/call) add a node
            // we connect the first of these new node to cn
            // since the node is an AST_CALL, we expect only one edge.
            // we copy this edge, because we need it in order to connect return-nodes of the
            // function with the cfg

            //remember where cn points to
            //dest:=TCFGEdge(cn.FEdgeList[0]).copy;
            //cn.FEdgeList.clear;
            lastnode:=cn;
            //cfg.AddOpenConnection(cfg.getCurrentLevel, cn);

            for j:=0 to arglist.count-1 do begin

              RHS:=arglist.getParamByIndex(j);
              LHS:=paramlist.getParamByIndex(j);
              cfgNode := TCFGNode.Create(nil);// no reference to a treenode
              cfgNode.FLabel:='param_subst_'+functionname+'_'+inttostr(j);
              cfgNode.smtlibString:='(assert(= '+LHS+' '+RHS+'))';
              cfgNode.infixString:=LHS+'='+RHS;
              dest:=lastNode.getConnection;
              //exportAsDotty.saveToFile('test_insert_step_'+inttostr(j)+'a.dot');
              lastNode.setConnection(cfgNode);
              //exportAsDotty.saveToFile('test_insert_step_'+inttostr(j)+'b.dot');
              cfgNode.setConnection(dest);
              //exportAsDotty.saveToFile('test_insert_step_'+inttostr(j)+'c.dot');
              lastnode:=cfgNode;
              //exportAsDotty.saveToFile('test_insert_step_'+inttostr(j)+'d.dot');
              end;

            // in main: A->B (call F)->C
            // in F: first->2->...->return...->last
            // A->first->2->...->return...->last->C
            // And return points to C
               // search cfg of function
               cfg2:=getCFGbyName( functionname );
               if cfg2<>nil then begin

               // voeg de cfg in
                 // todo: maximaliseer de diepte
                 //proces_cfg( cfg2 );
                 cfgCopy := cfg2.copyCfg('copy');

                 _cfg.insertCfg(lastnode, cfgCopy);
                 _cfg.altDotty('A').savetofile('altdotty1.dot');
                 _cfg.exportasDotty('A').savetofile('altdotty2.dot');
                 _cfg.altDotty('A').savetofile('altdotty3.dot');


                 end
               else begin
                 // todo
                 // handle buildin functions
                 showmessage('TODO: handle buildin function '+functionname);
                 end;
             end
          else
          begin
            //showmessage(inttostr(arglist.count)+' - '+inttostr(paramlist.count));
            // TODO: if the number of arguments>params we could
            // fill in the given arg upto the numbers of params
            // the only real error is if arg<params

            logging('missing_functions', functionname + ' argcount='+inttostr(arglist.count)+ ' paramcount='+inttostr(paramlist.count), true);
            //raiseError('ERROR: (MergeCFG>proces_cfg) number of parameters of function '+functionname+' unequal in AST_CALL and AST_FUNC_DECL.');
            end;

          end;

        // proces the nodes where this node points to
        //end;
        c:=cn.FedgeList.count;
        for i:=0 to c-1 do begin
          n:=TCFGEdge( cn.FedgeList[i]).node;
          proces_node( n )
          end;

      end;
    end;

  begin
    copiedNodes := TList.Create;
    proces_node( _cfg.Froot );
    _cfg.altDotty('A').savetofile('altdotty4.dot');

  end;

Begin
  // merge all CFG's in this world to

  // create a list of all functions in this world
  // for all cfg's
  //    vitsit all nodes of cfg
  //    if node is function calls
  //      replace the call node A->call node f(p,q)->B
  //      determine the arguments
  //      insert subsitution node for each argument, A -> x=p -> y=q etc
  //      insert a copy of the function's cfg y=q -> first node of f
  //      each return node of f is connected to B

  // parameter naam x wordt f_x (om de naam x uniek te maken)
  // locale variabele u wordt f_u

  //  speciale aandacht voor cyclische functie aanroepen
  //  f->f->f->... (recursie)
  //  f->g->f->g->...
  //  f->g->h->
  //  maximale recursiediepte : aantal keer dat een cycle ingevoegd wordt
  //  cycle is (f->), (f->g->), (f->g->h)

  // we zouden de cycles vooraf kunnen bepalen
  // alernatief is om de call diepte bij te houden

  // parameters en locale variabele worden genummerd per assignment
  // f_001_a_001 -> functie f, call nummer, variabele name, assignment (wordt pas toegevoegd in path)

  // van de cfg waarmee we beginnen hoeft elke node maar 1x bekeken te worden
  // daarom houden we bij elke node verwerkt zijn.
  // voor de nodes van een functie die ge-insert wordt ligt dat anders
  // een functie kan meer dan 1x aangeroepen worden, dus kan het ook nodig zijn om een node van een functie meer dan 1x te bezoeken
  // we lossen dat op door per verwekte functie een lijst met visitnodes bij te houden

  // todo: doorloop hier alle main functies
  globalcounter:=0;

  c:=world.count;


  //for i:=0 to c-1 do begin
  //  showmessage( 'aantal knopen in graaf '+inttostr(i+1)+':'+inttostr(TCFG(world[i]).NodeCount) );
  //  TCFG(world[i]).exportAsDotty(inttostr(i)).saveToFile('vooraf'+inttostr(i)+'.dot');
  //  end;


  for i:=0 to c-1 do begin
    //showmessage( TCFG(world[i]).Fname );
    //TCFG(world[i]).exportAsDotty(inttostr(i)).saveToFile('vooraf_'+inttostr(i)+'.dot');
    proces_cfg( TCFG(world[i]) );
    TCFG(world[i]).altDotty2('A').savetofile('altdotty5.dot');
    end;

  result:=TCFG(world[0]);
end;

function TCFG.exportPathAsSMTLIB(path:TPath):TStringList;
var SL:TStringList;
    c,i:integer;
    cn:TCFGnode;
    ast:TTreeNode;
    child:TTreeNode;
    LHS, RHS : string;
begin
  SL:=TStringList.Create;
  c:=Path.Count;
  for i:=0 to c-1 do begin

    cn := TCFGnode( Path.Items[i] );
    //ast:=cn.astNode;

    //SL.add( cn.smtlibString );
    if assigned( cn.astNode ) then begin
      if cn.astnode.getChildByKey(child,'var') then
        LHS:=child.value;

      if cn.astnode.getChildByKey(child,'expr') then
        RHS:=child.value;


      SL.add( cn.astNode.Value +':'+ LHS +' '+RHS );
      logging('ast_to_smtlib', cn.astNode.Value +'   '+ LHS +'   '+RHS, true );
      end
    else
      SL.add('');

  end;
  Result:=SL;
end;


function TCFG.getCyclomaticComplexity:integer;
// number of discission points + 1
var cc:integer;
    visitedNodes : tList;

    procedure process_cfg_node ( _cn :TCFGNode  );
    var c,i, nextnode: integer;
        n, child, child2, child3:TTreeNode;
    begin
      if visitedNodes.IndexOf( _cn ) = -1 then begin
        visitedNodes.add( _cn );

        // increase cc for each WHILE, SWITCH, IF, DO-WHILE, FOR, FOREACH
        if assigned(_cn.astnode) then begin
          case _cn.astNode.Value of
          'AST_WHILE',
          'AST_DO_WHILE',
          'AST_FOREACH',
          'AST_FOR'      : inc(cc);
          'AST_IF' :  // exclude else with condition==null element
          begin

            nextnode := 0;
            n:=_cn.astnode;
            while n.getChildByKey(child, IntToStr(nextnode)) do
            begin
              if child.getChildByKey(child2, 'cond') then
              begin
                if child2.Value = 'null' then
                begin
                  // do not increase the Cyclomatic complexity
                end
                else
                begin
                  inc( cc );
                end;
              end;
              inc(nextnode);

            end;

          end;
          'AST_SWITCH': // exclude default-branch (condition==null) case
          begin

            n:=_cn.astnode;
            if n.getChildByKey(child, 'stmts') then
            begin
              nextnode := 0;
              while child.getChildByKey(child2, IntToStr(nextnode)) do
              begin
                if child2.getChildByKey(child3, 'cond') then
                begin
                  if UnQuoteString( child3.value ) <> 'null' then inc ( cc );
                end;
                inc(nextnode);
              end;
            end;
          end;
          end;
        end;

        c:=_cn.FEdgeList.count;
        for i:=0 to c-1 do begin
          process_cfg_node( TCFGEdge( _cn.FedgeList[i]).node );
        end;
      end;


    end;

begin
  visitedNodes:=TList.Create;
  cc:=1;
  process_cfg_node( Froot );
  result := cc;
end;





end.
