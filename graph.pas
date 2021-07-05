unit graph;

//mode objfpc}
//H+}
{$MODE Delphi}
interface

uses
  Classes, SysUtils, Grids;

type
   TErrorObject = class(Exception);

   TAdjListElement = class
   private
      vertexname : string;
      edgelist   : tStringList;

   public
      constructor Create(_vertexName:string);
   end;

   TGraph = class
   private

      adjecencylist : TList;
      function GetVertexCount: integer;
   public
      property VertexCount: integer read GetVertexCount;

      constructor Create;

      procedure clear;

      procedure addVertex(_vertexname:string);
      procedure addEdge(_from, _to:string);

      function isVertexInGraph(_vertexname:string):boolean;
      function indexOfVertex(_vertexname:string):integer;
      function isEdgeInGraph(_from,_to:string):boolean;
      function GetConnectionsOf(_from:string):TStringList;

      procedure displayAdjecencyList(sg:TStringGrid);


      function find_all_simple_paths(_from,_to:string; var path:TstringList):tlist;
      //destructor destroy; override;
      //procedure Clear;

   end;


   procedure RaiseError(const msg: string);

implementation

procedure debug(appendtext:boolean; s:string);
var f:text;
begin
  assignfile(f,'.\debug.txt');
  if appendtext then
    append(f)
  else
    rewrite(f);
  writeln(f,s);
  closefile(f);
end;

constructor TAdjListElement.Create(_vertexName:string);
begin
  vertexname := _vertexname;
  edgelist := TStringList.Create;
  edgelist.sorted:=true;
end;



constructor TGraph.Create;
begin
  adjecencylist := TList.Create;

end;

procedure TGraph.Clear;
var e : TAdjListElement;
Begin
  for e in adjecencyList do Begin
    e.edgeList.clear;
    end;

end;

procedure TGraph.addVertex(_vertexname:string);
var e: TAdjListElement;
begin
  if not isVertexInGraph(_vertexname) then begin
    e := TAdjListElement.Create(_vertexname);
    adjecencylist.add(e);
    end;
end;

function TGraph.isVertexInGraph(_vertexname:string):boolean;
var i:integer;
    //found:boolean;
begin
  isVertexInGraph:=false;
  for i:=0 to GetVertexCount-1 do begin
    if TAdjListElement(adjecencylist[i]).vertexname=_vertexname then
      isVertexInGraph:=true;
    end;
end;

function TGraph.isEdgeInGraph(_from,_to:string):boolean;
var i:integer;
begin
  i:=IndexOfVertex(_from);
  if i=-1 then
    isEdgeInGraph:=false
  else
    isEdgeInGraph:=(TAdjListElement(adjecencylist[i]).edgeList.IndexOf(_to)<>-1);
end;

function TGraph.indexOfVertex(_vertexname:string):integer;
var i:integer;
begin
  indexOfVertex:=-1;
  for i:=0 to GetVertexCount-1 do begin
    if TAdjListElement(adjecencylist[i]).vertexname=_vertexname then
      indexOfVertex:=i;
    end;
end;



procedure TGraph.addEdge(_from,_to:string);
var i: integer;
begin
  // does _to exist?
  i := IndexOfVertex(_to);
  if i=-1 then
    RaiseError('Error while adding an edge. Vertex (destination) '+_to+' must be added before adding an edge to it.');

  // does _from exist?
  i := IndexOfVertex(_from);
  if i=-1 then
    RaiseError('Error while adding an edge. Vertex (source) '+_from+' not found.');

  // is there an edge between _from and _to allready?
  if not isEdgeInGraph(_from,_to) then
    TAdjListElement(adjecencylist[i]).edgelist.add(_to);
  //else
  //  RaiseError('This edge is already present in the graph. (source, destination)=('+_from+','+_to+')');

end;

function TGraph.GetVertexCount: integer;
begin
  result:=self.adjecencylist.Count;
end;

function TGraph.GetConnectionsOf(_from:string):TStringList;
var i:integer;
    sl:TStringList;
Begin
  // does _from exist?
  i := IndexOfVertex(_from);
  if i=-1 then
    RaiseError('Error while retrieving connections of vertex '+_from+'. Vertex '+_from+' is not in the graph.');

  sl := TStringList.Create;
  sl.addStrings( TAdjListElement(adjecencylist[i]).edgelist );

  result := sl;
end;

// display the adjecencylist in a StringGrid.
procedure TGraph.displayAdjecencyList(sg:TStringGrid);
var i:integer;
    ae:TAdjListElement;
begin
  sg.RowCount := self.adjecencylist.count+1;
  sg.ColCount := 2;
  sg.cells[0,0]:='vertex';
  sg.cells[1,0]:='connections';
  for i:=0 to self.adjecencylist.count-1 do begin
    ae := TAdjListElement(self.adjecencylist[i]);
    sg.Cells[0,i+1]:=ae.vertexname;
    sg.cells[1,i+1]:=ae.edgelist.delimitedtext;
    end;

end;

function TGraph.find_all_simple_paths(_from,_to:string; var path:TStringList):tlist;
var s               : string;
    connection      : string;
    pathcopy,
    connections,
    newpath         : TStringList;

    paths, newpaths : tlist;

    i: integer;

begin
  debug(true,'We start (again) with path:'+path.DelimitedText);
  debug(true,'_from:'+_from+' _to:'+_to);



  // create an empty list of paths
  paths:=tlist.Create;

  // local copy of the path (that is to be extended)
  pathcopy := tStringList.Create;
  for s in path do
    pathcopy.add(s);


  // _from is the first vertex in the path

  pathcopy.add(_from);

  debug(true,'current path:'+ pathcopy.DelimitedText);

  // when _from=_to we reached the endpoint
  if _from=_to then begin
    debug(true,'from=to --> endpoint reached');
    debug(true,'adding to path:'+pathcopy.delimitedtext);
    paths.add(pathcopy);

    //debug(true,'end of function');
    //result:=TList.Create;
    //
    //result.assign(paths);
    end

  else begin
    connections := TStringlist.Create;
    connections.addStrings( GetConnectionsOf(_from ) );
    debug(true,'connections of '+_from+':'+connections.DelimitedText);
    For connection in Connections Do Begin
      debug(true,'Try connection:'+connection);


      If Path.IndexOf(connection)=-1 Then Begin // connection not in path, so far
        //showmessage('BUG: path wordt na elke aanroep veranderd');
        // copy of paths, since it will be passed by reference.

        debug(true,connection+' NOT in path');

        //pathcopy:=TStringList.Create;
        //for i := 0 to Path.Count - 1 do
        //  Pathcopy.Add(Path[i]);

        debug(true,'path before search:'+pathcopy.delimitedText);

        newpaths := TList.Create;
        newPaths := find_all_simple_paths( connection, _to, pathcopy);
        debug(true,'newpaths:');
        for i:=0 to newpaths.count-1 do
          debug(true,'> '+TStringList(newpaths[i]).DelimitedText);


        debug(true,'adding newpaths to path');
        for newpath in newpaths do begin
          paths.add( newpath );
          debug(true,'->'+ newpath.delimitedText);
          end;

        End
      else
        debug(true,connection+' already in path');

      debug(true,'paths:');
      for i:=0 to paths.count-1 do
        debug(true,'> '+TStringList(paths[i]).DelimitedText);

      End;

    end;

  // the result is a tlist of tstrings (paths = list of path)
  debug(true,'end of function');
  //result:=TList.Create;

  //result.assign(paths);
  result := paths;
end;

procedure RaiseError(const msg: string);
begin
   raise TErrorObject.Create(msg);
end;

end.

