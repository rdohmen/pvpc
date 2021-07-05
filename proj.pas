unit proj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, treeclass, utils, LazFileUtils, graph, test, Forest, cfg;

type
  TErrorObject = class(Exception);

  TProject = class
  private
    // filename and path of projectfile
    filename, path: string;

    FFileList: TStringList;

    testList: TTestList;
    FcurrentTest: integer;

    // the forest contains the filenames and the trees
    FForest: TForest;
    FCFGWorld: TCFGWorld;

    function GetFileByIndex(i: integer): string;
    function GetFileCount: integer;
    function getCurrentTest: TTest;
  public
    functiondeclarations, functioncalls: TStringList;
    entrynode, exitnode: TTreeNode;
    g: TGraph;
    test: string;
    constructor Create();
    procedure LoadFromFile(fn: string);
    procedure SaveToFile(fn: string);
    function getFileList: TStringList;
    procedure addFile(phpfilename: string);
    procedure removefile(fn: string);

    function getPath: string;
    property currenttest: TTest read GetCurrentTest;

    property FileCount: integer read GetFileCount;
    property files[index: integer]: string read GetFileByIndex;
    function getFunctionDeclarations(node: TTreeNode; Source: string): TStringList;
    function getFunctionCalls(node: TTreeNode; Source: string): TStringList;

    function getTreeOfFile(phpFilename: string): TTree;

    procedure ToFile(fn: string);

    procedure CreateCallGraph;

    property Forest: TForest read FForest;
    property World: TCFGWorld read FCFGWorld;
  end;

implementation

procedure RaiseError(const msg: string);
begin
  raise TErrorObject.Create(msg);
end;


constructor TProject.Create();
begin
  FFileList := TStringList.Create;
  FunctionDeclarations := TStringList.Create;
  FunctionCalls := TStringList.Create;
  FForest := TForest.Create;
  FCFGWorld := TCFGWorld.Create;

end;

procedure TProject.LoadFromFile(fn: string);
var
  i: integer;
  projectfile: TStringList;
begin
  filename := fn;
  Path := ExtractFilePath(fn);

  projectfile := TStringList.Create;

  projectfile.loadFromFile(filename);
  // extract filenames and create trees
  for i := 0 to projectfile.Count - 1 do
  begin
    projectfile[i] := ExtractFileName(projectfile[i]);
    addFile(projectfile[i]);
    //FForest.LoadTreeFromFile( path + getASTfileOfPHPfile( projectFile[i] ));
  end;

  //showmessage(inttostr(Ffilelist.count));

  // create a place for all CFG's

  //FCFGWorld.LoadFromFiles(path, projectfile);

  //Showmessage('Number of trees'+inttostr(CFGWorld.Count));

  // create an CFG from the AST
  //cfg:=TCFG.Create(getASTfileOfPHPfile(opendialog1.files[i]));
  //cfg.BuildCFGFromAST(ast);
  //memo2.clear;
  //memo2.Lines.addstrings(cfg.ExportAsDotty);
  //cfg.addlabels;

end;

procedure TProject.SaveToFile(fn: string);
begin
  Ffilelist.SaveToFile(fn);
  filename := ExtractFilename(fn);
  Path := ExtractFilePath(fn);
end;

function TProject.getFileList: TStringList;
begin
  getFileList := Ffilelist;
end;

function TProject.getFunctionDeclarations(node: TTreeNode; Source: string): TStringList;
  //var
  //functiondeclarations: TStringList;

  function procesnode(node: TTreeNode; Source: string): TStringList;
  var
    fn: TStringList;
    c, i: integer;
    parent, child, child2: TTreeNode;
    variablename, functionname, filename: string;
    includetree: TTree;
  begin
    // the available function are declared either in the current file
    // or included/require from another file

    fn := TStringList.Create;
    case node.Value of
      'AST_CLOSURE':
      begin
        // vraag de naam van de variable waaraan de anonieme functie toegewezen werd
        parent := node.parent;
        if parent.getChildByKey(child, 'var') then
          if child.getChildByKey(child2, 'name') then
            variablename := UnQuoteString(child2.Value);

        // vraag de naam van de functie op (is in dit geval altijd {closure})
        if node.getChildByKey(child, 'name') then
        begin
          functionname := UnQuoteString(child.Value);
          fn.add(Source + '.' + variablename + ':' + functionname);

          for i := 0 to node.Count - 1 do
            fn.addstrings(procesnode(node.children[i],
              Source + '.' + variablename + ':' + functionname));

        end;
      end;
      'AST_FUNC_DECL':
      begin
        if node.getChildByKey(child, 'name') then
        begin
          functionname := UnQuoteString(child.Value);
          fn.add(functionname);
          for i := 0 to node.Count - 1 do
            fn.addstrings(procesnode(node.children[i], functionname));

        end;
      end;
      'AST_INCLUDE_OR_EVAL':
      begin
        // via een include/require kunnen functies beschikbaar komen
        // deze zetten we in een lijst functiondeclarations
        // dit doen we voor alle require/includes

        // het kind met key 'expr' bevat de bestandsnaam

        if node.getChildByKey(child, 'expr') then
        begin
          filename := UnquoteString(child.Value);
          filename := getASTfileOfPHPfile(filename);

          includetree := TTree.Create;
          includetree.getAST(Path + filename);
          fn.addStrings(getFunctionDeclarations(includetree.rootnode, Source));
        end;
      end

      else
      begin
        c := node.Count;
        for i := 0 to c - 1 do
        begin
          fn.addstrings(procesnode(node.children[i], Source));
        end;
      end;
    end;
    Result := fn;
  end;

begin
  // maak een lijst met alle functiedeclaraties in deze boom
  // value=AST_FUNC_DECL => value=expr => key=name, value=functionname
  functiondeclarations := procesnode(node, Source);
  Result := functiondeclarations;
end;

function TProject.getFunctionCalls(node: TTreeNode; Source: string): TStringList;
var
  //availablefunctions,
  //functioncalls: TStringList;

  variablename, functionname: string;

  function procesnode(node: TTreeNode; Source: string): TStringList;
  var
    fn: TStringList;
    c, i: integer;
    child, child2, child3: TTreeNode;
    s, filename: string;
    includetree: TTree;
  begin
    fn := TStringList.Create;
    case node.Value of
      'AST_CALL':
      begin
        if node.getChildByKey(child, 'expr') then
        begin
          if child.Value = 'AST_NAME' then
            if child.getChildByKey(child2, 'name') then
              //fn.add(Source + '->FUNCNAME:' + UnQuoteString(child2.Value));
              fn.add(Source + '->' + UnQuoteString(child2.Value));

        end;
      end;
      'AST_FUNC_DECL': // search for function call in a function declaration
      begin
        if node.getChildByKey(child, 'name') then
        begin
          functionname := UnQuoteString(child.Value);
          // voeg deze functie toe aan de beschikbare functie
          // .. nodig om functievariabelen te kunnen herkennen
          // ..   $var = 'functioname';
          functiondeclarations.add(functionname);

          for i := 0 to node.Count - 1 do
            //fn.addstrings(procesnode(node.children[i], source+'.'+functionname));
            fn.addstrings(procesnode(node.children[i], functionname));
        end;
      end;
      'AST_INCLUDE_OR_EVAL':
        // we hebben de beschikbare functies nodig om te kunnen bepalen
        // of een AST_ASSIGN een functievariable is
      begin
        // via een include/require kunnen functies beschikbaar komen
        // deze zetten we in een lijst functiondeclarations
        // dit doen we voor alle require/includes

        // het kind met key 'expr' bevat de bestandsnaam

        if node.getChildByKey(child, 'expr') then
        begin
          filename := UnquoteString(child.Value);
          filename := getASTfileOfPHPfile(filename);

          includetree := TTree.Create;
          includetree.getAST(Path + filename);
          functiondeclarations.addStrings(getFunctionDeclarations(
            includetree.rootnode, Source));
        end;
      end;


      'AST_ASSIGN':
      begin
        if node.getChildByKey(child, 'var') then
        begin
          // "leftside" -> the variable that is assigned to
          if child.getChildByKey(child2, 'name') then
            variablename := UnQuoteString(child2.Value);


          // "rightside"
          if node.getChildByKey(child, 'expr') then
          begin
            case child.Value of
              'AST_CLOSURE':
              begin
                if child.getChildByKey(child2, 'name') then
                begin
                  functionname := UnQuoteString(child2.Value);
                  //fn.add(Source + '->FUNCVARCLOSURE:' + variablename+','+functionname);
                  fn.add(Source + '->' + functionname);

                  // doorloop de stmts om te zien of er evt een functiecall in zit
                  for i := 0 to child.Count - 1 do
                    //fn.addstrings(procesnode(child.children[i], source+'.'+functionname));
                    fn.addstrings(procesnode(child.children[i], functionname));
                end;
              end;
              'AST_CALL': // call to buid-in function e.g. $t = date("H");
              begin
                //RaiseError('assigning a function call to a variabele is not a valid syntax');
                if child.getChildByKey(child2, 'expr') then
                begin
                  if child2.Value = 'AST_NAME' then
                    if child2.getChildByKey(child3, 'name') then
                    begin
                      functionname := UnQuoteString(child3.Value);
                      //fn.add(Source + '->FUNCVARNAME:' + variablename+','+functionname);
                      fn.add(Source + '->' + functionname);
                    end;
                end;
              end
              else
              begin
                // todo: other types
                // when a string is assigned, check whether the string matches a function identifier
                //RaiseError('todo: is the string a function identifier?');
                //showmessage(child.value);

                if IsQuotedString(child.Value) then
                begin
                  // the expression is a string in quotes
                  s := UnquoteString(child.Value);
                  // If the string a function the variable is an function variable
                  if functiondeclarations.indexof(s) <> -1 then
                  begin
                    fn.add(Source + '->' + functionname);
                    //fn.add(Source + '->FUNCVARNAME:' + variablename+','+functionname);
                  end;
                  //Else If IsNumericString(child.value) then Begin

                  //  End
                end;
              end;
            end;
          end;
        end;
      end
      else
      begin
        c := node.Count;
        for i := 0 to c - 1 do
          fn.addstrings(procesnode(node.children[i], Source));
      end;
    end;
    Result := fn;
  end;

begin
  // geef een lijst met alle functiedeclaraties in deze boom
  //functioncalls := TStringList.Create;
  //functiondeclarations:= TStringList.Create;
  // value=AST_FUNC_DECL => value=expr => key=name, value=functionname **
  functioncalls := procesnode(node, Source);
  Result := functioncalls;
end;



procedure TProject.addFile(phpfilename: string);
var
  astfilename: string;
  tree: TTree;
begin

  astfilename := getASTfileOfPHPfile(phpfilename);
  if not fileexists(Path + astfilename) then
    RaiseError('AST-file "' + astfilename + '" not found. Create it first.')
  else
  begin

    // create the tree and add it to the forest
    tree := TTree.Create;
    tree.getAST(self.Path + astfilename);
    FForest.addTree(tree);

    // add th efile to the filelist
    Ffilelist.add(phpfilename);

    FCFGworld.loadfromFile(self.Path,phpfilename);




    // vraag de beschikbare functies op
    //functiondeclarations.addStrings(getFunctionDeclarations(tree.rootnode, 'main_' + ExtractFileNameWithoutExt(phpfilename)));
    // vraag de functiecalls op
    //functioncalls.addstrings(getFunctionCalls(tree.rootnode, 'main_' + ExtractFileNameWithoutExt(phpfilename)));

    //procestree(tree.rootnode, 'main_' + ExtractFileNameWithoutExt(phpfilename));

  end;

end;

procedure TProject.removefile(fn: string);
var
  i: integer;
begin
  i := Ffilelist.indexof(fn);
  if i <> -1 then
  begin
    Ffilelist.Delete(i);
    // todo: wis de ast en bouw de lijst met functiedeclaraties en functiecalls opnieuw op
  end
  else
    RaiseError('ERROR removing file ' + fn + ' from filelist.');
end;


function Tproject.getPath: string;
begin
  getPath := Path;
end;

function TProject.GetFileByIndex(i: integer): string;
begin
  GetFileByIndex := FFileList[i];
end;

function TProject.GetFileCount: integer;
begin
  GetFileCount := FFileList.Count;
end;

procedure Tproject.ToFile(fn: string);
begin
  FCFGWorld.ExportAsDotty.savetofile(fn);
end;

procedure TProject.CreateCallGraph;
var
  i: integer;
  leftpart, rightpart: string;

begin
  // create the call graph using the list of functioncalls

  g := TGraph.Create;
  for i := 0 to functioncalls.Count - 1 do
  begin
    SplitStringAt(functioncalls[i], '->', leftpart, rightpart);
    g.addVertex(leftpart);
    g.addVertex(rightpart);
    g.addEdge(leftpart, rightpart);
  end;
end;

function TProject.getCurrentTest: TTest;
begin
  Testlist.item[FcurrentTest];
end;

function TProject.getTreeOfFile(phpFilename: string): TTree;
var
  i: integer;
begin
  i := FFilelist.indexof(phpFilename);
  if i <> -1 then
  begin
    Result := Forest.getTreeByIndex(i);
  end
  else
    RaiseError('ERROR: Trying to retieve the tree of a file (' + phpfilename +
      ') that is not in the project.');

end;


end.
