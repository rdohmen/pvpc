unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Grids, Spin, treeClass,
  LazFileUtils,
  graph,
  utils,
  proj,
  test,
  cfg,
  paths,
  ShellApi,
  unit2, Unit3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button14: TButton;
    Button13: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox8: TListBox;
    ListBox9: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Timer1: TTimer;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox10Click(Sender: TObject);
    procedure ListBox11Click(Sender: TObject);
    procedure ListBox8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    //astTree: TTree;
    //CurrentNode: TTreeNode;
    //ProjectPath, ProjectFilename: string;


  public
    //function getAST(filename: string): ttree;
  end;

var
  Form1: TForm1;
  project : TProject;

implementation

{$R *.lfm}

{ TForm1 }


//function TForm1.getAST(filename: string): ttree;
//var
//  i, j: integer;
//  //indentlevel : integer;
//  code: TStringList;
//  //lastlevel,
//  level: integer;
//
//  line, leftpart, key, Value, linenumbers: string;
//
//  sa, ta: TStringArray;
//
//  function leadingspaces(s: string): integer;
//  begin
//    leadingspaces := length(s) - length(trimleft(s));
//  end;
//
//begin
//  code := TStringList.Create;
//  code.LoadFromFile(filename);
//  astTree := TTree.Create;
//  currentNode := astTree.RootNode;
//  currentNode.key := filename;
//  currentNode.Value := 'main';
//
//
//
//  for i := 0 to code.Count - 1 do
//  begin
//
//    // in de echte boom beginnen we bij level 1
//    level := leadingspaces(code[i]) div 4 + 1;
//
//    // splits de regel in regelnummer(s), sleutel en waarde
//
//    // als er een @ in zit splitsen we het regelnummer/de regelnummers af
//    line := code[i];
//    sa := line.split('@');
//
//    leftpart := sa[0];
//    linenumbers := trim(sa[1]);
//
//
//    // als er : in zit spliten we de key en de waarde hier af
//    ta := leftpart.split(':');
//    key := trim(ta[0]);
//    Value := trim(ta[1]);
//
//    if level > currentNode.level then
//    begin
//      currentNode := astTree.addNode(currentNode);
//    end
//    else if level = currentNode.level then
//    begin
//      currentNode := astTree.addNode(CurrentNode.parent);
//    end
//    else
//    begin // level<lastlevel
//      //debug(true,' level='+inttostr(level)+' currentlevel='+inttostr(currentNode.level)+' < '+code[i]);
//      for j := 0 to currentNode.level - level do
//      begin
//        currentNode := currentNode.parent;
//      end;
//
//      currentNode := astTree.addnode(CurrentNode);
//    end;
//
//    currentNode.key := key;
//    currentNode.Value := Value;
//    currentNode.linenumbers := linenumbers;
//
//  end;
//  getAST := asttree;
//end;



//procedure TForm1.Button2Click(Sender: TObject);
//var
//  astfilename, phpfilename: string;
//  tree: ttree;
//  sl: TStringList;
//
//  function indent_string(l: integer): string;
//  var
//    i: integer;
//    s: string;
//  begin
//    s := '';
//    for i := 1 to L do
//      s := s + ' '; //inttostr(i);
//    Result := s;
//  end;
//
//  procedure procesASTnode(node: TTReeNode);
//  var
//    childcount: integer;
//    i, j, k, l: integer;
//    destfunction, functionname: string;
//
//    callList, functionlist: TStringList;
//  begin
//    functionList := TStringList.Create;
//    callList := TStringList.Create;
//    case node.Value of
//
//      'AST_FUNC_DECL':
//      begin
//        childcount := node.Count;
//        for i := 0 to childcount - 1 do
//        begin
//          case node.children[i].Value of
//
//            'name':
//            begin
//              functionname := node.children[i].Value;
//              functionlist.add(functionname);
//            end;
//
//            'AST_STMT_LIST':
//            begin
//              //memo3.Lines.add('track statement nodes');
//              //procesnode(node.children[i]);
//              // collect function calls
//
//              for j := 0 to node.children[i].Count - 1 do
//              begin
//                if node.children[i].children[j].Value = 'AST_CALL' then
//                begin
//                  for k := 0 to node.Count - 1 do
//                  begin
//                    if node.children[i].children[j].children[k].key =
//                      'expr' then
//                    begin
//                      for l := 0 to node.Count - 1 do
//                      begin
//                        if node.children[
//                          i].children[j].children[k].children[l].key = 'name' then
//                        begin
//                          destfunction :=
//                            node.children[i].children[j].children[k].children[l].Value;
//                          callList.add(
//                            functionname + '-' + destfunction);
//                        end;
//                      end;
//                    end;
//                  end;
//                end;
//              end;
//            end;
//
//            'AST_PARAM_LIST':
//            begin
//              //memo3.Lines.add('track param list');
//
//            end;
//
//              //end;
//            else
//            begin
//              //memo3.Lines.add( node.children[i].value );
//
//            end;
//
//          end;
//
//        end;
//      end;
//
//      else
//      begin
//
//      end;
//
//    end;
//
//    //listbox2.items.addstrings(functionlist);
//    //listbox3.items.addstrings(calllist);
//
//  end;
//
//  procedure procesnode(node: ttreenode);
//  var
//    i, c: integer;
//  begin
//    listbox2.items.add(indent_string(node.level) + node.key + '->' +
//      node.Value + ' at line(s):' + node.linenumbers);
//
//    if copy(node.Value, 1, 4) = 'AST_' then
//    begin
//      //procesASTnode(node);
//      //listbox1.items.add(node.value)
//    end;
//
//
//
//
//    c := node.Count;
//    for i := 0 to c - 1 do
//    begin
//      procesnode(node.children[i]);
//    end;
//
//  end;
//
//  //Function ExtractFilePathAndNameWithoutExt(Filename:String):String;
//  //Begin
//  //   ExtractFilePathAndNameWithoutExt := copy(Filename,1,pos(ExtractFileExt(Filename),Filename)-1);
//  //End;
//
//begin
//  listbox1.Clear;
//  listbox2.Clear;
//
//  //filename:='C:\Users\Docent\Desktop\ou\grad\000\functions.ast';
//  if opendialog1.Execute then
//  begin
//    phpfilename := opendialog1.filename;
//    astfilename := getASTfileOfPHPfile(phpfilename);
//
//    sl := TStringList.Create;
//    sl.loadfromfile(phpfilename);
//    listbox1.items.Assign(sl);
//    //sl2:=TStringList.Create;
//    //sl2.loadfromfile(phpfilename);
//    //listbox2.items.assign(sl2);
//
//    if not fileexists(astfilename) then
//      ShowMessage(astfilename + ' not found. Create it first.')
//    else
//    begin
//      tree.getAST(Project.Path + astfilename);
//
//      // toon iets van de boom
//
//
//
//      if tree.RootNode <> nil then
//      begin
//        //memo2.Lines.add('root');
//        // de rootnode zetten we niet in de visuele boom
//        procesnode(tree.rootnode);
//      end
//      else
//      begin
//        //memo2.lines.add('De boom is leeg');
//      end;
//
//    end;
//  end;
//end;

procedure TForm1.Button1Click(Sender: TObject);
//var
//  linenumber: integer;
  //startnode: TTreeNode;

  function getNodeByLinenumber(node: ttreenode; _linenumber: string): TTreeNode;
  var
    i, c: integer;
    res, found: TTreeNode;
  begin
    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);

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

        res := getNodeByLinenumber(node.children[i], _linenumber);
        if res <> nil then
          found := res;
      end;

      Result := found;
    end;

  end;

begin
  Project := TProject.Create;
  //Project.LoadFromFile('C:\Users\Docent\Desktop\ou\grad\testproject\for\for.pvc');
  Project.LoadFromFile('C:\Users\Docent\Desktop\ou\grad\testproject\function_simple\function.pvc');
  project.test:='while.php:2>while.php:5';
  Project.tofile('supercfg.dot');
  // assign filelist tot listboxes
  ListBox8.Clear;
  Listbox8.items.Assign(Project.getFileList);

  listbox9.clear;
  listbox9.items.assign(project.functiondeclarations);
  listbox1.clear;
  listbox1.items.assign(project.functioncalls);


end;

procedure TForm1.Button2Click(Sender: TObject);

begin
  With Form2 Do Begin
    listbox10.clear;
    listbox10.Items.AddStrings(project.getFileList);
    listbox13.clear;
    listbox13.Items.AddStrings(project.getFileList);
    listbox10.itemIndex:=0;
    listbox13.itemIndex:=0;

    listbox1.clear;
    listbox1.Items.LoadFromFile(Project.GetPath+listbox10.items[0]);
    listbox1.itemIndex:=0;

    listbox2.clear;
    listbox2.Items.LoadFromFile(Project.GetPath+listbox13.items[0]);
    listbox2.itemIndex:=0;
    path:=project.getpath;

    If ShowModal = mrOk Then Begin
      entryFile := listbox10.items[listbox10.ItemIndex];
      entryLine := intToStr(listbox1.ItemIndex+1);
      exitFile  := listbox13.items[listbox13.ItemIndex];
      exitLine := intToStr(listbox2.ItemIndex+1);
      project.test:=entryfile+':'+entryline+'>'+exitfile+':'+exitline;
      with stringgrid1 do begin
        cells[1,1]:=entryfile;
        cells[2,1]:=entryline;
        cells[3,1]:=exitfile;
        cells[4,1]:=exitline;

        end;
      end;
    end;
end;

procedure TForm1.Button25Click(Sender: TObject);
//var
//  i, j: integer;
//  phpfilename, astfilename: string;
//  tree: ttree;
//  //forest: TList;
//  sl: TStringList;
//  functioncount: integer;
//
//
//  function getFunctionNames(node: TTreeNode): TStringList;
//  var
//    i, c: integer;
//    functionnames: TStringList;
//  begin
//    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);
//    functionnames := TStringList.Create;
//
//    if (node.Value = 'AST_FUNC_DECL') or (node.Value = 'AST_CLOSURE') then
//    begin
//      // it is a function declaration
//      // the child 'name' contains the .... name
//      c := node.Count;
//      for i := 0 to c - 1 do
//        if node.children[i].key = 'name' then
//          functionnames.add(node.children[i].Value);
//    end;
//
//    // search for declaration in the child-nodes
//    c := node.Count;
//
//    for i := 0 to c - 1 do
//    begin
//      functionnames.addstrings(getFunctionNames(node.children[i]));
//    end;
//
//    Result := functionnames;
//  end;
//
//
//
//  function getFunctionCalls(node: TTreeNode): TStringList;
//  var
//    i, j, c, c2: integer;
//    functioncalls: TStringList;
//  begin
//    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);
//    functioncalls := TStringList.Create;
//
//    if (node.Value = 'AST_CALL') then
//    begin
//      // it is a function declaration
//      // the child 'name' contains the .... name
//      c := node.Count;
//      for i := 0 to c - 1 do
//        if node.children[i].key = 'expr' then
//        begin
//          c2 := node.children[i].Count;
//          for j := 0 to c2 - 1 do
//            if node.children[i].children[j].key = 'name' then
//              functioncalls.add(node.children[i].children[j].key +
//                ' ' + node.children[i].children[j].Value + ' ' +
//                node.children[i].children[j].linenumbers);
//        end;
//    end;
//
//    // search for declaration in the child-nodes
//    c := node.Count;
//
//    for i := 0 to c - 1 do
//    begin
//      functioncalls.addstrings(getFunctionCalls(node.children[i]));
//    end;
//
//    Result := functioncalls;
//  end;
//
begin
  //functioncount := 0;
  //forest := TList.Create;
  //
  //// for all files in the project
  //for i := 0 to Project.FileCount - 1 do
  //begin
  //
  //  phpfilename := listbox8.Items[i];
  //  astfilename := getASTfileOfPHPfile(phpfilename);
  //
  //
  //
  //  if not fileexists(astfilename) then
  //    ShowMessage(astfilename + ' not found. Create it first.')
  //  else
  //  begin
  //    //tree.getAST(Path + astfilename);
  //    //forest.add(tree);
  //    // zet de main functie in de lijst
  //    Inc(functioncount);
  //    stringgrid2.rowcount := functioncount + 1;
  //    stringgrid2.Cells[1, functioncount] := 'main';
  //    stringgrid2.Cells[0, functioncount] := tree.rootnode.key;
  //
  //    //sl:=getFunctionNames(tree.rootnode);
  //    //for j:=0 to sl.count-1 do begin
  //    //  inc(functioncount);
  //    //  stringgrid2.rowcount:=functioncount+1;
  //    //  stringgrid2.Cells[1,functioncount]:=tree.rootnode.key;
  //    //  stringgrid2.Cells[0,functioncount]:=tree.rootnode.value;
  //    //  end;
  //
  //
  //    sl := getFunctionCalls(tree.rootnode);
  //
  //
  //    if stringgrid2.colcount < sl.Count + 2 then
  //      stringgrid2.colcount := sl.Count + 2;
  //
  //
  //    for j := 0 to sl.Count - 1 do
  //    begin
  //      stringgrid2.Cells[j + 2, functioncount] := sl[j];
  //      //stringgrid2.Cells[j+2,functioncount]:=tree.rootnode.key;
  //    end;
  //  end;
  //end;
  //
end;

procedure TForm1.Button13Click(Sender: TObject);
//var
//  i: integer;
//  leftpart, rightpart,
//  phpfilename, astfilename: string;
//  tree: ttree;
////  forest: TList;
//  functioncount: integer;
//  graph:TGraph;
//
//
//  functioncalls, availablefunctions: TStringList;
//
//  function getFunctionDeclarations(node: TTreeNode; Source: string): TStringList;
//  var
//    functiondeclarations: TStringList;
//
//    function procesnode(node: TTreeNode; Source: string): TStringList;
//    var
//      fn: TStringList;
//      i: integer;
//      parent,
//      child,
//      child2: TTreeNode;
//      variablename,
//      functionname,
//      filename: string;
//      includetree: TTree;
//    begin
//      // the available function are declared either in the current file
//      // or included/require from another file
//
//      fn := TStringList.Create;
//      case node.Value of
//        'AST_CLOSURE':
//        begin
//          // vraag de naam van de variable waaraan de anonieme functie toegewezen werd
//          parent:=node.parent;
//          if parent.getChildByKey(child, 'var') then
//            if child.getChildByKey(child2, 'name') then
//               variablename:=UnQuoteString(child2.value);
//
//          // vraag de naam van de functie op (is in dit geval altijd {closure})
//          if node.getChildByKey(child, 'name') then
//          begin
//            functionname:=UnQuoteString(child.Value);
//            fn.add(Source + '.' + variablename+':'+functionname);
//
//            for i := 0 to node.Count - 1 do
//              fn.addstrings(procesnode(node.children[i], source+'.'+variablename+':'+functionname));
//
//          end;
//        end;
//        'AST_FUNC_DECL':
//        begin
//          if node.getChildByKey(child, 'name') then
//          begin
//            functionname:=UnQuoteString(child.Value);
//            fn.add(functionname);
//            for i := 0 to node.Count - 1 do
//              fn.addstrings(procesnode(node.children[i], functionname));
//
//          end;
//        end;
//        'AST_INCLUDE_OR_EVAL':
//        begin
//          // via een include/require kunnen functies beschikbaar komen
//          // deze zetten we in een lijst functiondeclarations
//          // dit doen we voor alle require/includes
//
//          // het kind met key 'expr' bevat de bestandsnaam
//
//          if node.getChildByKey(child, 'expr') then
//          begin
//            filename := UnquoteString(child.Value);
//            filename := getASTfileOfPHPfile(filename);
//
//            includetree := getAST(ProjectPath + filename);
//            fn.addStrings(getFunctionDeclarations(includetree.rootnode, Source));
//          end;
//        end
//
//        else
//        begin
//          for i := 0 to node.Count - 1 do
//          begin
//            fn.addstrings(procesnode(node.children[i], Source));
//          end;
//        end;
//      end;
//      Result := fn;
//    end;
//
//  begin
//    // maak een lijst voor alle functiedeclaraties in deze boom
//    //temp:=TStringList.Create;
//    functiondeclarations := TStringList.Create;
//    // value=AST_FUNC_DECL => value=expr => key=name, value=functionname
//    functiondeclarations := procesnode(node, Source);
//    Result := functiondeclarations;
//  end;
//
//  function getFunctionCalls(node: TTreeNode; Source: string): TStringList;
//  var
//    availablefunctions,
//    functioncalls: TStringList;
//
//    variablename,
//    functionname: string;
//
//    function procesnode(node: TTreeNode; Source: string): TStringList;
//    var
//      fn: TStringList;
//      i: integer;
//      child, child2, child3: TTreeNode;
//      s,
//      filename:string;
//      includetree:TTree;
//    begin
//      fn := TStringList.Create;
//      case node.Value of
//        'AST_CALL':
//        begin
//          if node.getChildByKey(child, 'expr') then begin
//            if child.value='AST_NAME' then
//              if child.getChildByKey(child2, 'name') then
//                //fn.add(Source + '->FUNCNAME:' + UnQuoteString(child2.Value));
//                fn.add(Source + '->' + UnQuoteString(child2.Value));
//
//          end;
//        end;
//        'AST_FUNC_DECL': // search for function call in a function declaration
//        begin
//          if node.getChildByKey(child, 'name') then
//          begin
//            functionname := UnQuoteString(child.Value);
//            // voeg deze functie toe aan de beschikbare functie
//            // .. nodig om functievariabelen te kunnen herkennen
//            // ..   $var = 'functioname';
//            availablefunctions.add(functionname);
//
//            for i := 0 to node.Count - 1 do
//              //fn.addstrings(procesnode(node.children[i], source+'.'+functionname));
//              fn.addstrings(procesnode(node.children[i], functionname));
//          end;
//        end;
//        'AST_INCLUDE_OR_EVAL':
//        // we hebben de beschikbare functies nodig om te kunnen bepalen
//        // of een AST_ASSIGN een functievariable is
//        begin
//          // via een include/require kunnen functies beschikbaar komen
//          // deze zetten we in een lijst functiondeclarations
//          // dit doen we voor alle require/includes
//
//          // het kind met key 'expr' bevat de bestandsnaam
//
//          if node.getChildByKey(child, 'expr') then
//          begin
//            filename := UnquoteString(child.Value);
//            filename := getASTfileOfPHPfile(filename);
//
//            includetree := getAST(ProjectPath + filename);
//            availablefunctions.addStrings(getFunctionDeclarations(includetree.rootnode, Source));
//          end;
//        end;
//
//
//        'AST_ASSIGN':
//        begin
//          if node.getChildByKey(child, 'var') then begin
//            // "leftside" -> the variable that is assigned to
//            if child.getChildByKey(child2, 'name') then
//              variablename:=UnQuoteString(child2.Value);
//
//
//            // "rightside"
//            if node.getChildByKey(child, 'expr') then begin
//              case child.value of
//              'AST_CLOSURE':
//              begin
//                if child.getChildByKey(child2, 'name') then begin
//                  functionname:=UnQuoteString(child2.value);
//                  //fn.add(Source + '->FUNCVARCLOSURE:' + variablename+','+functionname);
//                  fn.add(Source + '->' + functionname);
//
//                  // doorloop de stmts om te zien of er evt een functiecall in zit
//                  for i := 0 to child.Count - 1 do
//                    //fn.addstrings(procesnode(child.children[i], source+'.'+functionname));
//                    fn.addstrings(procesnode(child.children[i], functionname));
//                  end;
//              end;
//              'AST_CALL': // niet geldig!!!
//              begin
//                RaiseError('assigning a function call to a variabele is not a valid syntax');
//                if child.getChildByKey(child2, 'expr') then begin
//                  if child2.value='AST_NAME' then
//                    if child2.getChildByKey(child3, 'name') then begin
//                      functionname:=UnQuoteString(child3.value);
//                      //fn.add(Source + '->FUNCVARNAME:' + variablename+','+functionname);
//                      fn.add(Source + '->' +functionname);
//                     end;
//                   end;
//              end
//              else
//              begin
//                // todo: other types
//                // when a string is assigned, check whether the string matches a function identifier
//                //RaiseError('todo: is the string a function identifier?');
//                //showmessage(child.value);
//
//                If IsQuotedString(child.value) Then Begin
//                  // the expression is a string in quotes
//                  s:=UnquoteString(child.value);
//                  // If the string a function the variable is an function variable
//                  if availablefunctions.indexof(s)<>-1 then begin
//                    fn.add(Source + '->'+functionname);
//                    //fn.add(Source + '->FUNCVARNAME:' + variablename+','+functionname);
//                    End;
//                //Else If IsNumericString(child.value) then Begin
//                //
//                //  End
//                  end;
//                end;
//              end;
//            end;
//          end;
//        end
//        else
//        begin
//          for i := 0 to node.Count - 1 do
//            fn.addstrings(procesnode(node.children[i], Source));
//        end;
//      end;
//      Result := fn;
//    end;
//
//  begin
//    // geef een lijst met alle functiedeclaraties in deze boom
//    functioncalls := TStringList.Create;
//    availablefunctions:= TStringList.Create;
//    // value=AST_FUNC_DECL => value=expr => key=name, value=functionname **
//    functioncalls := procesnode(node, Source);
//    Result := functioncalls;
//  end;
//
//
//  // geef de functiecalls in de subtree met root node
//  // procestree levert alle functioncalls in een tstringlist met strings van de vorm
//  // 'functie1->functie2'
//
//  procedure procestree(node: TTreeNode; Source: string);
//  begin
//    // list of function available via require/included file(s)
//
//    // vraag de beschikbare functies op
//    availablefunctions.addStrings(getFunctionDeclarations(node, Source));
//    // vraag de functiecalls op
//    functioncalls.addstrings(getFunctionCalls(node, Source));
//
//  end;

begin
  project.CreateCallGraph;
  Project.g.displayAdjecencyList(stringgrid2);
  //listbox9.items.addstrings(project.getFunctioncalls);

end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  if savedialog1.Execute then
  begin
    Project.SaveToFile(savedialog1.filename);
  end;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  If SaveDialog1.Execute then Begin
    Project:=TProject.Create;
    Project.SaveToFile(SaveDialog1.Filename);
    end;
end;

procedure TForm1.Button16Click(Sender: TObject);
var cfg:TCFG;
begin
  cfg:=Project.World.MergeCFGs;
  ShowMessage('The Cyclomatic Complexity of the resulting graph is :'+inttostr(cfg.getCyclomaticComplexity));
  Project.tofile('merged_cfgs.dot');
end;

procedure TForm1.Button17Click(Sender: TObject);
var testDescription,
    leftside, rightside,
    entryFile,
    exitFile,
    entryLine,
    exitLine: string;
    entryTree,
    exitTree : TTree;
    entryNode,
    exitNode : TTreeNode;
    entryCFGNode,
    exitCFGNode : TCFGNode;
    cfg : TCFG;
    path:TPath;
    pathList : TPathList;

    i,j:integer;
    p:TPath;
    q:TCFGNode;
    a:TTreeNode;

begin
  // retrieve the node belonging to file:linenumber for entry and exit point
  testDescription := project.test;
  //project.test:=entryfile+':'+entryline+'>'+exitfile+':'+exitline;
  SplitStringAt(testDescription,'>',leftside,rightside);
  SplitStringAt(leftside,':',entryFile,entryLine);
  SplitStringAt(rightside,':',exitFile,exitLine);

  entryTree:=project.getTreeOfFile(entryFile);
  exitTree:=project.getTreeOfFile(exitFile);

  entryNode:=entryTree.getNodeByLinenumber(entryLine);
  exitNode:=exitTree.getNodeByLinenumber(exitLine);

  // find CFG with entry/exitnode;


  // StartcfgNode:=project.getCFGNodeWithASTNode
  entryCfgNode:= project.World.getCFGNodeWithASTNode( entryNode );
  exitCfgNode:= project.World.getCFGNodeWithASTNode( exitNode );

  cfg :=project.world.getCFGwithCFGnode( entryCFGnode );

  // find all simple paths
  path:=TPath.Create;
  //pathlist := cfg.find_all_simple_paths( entryCfgNode, exitCfgNode, path );


  pathlist := cfg.find_all_paths( entryCfgNode, exitCfgNode, spinedit1.Value, path );

  listbox2.clear;
  //listbox2.Items.AddStrings( cfg.tostringlist( pathList ) );
  showmessage('number of paths:'+inttostr(pathlist.count));
  // display the paths
  listbox2.clear;
  for i:=0 to pathlist.count-1 do begin
    p:=TPath(pathlist.items[i]);
    cfg.exportPathAsSMTLIB( p ).savetofile( 'path'+ZeroPad(inttostr(i),5)+'.txt' );
    end;


end;



procedure TForm1.Button3Click(Sender: TObject);
var entryTree, exitTree : TTree;
    t:TTest;
    lca,
    //firstnode_in_stmtlist,
    //lastnode_in_stmtlist,
    entryNode,
    exitNode :TTreeNode;
    entryfunction,
    exitfunction :string;

  function getFunctionNames(node: TTreeNode): TStringList;
  var
    i, c: integer;
    functionnames: TStringList;
  begin
    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);
    functionnames := TStringList.Create;

    if (node.Value = 'AST_FUNC_DECL') or (node.Value = 'AST_CLOSURE') then
    begin
      // it is a function declaration
      // the child 'name' contains the .... name
      c := node.Count;
      for i := 0 to c - 1 do
        if node.children[i].key = 'name' then
          functionnames.add(node.children[i].Value);
    end;

    // search for declaration in the child-nodes
    c := node.Count;
    for i := 0 to c - 1 do
    begin
      functionnames.addstrings(getFunctionNames(node.children[i]));
    end;

    Result := functionnames;
  end;


begin
  //
  t:=TTest.Create('while_after_while.php','5','while_after_while.php','19');

  //bepaal tree voor entrypoint
  //bepaal tree voor exitpoint

  //entryTree := Project.GetTreeOfFile(Project.currenttest.entryFile);
  //exitTree := Project.GetTreeOfFile(Project.currenttest.exitFile);

  entryTree := Project.GetTreeOfFile(t.entryFile);
  exitTree := Project.GetTreeOfFile(t.exitFile);

  //bepaal node van entrypoint
  //bepaal node van exitpoint

  entryNode := entryTree.getNodeByLinenumber(t.entryline);
  exitNode := exitTree.getNodeByLinenumber(t.exitline);


  //bepaal functie waarin entrypoint ligt
  //bepaal functie waarin exitpoint ligt
  entryFunction:=GetFunctionOfNode(t.entryFile,entryNode);
  exitFunction:=GetFunctionOfNode(t.exitFile,exitNode);



  //als e en x NIET in zelfde functie liggen
  if entryFunction <> exitFunction then begin
  //	bepaal met call graph keten van functieaanroepen die van entry functie naar exit functie leiden
  //	bepaal set local simple paths in startfunctie van e naar functieaanroep
  //	bepaal in tussenliggende functies set local simple paths van root naar functieaanroep
  //	bepaal set local simple paths in eindfunctie van root naar x
  //	koppel de set van local simple paths aan elkaar tot een set global simple paths
    end

  //anders
  else begin
  //	bepaal set simple paths in startfunctie die van e naar x leiden

    lca:=entryTree.getLCA(entryNode,exitNode);
    listbox2.clear;
    listbox2.Items.add('entry='+entrynode.key+'->'+entrynode.value);
    listbox2.Items.add('exit='+exitnode.key+'->'+exitnode.value);
    listbox2.Items.add('LCA='+lca.key+'->'+lca.value);
    listbox2.items.addStrings( entryTree.BuildLocalSimplePath( entryNode, exitNode) );
    end;

end;

procedure TForm1.Button4Click(Sender: TObject);
var testDescription,
    leftside, rightside,
    entryFile,
    exitFile,
    entryLine,
    exitLine: string;
    entryTree,
    exitTree : TTree;
    entryNode,
    exitNode : TTreeNode;
    entryCFGNode,
    exitCFGNode : TCFGNode;
    cfg : TCFG;
    path:TPath;
    pathList : TPathList;

begin
  // retrieve the node belonging to file:linenumber for entry and exit point
  testDescription := project.test;
  //project.test:=entryfile+':'+entryline+'>'+exitfile+':'+exitline;
  SplitStringAt(testDescription,'>',leftside,rightside);
  SplitStringAt(leftside,':',entryFile,entryLine);
  SplitStringAt(rightside,':',exitFile,exitLine);

  entryTree:=project.getTreeOfFile(entryFile);
  exitTree:=project.getTreeOfFile(exitFile);

  entryNode:=entryTree.getNodeByLinenumber(entryLine);
   exitNode:=exitTree.getNodeByLinenumber(exitLine);

  // find CFG with entry/exitnode;


  // StartcfgNode:=project.getCFGNodeWithASTNode
  entryCfgNode:= project.World.getCFGNodeWithASTNode( entryNode );
  exitCfgNode:= project.World.getCFGNodeWithASTNode( exitNode );

  cfg :=project.world.getCFGwithCFGnode( entryCFGnode );

  // find all simple paths
  path:=TPath.Create;
  //pathlist := cfg.find_all_simple_paths( entryCfgNode, exitCfgNode, path );


  pathlist := cfg.find_all_simple_paths( entryCfgNode, exitCfgNode, path );

  listbox2.clear;
  listbox2.Items.AddStrings( cfg.tostringlist( pathList ) );

  // display the paths
end;

//procedure TForm1.Button4Click(Sender: TObject);
//var i:integer;
//    tree:TTree;
//
//  function getFunctionCalls(node: TTreeNode): TStringList;
//  var
//    i, j, c, c2: integer;
//    functioncalls: TStringList;
//  begin
//    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);
//    functioncalls := TStringList.Create;
//
//    if (node.Value = 'AST_CALL') then
//    begin
//      // it is a function declaration
//      // the child 'name' contains the .... name
//      c := node.Count;
//      for i := 0 to c - 1 do
//        if node.children[i].key = 'expr' then
//        begin
//          c2 := node.children[i].Count;
//          for j := 0 to c2 - 1 do
//            if node.children[i].children[j].key = 'name' then
//              functioncalls.add(node.children[i].children[j].key +
//                ' ' + node.children[i].children[j].Value + ' ' +
//                node.children[i].children[j].linenumbers);
//        end;
//    end;
//
//    // search for declaration in the child-nodes
//    c := node.Count;
//
//    for i := 0 to c - 1 do
//    begin
//      functioncalls.addstrings(getFunctionCalls(node.children[i]));
//    end;
//
//    Result := functioncalls;
//  end;
//
//  procedure procestree(n:ttreenode);
//  var i:integer;
//  begin
//    if n.key='cond' then
//      ShowMessage(UnparseConditionToPrefix(n))
//    else
//      for i := 0 to n.count - 1 do
//      begin
//        procestree(n.children[i]);
//      end;
//
//
//
//  end;
//
//begin
//  i:=listbox8.itemindex;
//  if project.forest[i]<>nil then begin
//    tree:=TTree(project.forest[i]);
//
//    procestree(tree.rootnode);
//    // todo get condition
//
//    end
//  else begin
//    showmessage('ERROR: no tree in forest for this module');
//    end;
//end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    Project := TProject.Create;

    Project.LoadFromFile(OpenDialog2.FileName);

    Project.tofile('supercfg.dot');
    // assign filelist tot listboxes
    ListBox8.Clear;
    Listbox8.items.Assign(Project.getFileList);

    listbox9.clear;
    //listbox9.items.assign(project.functiondeclarations);
    listbox1.clear;
    //listbox1.items.assign(project.functioncalls);
    end;

end;

procedure TForm1.Button6Click(Sender: TObject);


  function getConditions(node: TTreeNode): TStringList;
  var
    i, j, c, c2: integer;
    conditions: TStringList;
  begin
    //listbox2.items.add(indent_string(node.level)+node.key+'->'+node.value+' at line(s):'+node.linenumbers);
    conditions := TStringList.Create;

    if (node.Value = 'AST_WHILE') or (node.Value = 'AST_DO_WHILE') or
      (node.Value = 'AST_FOR') or (node.Value = 'AST_SWITCH') or
      (node.Value = 'AST_SWITCH_CASE') then
    begin
      // it is a WHILE, DO WHILE, FOR, SWITCH, SWITCH CASE
      // the child 'cond' contains the .... condition
      c := node.Count;
      for i := 0 to c - 1 do
        if node.children[i].key = 'cond' then
        begin
          conditions.add(node.children[i].key + ' ' + node.children[i].Value +
            ' ' + node.children[i].linenumbers);

        end;
    end;



    if (node.Value = 'AST_IF') then
    begin
      // it is an AST_IF node
      // the child 'ast_if_elem' contains a child cond
      c := node.Count;
      for i := 0 to c - 1 do
        if node.children[i].Value = 'AST_IF_ELEM' then
        begin
          c2 := node.children[i].Count;
          for j := 0 to c2 - 1 do
            if node.children[i].children[j].key = 'cond' then
              conditions.add(node.children[i].children[j].key +
                ' ' + node.children[i].children[j].Value + ' ' +
                node.children[i].children[j].linenumbers);
        end;
    end;


    // search for declaration in the child-nodes
    c := node.Count;

    for i := 0 to c - 1 do
    begin
      conditions.addstrings(getConditions(node.children[i]));
    end;

    Result := conditions;
  end;

begin
  deletefile('debug.txt');
  button6.hide;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  //if ShellExecute(0,'open', PChar('python'),PChar('smtlib2.py'),Pchar('C:\Users\Docent\Desktop\ou\grad\laz\laz_openPHPproject 20'),1) =0 then
  if ShellExecute(0,'open', PChar('python'),PChar('smt_aristotle_number_puzzle.py'),nil,1) =0 then

    showmessage('succes')
  else
    showmessage('failure');
end;

procedure TForm1.Button8Click(Sender: TObject);
var filename:string;
    errorlist:TStringList;
    i:integer;
begin
  if OpenDialog1.Execute then
  begin
    ErrorList:=TStringList.Create;
    for filename in OpenDialog1.Files do
    begin
      If not FileExists( getASTfileOfPHPfile( filename ) ) then
        errorList.add( filename )
      else begin
        project.addFile(ExtractFileName(filename));
        ListBox8.Items.add(ExtractFileName(filename));
        end;
    end;

    // show errorlist (when errors occured)
    if errorlist.count >0 then begin
      Form3.Listbox1.Items.Assign( errorlist );
      Form3.ShowModal;
      end;
    end;

end;

procedure TForm1.Button9Click(Sender: TObject);
var
  i: integer;
begin
  for i := listbox8.Items.Count - 1 downto 0 do
  begin
    if listbox8.Selected[i] then
      project.removefile(listbox8.Items[i]);
      listbox8.items.Delete(i);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pagecontrol1.pageindex := 0;
  with stringgrid1 do begin
    cells[0,1]:='test 1';
    //cells[0,2]:='test 2';
    //cells[0,3]:='test 3';
    //cells[0,4]:='test 4';

    cells[1,0]:='file 1';
    cells[2,0]:='line 1';
    cells[3,0]:='file 2';
    cells[4,0]:='line 3';

    end;
end;

procedure TForm1.ListBox10Click(Sender: TObject);
begin
end;

procedure TForm1.ListBox11Click(Sender: TObject);
//var
//  astfilename, phpfilename: string;
//  tree: ttree;
//  sl: TStringList;
//
begin
  //project.setExitFile(listbox10.Items[Listbox10.ItemIndex]);
  //
  //  phpfilename := listbox11.items[listbox11.itemindex];
  //  //astfilename := getASTfileOfPHPfile(ProjectPath + phpfilename);
  //
  //  sl := TStringList.Create;
  //  sl.loadfromfile(ProjectPath + phpfilename);
  //  listbox12.items.Assign(sl);

end;

procedure TForm1.ListBox8Click(Sender: TObject);
var i:integer;
    fn:string;
    tree:TTree;
begin
  i:=listbox8.itemindex;
  fn:=listbox8.items[i];
  tree:=project.getTreeOfFile(fn);
  memo1.lines.assign(tree.print);
end;

//procedure TForm1.ListBox1Click(Sender: TObject);
////var linenumber:string;
//begin
  //label2.Caption := 'selected line: ' + IntToStr(listbox1.ItemIndex + 1);
  //
  //if Listbox1.ItemIndex <> -1 then
  //begin
  //  // search the vertex of the selected line
  //  linenumber := ListBox1.ItemIndex + 1;
  //  project.startnode := Project.forest.getNodeByLinenumber(rootnode, IntToStr(linenumber));
  //  if project.startnode <> nil then
  //  begin
  //    //showmessage(node.key+' '+node.value+' ' + node.linenumbers);
  //    listbox5.Clear;
  //    //listbox5.items.add(startnode.key + ' ' + startnode.Value + ' ' + startnode.linenumbers);
  //  end
  //  else
  //    ShowMessage('no node found');
  //
  //end
  //else
  //  ShowMessage('Please, select a line first');
  //
//end;
//
//procedure TForm1.ListBox8Click(Sender: TObject);
//var i:integer;
//    tree:TTree;
//begin
//  i:=listbox8.itemindex;
//  if project.forest[i]<>nil then begin
//    tree:=TTree(project.forest[i]);
//    memo1.lines.assign(tree.print);
//    end
//  else begin
//    showmessage('no tree in forest at this point');
//    end;
//end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  button6.visible:=fileexists('debug.txt');
end;






end.
