unit TypeTreeClass;

interface

uses
   classes, sysutils;

type
   TTypeId = String;
   TTypeSet = TStringList;


   TTypeTreeNode = class;
   TErrorObject = class(Exception);

   TTypeTree = class
   private
      fremoveall: boolean;
      findex: integer;
      fRootnode: TTypeTreeNode;
      fnodes: Tlist;
      function GetNodeFromIndex(index: integer): TTypeTreeNode;
      function GetCount: integer;
   public
      constructor Create;
      destructor destroy; override;
      function AddType(parentnode: TTypeTreeNode): TTypeTreeNode;
      procedure RemoveNode(anode: TTypeTreeNode);
      procedure Clear;
      property Count: integer read GetCount;
      property RootNode: TTypeTreeNode read fRootnode;
      property Item[index: integer]: TTypeTreeNode read GetNodeFromIndex;
   end;

   TTypeTreeNode = class
   private
      ptree: TTypeTree;
      findex: integer;
      frelindex: integer;
      flevel: integer;
      fdata: pointer;
      fchildren: Tlist;
      fparent: TTypeTreeNode;
      function GetNode(index: integer): TTypeTreeNode;
      function GetCount: integer;
      function GetNextSibling: TTypeTreeNode;
      function GetFirstSibling: TTypeTreeNode;
   public
      constructor Create(atree: TTypeTree);
      destructor destroy; override;
      procedure Clear;
      property Tree: TTypeTree read ptree;
      property AbsoluteIndex: integer read findex;
      property RelativeIndex: integer read frelindex;
      property Level: integer read flevel;
      property Count: integer read GetCount;
      property Data: pointer read fdata write fdata;
      property Children[Index: integer]: TTypeTreeNode read GetNode;
      property NextSibling: TTypeTreeNode read GetNextSibling;
      property FirstSibling: TTypeTreeNode read GetFirstSibling;
      property Parent: TTypeTreeNode read fparent;

      procedure SetRootType(_type:string);
      procedure AddSubType(_subtype, _type:string);
      procedure addClass(_classname,_parentclass:string);

      function isSubTypeOf(_type:TTypeId):string;



   end;

procedure RaiseError(const msg: string);

implementation

procedure TTypeTreeNode.SetRootType(_type:string);
begin

end;

procedure TTypeTreeNode.AddSubType(_subtype, _type:string);
begin

end;

procedure TTypeTreeNode.addClass(_classname,_parentclass:string);
begin

end;


function TTypeTreeNode.isSubTypeOf(_type:TTypeId):string;
begin

end;



function TTypeTree.GetCount: integer;
begin
   result := fnodes.Count;
end;

function TTypeTree.GetNodeFromIndex(index: integer): TTypeTreeNode;
begin
   result := nil;
   if (index >= 0) and (index < Count) then
      result := TTypeTreeNode(fnodes[index])
   else
      RaiseError('Range Out of Bounds');
end;

procedure TTypeTree.Clear;
var
   i: integer;
   tmp: TTypeTreeNode;
begin
   fremoveall := true;
   findex := 1;
   for i := 1 to fnodes.Count - 1 do
   begin
      tmp := TTypeTreeNode(fnodes[i]);
      FreeAndNil(tmp);
   end;
   fnodes.Clear;
   fRootnode.fchildren.Clear;
   fnodes.add(fRootnode);
   fremoveall := false;
end;

constructor TTypeTreeNode.Create(atree: TTypeTree);
begin
   ptree := atree;
   findex := ptree.findex;
   inc(ptree.findex);
   fchildren := Tlist.Create;
   fdata := nil;
   fparent := atree.fRootnode;
end;

destructor TTypeTreeNode.destroy;
var
   i: integer;
   tmp: TTypeTreeNode;
begin
   if ptree.fremoveall = false then
   begin
      ptree.fnodes.remove(self);
      for i := 0 to fchildren.Count - 1 do
      begin
         tmp := TTypeTreeNode(fchildren[i]);
         FreeAndNil(tmp);
      end;
      FreeAndNil(fchildren);
   end;
   ptree := nil;
   fparent := nil;
   fdata := nil;
   inherited;
end;

function TTypeTreeNode.GetCount: integer;
begin
   result := fchildren.Count;
end;

function TTypeTreeNode.GetNode(index: integer): TTypeTreeNode;
begin
   result := nil;
   if (index >= 0) and (index < Count) then
      result := TTypeTreeNode( fchildren[index] )
   else
      RaiseError('Range Out of Bounds');
end;

function TTypeTreeNode.GetNextSibling: TTypeTreeNode;
var
   i, tmp1: integer;
begin
   result := nil;
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
               result := TTypeTreeNode( fparent.fchildren[tmp1 + 1] )
            else
            begin
               if assigned(fparent.fparent) then
                  result := fparent.GetNextSibling
               else
                  result := nil;
            end;

   end;
end;

function TTypeTreeNode.GetFirstSibling: TTypeTreeNode;
begin
   if fparent <> nil then
      result := TTypeTreeNode( fparent.fchildren[0] )
   else
      result := self;
end;

procedure TTypeTreeNode.Clear;
var
   i: integer;
   tmp: TTypeTreeNode;
begin
   for i := 0 to fchildren.Count - 1 do
   begin
      tmp := TTypeTreeNode(fchildren[i]);
      FreeAndNil(tmp);
   end;
   fchildren.Clear;
   frelindex := 1;
end;

constructor TTypeTree.Create;
begin
   findex := 0;
   fremoveall := false;
   fRootnode := TTypeTreeNode.Create(self);
   fRootnode.fparent := nil;
   fRootnode.flevel := 0;
   fnodes := Tlist.Create;
   fnodes.add(fRootnode);
end;

destructor TTypeTree.destroy;
begin
   Clear;
   fremoveall := true;
   FreeAndNil(fRootnode);
   FreeAndNil(fnodes);
   inherited;
end;

function TTypeTree.AddType(parentnode: TTypeTreeNode): TTypeTreeNode;
begin
   if parentnode = nil then
      RaiseError('Invalid Parent Node');
   result := TTypeTreeNode.Create(self);
   result.ptree := self;
   result.fparent := parentnode;
   result.fdata := nil;
   parentnode.fchildren.add(result);
   result.frelindex := parentnode.fchildren.indexof(result);
   result.flevel := parentnode.flevel + 1;
   fnodes.add(result);
end;

procedure TTypeTree.RemoveNode(anode: TTypeTreeNode);
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

procedure RaiseError(const msg: string);
begin
   raise TErrorObject.Create(msg);
end;

end.


