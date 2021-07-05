unit paths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TreeClass;

type
   TErrorObject = class(Exception);

   TPathElement = class

     vertex : pointer;
     pathcondition: TList;
     pathvault : TList;

   end;

   TPath = class
   private
     //list of pathelements
     Fitems   : TList;
     function getCount:integer;
     function GetNodeByIndex(index: integer): Pointer;
   public
     constructor Create;
     procedure add(n:pointer);
     function indexOf( n:pointer):integer;

     property Count: integer read GetCount;
     property Items[index: integer]: Pointer read GetNodeByIndex;
   end;

   TPathList = class
   private
     //list of paths
     Fitems   : tlist;
     function getCount:integer;
     function GetPathByIndex(index: integer): TPath;
   public
     constructor Create();
     procedure add(p:TPath);

     property Count: integer read GetCount;
     property Items[index: integer]: TPath read GetPathByIndex;

   end;


implementation

constructor TPath.Create;
begin
  Fitems := tList.Create;
end;

procedure TPath.add(n:pointer);
begin
  Fitems.add(n);
end;

function TPath.GetCount: integer;
begin
  Result := Fitems.Count;
end;

function TPath.GetNodeByIndex(index: integer): Pointer;
begin
  Result := nil;
  if (index >= 0) and (index < Count) then
    Result := Fitems[index]
  else
    RaiseError('Range Out of Bounds');
end;

function TPath.indexOf( n:pointer):integer;
var i:integer;
begin

  for i:=0 to Fitems.count-1 do begin
    if Fitems[i]= n then
    result:=i;
    end;
  result:=-1;
end;



constructor TPathList.Create();
begin
  Fitems := tList.Create;
end;

procedure TPathList.add(p:TPath);
begin
  Fitems.add(p);
end;

function TPathList.GetCount: integer;
begin
  Result := Fitems.Count;
end;

function TPathList.GetPathByIndex(index: integer): TPath;
begin
  Result := nil;
  if (index >= 0) and (index < Count) then
    Result := TPath(Fitems[index])
  else
    RaiseError('Range Out of Bounds');
end;

end.

