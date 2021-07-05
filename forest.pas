unit Forest;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, TreeClass, utils;


type
  TForest = class
  private
    FTreelist : TList;
    function GetCount:integer;
  public
    constructor Create;

    procedure addTree( t:TTree);
    function getTreeByIndex( i: integer ) : TTree;

    property count:integer read GetCount;

  end;



implementation

procedure TForest.addTree(t:TTree);
begin
  FTreeList.add( t );
end;

function TForest.GetCount:integer;
begin
  result:=FTreeList.count;
end;

constructor TForest.Create;
begin
  FTreeList := TList.Create;
end;

function TForest.getTreeByIndex( i: integer ) : TTree;
Begin
  result := TTree( FTreeList[i] );
end;






end.

