unit test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTest = class
  private

    FentryFile,
    FexitFile,
    FentryLineNumber,
    FexitLinenumber    : String;
    procedure setEntryFile(fn:string);
    procedure setExitFile(fn:string);
    procedure setEntryLine(ln:string);
    procedure setExitLine(ln:string);
    function getEntryFile:string;
    function getExitFile:string;
    function getEntryLine:string;
    function getExitLine:string;

  public
    constructor Create(_entryfile,_entryLine, _exitFile, _exitLine:string);

    property entryFile:string read getEntryFile write setEntryFile ;
    property exitFile:string read getExitFile write setExitFile;
    property entryLine:string read getEntryLine write setEntryLine;
    property exitLine:string read getExitLine write setExitLine;


  end;

  TTestList = Class(TList)
    private
      function getTestByIndex(i:integer):TTest;

    public
      procedure addtest(t:TTest);
      procedure deleteTest(i:integer);
      property item[i:integer]:TTest read getTestByIndex;

    End;

implementation

constructor TTest.Create(_entryfile,_entryLine, _exitFile, _exitLine:string);
begin
  Fentryfile:=_entryfile;
  Fentrylinenumber:=_entryline;
  Fexitfile:=_exitfile;
  Fexitlinenumber:=_exitline;
  end;

procedure TTest.setEntryFile(fn:string);
begin
  FEntryFile := fn;
end;

procedure TTest.setExitFile(fn:string);
begin
  FExitFile := fn;
end;

procedure TTest.setEntryLine(ln:string);
begin
  FEntryLinenumber := ln;
  end;

procedure TTest.setExitLine(ln:string);
begin
  FExitLinenumber := ln;
end;

function TTest.getEntryFile:string;
begin
  result:=FEntryFile;
end;

function TTest.getExitFile:string;
begin
  result:=FExitFile;
  end;

function TTest.getEntryLine:string;
begin
  result:=FEntryLineNumber;
  end;

function TTest.getExitLine:string;
begin
  result:=FExitLineNumber;
end;

//============= TestList


procedure TTestList.addtest(t:TTest);
begin
  inherited add(t);
  end;

procedure TTestList.deleteTest(i:integer);
begin
  inherited delete(i);
  end;

function TTestList.getTestByIndex(i:integer):TTest;
begin
  result:=TTest(getTestByIndex(i))
end;




end.



