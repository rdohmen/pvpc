unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    ListBox10: TListBox;
    ListBox13: TListBox;
    ListBox2: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure ListBox10Click(Sender: TObject);
    procedure ListBox13Click(Sender: TObject);
  private

  public
    path,
    entryFile,
    entryLine,
    exitFile,
    exitLine : string;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
begin

end;

procedure TForm2.ListBox10Click(Sender: TObject);
begin
  listbox1.Items.LoadFromFile(Path+listbox10.items[ListBox10.ItemIndex]);
  listbox1.itemIndex:=0;


end;

procedure TForm2.ListBox13Click(Sender: TObject);
begin
  listbox2.Items.LoadFromFile(Path+listbox13.items[ListBox13.ItemIndex]);
  listbox2.itemIndex:=0;

end;

end.

