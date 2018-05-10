unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,CheTek.SerialObject;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTestObject = class(TSerialObject)
  public
    S:String;
    No:Integer;
  published
    property SS : string read S write S;
  end;

  TMyRecord = record
    Name:String;
    Item : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  Obj:TTestObject;
  Stream:TMemoryStream;
  myData : TMyRecord;
  BData  : TMyRecord;
begin
  Obj := TTestObject.Create;
  Stream := TMemoryStream.Create;
  Obj.SaveToStream(Stream);
  Stream.Position := 0;

  mmo1.Lines.LoadFromStream(Stream);
  Stream.Free;
  Obj.Free;

  myData.Name := 'Ã×ÄÈÑÇ';
  myData.Item := ' my word';

  mmo1.Lines.Text :=  TSerialObject.RecordToJson(myData);

  TSerialObject.JsonToRecord(mmo1.Lines.Text,BData);

  mmo1.Lines.Text :=  TSerialObject.RecordToJson(BData);

end;

end.
