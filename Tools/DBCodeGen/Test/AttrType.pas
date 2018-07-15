unit AttrType;

interface
uses System.Generics.Collections,CheTek.SerialObject;
type
  TAttr = record
     AttrType : Integer;
     Value: Integer;
  end;

  TAttrs = TJsonArray<TAttr>;


  TTestMyList = class(TSerialObject)
  private
    FName:String;
    FAge : Integer;
  published
    property Name:String read FName write FName;
    property Age:Integer read FAge write FAge;
  end;

implementation

end.
