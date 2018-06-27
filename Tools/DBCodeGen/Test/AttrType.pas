unit AttrType;

interface
uses System.Generics.Collections,CheTek.SerialObject;
type
  TAttr = record
     AttrType : Integer;
     Value: Integer;
  end;

  TAttrs = TJsonArray<TAttr>;

implementation

end.
