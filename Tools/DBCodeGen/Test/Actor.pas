unit Actor;

interface
uses AttrType;

Type
  TActor  = record
    ActorName : string;  //#[pkey]
    Age  : Integer;
    Attr : TAttrs; // #[json]
  end;

implementation

end.
