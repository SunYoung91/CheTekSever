unit CheTek.Pool;

interface
uses System.Generics.Collections;
type

  TCheTekBasePool<T> = class
  type
   TCreatePoolObjectFunc = reference to function():T;
   TDestroyPoolObjectFunc = reference to procedure(const T);
  private
    FPoolObjectList : TList<T>;
    FTotalObjectList : TList<T>;
    FCreatePoolObjectFunc : TCreatePoolObjectFunc;
    FDestroyPoolObjectFunc : TDestroyPoolObjectFunc;
  public
    constructor Create(CreateFunc:TCreatePoolObjectFunc;DestroyFunc:TDestroyPoolObjectFunc);
    destructor Destroy();override;
    function Borrow():T;
    Procedure GiveBack(const ObjectData : T);
  end;
implementation

{ TCheTekBasePool<T> }

function TCheTekBasePool<T>.Borrow: T;
begin
  if FPoolObjectList.Count > 0 then
  begin
    Result := FPoolObjectList[0];
    FPoolObjectList.Delete(0);
  end else
  begin
    Result := FCreatePoolObjectFunc();
    FTotalObjectList.Add(Result);
  end;
end;

constructor TCheTekBasePool<T>.Create(CreateFunc: TCreatePoolObjectFunc;
  DestroyFunc: TDestroyPoolObjectFunc);
begin
  FCreatePoolObjectFunc := CreateFunc;
  FDestroyPoolObjectFunc := DestroyFunc;
  FPoolObjectList := TList<T>.Create;
  FTotalObjectList := TList<T>.Create;
end;

destructor TCheTekBasePool<T>.Destroy;
var
  I:Integer;
  Data : T;
begin
  FPoolObjectList.Free;
  for i := 0  to FTotalObjectList.Count  - 1 do
  begin
    Data := FTotalObjectList[i];
    FDestroyPoolObjectFunc(Data);
  end;
  FTotalObjectList.Create;
  inherited;
end;

procedure TCheTekBasePool<T>.GiveBack(const ObjectData : T);
begin
  FPoolObjectList.Add( ObjectData );
end;

end.
