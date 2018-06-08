unit DoubleDict;

interface
   uses System.Generics.Collections,System.SysUtils;
Type
   TDoubleDict<TKey,TValue> = class(TDictionary<TKey,TValue>)
   private
     VKDict : TDictionary<TValue,TKey>;
     FLastKeyEvent:TKey;
     FKeyIsVaild:Boolean;
     procedure KeyEvent(Sender: TObject; const Item:TKey;Action: TCollectionNotification);
     procedure ValueEvent(Sender: TObject; const Item:TValue;Action: TCollectionNotification);
   public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy;override;
    function TryGetKeyByValue(const Value:TValue ; out Key:TKey):boolean;
   end;
implementation

{ TDoubleDict<TKey, TValue> }

constructor TDoubleDict<TKey, TValue>.Create(ACapacity: Integer);
begin
  inherited Create(ACapacity);
  VKDict := TDictionary<TValue,TKey>.Create(ACapacity);
end;

destructor TDoubleDict<TKey, TValue>.Destroy;
begin
  VKDict.Free;
  inherited;
end;

function TDoubleDict<TKey, TValue>.TryGetKeyByValue(const Value: TValue;
  out Key:TKey): boolean;
begin
  Result := VKDict.TryGetValue(Value,Key);
end;


procedure TDoubleDict<TKey, TValue>.KeyEvent(Sender: TObject; const Item: TKey;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: begin
      FLastKeyEvent := Item;
      FKeyIsVaild := True;
    end;
    cnRemoved:begin

    end;
    cnExtracted:begin

    end;
  end;
end;

procedure TDoubleDict<TKey, TValue>.ValueEvent(Sender: TObject;
  const Item: TValue; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
    begin
      if FKeyIsVaild then
      begin
        VKDict.AddOrSetValue(Item,FLastKeyEvent);
      end else
      begin
        raise Exception.Create('TDoubleDict.ValueEvent FKeyIsVaild is False');
      end;
    end;
    cnRemoved:
    begin;
        VKDict.Remove(Item);
    end;
    cnExtracted:
    begin

    end;
  end;
end;

end.
