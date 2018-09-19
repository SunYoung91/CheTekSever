unit RecordProtocol;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils,Utils,JclDateTime;
type

    TFieldInfo = class
      Name:String;
      FiledType:String;
      IsArray:Boolean;
      OrginalStr : String;
    end;

    TRecordProtocol = class
    private
      FProtocolName:String;
      FUsesStr:String;
      FHasArrayType:Boolean;
      FFields : TObjectList<TFieldInfo>;
      function isRawType(const TypeName:String):Boolean;
      function GetFieldSerializeCode(FiledInfo : TFieldInfo ; Index : Integer = - 1):String;
      function GetFieldDeSerializeCode(FiledInfo : TFieldInfo;Index : Integer = - 1):String;
    public
      constructor Create(const TableName:String ;const usesStr ,TypeType:String );
      procedure AddField(const FieldName,DelphiType,OrginalStr:string);overload;
      procedure AddTo(InterfaceList , implList:TStringList);
      destructor Destroy;override;
    end;

    var Templelate:TStringList;


implementation

{ TDBTable }

procedure TRecordProtocol.AddField(const FieldName, DelphiType , OrginalStr : string);
var
  FiledInfo : TFieldInfo;
  TempStr:String;
begin
  FiledInfo := TFieldInfo.Create;
  FiledInfo.Name := FieldName;
  FiledInfo.FiledType := DelphiType;
  FiledInfo.IsArray := False;
  FiledInfo.OrginalStr := OrginalStr;
  TempStr := LowerCase(Trim(DelphiType));
  if Pos('array of',TempStr) > 0 then
  begin
    FiledInfo.IsArray := True;
    FiledInfo.FiledType := Trim(Copy(DelphiType,9,Length(DelphiType)));
    FHasArrayType := True;
  end;


  FFields.Add(FiledInfo);
end;



procedure TRecordProtocol.AddTo(InterfaceList , implList:TStringList);
var
  I : Integer;
  FieldInfo : TFieldInfo;
begin
  InterfaceList.Add(Format('    %s = Record ',[FProtocolName]));

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];
    InterfaceList.Add('      ' + TrimLeft(FieldInfo.OrginalStr));
  end;


  InterfaceList.Add('      procedure SerializeTo(ByteArray:TCheTekByteArray);');
  InterfaceList.Add('      procedure DeserializeFrom(ByteArray:TCheTekByteArray);');
  InterfaceList.Add('    end; ');

  implList.Add(Format('procedure %s.SerializeTo(ByteArray:TCheTekByteArray);',[FProtocolName]));

  if FHasArrayType then
  begin
    implList.Add('var');
    implList.Add('  I:Integer;');
  end;

  implList.Add('begin');
  //开始序列化部分
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];

    if FieldInfo.IsArray then
    begin
      implList.Add(Format('  ByteArray.WriteInteger(Length(%s));',[FieldInfo.Name]) );
      implList.Add(format('  for i := 0 to Length(%s) - 1 do begin',[FieldInfo.Name]) );
      implList.Add('    ' + GetFieldSerializeCode(FieldInfo,1) );
      implList.Add('  end;' );
    end else
    begin
      implList.Add('  ' + GetFieldSerializeCode(FieldInfo) );
    end;

  end;

  implList.Add('end;');


  implList.Add(Format('procedure %s.DeserializeFrom(ByteArray:TCheTekByteArray);',[FProtocolName]));
  if FHasArrayType then
  begin
    implList.Add('var');
    implList.Add('  I:Integer;');
    implList.Add('  ArrayLen:Integer;');
  end;
  implList.Add('begin');
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];
    if FieldInfo.IsArray then
    begin
      implList.Add('  ArrayLen := ByteArray.ReadInteger();');
      implList.Add(format('  SetLength(%s,ArrayLen);',[FieldInfo.Name]));
      implList.Add(format('  for i := 0 to ArrayLen -1 do begin',[FieldInfo.Name]) );
      implList.Add(Format('     ' + GetFieldDeSerializeCode(FieldInfo,1),[FieldInfo.Name]) );
      implList.Add('  end;' );
    end else
    begin
      implList.Add('  ' + GetFieldDeSerializeCode(FieldInfo));
    end;
  end;

   implList.Add('end;');

end;

constructor TRecordProtocol.Create(const TableName: String ; const usesStr , TypeType:String  );
begin
  FProtocolName := TableName;
  FFields := TObjectList<TFieldInfo>.Create;
  FUsesStr := usesStr;
end;

destructor TRecordProtocol.Destroy;
begin
  FFields.Free;
  inherited;
end;



function TRecordProtocol.GetFieldDeSerializeCode(FiledInfo : TFieldInfo;Index : Integer = - 1): String;
begin
  if isRawType(FiledInfo.FiledType) then
  begin
    Result := Format('%s := ByteArray.Read%s();',[FiledInfo.Name,FiledInfo.FiledType]);
  end else
  begin
    if Index >= 0  then
      Result := Format('%s[i].DeSerializeFrom(ByteArray);',[FiledInfo.Name])
   else
      Result := Format('%s.DeSerializeFrom(ByteArray);',[FiledInfo.Name]);
  end;
end;

function TRecordProtocol.GetFieldSerializeCode(FiledInfo: TFieldInfo ; Index : Integer = - 1): String;
begin
  if isRawType(FiledInfo.FiledType) then
  begin
    Result := Format('ByteArray.Write%s(%s);',[FiledInfo.FiledType,FiledInfo.Name]);
  end else
  begin
     if Index >= 0  then
        Result := Format('%s[i].SerializeTo(ByteArray);',[FiledInfo.Name])
     else
        Result := Format('%s.SerializeTo(ByteArray);',[FiledInfo.Name]);
  end;
end;

function TRecordProtocol.isRawType(const TypeName: String): Boolean;
var
  RT : String;
begin
  RT := LowerCase(Trim(TypeName));
  Result := False;
  if (RT = 'string') or (RT = 'byte') or (RT = 'cardinal') or (RT = 'integer') or (RT = 'boolean') or (RT = 'uint64') or
  (RT = 'int64') then
  begin
    Result := True;
  end;

end;

var
 R : TResourceStream;

initialization
  Templelate := TStringList.Create;
  R := TResourceStream.Create(HInstance,'DB','RC_DATA');
  Templelate.LoadFromStream(R);
  R.Free;
finalization
  Templelate.Free;


end.
