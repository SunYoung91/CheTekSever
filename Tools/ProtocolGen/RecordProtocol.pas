unit RecordProtocol;

interface
uses System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils,Utils,JclDateTime,StringUtils;
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
      FProtocolID : Word;
      FFields : TObjectList<TFieldInfo>;
      function isRawType(const TypeName:String):Boolean;
      function GetFieldSerializeCode(FiledInfo : TFieldInfo ; Index : Integer = - 1):String;
      function GetFieldDeSerializeCode(FiledInfo : TFieldInfo;Index : Integer = - 1):String;
    public
      constructor Create(const TableName:String ;const usesStr ,TypeType:String );
      procedure AddField(const FieldName,DelphiType,OrginalStr:string);overload;
      procedure AddTo(InterfaceList , implList:TStringList);
      destructor Destroy;override;
      property ProtocolID : Word read FProtocolID ;
      property Name : string read FProtocolName;
    end;

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
  InterfaceList.Add(Format('    P%s = ^%s; ',[FProtocolName,FProtocolName]));
  implList.Add(Format('procedure %s.SerializeTo(ByteArray:TCheTekByteArray);',[FProtocolName]));

  if FHasArrayType then
  begin
    implList.Add('var');
    implList.Add('  I:Integer;');
  end;

  implList.Add('begin');

  if FProtocolID > 0  then
  begin
    implList.Add(Format('  ByteArray.WriteWord(%d);',[FProtocolID]));
  end;

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


//  if FProtocolID > 0  then
//  begin
//    implList.Add('  ByteArray.ReadWord();');
//  end;


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
var
  protocolStr : String;
begin
  FProtocolName := TableName;
  protocolStr := TStringUtil.StrBetween(TableName + ';','_',';');
  FProtocolID := 0;
  if protocolStr <> '' then
  begin
    FProtocolID := TStringUtil.LastInteger(TableName,0);
    if FProtocolID = 0  then
    begin
      raise Exception.Create(Format('Parse Protocol Head Error , Can Not Get Protocol ID : %s',[TableName]));
    end;
  end;

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
  (RT = 'int64') or (rt = 'word') then
  begin
    Result := True;
  end;

end;


end.
