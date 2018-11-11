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
      function GetTypeStaticSize(const TypeName:String):Integer; //获取字段类型的静态长度； 返回0 表示这个字段不是静态的。
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
  StaticSize , FieldStaticSize:Integer;
begin
  InterfaceList.Add(Format('    %s = Record ',[FProtocolName]));

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];
    InterfaceList.Add('      ' + TrimLeft(FieldInfo.OrginalStr));
  end;


  InterfaceList.Add('      procedure SerializeTo(ByteArray:TCheTekByteArray);');
  InterfaceList.Add('      procedure DeserializeFrom(ByteArray:TCheTekByteArray);');
  InterfaceList.Add('      function GetSerializeSize():NativeInt;');
  InterfaceList.Add('      class function GetProtocolID():Word; static ;');
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

      //可以批量优化的部分
      if isRawType(FieldInfo.FiledType) and (LowerCase(FieldInfo.FiledType) <> 'string') then
      begin
         implList.Add(format('   ByteArray.Write(@%s[0],Length(%s) * %d);',[FieldInfo.Name,FieldInfo.Name,GetTypeStaticSize(FieldInfo.FiledType)]) );
      end else
      begin
        implList.Add(format('  for i := 0 to Length(%s) - 1 do begin',[FieldInfo.Name]) );
        implList.Add('    ' + GetFieldSerializeCode(FieldInfo,1) );
        implList.Add('  end;' );
      end;
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
       //可以批量优化的部分
      if isRawType(FieldInfo.FiledType) and (LowerCase(FieldInfo.FiledType) <> 'string') then
      begin
         implList.Add(format('  SetLength(%s,ArrayLen);',[FieldInfo.Name]));
         implList.Add(format('   ByteArray.Read(@%s[0],ArrayLen * %d);',[FieldInfo.Name,GetTypeStaticSize(FieldInfo.FiledType)]) );
      end else
      begin
        implList.Add(format('  SetLength(%s,ArrayLen);',[FieldInfo.Name]));
        implList.Add(format('  for i := 0 to ArrayLen -1 do begin',[FieldInfo.Name]) );
        implList.Add(Format('     ' + GetFieldDeSerializeCode(FieldInfo,1),[FieldInfo.Name]) );
        implList.Add('  end;' );
      end;


    end else
    begin
      implList.Add('  ' + GetFieldDeSerializeCode(FieldInfo));
    end;
  end;

   implList.Add('end;');


  //获取一个结构体序列化后占用多大的空间。
  implList.Add(Format('Function %s.GetSerializeSize():NativeInt;',[FProtocolName]));
  if FHasArrayType then
  begin
    implList.Add('var');
    implList.Add('  I:Integer;');
  end;
  implList.Add('begin');
  implList.Add('  Result := 0;');

  StaticSize := 0;
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];

    if FieldInfo.IsArray then
    begin
      FieldStaticSize := GetTypeStaticSize(FieldInfo.FiledType);
      implList.Add('  Result := Result + 4; //读取数组长度标记');
      if FieldStaticSize = 0 then //说明是结构体
      begin
        if LowerCase(FieldInfo.FiledType) = 'string' then
        begin
          implList.Add(format('  for i := 0 to Length(%s) - 1 do begin',[FieldInfo.Name]) );
          implList.Add(Format('    Result := Result + 2 + TEncoding.UTF8.GetByteCount(%s[i]);',[FieldInfo.Name]) );
          implList.Add('  end;' );
        end else
        begin
          implList.Add(format('  for i := 0 to Length(%s) - 1 do begin',[FieldInfo.Name]) );
          implList.Add(Format('    Result := Result + %s[i].GetSerializeSize();',[FieldInfo.Name]) );
          implList.Add('  end;' );
        end;
      end else
      begin
        implList.Add( Format('  Result := Result + (Length(%s) * %d); //读取数组长度标记',[FieldInfo.Name,FieldStaticSize]));
      end;
    end else
    begin
      FieldStaticSize := GetTypeStaticSize(FieldInfo.FiledType);
      if FieldStaticSize = 0 then //说明是结构体
      begin
        if LowerCase(FieldInfo.FiledType) = 'string' then
        begin
          implList.Add(Format('   Result := Result + 2 + TEncoding.UTF8.GetByteCount(%s);',[FieldInfo.Name]) );
        end else
        begin
          implList.Add(Format('   Result := Result + %s.GetSerializeSize(); // %s',[FieldInfo.Name,FieldInfo.Name]) );
        end;

      end else
      begin
        StaticSize := StaticSize + FieldStaticSize;
        implList.Add( Format('  //%s = %d',[FieldInfo.Name,FieldStaticSize]));
      end;
    end;

  end;

  implList.Add( Format('  Result := Result + %d ; // StaticSize  ',[StaticSize]));

  implList.Add('end;');

  //增加获取协议ID的函数
  implList.Add(Format('class function %s.GetProtocolID():Word;',[FProtocolName]));
  implList.Add('begin');
  implList.Add(Format('  result := %d;',[Self.ProtocolID]));
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

function TRecordProtocol.GetTypeStaticSize(const TypeName: String): Integer;
var
  RT:String;
begin
  RT := LowerCase(Trim(TypeName));
  if (RT = 'byte') or (RT = 'boolean') then
  begin
    Result := 1;
  end else
  if (RT = 'word') or (RT = 'smallint') then
  begin
    Result := 2;
  end else
  if (rt = 'integer') or (rt = 'cardinal') then
  begin
    Result := 4;
  end else
  if (rt = 'uint64') or (rt = 'int64') then
  begin
    Result := 8;
  end else
  begin
    Result := 0;
  end;

end;

function TRecordProtocol.isRawType(const TypeName: String): Boolean;
var
  Size : Integer;
begin
  Size := GetTypeStaticSize(TypeName);
  if Size > 0 then
  begin
    Result := True;
  end else
  begin
    if LowerCase(TypeName) = 'string' then
    begin
      Result := True;
    end else
    begin
      Result := False;
    end;
  end;
end;




end.
