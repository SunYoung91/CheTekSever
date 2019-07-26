unit RecordProtocol;

interface
uses System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils,Utils,JclDateTime,StringUtils,Winapi.Windows;
type

    TFieldInfo = class
      Name:String;
      FiledType:String;
      IsArray:Boolean;
      OrginalStr : String;
      Comment : String;
      IsPathArray:Boolean;//占位长度 array[0..0] of 之类的
      function FieldTypePtrName():String;
    end;

    TRecordProtocol = class
    private
      FProtocolName:String;
      FUsesStr:String;
      FHasArrayType:Boolean;
      FProtocolID : Byte;  //协议ID   0-255
      FSystemID:Byte; //系统ID        0-255
      FFields : TObjectList<TFieldInfo>;
      FDynmicArrayFieldInfo : TFieldInfo; //动态长度的数组字段 用于判定长度是否有效
      function isRawType(const TypeName:String):Boolean;
      function GetTypeStaticSize(const TypeName:String):Integer; //获取字段类型的静态长度； 返回0 表示这个字段不是静态的。
      function GetFieldSerializeCode(FiledInfo : TFieldInfo ; Index : Integer = - 1):String;
      function GetFieldDeSerializeCode(FiledInfo : TFieldInfo;Index : Integer = - 1):String;
      function GetProtoBufName :String;
      function IsNumberType(const TypeName:String):Boolean;
      function GetIdent():Word;
    public
      constructor Create(const TableName:String ;const usesStr ,TypeType:String );
      procedure AddField(const FieldName,DelphiType,OrginalStr:string);overload;
      procedure AddTo(InterfaceList , implList:TStringList);
      procedure GenProto(List:TStringList); //生成 protobuf 代码
      procedure GenRecordToBufferCode(List:TStringList;InterfaceList:TStringList); //生成 结构体 转换为 ProtoBuf 类的代码
      procedure GenProtoBufToRecordCode(List:TStringList;InterfaceList:TStringList); //生成 ProtoBuf 类转换为 结构体的代码
      procedure GenCaseRecordToProtoCode(List:TStringList);
      procedure GenCaseProtoBufToRecord(List:TStringList);
      procedure genProtoBufUtils(List:TStringList ; interfaceList : TStringList);
      destructor Destroy;override;
      property ProtocolID : Byte read FProtocolID ;
      property SystemID : Byte Read FSystemID;
      property Ident : Word Read GetIdent; //ident 实际就是 SystemID 和 ProtocolID 组合起来。
      property Name : string read FProtocolName;
      property ProtoBufName:String read GetProtoBufName;
    end;


implementation

var DelphiTypeToProtoBufType :TDictionary<String,String>;

function GetProtoBufType(const DelphiType:String):String;
begin
  if DelphiTypeToProtoBufType.TryGetValue(LowerCase(DelphiType),Result) then
  begin
    Exit;
  end;
  Result := Copy(DelphiType,2,Length(DelphiType)) + '_P';
end;
{ TRecordProtocol }

procedure TRecordProtocol.AddField(const FieldName, DelphiType , OrginalStr : string);
var
  FiledInfo : TFieldInfo;
  TempStr:String;
  ArrayLenght:String;
begin
  FiledInfo := TFieldInfo.Create;
  FiledInfo.Name := FieldName;
  FiledInfo.FiledType := DelphiType;
  FiledInfo.IsArray := False;
  FiledInfo.OrginalStr := OrginalStr;
  FiledInfo.Comment := StrAfter('//',OrginalStr);
  TempStr := LowerCase(Trim(DelphiType));
  if Pos('array',TempStr) > 0 then
  begin
    FiledInfo.IsArray := True;
    FiledInfo.FiledType := Trim(StrAfter('of',DelphiType));
    FHasArrayType := True;
    ArrayLenght := Trim(StrBetween(TempStr,'[',']'));
    if (ArrayLenght <> '') and (Length(ArrayLenght) > 3) and (ArrayLenght[1] = '0') and (ArrayLenght[Length(ArrayLenght)] = '0') then
    begin
      FiledInfo.IsPathArray := True;
      if FDynmicArrayFieldInfo = nil then
        FDynmicArrayFieldInfo := FiledInfo
      else
        raise Exception.Create('不允许一个协议中出现两个动态字段: ' + Name );
    end;
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
	implList.add('#IFDEF DEBUG');
	implList.add( 'oldPosition : NativeInt' );
	implList.add('#ENDIF');
  end else
  begin
	implList.add('#IFDEF DEBUG');
	implList.Add('var');
	implList.add( 'oldPosition : NativeInt' );
	implList.add('#ENDIF');
  end;

  implList.Add('begin');
  
  implList.add('#IFDEF DEBUG');
  implList.add('oldPosition := ByteArray.Position;');
  implList.add('#ENDIF');

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
  
  implList.add('#IFDEF DEBUG');
  implList.add('if ByteArray.Position - oldPosition <> GetSerializeSize() then ');
  implList.add('	Raise Exception.CreateFmt('' ' +FProtocolName + ', Serialize format error , GetSerializeSize : %d , WriteSize : %d '',[GetSerializeSize() , ByteArray.Position - oldPosition]); ');
  implList.add('#ENDIF');

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
  namelist : TStringList;
begin
  FProtocolName := TableName;
  namelist := TStringList.Create;
  namelist.Delimiter := '_';
  namelist.DelimitedText := TableName;
  if NameList.Count >= 2 then
  begin
    FProtocolID := StrToIntDef(namelist[nameList.Count - 1],0);
    FSystemID := StrToIntDef(namelist[nameList.Count - 2],0);
  end else
  begin
    FProtocolID := 0;
    FSystemID := 0;
  end;

  namelist.Free;

  FFields := TObjectList<TFieldInfo>.Create;
  FUsesStr := usesStr;
end;

destructor TRecordProtocol.Destroy;
begin
  FFields.Free;
  inherited;
end;

procedure TRecordProtocol.GenCaseProtoBufToRecord(List: TStringList);
var
  FieldInfo : TFieldInfo;
begin
  List.Add(Format('  %d: begin',[Ident]));
         List.Add('      if FProtoBufObjs[Ident] = nil then begin');
  List.Add(Format('        ProtoBufObj := %s.Create();',[ProtoBufName]));
         List.Add('        FProtoBufObjs[Ident] := ProtoBufObj;');
         List.Add('      end else begin');
         List.Add('        ProtoBufObj := FProtoBufObjs[Ident];');
if FDynmicArrayFieldInfo <> nil then begin
  List.Add(Format('        %s(ProtoBufObj).%s.Clear();',[ProtoBufName,FDynmicArrayFieldInfo.Name + 'List']));
end;
         List.Add('      end;');

         //List.Add('      Result := 0;');
         List.Add('      ProtoBufObj.LoadFromMem(pProtoBufBuffer,ProtoBufBufferSize);');

  if FDynmicArrayFieldInfo <> nil then
  List.Add(Format('      RealRecordSize := SizeOf(%s) + %s(ProtoBufObj).%s.Count * SizeOf(%s) ;',[Name,Name + '_P',FDynmicArrayFieldInfo.Name + 'List', FDynmicArrayFieldInfo.FiledType]))
    else
  List.Add(Format('      RealRecordSize := SizeOf(%s);',[Name]));

         List.Add('      if RealRecordSize > RecordDataLen then begin');
         List.Add('        Result := -1; //Record 缓存区不足');
         List.Add('        Exit;');
         List.Add('      end;');


  List.Add(Format('      PToR_%s(%s(pRecordData),%s(ProtoBufObj));',[Name,'ptr_' + Name,ProtoBufName]));
         List.Add('      Result := RealRecordSize;');
         List.Add('    end;');
         //List.Add('    end;');
end;

procedure TRecordProtocol.GenCaseRecordToProtoCode(List: TStringList);
var
  FieldInfo : TFieldInfo;
begin
  List.Add(Format('   %d: begin',[Ident]));
         List.Add('      if FProtoBufObjs[Ident] = nil then begin');
  List.Add(Format('        ProtoBufObj := %s.Create();',[ProtoBufName]));
         List.Add('        FProtoBufObjs[Ident] := ProtoBufObj;');
         List.Add('      end else begin');
         List.Add('        ProtoBufObj := FProtoBufObjs[Ident];');
if FDynmicArrayFieldInfo <> nil then begin
  List.Add(Format('        %s(ProtoBufObj).%s.Clear();',[ProtoBufName,FDynmicArrayFieldInfo.Name + 'List']));
end;
         List.Add('      end;');

         //List.Add('      Result := 0;');
  List.Add(Format('      RealRecordSize := SizeOf(%s);',[Name]));

         List.Add('      if RealRecordSize > RecordDataLen then begin ');
         List.Add('         Exit; ');
         List.Add('      end; ');

if FDynmicArrayFieldInfo <> nil then begin
  List.Add(Format('      RealRecordSize := RealRecordSize + %s(pRecordData).Size * Sizeof(%s);',['ptr_' + Name,FDynmicArrayFieldInfo.FiledType]));
         List.Add('      if RealRecordSize > RecordDataLen then begin ');
         List.Add('         Exit; ');
         List.Add('      end; ');
end;


         List.Add('      ProtoBufObj.Clear();');
  List.Add(Format('      RToP_%s(%s(pRecordData),%s(ProtoBufObj));',[Name,'ptr_' + Name,ProtoBufName]));
         List.Add('      FPbOutPutBuffer.Clear();');
         List.Add('      ProtoBufObj.SaveToBuf(FPbOutPutBuffer);');
         List.Add('      if Cardinal(FPbOutPutBuffer.getSerializedSize())  < RecordDataLen then begin');
         List.Add('        FPbOutPutBuffer.writeToMem(pRecordData,RecordDataLen);');
         List.Add('        Result := FPbOutPutBuffer.getSerializedSize();');
         List.Add('      end;');
         List.Add('    end;');
end;

procedure TRecordProtocol.GenProto(List: TStringList);
var
  I : Integer;
  FieldInfo : TFieldInfo;
  ProtoBufType , ProtoBufCode : String;
  Tag : Integer;
begin
  List.Add(Format('message %s {',[Copy(ProtoBufName,2,Length(ProtoBufName))]));
  Tag := 1;
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];

    if FieldInfo.IsArray then
    begin
      ProtoBufType := GetProtoBufType(FieldInfo.FiledType);
      ProtoBufCode := Format('  repeated %s %s = %d ;',[ProtoBufType,FieldInfo.Name,Tag]);
    end else
    begin
      ProtoBufType := GetProtoBufType(FieldInfo.FiledType);
      ProtoBufCode := Format('  optional %s %s = %d ;',[ProtoBufType,FieldInfo.Name,Tag]);
    end;

    if FieldInfo.Comment <> '' then
        ProtoBufCode := ProtoBufCode + ' // ' + FieldInfo.Comment;
    List.Add(ProtoBufCode);
    Inc(Tag);
  end;

  List.Add('}');
end;

procedure TRecordProtocol.GenProtoBufToRecordCode(List:TStringList;InterfaceList:TStringList);
var
  I , Index  : Integer;
  FieldInfo : TFieldInfo;
  Code : String;
  VarList:TStringList;
  CodeList : TStringList;

begin

  CodeList := TStringList.Create;
  VarList  := TStringList.Create;

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];

    if FieldInfo.IsArray then
    begin
      //占位数组 动态长度
      if FieldInfo.IsPathArray then
      begin
        CodeList.Add(Format('  R.Size := P.%sList.Count;',[FieldInfo.Name]));

        CodeList.Add('  if R.Size > 0 then begin' );
        CodeList.Add('  for I := 0 to R.Size - 1 do begin' );
        if isRawType(FieldInfo.FiledType) then
        begin
          CodeList.Add(Format('      R.%s := P.%sList[I];',[FieldInfo.Name,FieldInfo.Name]));
        end else
        begin
          CodeList.Add(Format('      PToR_%s(@R.%s[i],P.%sList[i]);',[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]));
        end;
        CodeList.Add('    end;');
        CodeList.Add('  end;');
      end else
      begin
        //静态长度
        CodeList.Add(Format('  for I := 0 to P.%sList.Count - 1 do begin',[FieldInfo.Name,FieldInfo.Name] ));
        if isRawType(FieldInfo.FiledType) then
        begin
          CodeList.Add(Format('    R.%s[I] := P.%sList[i];',[FieldInfo.Name,FieldInfo.Name]));
        end else
        begin
          CodeList.Add(Format('    PToR_%s(@R.%s[I],P.%sList[i]);',[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]));
        end;
        CodeList.Add('  end;');
      end;
    end else
    begin
       if LowerCase(FieldInfo.Name) = 'size' then
        Continue;

       if isRawType(FieldInfo.FiledType) then
       begin
         Code := Format('  R.%s := P.%s;',[FieldInfo.Name,FieldInfo.Name])
       end else
       begin
         Code := Format('  PToR_%s(@R.%s,P.%s);' ,[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]);
       end;
       CodeList.Add(Code);
    end;

  end;

  Index := List.Add(Format('procedure PToR_%s(R:%s;P:%s );',[FProtocolName,'ptr_'+ FProtocolName,ProtoBufName]));
  //interfaceList.Add(List[Index]);

  List.Add('var');
  List.Add('  I:Integer;');
  List.AddStrings(VarList);
  List.Add('begin');
  List.AddStrings(CodeList);
  List.Add('end;');
  CodeList.Free;
  VarList.Free;
end;

procedure TRecordProtocol.genProtoBufUtils(List, interfaceList: TStringList);
var
  Index , index2 : Integer;
begin
  interfaceList.Add(Format('type P%s = ^%s;',[Name,Name]));
  Index := interfaceList.Add(Format('function GetIdent(P:P%s):Word;overload;',[Name]));
  index2 := interfaceList.Add(Format('function GetProtocolSize(P:P%s):Integer;overload;',[Name]));
  List.Add(interfaceList[Index]);
  List.Add('begin');
  List.Add(Format('  Result := (%d shl 8) or %d;',[SystemID,ProtocolID]));
  List.Add('end;');

  List.Add(interfaceList[Index2]);
  List.Add('begin');
  if FDynmicArrayFieldInfo = nil then
    List.Add(Format('  Result := SizeOf(%s);',[Name]))
  else
    List.Add(Format('  Result := SizeOf(%s) + P.Size * SizeOf(%s);',[Name,FDynmicArrayFieldInfo.FiledType]));
  List.Add('end;');
end;

procedure TRecordProtocol.GenRecordToBufferCode(List:TStringList;InterfaceList:TStringList);
var
  I  , Index: Integer;
  FieldInfo : TFieldInfo;
  Code : String;
  VarList:TStringList;
  CodeList : TStringList;
begin

  CodeList := TStringList.Create;
  VarList  := TStringList.Create;

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields[i];

    if FieldInfo.IsArray then
    begin
      //占位数组 动态长度
      if FieldInfo.IsPathArray then
      begin
        CodeList.Add('  if R.Size > 0 then begin' );
        CodeList.Add('    for I := 0 to R.Size - 1 do begin' );
        if isRawType(FieldInfo.FiledType) then
        begin
          CodeList.Add(Format('      P.%sList.Add(R.%s[I]);',[FieldInfo.Name,FieldInfo.Name]));
        end else
        begin
          VarList.Add('  ' + FieldInfo.Name + ':'  + FieldInfo.FiledType + '_P;');
          CodeList.Add(Format('      %s := %s.Create();',[FieldInfo.Name,FieldInfo.FiledType + '_P']));
          CodeList.Add(Format('      RToP_%s(@R.%s[I],%s);',[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]));
          CodeList.Add(Format('      P.%sList.Add(%s);',[FieldInfo.Name,FieldInfo.Name]));
          CodeList.Add(Format('      P.FieldHasValue[p.%s] := True;',['tag_'+ FieldInfo.Name + 'List']));
        end;
        CodeList.Add('    end;');
        CodeList.Add('  end;');
      end else
      begin
        //静态长度
        CodeList.Add(Format('  for I := Low(R.%s) to High(R.%s) do begin',[FieldInfo.Name,FieldInfo.Name] ));
        if isRawType(FieldInfo.FiledType) then
        begin
          CodeList.Add(Format('    P.%sList.Add(R.%s[I]);',[FieldInfo.Name,FieldInfo.Name]));
        end else
        begin
          VarList.Add('  ' + FieldInfo.Name + ':'  + FieldInfo.FiledType + '_P;');
          CodeList.Add(Format('    %s := %s.Create();',[FieldInfo.Name,FieldInfo.FiledType + '_P']));
          CodeList.Add(Format('    RToP_%s(@R.%s[I],%s);',[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]));
          CodeList.Add(Format('    P.%sList.Add(%s);',[FieldInfo.Name,FieldInfo.Name]));
          CodeList.Add(Format('      P.FieldHasValue[p.%s] := True;',['tag_'+ FieldInfo.Name + 'List']));
        end;
        CodeList.Add('  end;');
      end;
    end else
    begin
      //Record 转 proto buf 不需要 赋值size 使其在网络中不要传输
      if LowerCase(FieldInfo.Name) = 'size' then
      begin
        Continue;
      end;


      if IsNumberType(FieldInfo.FiledType) then
      begin
        Code := Format('  if R.%s <> 0 then P.%s := R.%s ; ',[FieldInfo.Name,FieldInfo.Name,FieldInfo.Name]);
      end else

      if Trim(LowerCase(FieldInfo.FiledType)) = 'string' then
      begin
        Code := Format('  if R.%s <> ' + #39#39 + 'then P.%s := R.%s;',[FieldInfo.Name,FieldInfo.Name,FieldInfo.Name])
      end else

      if Trim(LowerCase(FieldInfo.FiledType)) = 'boolean' then
      begin
        Code := Format('  if R.%s <> false then P.%s := R.%s;',[FieldInfo.Name,FieldInfo.Name,FieldInfo.Name])
      end else
      begin
        Code := Format('RToP_%s(R.%s,P.%s);' ,[FieldInfo.FiledType,FieldInfo.Name,FieldInfo.Name]);
      end;
    end;

    CodeList.Add(Code);
  end;
  Index := List.Add(Format('procedure RToP_%s(R:%s;P:%s );',[FProtocolName,'ptr_'+ FProtocolName,ProtoBufName]));
  //interfaceList.Add(List[Index]);
  List.Add('var');
  List.Add('  I:Integer;');
  List.AddStrings(VarList);
  List.Add('begin');
  List.AddStrings(CodeList);
  List.Add('end;');
  CodeList.Free;
  VarList.Free;
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

function TRecordProtocol.GetIdent: Word;
begin
  Result := MakeWord(FSystemID,FProtocolID);
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

function TRecordProtocol.IsNumberType(const TypeName: String): Boolean;
var
  RT:String;
begin
  RT := LowerCase(Trim(TypeName));
  if (RT = 'byte') or (RT = 'word') or (RT = 'smallint') or (rt = 'integer') or (rt = 'cardinal') or (rt = 'uint64') or (rt = 'int64') or (rt = 'dword')then
  begin
    Result := True;
  end else
  begin
    Result := False;
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

function TRecordProtocol.GetProtoBufName: String;
begin
  Result :=  FProtocolName + '_P';
end;

{ TFieldInfo }

function TFieldInfo.FieldTypePtrName: String;
begin
  Result := 'ptr_' + FiledType;
end;

initialization
  DelphiTypeToProtoBufType := TDictionary<String,String>.Create();

  DelphiTypeToProtoBufType.Add('shortint','int8');
  DelphiTypeToProtoBufType.Add('smallint','int32');
  DelphiTypeToProtoBufType.Add('integer','int32');
  DelphiTypeToProtoBufType.Add('int64','int64');


  DelphiTypeToProtoBufType.Add('byte','uint8');
  DelphiTypeToProtoBufType.Add('word','int32');
  DelphiTypeToProtoBufType.Add('cardinal','uint32');
  DelphiTypeToProtoBufType.Add('dword','uint32');
  DelphiTypeToProtoBufType.Add('uint64','uint64');
  DelphiTypeToProtoBufType.Add('string','string');

end.
