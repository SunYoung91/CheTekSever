unit GenCode;

interface
uses
  StringPool,System.Classes,Winapi.Windows,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,DelphiAst.Consts,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,System.SysUtils,DB,System.Generics.Collections,JclStrings,RecordProtocol,StringUtils,System.Types,Vcl.Dialogs;

const
  INSERT = 'INSERT';
  UPDATE = 'UPDATE';
type
    TRecordProtocolList = TList<TRecordProtocol>;
    TCodeGen = class
    private
      FSourceDir:String;
      FTargetDir:String;
      FProtocols: TObjectList<TRecordProtocol>;
      FFileProtocol : TDictionary<string,TRecordProtocolList>;
      FProtocolDict : TDictionary<Word,TRecordProtocol>;
      FSource:TStringList;
      FIsS2C : Boolean;      // 是否 S2C 代码 S2C 不需要 SocketManager 和 TPlayObject 绑定。
      Function GetFieldNodeInfo(SyntaxTree: TSyntaxNode;out FieldName,FieldType,ControlString:String):Boolean;
    public
      constructor Create(const SourceDir,TargetDir:String;IsS2C:String);

      procedure BuildTargetFile(const SourceFileName:String ; SyntaxTree: TSyntaxNode);
      function FindInterfaceNode(SyntaxTree: TSyntaxNode):TSyntaxNode;
      procedure ProcessTypeDecl(SyntaxTree: TSyntaxNode;const FileName:String);
      function GetUsesString(SyntaxTree: TSyntaxNode):string;
      procedure Gen();
      procedure GenDecodeUnit(Const UseStr : String);//生成Decode单元
      procedure GenSenderUnit(const UseStr : String);//生成发送数据单元
      procedure GenProtoBuffProto();  //生成protobuf 文件
      procedure GenProtoBufConvertCode();//生成 结构体转 protobuf 类的代码。
      procedure GenProtoBufUtilsCode(); //生成 protobuf 结构体和 一些杂项工具单元

    end;

implementation
uses uProtoBufGenerator,uProtoBufParserClasses;
{$R DBCodeGenResource.res}
{ TCodeGen }

var Templelate : TStringList;

procedure TCodeGen.BuildTargetFile(const SourceFileName:String ; SyntaxTree: TSyntaxNode);
var
  InterfaceNode , ChildNode,Node : TSyntaxNode;
  i , j : Integer;
  Protocol : TRecordProtocol;
  InterfaceList:TStringList;
  ImplList : TStringList;
  Source:TStringList;
  UnitName:String;
  ProtocolList : TRecordProtocolList;
begin
  InterfaceNode := FindInterfaceNode(SyntaxTree);

  if InterfaceNode <> nil then
  begin
    for i := Low(InterfaceNode.ChildNodes) to High(InterfaceNode.ChildNodes) do
    begin
      ChildNode :=  InterfaceNode.ChildNodes[i] ;
      if ChildNode.Typ = ntTypeSection  then
      begin
        for J := Low(ChildNode.ChildNodes) to High(ChildNode.ChildNodes) do
        begin
           Node := ChildNode.ChildNodes[J];
           if Node.Typ = ntTypeDecl then
           begin
              ProcessTypeDecl(Node,SourceFileName);
           end;
        end;

      end;
    end;
  end;

  InterfaceList := TStringList.Create;
  ImplList := TStringList.Create;
  FFileProtocol.TryGetValue(SourceFileName,ProtocolList);
  for I := 0 to ProtocolList.Count - 1 do
  begin
    Protocol := ProtocolList[i];
    Protocol.AddTo(InterfaceList,ImplList);
    InterfaceList.Add('');
    ImplList.Add('');
  end;

  Source := TStringList.Create;
  UnitName := StringReplace(ExtractFileName(SourceFileName),'.pas','',[rfReplaceAll]);
  Source.Add('unit ' + UnitName + ';');
  Source.Add('interface');
  Source.Add('uses System.Classes,System.SysUtils,CheTek.ByteArray;');
  Source.Add('type');
  Source.AddStrings(InterfaceList);
  Source.Add('implementation');
  Source.AddStrings(ImplList);
  Source.Add('end.');

  ImplList.Free;
  InterfaceList.Free;

  Source.SaveToFile(FTargetDir + '\' +  UnitName + '.pas',TEncoding.UTF8);
  Source.Free;
end;

constructor TCodeGen.Create(const SourceDir, TargetDir: String;IsS2C:String);
begin
  FSourceDir := SourceDir;
  FTargetDir := TargetDir;
  FSource := TStringList.Create;
  FProtocols := TObjectList<TRecordProtocol>.Create;
  FProtocolDict := TDictionary<Word,TRecordProtocol>.Create();
  FFileProtocol := TDictionary<String,TRecordProtocolList>.Create();
  FIsS2C := LowerCase(IsS2C) = 'true';
end;


function TCodeGen.FindInterfaceNode(SyntaxTree: TSyntaxNode): TSyntaxNode;
var
  ChildNode : TSyntaxNode;
begin
  Result:= nil;
  if SyntaxTree.Typ = TSyntaxNodeType.ntInterface  then
  begin
    Result := SyntaxTree;
    Exit;
  end;

  if SyntaxTree.HasChildren then
  begin
      for ChildNode in SyntaxTree.ChildNodes do
      begin
         Result := FindInterfaceNode(ChildNode);
         if Result  <> nil then
            Exit;
      end;
  end;
end;

procedure TCodeGen.Gen;
var
  SyntaxTree: TSyntaxNode;
  HasException : Boolean;
  Files:TStringDynArray;
  I , J :Integer;
  ProtocolIDArray:TArray<Word>;
  ID:Word;
  List: TStringList;
  TargetHandleProtocolFile,TargetDirPath:String;
  InterfaceList:TStringList;
  ImplList , UsesList:TStringList;
  Protocol : TRecordProtocol;
  ProcName:String;
  FileName , UseStr , TempStr,SenderUseStr:String;
  InterfaceDict : TDictionary<string,Boolean>;
  None:Boolean;
  SourceDirName , HandleProtocolName:String;
  UseList:TStringList;
  UnitName:String;
  SubUseStr : TStringList;
  HandlerInitProc : TStringList;
  ProtocolList : TRecordProtocolList;
begin

  Files := TDirectory.GetFiles(FSourceDir,'*.pas',TSearchOption.soTopDirectoryOnly);
  UseList := TStringList.Create;
  for i := 0 to High(Files) do
  begin
    FileName := ExtractFileName(Files[i]);
    if FileName <> 'HandleProtocol.pas' then
    begin
      UseStr := UseStr + StrBefore('.',FileName) + ',';
      FSource.LoadFromFile(Files[i],TUTF8Encoding.UTF8);
      SyntaxTree := nil;
      HasException := False;
      try
        SyntaxTree := TPasSyntaxTreeBuilder.Run(Files[i], True,
            nil);
      except
        on E: ESyntaxTreeException do
        begin
          HasException := True;
          ShowMessage(E.Message);
        end;
      end;

      if not HasException then
      begin
        UseList.Add(ChangeFileExt(ExtractFileName(Files[i]),''));
        BuildTargetFile(Files[i],SyntaxTree);
      end;
    end;
  end;

  if not FIsS2C then
  begin
    SenderUseStr := UseStr + ' System.Classes , CheTek.ByteArray , System.SysUtils , GameSocketData , Crc ; ';
    UseStr := UseStr + ' SocketManager , System.Classes , CheTek.ByteArray , System.SysUtils , ObjPlayer ; ' ;
  end else
  begin
    SenderUseStr := UseStr + ' SocketManager , System.Classes , CheTek.ByteArray , System.SysUtils , GameSocketData,Crc , M2Share  ; ' ;
    UseStr := UseStr + ' System.Classes , CheTek.ByteArray , System.SysUtils; ';
  end;


  //处理所有协议 生成 协议处理文件。
  ProtocolIDArray := FProtocolDict.Keys.ToArray();
  TArray.Sort<Word>(ProtocolIDArray);
  List := TStringList.Create();

  ImplList := TStringList.Create;
  InterfaceList := TStringList.Create;
  UsesList := TStringList.Create;
  InterfaceDict := TDictionary<string,Boolean>.Create();
  HandlerInitProc := TStringList.Create;



  //处理生成Handler 文件
  TargetDirPath := FTargetDir + '\Handler';
  if not DirectoryExists(TargetDirPath) then
  begin
    ForceDirectories(TargetDirPath);
  end;


  SubUseStr := TStringList.Create;
  for i := 0 to High(Files) do
  begin
    List.Clear;
    InterfaceList.Clear;
    ImplList.Clear;
    InterfaceDict.Clear;

    FileName := ExtractFileName(Files[i]);
    ProtocolList := nil;

    FFileProtocol.TryGetValue(Files[i],ProtocolList);
    if ProtocolList = nil  then
    begin
      Continue;
    end;

    FileName := 'Handle_' + FileName;
    TargetHandleProtocolFile := FTargetDir + Format('\Handler\%s',[FileName]);

    if FileExists(TargetHandleProtocolFile) then
    begin
      List.LoadFromFile(TargetHandleProtocolFile);
      InterfaceList.Text := TStringUtil.StrBetween(List.Text,'//INTERFACE','//INTERFACE');
      ImplList.Text := TStringUtil.StrBetween(List.Text,'//IMPL','//IMPL');
      UsesList.Text := TStringUtil.StrBetween(List.Text,'//USES','//USES');
      for J := 0 to interfaceList.Count - 1 do
      begin
        TempStr := Trim(interfaceList[J]);
        if TStringUtil.StartWith(TempStr,'procedure') then
        begin
          TempStr := Trim(TStringUtil.StrBetween(TempStr,'procedure','('));
          InterfaceDict.Add(TempStr,True);
        end;

      end;

      ImplList[0] := ('//IMPL');
      InterfaceList[0] := '//INTERFACE';
    end else
    begin
      ImplList.Add('//IMPL');
      InterfaceList.Add('//INTERFACE');
    end;


    for J := 0 to ProtocolList.Count - 1 do
    begin
      Protocol := ProtocolList[J];

      if Protocol.ProtocolID = 0 then
        Continue;

      ProcName := 'Handle_' + Protocol.Name;

      //原来有的函数就算了

      HandlerInitProc.Add(Format('  HandleProc[%d] := Handle_%s ;',[Protocol.ProtocolID,Protocol.Name]));

      if not InterfaceDict.TryGetValue(ProcName,None) then
      begin
        if not FIsS2C then
          ProcName := Format('procedure Handle_%s(SocketID:Integer; _data : Pointer ; Player : TPlayObject);',[Protocol.Name])
        else
          ProcName := Format('procedure Handle_%s(_data : Pointer);',[Protocol.Name]);

        InterfaceList.Add(ProcName);
        ImplList.Add('');
        ImplList.Add(ProcName);
        ImplList.Add('var');
        ImplList.Add('  Data : P' + Protocol.Name + ';');
        ImplList.Add('begin');
        ImplList.Add(format('  Data := P%s(_data);',[Protocol.Name]));
        ImplList.Add('');
        ImplList.Add('end;');
      end;

    end;

    HandleProtocolName := ChangeFileExt(FileName,'');

    SubUseStr.Add(HandleProtocolName + ',');

    ImplList.Add('//IMPL');
    InterfaceList.Add('//INTERFACE');
    List.Clear;
    List.Add('unit ' + HandleProtocolName + ';');
    List.Add('interface');
    List.Add('uses ' + UseStr);
    List.AddStrings(InterfaceList);

    List.Add('implementation');
    List.Add('//USES');
    if UsesList.Count > 0 then
      UsesList.Delete(0);
    List.AddStrings(UsesList);
    List.Add('//USES');

    List.AddStrings(ImplList);

    List.Add('end.');

    //List.SaveToFile(TargetHandleProtocolFile,TEncoding.UTF8);

  end;

  SourceDirName := ExtractFileName(FSourceDir);
  HandleProtocolName := 'Handle_' + SourceDirName;
  TargetHandleProtocolFile := FTargetDir + Format('\%s.pas',[HandleProtocolName]);

  if not FIsS2C then
  begin
    ProcName := Format('procedure DoHandle_%s(ID:Integer ; Data : Pointer ; SocketID : Integer ; Player : TPlayObject);',[SourceDirName]);
  end else
  begin
    ProcName := Format('procedure DoHandle_%s(ID:Integer ; Data : Pointer);',[SourceDirName]);
  end;

  List.Clear;

  List.Add('unit ' + HandleProtocolName + ';');
  List.Add('interface');
  List.Add('uses '  + SubUseStr.Text + UseStr);

  List.Add(ProcName);

  List.Add('implementation');

  if not FIsS2C then
  begin
    List.Add('var HandleProc : array[0..65535] of procedure(SocketID : Integer; _Data : Pointer; Player : TPlayObject );');
  end else
  begin
    List.Add('var HandleProc : array[0..65535] of procedure(_Data : Pointer);');
  end;

  List.Add(ProcName);
  List.Add('begin');
  List.Add('  if Assigned(HandleProc[ID]) then');
  List.Add('  begin');
  if  not FIsS2C then
     List.Add('    HandleProc[ID](SocketID,Data,Player);')
  else
     List.Add('    HandleProc[ID](Data);');

  List.Add('  end else');
  List.Add('  begin');
  List.Add('    Raise Exception.Create(''Procol Handle Proc not Exist : '' + IntToStr(ID))');
  List.Add('  end;');
  List.Add('end;');

  List.Add('initialization');
  List.Add('FillChar(@HandleProc[0],0,SizeOf(HandleProc));');
  List.AddStrings(HandlerInitProc);

  List.Add('end.');
  //List.SaveToFile(TargetHandleProtocolFile,TEncoding.UTF8);


  HandlerInitProc.Free;
  List.Free;
  UseList.Free;
  UsesList.Free;

  //GenDecodeUnit(UseStr);
  //GenSenderUnit(SenderUseStr);


  GenProtoBuffProto();
  GenProtoBufConvertCode();

  GenProtoBufUtilsCode();
end;

procedure TCodeGen.GenDecodeUnit(Const UseStr : String);
var
  SourceFile , InterfaceList , ImplList , DecodeBlock:TStringList;
  SourceDirName , UnitName , FileName: String;
  I:Integer;
  ID:Word;
  ProtocolIDArray:TArray<Word>;
  Protocol : TRecordProtocol;
  ProcName , DisposeProcName , NewProcName:String;
begin
  SourceFile := TStringList.Create;
  InterfaceList := TStringList.Create;
  ImplList := TStringList.Create;
  DecodeBlock := TStringList.Create;

  SourceDirName := ExtractFileName(FSourceDir);
  ProtocolIDArray := FProtocolDict.Keys.ToArray();
  TArray.Sort<Word>(ProtocolIDArray);

  for i := 0 to High(ProtocolIDArray) do
  begin
    ID := ProtocolIDArray[i];
    if FProtocolDict.TryGetValue(ID,Protocol) then
    begin
      ProcName := Format('function Decode_%s(ByteArray:TCheTekByteArray):Pointer;',[Protocol.Name]);
      //InterfaceList.Add(ProcName);
      DecodeBlock.Add(Format(' DecodeProc[%d] := Decode_%s;',[ID,Protocol.Name]));
      ImplList.Add(ProcName);
      ImplList.Add('var');
      ImplList.Add('  data : P' + Protocol.Name + ' ; ' );
      ImplList.Add('begin');
      ImplList.Add('  New(data); ');
      ImplList.Add('  data^.DeserializeFrom(ByteArray); ');
      ImplList.Add('  Result := data ;');
      ImplList.Add('end;');

      ProcName := Format('procedure Dispose_%s(Ptr:Pointer);',[Protocol.Name]);
      DecodeBlock.Add(Format(' DisposeProc[%d] := Dispose_%s;',[ID,Protocol.Name]));
      ImplList.Add(ProcName);
      ImplList.Add('begin');
      ImplList.Add(Format('  Dispose(P%s(Ptr));',[Protocol.Name]));
      ImplList.Add('end;');


      ProcName := Format('function New_%s():Pointer;',[Protocol.Name,Protocol.Name]);
      DecodeBlock.Add(Format(' NewProc[%d] := New_%s;',[ID,Protocol.Name]));
      ImplList.Add(ProcName);
      ImplList.Add('begin');
      ImplList.Add(Format(' New(P%s(Result));',[Protocol.Name]));
      ImplList.Add('end;');
    end else
    begin
      raise Exception.Create(Format('Cant Get Protocol , ID : %d , I:%d ',[ID,I]));
    end;
  end;

  UnitName := 'Decode_' + SourceDirName;
  ProcName := Format('procedure DoDecode_%s(ByteArray : TCheTekByteArray ;ID:Word; var Ptr:Pointer);',[SourceDirName]);
  DisposeProcName :=  Format('procedure DoDispose_%s(ID:Word; Ptr:Pointer);',[SourceDirName]);
  NewProcName := Format('function New_%s(ID:Word):Pointer;',[SourceDirName]);
  InterfaceList.Add(ProcName);
  InterfaceList.Add(DisposeProcName);
  InterfaceList.Add(NewProcName);

  SourceFile.Add('unit ' + UnitName + ';');
  SourceFile.Add('interface');
  SourceFile.Add('uses ' + UseStr);

  SourceFile.AddStrings(InterfaceList);
  SourceFile.Add('implementation');
  SourceFile.Add('var DecodeProc : array[0..65535] of function(ByteArray:TCheTekByteArray):Pointer;');
  SourceFile.Add('var DisposeProc : array[0..65535] of procedure(Ptr:Pointer);');
  SourceFile.Add('var NewProc : array[0..65535] of function():Pointer;');

  SourceFile.AddStrings(ImplList);
  SourceFile.Add(ProcName);
  SourceFile.Add('begin');
  SourceFile.Add('  if Assigned(DecodeProc[ID]) then');
  SourceFile.Add('  begin');
  SourceFile.Add('    Ptr := DecodeProc[ID](ByteArray);');
  SourceFile.Add('  end else');
  SourceFile.Add('  begin');
  SourceFile.Add('    Raise Exception.Create(''Procol Decode Proc not Exist : '' + IntToStr(ID))');
  SourceFile.Add('  end;');
  SourceFile.Add('end;');


  SourceFile.Add(DisposeProcName);
  SourceFile.Add('begin');
  SourceFile.Add('  if Assigned(DisposeProc[ID]) then');
  SourceFile.Add('  begin');
  SourceFile.Add('    DisposeProc[ID](Ptr);');
  SourceFile.Add('  end else');
  SourceFile.Add('  begin');
  SourceFile.Add('    Raise Exception.Create(''Procol Dispose Proc not Exist : '' + IntToStr(ID))');
  SourceFile.Add('  end;');
  SourceFile.Add('end;');

  SourceFile.Add(NewProcName);
  SourceFile.Add('begin');
  SourceFile.Add('  if Assigned(NewProc[ID]) then');
  SourceFile.Add('  begin');
  SourceFile.Add('   Result :=  NewProc[ID]();');
  SourceFile.Add('  end else');
  SourceFile.Add('  begin');
  SourceFile.Add('    Raise Exception.Create(''Procol New Proc not Exist : '' + IntToStr(ID))');
  SourceFile.Add('  end;');
  SourceFile.Add('end;');


  SourceFile.Add('initialization');
  SourceFile.Add('FillChar(@DecodeProc[0],0,SizeOf(DecodeProc));');
  SourceFile.Add('FillChar(@DisposeProc[0],0,SizeOf(DisposeProc));');
  SourceFile.Add('FillChar(@NewProc[0],0,SizeOf(NewProc));');
  SourceFile.AddStrings(DecodeBlock);
  SourceFile.Add('end.');
  FileName := FTargetDir + '\' + UnitName + '.pas';
  SourceFile.SaveToFile(FileName);

  DecodeBlock.Free;
  ImplList.Free;
  InterfaceList.Free;
  SourceFile.Free;
end;

function ExtractFileNameOnly(const Str:String):String;
begin
  Result := ExtractFileName(Str);
  Result := Copy(Result,1,Length(Result) - 4);
end;

procedure TCodeGen.GenProtoBufConvertCode;
var
  ProtocolList : TRecordProtocolList;
  Pair : TPair<String,TRecordProtocolList>;
  I , Index :Integer;
  Protocol : TRecordProtocol;
  List , InterfaceList , PasSource , ConvertDict: TStringList;
  FileName , ClassName :String;
  SameProtoCheck : TDictionary<DWORD,TRecordProtocol>;
  ProtoPair : TPair<DWORD,TRecordProtocol>;
begin
  SameProtoCheck := TDictionary<DWORD,TRecordProtocol>.Create();
  ConvertDict := TStringList.Create;

  for Pair in FFileProtocol do
  begin
    PasSource := TStringList.Create;
    ProtocolList := Pair.value;
    List := TStringList.Create();
    InterfaceList := TStringList.Create();
    SameProtoCheck.Clear();

    InterfaceList.Add(Format('uses pbInPut,pbOutPut,pbPublic,uAbstractProtoBufClasses,%s.ProtoClass,',[ExtractFileNameOnly (Pair.Key)]));

    InterfaceList.Add(Format('  %s;',[ExtractFileNameOnly (Pair.Key)]));

    //添加转换类声明
    InterfaceList.Add('type');
    InterfaceList.Add(Format('  T%sProtoBufConverter = class',[ExtractFileNameOnly (Pair.Key)]));
    ClassName := Format('T%sProtoBufConverter',[ExtractFileNameOnly (Pair.Key)]);
           InterfaceList.Add('  private');
           InterfaceList.Add('    FPbOutPutBuffer : TProtoBufOutput;');
           InterfaceList.Add('    FProtoBufObjs:Array[0..65535] of TAbstractProtoBufClass;');
           InterfaceList.Add('  public');
           InterfaceList.Add('    constructor Create();');
           InterfaceList.Add('    destructor Destroy();override;');
           InterfaceList.Add('    function RecordToProtoBuf(const Ident:Word; const pRecordData:Pointer; const RecordDataLen:Cardinal ; const pProtoBufBuffer:Pointer; const ProtoBufBufferSize:Cardinal ):Integer;');
           InterfaceList.Add('    function ProtoBufToRecord(const Ident:Word; const pRecordData:Pointer; const RecordDataLen:Cardinal ; const pProtoBufBuffer:Pointer; const ProtoBufBufferSize:Cardinal ):Integer;');
           InterfaceList.Add('  end;');

           List.Add(Format('  {%s}',[ClassName]));
           List.Add(Format('constructor %s.Create();',[ClassName]));
                  List.Add('begin');
                  List.Add('  FPbOutPutBuffer := TProtoBufOutput.Create();');
                  List.Add('  FillChar(FProtoBufObjs,SizeOf(FProtoBufObjs),0);');
                  List.Add('end;');
            List.Add('');
            List.Add(Format('destructor %s.Destroy();',[ClassName]));
            List.Add('var');
            List.Add('  I : Integer;');
            List.Add('begin');
            List.Add('  FPbOutPutBuffer.Free;');
            List.Add('  For i := 0 to 65535 do begin');
            List.Add('    if FProtoBufObjs[i] <> nil then FProtoBufObjs[i].Free; ');
            List.Add('  end;');
            List.Add('end;');

    //生成互相转换的代码
    for I := 0 to ProtocolList.Count - 1 do
    begin
      Protocol := ProtocolList[i];
      List.Add(Format('type ptr_%s = ^%s;',[Protocol.Name,Protocol.Name]));
      Protocol.GenRecordToBufferCode(List,InterfaceList);
      List.Add('');
      Protocol.GenProtoBufToRecordCode(List,InterfaceList);

      if (Protocol.SystemID <> 0) or (Protocol.ProtocolID <> 0) then
      begin
        SameProtoCheck.Add(MakeLong(Protocol.SystemID,Protocol.ProtocolID),Protocol);
        //ConvertDict.Add(Format('RtoP[%d] := @RToP_%s;',[MakeWord(Protocol.SystemID,Protocol.ProtocolID),Protocol.Name] ));
        //ConvertDict.Add(Format('PtoR[%d] := @PToR_%s;',[MakeWord(Protocol.SystemID,Protocol.ProtocolID),Protocol.Name] ));
      end;
    end;

    //添加转换代码
    List.Add(Format('function %s.RecordToProtoBuf(const Ident:Word; const pRecordData:Pointer; const RecordDataLen:Cardinal ; const pProtoBufBuffer:Pointer; const ProtoBufBufferSize:Cardinal ):Integer;',[ClassName]));
    List.Add('var');
    List.Add('  RealRecordSize :Cardinal;');
    List.Add('  ProtoBufObj : TAbstractProtoBufClass;');

    List.Add('begin');
    List.Add('  Result := 0;');
    List.Add('  case Ident of');
    for ProtoPair in SameProtoCheck do
    begin
      ProtoPair.Value.GenCaseRecordToProtoCode(List);
    end;
    List.Add('  end;');
    List.Add('end;');



    List.Add(Format('function %s.ProtoBufToRecord(const Ident:Word; const pRecordData:Pointer; const RecordDataLen:Cardinal ; const pProtoBufBuffer:Pointer; const ProtoBufBufferSize:Cardinal ):Integer;',[ClassName]));
    List.Add('var');
    List.Add('  RealRecordSize :Cardinal;');
    List.Add('  ProtoBufObj : TAbstractProtoBufClass;');
    List.Add('begin');
    List.Add('  Result := 0;');
    List.Add('  case Ident of');
    for ProtoPair in SameProtoCheck do
    begin
      ProtoPair.Value.GenCaseProtoBufToRecord(List);
    end;
    List.Add('  end;');
    List.Add('end;');


    FileName := FTargetDir + '\' + ExtractFileNameOnly (Pair.Key) + '.ProtoConvert.pas';
    PasSource.Add('unit ' + ExtractFileNameOnly (Pair.Key) + '.ProtoConvert;');
    PasSource.Add('interface');
    PasSource.AddStrings(interfaceList);
    PasSource.Add('implementation');
    PasSource.Add('{$WARN IMPLICIT_STRING_CAST OFF}');
    PasSource.Add('{$WARN IMPLICIT_STRING_CAST_LOSS OFF}');
    PasSource.Add('{$HINTS OFF}');
    PasSource.AddStrings(List);

    PasSource.Add('{$WARN IMPLICIT_STRING_CAST ON}');
    PasSource.Add('{$WARN IMPLICIT_STRING_CAST_LOSS ON}');
    PasSource.Add('{$HINTS ON}');
    PasSource.Add('initialization');

    //PasSource.AddStrings(ConvertDict);
    PasSource.Add('end.');



    PasSource.SaveToFile(FileName,TEncoding.UTF8);
    PasSource.Free;
  end;

  SameProtoCheck.Free;
  //ConvertDict.Free;
end;

procedure TCodeGen.GenProtoBuffProto;
var
  ProtocolList : TRecordProtocolList;
  Pair : TPair<String,TRecordProtocolList>;
  I:Integer;
  Protocol : TRecordProtocol;
  List: TStringList;
  FileName :String;
  Gen: TProtoBufGenerator;
  GenPasCodeFileName:String;
  OutProtoBufStringList:TStringList;
  ProFile :TProtoFile;
  IPos:Integer;
begin
  for Pair in FFileProtocol do
  begin
    ProtocolList := Pair.value;
    List := TStringList.Create();
    for I := 0 to ProtocolList.Count - 1 do
    begin
      Protocol := ProtocolList[i];
      Protocol.GenProto(List);
    end;
    FileName := FTargetDir + '\' + ExtractFileNameOnly (Pair.Key) + '.proto';
    List.SaveToFile(FileName,TEncoding.UTF8);

    ProFile := TProtoFile.Create(nil);
    ProFile.FileName := ExtractFileNameOnly (Pair.Key) + '.proto';
    IPos := 1;
    ProFile.ParseFromProto(List.Text, IPos);

    Gen := TProtoBufGenerator.Create;
    OutProtoBufStringList := TStringList.Create;
    Gen.Generate(ProFile,OutProtoBufStringList);

    Gen.Free;
    GenPasCodeFileName := FTargetDir + '\' + ExtractFileNameOnly (Pair.Key) + '.ProtoClass.pas';
    OutProtoBufStringList[0] := 'unit ' + ExtractFileNameOnly (Pair.Key) + '.ProtoClass;';
    OutProtoBufStringList.SaveToFile(GenPasCodeFileName,TEncoding.UTF8);

  end;
end;

procedure TCodeGen.GenProtoBufUtilsCode;
var
  ProtocolList : TRecordProtocolList;
  Pair : TPair<String,TRecordProtocolList>;
  I:Integer;
  Protocol : TRecordProtocol;
  CodeImplementation , CodeInterface,OutPas: TStringList;
  Index : Integer;
  Name:String;
begin
  for Pair in FFileProtocol do
  begin
    ProtocolList := Pair.value;
    CodeImplementation := TStringList.Create();
    CodeInterface := TStringList.Create();
    for I := 0 to ProtocolList.Count - 1 do
    begin
      Protocol := ProtocolList[i];
      if Protocol.Ident <> 0 then
        Protocol.genProtoBufUtils(CodeImplementation,CodeInterface);
    end;

    Index := CodeInterface.Add('function CreateProtocolClass(Ident:Word):TAbstractProtoBufClass;');
    CodeImplementation.Add(CodeInterface[Index]);
    CodeImplementation.Add('begin');
    CodeImplementation.Add('  Result := nil;');
    CodeImplementation.Add('  case Ident of');
    for I := 0 to ProtocolList.Count - 1 do
    begin
      Protocol := ProtocolList[i];
      if Protocol.Ident <> 0 then
       CodeImplementation.Add(Format('  %d : Result := %s.Create();',[Protocol.Ident,Protocol.ProtoBufName]));
    end;

    CodeImplementation.Add('  end');
    CodeImplementation.Add('end;');

    OutPas := TStringList.Create;
    OutPas.Add(Format('unit %s.ProtoUtils;',[ExtractFileNameOnly(Pair.Key)]));
    OutPas.Add('interface');
    Name := ExtractFileNameOnly (Pair.Key);
    OutPas.Add(Format('uses uAbstractProtoBufClasses,%s,%s,%s;',[Name,Name + '.ProtoClass',Name + '.ProtoConvert']));
    OutPas.AddStrings(CodeInterface);
    OutPas.Add('implementation');
    OutPas.AddStrings(CodeImplementation);
    OutPas.Add('end.');
    OutPas.SaveToFile(FTargetDir + '\' + ExtractFileNameOnly (Pair.Key) + '.ProtoUtils.pas');
    CodeImplementation.Free;
    CodeInterface.Free;
  end;
end;

procedure TCodeGen.GenSenderUnit(const UseStr: String);
var
  SourceFile , InterfaceList , ImplList , DecodeBlock:TStringList;
  SourceDirName , UnitName , FileName: String;
  I:Integer;
  ID:Word;
  ProtocolIDArray:TArray<Word>;
  Protocol : TRecordProtocol;
  ProcName , DisposeProcName:String;
begin
  SourceFile := TStringList.Create;
  InterfaceList := TStringList.Create;
  ImplList := TStringList.Create;
  DecodeBlock := TStringList.Create;

  SourceDirName := ExtractFileName(FSourceDir);
  ProtocolIDArray := FProtocolDict.Keys.ToArray();
  TArray.Sort<Word>(ProtocolIDArray);

  for i := 0 to High(ProtocolIDArray) do
  begin
    ID := ProtocolIDArray[i];
    if FProtocolDict.TryGetValue(ID,Protocol) then
    begin
      if not FIsS2C then
      begin
        ProcName := Format('procedure Send(var Data : %s);overload;',[Protocol.Name]);
        InterfaceList.Add(ProcName);
      end
      else
      begin
        ProcName := Format('procedure Send_%s(_Data : Pointer);overload;',[Protocol.Name]);
        DecodeBlock.add(Format('  SendProc[%d] := Send_%s; ',[Protocol.ProtocolID,Protocol.Name]));
      end;

      ImplList.Add(ProcName);
      ImplList.Add('var');
      ImplList.Add('  Size:Integer;' );
      ImplList.Add('  OldPos , NewPos,CrcDataPosition:NativeInt;' );
      ImplList.Add('  ByteArray:TCheTekByteArray;');
      ImplList.Add('  CrcValue:Word;');
      ImplList.Add('  GamePacket : TGamePacket;');

      if  FIsS2C then
        ImplList.Add( Format('  Data : P%s;',[Protocol.Name]));

      ImplList.Add('begin');

      if not FIsS2C then
      begin
        ImplList.Add('  ByteArray := SendBuffer;');
      end else
      begin
        ImplList.Add( Format('  Data := P%s(_Data);',[Protocol.Name]));
        ImplList.Add('  ByteArray := TSocketManager.SendBytes;  ');
      end;

      ImplList.Add('  OldPos := ByteArray.Position;');
      ImplList.Add('  GamePacket.wPacketFlag := PACK_STARFLAG;');
      ImplList.Add('  ByteArray.Write(@GamePacket,GAMEPACKETSIZE);');

      ImplList.Add('  CrcDataPosition := ByteArray.Position;');

      //ImplList.Add(Format('  ByteArray.WriteWord(%d); ',[Protocol.ProtocolID]));
      ImplList.Add('  Data.SerializeTo(ByteArray);');
      ImplList.Add('  NewPos := ByteArray.Position;');
      ImplList.Add('  Size := NewPos - CrcDataPosition;');

      ImplList.Add('  GamePacket.SetPacketLen(Size);');
      ImplList.Add('  GamePacket.wDataCrc := Crc32( PByte(NativeInt(ByteArray.Memory) + CrcDataPosition) , Size );');
      ImplList.Add('  ByteArray.Position := OldPos;');
      ImplList.Add('  ByteArray.Write(@GamePacket,GAMEPACKETSIZE);');
      ImplList.Add('  ByteArray.Position := NewPos;');
      ImplList.Add('end;');
    end else
    begin
      raise Exception.Create(Format('Cant Get Protocol , ID : %d , I:%d ',[ID,I]));
    end;
  end;

  //服务端的代码 那么需要 总的SendData
  if FIsS2C then
  begin
    InterfaceList.Add('procedure Send(ProtocolID:Word ; Data:Pointer);');
    ImplList.Add('procedure Send(ProtocolID:Word ; Data:Pointer);');
    ImplList.Add('begin');
    ImplList.Add('  if Assigned(SendProc[ProtocolID]) then');
    ImplList.Add('  begin');
    ImplList.Add('   SendProc[ProtocolID](Data);');
    ImplList.Add('  end else');
    ImplList.Add('  begin');
    ImplList.Add('    Raise Exception.Create(''Procol Send Proc not Exist : '' + IntToStr(ProtocolID))');
    ImplList.Add('  end;');
    ImplList.Add('end;');
  end;

  UnitName := 'Sender_' + SourceDirName;

  SourceFile.Add('unit ' + UnitName + ';');
  SourceFile.Add('interface');
  SourceFile.Add('uses ' + UseStr);

  SourceFile.AddStrings(InterfaceList);
  if not FIsS2C then
     SourceFile.Add('function GetSendBuffer():TCheTekByteArray;');

  SourceFile.Add('implementation');

  if not FIsS2C then
  begin
     SourceFile.Add('var');
     SourceFile.Add('SendBuffer : TCheTekByteArray;');
  end else
  begin
    SourceFile.Add('var');
    SourceFile.Add('SendProc : Array [0..65535] of procedure (Data : Pointer);');
  end;



  SourceFile.AddStrings(ImplList);



  if not FIsS2C then
  begin
    SourceFile.Add('function GetSendBuffer():TCheTekByteArray;');
    SourceFile.Add('begin');
    SourceFile.Add('  result := SendBuffer;');
    SourceFile.Add('end;');

    SourceFile.Add('initialization');
    SourceFile.Add('SendBuffer := TCheTekByteArray.Create();');
    SourceFile.Add('finalization');
    SourceFile.Add('SendBuffer.Free');
  end else
  begin
    SourceFile.Add('initialization');
    SourceFile.addStrings(DecodeBlock);
  end;



  SourceFile.Add('end.');

  FileName := FTargetDir + '\' + UnitName + '.pas';
  SourceFile.SaveToFile(FileName);

  DecodeBlock.Free;
  ImplList.Free;
  InterfaceList.Free;
  SourceFile.Free;
end;

function TCodeGen.GetFieldNodeInfo(SyntaxTree: TSyntaxNode; out FieldName,
  FieldType, ControlString: String): Boolean;
var
  NodeField : TSyntaxNode;
  NodeValue : TSyntaxNode;
begin
  Result := False;
  if SyntaxTree <> nil then
  begin
    if (SyntaxTree.Typ = ntField) and (Length(SyntaxTree.ChildNodes) = 2) then
    begin
       NodeField := SyntaxTree.ChildNodes[0];
       NodeValue := SyntaxTree.ChildNodes[1];
      if not (NodeField is TValuedSyntaxNode) then
      begin
        raise Exception.Create('GenCode.pas GetFieldNodeInfo : NodeField is not TValuedSyntaxNode');
      end;

      FieldName :=  TValuedSyntaxNode(NodeField).Value;
      FieldType := NodeValue.GetAttribute(anName);
      ControlString := StrAfter('#',FSource[SyntaxTree.Line -1]);
      Result := True;
    end else
    begin
      raise Exception.Create('GenCode.pas GetFieldNodeInfo : SyntaxTree is not normal FieldTree');
    end;
  end;

end;

function TCodeGen.GetUsesString(SyntaxTree: TSyntaxNode): string;
var
  line:Integer;
  Str:String;
begin
  line := SyntaxTree.Line;
  Result := '';
  if FSource.Count > line then
  begin
    Str := FSource[line];
    if StrMatch('uses',Str) >= 1 then
    begin
      Str := StrAfter('//',Str);
      Str := StrBefore('//',Str);
      Str := StrAfter('uses',Str);
      Str := StringReplace(Str,';','',[rfReplaceAll]);
      Result := ',' +  Str;
    end;
  end;
end;

procedure TCodeGen.ProcessTypeDecl(SyntaxTree: TSyntaxNode ; const FileName:String);
var
  Node,FieldNode : TSyntaxNode;
  i ,j: Integer;
  RecordProtocol: TRecordProtocol;
  FieldName,TypeName,ControlString:String;
  ProtocolList : TRecordProtocolList;
  RecordName:String;
begin
  for I := Low(SyntaxTree.ChildNodes) to High(SyntaxTree.ChildNodes) do
  begin
    Node := SyntaxTree.ChildNodes[i];
    if Node.Typ = ntType then
    begin
      RecordName := SyntaxTree.GetAttribute(anName);

      //P 开头的忽略 约定为指针定义
      if (RecordName[1] = 'p') or (RecordName[1]= 'P')  then
        Continue;


      RecordProtocol := TRecordProtocol.Create(RecordName,GetUsesString(SyntaxTree),Node.GetAttribute(anType));
      FProtocols.Add(RecordProtocol);
      if not FFileProtocol.TryGetValue(FileName,ProtocolList) then
      begin
        ProtocolList := TRecordProtocolList.Create();
        FFileProtocol.AddOrSetValue( FileName , ProtocolList );
      end;

      if RecordProtocol.Ident > 0 then
      begin
        if FProtocolDict.ContainsKey(RecordProtocol.Ident) then
        begin
          raise Exception.Create(Format('协议ID 重复 : %s',[RecordProtocol.Name]));
        end;

        FProtocolDict.Add(RecordProtocol.Ident,RecordProtocol);
      end;



      ProtocolList.Add(RecordProtocol);

      for j := Low(Node.ChildNodes) to High(Node.ChildNodes) do
      begin
        FieldNode := Node.ChildNodes[J];
        if FieldNode.Typ = ntField then
        begin
          if GetFieldNodeInfo(FieldNode,FieldName,TypeName,ControlString) then
          begin
            if TypeName = '' then
            begin
              TypeName := Trim (TStringUtil.StrBetween(FSource[FieldNode.Line - 1],':',';'));
              RecordProtocol.AddField(FieldName,TypeName,FSource[FieldNode.Line - 1]);
            end else
            begin
              RecordProtocol.AddField(FieldName,TypeName,FSource[FieldNode.Line - 1]);
            end;
          end;
        end;
      end;
    end;
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
