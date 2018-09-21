unit GenCode;

interface
uses
  StringPool,System.Classes,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,DelphiAst.Consts,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,System.SysUtils,DB,System.Generics.Collections,MysqlOP,JclStrings,RecordProtocol,StringUtils,System.Types;

const
  INSERT = 'INSERT';
  UPDATE = 'UPDATE';
type

    TCodeGen = class
    private
      FSourceDir:String;
      FTargetDir:String;
      FProtocols: TObjectList<TRecordProtocol>;
      FProtocolDict : TDictionary<Word,TRecordProtocol>;
      FSource:TStringList;
      Function GetFieldNodeInfo(SyntaxTree: TSyntaxNode;out FieldName,FieldType,ControlString:String):Boolean;
    public
      constructor Create(const SourceDir,TargetDir:String);
      procedure BuildTargetFile(const SourceFileName:String ; SyntaxTree: TSyntaxNode);
      function FindInterfaceNode(SyntaxTree: TSyntaxNode):TSyntaxNode;
      procedure ProcessTypeDecl(SyntaxTree: TSyntaxNode);
      function GetUsesString(SyntaxTree: TSyntaxNode):string;
      procedure Gen();
    end;

implementation

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
              ProcessTypeDecl(Node);
           end;
        end;

      end;
    end;
  end;

  InterfaceList := TStringList.Create;
  ImplList := TStringList.Create;
  for I := 0 to FProtocols.Count - 1 do
  begin
    Protocol := FProtocols[i];
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

constructor TCodeGen.Create(const SourceDir, TargetDir: String);
begin
  FSourceDir := SourceDir;
  FTargetDir := TargetDir;
  FSource := TStringList.Create;
  FProtocols := TObjectList<TRecordProtocol>.Create;
  FProtocolDict := TDictionary<Word,TRecordProtocol>.Create();
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
  I:Integer;
  ProtocolIDArray:TArray<Word>;
  ID:Word;
  List: TStringList;
  TargetHandleProtocolFile:String;
  InterfaceList:TStringList;
  ImplList:TStringList;
  Protocol : TRecordProtocol;
  ProcName:String;
  FileName , UseStr , TempStr:String;
  InterfaceDict : TDictionary<string,Boolean>;
  None:Boolean;
  SourceDirName , HandleProtocolName:String;
  UseList:TStringList;
begin

  Files := TDirectory.GetFiles(FSourceDir,'*.pas',TSearchOption.soTopDirectoryOnly);
  UseList := TStringList.Create;
  for i := 0 to High(Files) do
  begin
    FileName := ExtractFileName(Files[i]);
    if FileName <> 'HandleProtocol.pas' then
    begin
      UseStr := UseStr + StrBefore('.',FileName) + ',';
      FSource.LoadFromFile(Files[i]);
      SyntaxTree := nil;
      HasException := False;
      try
        SyntaxTree := TPasSyntaxTreeBuilder.Run(Files[i], True,
            nil);
      except
        on E: ESyntaxTreeException do
        begin
          HasException := True;
        end;
      end;

      if not HasException then
      begin
        UseList.Add(ChangeFileExt(ExtractFileName(Files[i]),''));
        BuildTargetFile(Files[i],SyntaxTree);
      end;
    end;
  end;

  UseStr := UseStr + ' SocketInterface , System.Classes ; ';


  //处理所有协议 生成 协议处理文件。
  ProtocolIDArray := FProtocolDict.Keys.ToArray();
  TArray.Sort<Word>(ProtocolIDArray);
  List := TStringList.Create();
  SourceDirName := ExtractFileName(FSourceDir);
  HandleProtocolName := 'Handle_' + SourceDirName;
  TargetHandleProtocolFile := FTargetDir + Format('\%s.pas',[HandleProtocolName]);
  ImplList := TStringList.Create;
  InterfaceList := TStringList.Create;
  InterfaceDict := TDictionary<string,Boolean>.Create();
  if FileExists(TargetHandleProtocolFile) then
  begin
    List.LoadFromFile(TargetHandleProtocolFile);
    InterfaceList.Text := TStringUtil.StrBetween(List.Text,'//INTERFACE','//INTERFACE');
    ImplList.Text := TStringUtil.StrBetween(List.Text,'//IMPL','//IMPL');

    for I := 0 to interfaceList.Count - 1 do
    begin
      TempStr := Trim(interfaceList[i]);
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


  for i := 0 to High(ProtocolIDArray) do
  begin
    ID := ProtocolIDArray[i];
    FProtocolDict.TryGetValue(ID,Protocol);
    ProcName := 'Handle_' + Protocol.Name;

    //原来有的函数就算了
    if not InterfaceDict.TryGetValue(ProcName,None) then
    begin
      ProcName := Format('procedure Handle_%s(Socket:ISocket;ByteArray:TCheTekByteArray);',[Protocol.Name]);
      InterfaceList.Add(ProcName);
      ImplList.Add('');
      ImplList.Add(ProcName);
      ImplList.Add('var');
      ImplList.Add('data : ' + Protocol.Name  + ';');
      ImplList.Add('begin');
      ImplList.Add('  data.DeserializeFrom(ByteArray);');
      ImplList.Add('');
      ImplList.Add('end;');
    end;

  end;

  ImplList.Add('//IMPL');
  InterfaceList.Add('//INTERFACE');
  List.Clear;
  List.Add('unit ' + HandleProtocolName + ';');
  List.Add('interface');
  List.Add('uses ' + UseStr);
  List.AddStrings(InterfaceList);

  List.Add('implementation');
  List.AddStrings(ImplList);
  List.Add('end.');
  List.SaveToFile(TargetHandleProtocolFile,TEncoding.UTF8);

  //处理总socket 协议接收部分。
  List.Clear;
  ImplList.Clear;
  for i := 0 to UseList.Count - 1 do
  begin
    if i <> UseList.Count - 1 then
    begin
      UseList[i] := UseList[i] + ',';
    end;
  end;
  UseList.Insert(0,',');

  List.Assign(Templelate);
  List.Text := StringReplace(List.Text,'{uses}',UseList.Text,[rfReplaceAll]);
  for i := 0 to High(ProtocolIDArray) do
  begin
    ID := ProtocolIDArray[i];
    FProtocolDict.TryGetValue(ID,Protocol);
    ProcName := 'Handle_' + Protocol.Name;
    ImplList.Add(Format('ProcArray[%d] := %s ;',[ID,ProcName]));
  end;

  List.Text := StringReplace(List.Text,'{$PROTOCOLREGISTER}',ImplList.Text,[rfReplaceAll]);
  TargetHandleProtocolFile := ExtractFilePath(TargetHandleProtocolFile) + '\RegisterHanderProc_' + SourceDirName + '.pas';
  List.SaveToFile(TargetHandleProtocolFile,TEncoding.UTF8);
  List.Free;
  UseList.Free;
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

procedure TCodeGen.ProcessTypeDecl(SyntaxTree: TSyntaxNode);
var
  Node,FieldNode : TSyntaxNode;
  i ,j: Integer;
  TRecord: TRecordProtocol;
  FieldName,TypeName,ControlString:String;
begin
  for I := Low(SyntaxTree.ChildNodes) to High(SyntaxTree.ChildNodes) do
  begin
    Node := SyntaxTree.ChildNodes[i];
    if Node.Typ = ntType then
    begin
      TRecord := TRecordProtocol.Create(SyntaxTree.GetAttribute(anName),GetUsesString(SyntaxTree),Node.GetAttribute(anType));
      FProtocols.Add(TRecord);
      if TRecord.ProtocolID > 0 then
      begin
        if FProtocolDict.ContainsKey(TRecord.ProtocolID) then
        begin
          raise Exception.Create(Format('协议ID 重复 : %d',[TRecord.ProtocolID]));
        end else
        begin
          FProtocolDict.Add(TRecord.ProtocolID,TRecord);
        end;
      end;
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
              TRecord.AddField(FieldName,TypeName,FSource[FieldNode.Line - 1]);
            end else
            begin
             TRecord.AddField(FieldName,TypeName,FSource[FieldNode.Line - 1]);
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
