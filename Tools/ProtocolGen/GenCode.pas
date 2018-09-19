unit GenCode;

interface
uses
  StringPool,System.Classes,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,DelphiAst.Consts,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,System.SysUtils,DB,System.Generics.Collections,MysqlOP,JclStrings,RecordProtocol,StringUtils;

const
  INSERT = 'INSERT';
  UPDATE = 'UPDATE';
type

    TCodeGen = class
    private
      FSourceFile:String;
      FTargetDir:String;
      FProtocols: TObjectList<TRecordProtocol>;
      FSource:TStringList;
      Function GetFieldNodeInfo(SyntaxTree: TSyntaxNode;out FieldName,FieldType,ControlString:String):Boolean;
    public
      constructor Create(const SourceFile,TargetDir:String);
      procedure BuildTargetFile(SyntaxTree: TSyntaxNode);
      function FindInterfaceNode(SyntaxTree: TSyntaxNode):TSyntaxNode;
      procedure ProcessTypeDecl(SyntaxTree: TSyntaxNode);
      function GetUsesString(SyntaxTree: TSyntaxNode):string;
      procedure Gen();
    end;

implementation

{$R DBCodeGenResource.res}
{ TCodeGen }

procedure TCodeGen.BuildTargetFile(SyntaxTree: TSyntaxNode);
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
  UnitName := StringReplace(ExtractFileName(FSourceFile),'.pas','',[rfReplaceAll]);
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

  Source.SaveToFile(FTargetDir + '\' +  UnitName + '.pas');
  Source.Free;
end;

constructor TCodeGen.Create(const SourceFile, TargetDir: String);
begin
  FSourceFile := SourceFile;
  FTargetDir := TargetDir;
  FSource := TStringList.Create;
  FSource.LoadFromFile(SourceFile);
  FProtocols := TObjectList<TRecordProtocol>.Create;
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
begin
  SyntaxTree := nil;
  HasException := False;
  try
    SyntaxTree := TPasSyntaxTreeBuilder.Run(FSourceFile, True,
        nil);
  except
    on E: ESyntaxTreeException do
    begin
      HasException := True;
    end;
  end;

  if not HasException then
  begin
    BuildTargetFile(SyntaxTree);
  end;

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



end.
