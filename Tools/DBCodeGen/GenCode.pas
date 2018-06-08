unit GenCode;

interface
uses
  StringPool,System.Classes,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,DelphiAst.Consts,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,System.SysUtils,DB,System.Generics.Collections,MysqlOP,JclStrings,TableCodeGen;

const
  INSERT = 'INSERT';
  UPDATE = 'UPDATE';
type

    TCodeGen = class
    private
      FSourceFile:String;
      FTargetDir:String;
      FTables: TList<TDBTable>;
      FSource:TStringList;
      Function GetFieldNodeInfo(SyntaxTree: TSyntaxNode;out FieldName,FieldType,ControlString:String):Boolean;
    public
        constructor Create(const SourceFile,TargetDir:String);
        procedure BuildTargetFile(SyntaxTree: TSyntaxNode);
        function FindInterfaceNode(SyntaxTree: TSyntaxNode):TSyntaxNode;
	    	procedure ProcessTypeDecl(SyntaxTree: TSyntaxNode);
        procedure Gen();
    end;

implementation

{$R DBCodeGenResource.res}
{ TCodeGen }

procedure TCodeGen.BuildTargetFile(SyntaxTree: TSyntaxNode);
var
  InterfaceNode , ChildNode,Node : TSyntaxNode;
  i , j : Integer;
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
  end
end;

constructor TCodeGen.Create(const SourceFile, TargetDir: String);
begin
  FSourceFile := SourceFile;
  FTargetDir := TargetDir;
  FSource := TStringList.Create;
  FSource.LoadFromFile(SourceFile);
  FTables := TList<TDBTable>.Create;
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

procedure TCodeGen.ProcessTypeDecl(SyntaxTree: TSyntaxNode);
var
  Node,FieldNode : TSyntaxNode;
  i ,j: Integer;
  Table: TDBTable;
  FieldName,TypeName,ControlString:String;
begin
  for I := Low(SyntaxTree.ChildNodes) to High(SyntaxTree.ChildNodes) do
  begin
    Node := SyntaxTree.ChildNodes[i];
    if Node.Typ = ntType then
    begin
      Table := TDBTable.Create(SyntaxTree.GetAttribute(anName));
      FTables.Add(Table);
      for j := Low(Node.ChildNodes) to High(Node.ChildNodes) do
      begin
        FieldNode := Node.ChildNodes[J];
        if FieldNode.Typ = ntField then
        begin
          if GetFieldNodeInfo(FieldNode,FieldName,TypeName,ControlString) then
          begin
            Table.AddField(FieldName,TypeName,ControlString);
          end;
        end;
      end;
    end;
  end;

  //处理所有的Table定义
  for i := 0 to FTables.Count - 1 do
  begin
    FTables[i].SaveToDir(FTargetDir);
  end;
end;



end.
