unit GenCode;

interface
uses
  StringPool,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,DelphiAst.Consts,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,System.SysUtils;
type
    TCodeGen = class
    private
      FSourceFile:String;
      FTargetDir:String;
    public
        constructor Create(const SourceFile,TargetDir:String);
        procedure BuildTargetFile(SyntaxTree: TSyntaxNode);
        function FindInterfaceNode(SyntaxTree: TSyntaxNode):TSyntaxNode;
		procedure ProcessTypeDecl(SyntaxTree: TSyntaxNode);
        procedure Gen();
    end;
implementation

{ TCodeGen }

procedure TCodeGen.BuildTargetFile(SyntaxTree: TSyntaxNode);
var
  InterfaceNode , ChildNode : TSyntaxNode;
begin
  InterfaceNode := FindInterfaceNode(SyntaxTree);

  if InterfaceNode <> nil then
  begin
	
  
  end
end;

constructor TCodeGen.Create(const SourceFile, TargetDir: String);
begin
    FSourceFile := SourceFile;
    FTargetDir := TargetDir;
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
  memused: Cardinal;
  StringPool: TStringPool;
  OnHandleString: TStringEvent;
  XML:String;
  HasException : Boolean;
begin

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

end.
