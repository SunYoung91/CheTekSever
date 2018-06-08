unit TableCodeGen;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils;



type

    TFieldData = class
    private
      FFieldName:String;
      FDelphiType:String;
      FMysqlOps : TMysqlOPSets;
    public
      constructor Create(const FiledName,DelphiType:string ; MysqlOps : TMysqlOPSets );
      function GetDelphiFiledString():String;
      function IsNeedQuote():Boolean; //需不需要用单引号 引起来
      function CodeOfValueToString():String;
    end;

    TDBTable = class
    private
      FTableName:String;
      FFields : TObjectList<TFieldData>;
      procedure AddField(const FieldName,DelphiType:string;MysqlOps: TMysqlOPSets);overload;
      function GetClassName():String;
      function GenInsertProcCodeText():String;
    public
      constructor Create(const TableName:String);
      procedure AddField(const FieldName,DelphiType,ControlString:string);overload;
      Procedure SaveToDir(Const Path:String);
      destructor Destroy;override;
    end;
implementation

const LowPreFix = 1;
const HighPreFix = 4;
const  WidthPreFix : Array[LowPreFix..HighPreFix] of string = (' ', '  ', '   ', '    ');
var Templelate:TStringList;

function _WD(Width:Integer):string;
var
  I : Integer;
begin
   Result := '';
   if (Width >= LowPreFix) and (Width <= HighPreFix) then
   begin
     Result := WidthPreFix[Width];
   end;
end;
{ TDBTable }

procedure TDBTable.AddField(const FieldName, DelphiType: string;
  MysqlOps: TMysqlOPSets);
var
  FieldData:TFieldData;
begin
  FieldData := TFieldData.Create(FieldName,DelphiType,MysqlOps);
  FFields.Add(FieldData);
end;

procedure TDBTable.AddField(const FieldName, DelphiType, ControlString: string);
var
  ControlList:TStringList;
  CText:String;
  I:Integer;
  MysqlOps: TMysqlOPSets;
  OP:TMysqlOP;
begin
  CText := StrBetween(ControlString,'[',']');
  ControlList := TStringList.Create;
  ControlList.Delimiter := ',';
  ControlList.DelimitedText  := CText;

  MysqlOps := [];
  for i := 0 to ControlList.Count - 1 do
  begin
    OP := GetMysqlOP(Trim(ControlList[i]));
    MysqlOps := MysqlOps + [OP];
  end;
  ControlList.Free;

  AddField(FieldName,DelphiType,MysqlOps);
end;

constructor TDBTable.Create(const TableName: String);
begin
  FTableName := TableName;
  FFields := TObjectList<TFieldData>.Create;
end;

destructor TDBTable.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TDBTable.GenInsertProcCodeText: String;
var
  Fileds,Values,FormatControl,InsertSql:String;
  FieldData : TFieldData;
  IsFirst : Boolean;
  SourceText:TStringList;
  I:Integer;
begin
  SourceText := TStringList.Create;
  SourceText.Add('var');
  SourceText.Add(_WD(2) + 'Sql:String;');
  SourceText.Add(_WD(2) + Format('V : Array[1..%d] of String;',[FFields.Count]));
  SourceText.Add('begin');

  IsFirst := True;
  I := 1;
  for FieldData in FFields do
  begin
    if IsFirst then
    begin
      Fileds := Fileds +  FieldData.FFieldName ;
      if FieldData.IsNeedQuote() then
         Values := Values + '''%s'''
      else
         Values := Values + '%s';
    end
    else
    begin
      Fileds := Fileds + ',' + FieldData.FFieldName ;
      if FieldData.IsNeedQuote() then
         Values := Values + ''',%s'''
      else
         Values := Values + ',%s';
    end;

    SourceText.Add(_WD(2) +  Format('V[%d] := %s; // %s',[i,FieldData.CodeOfValueToString(),FieldData.FFieldName]));

    if IsFirst then
       FormatControl := FormatControl + Format( ' V[%d]',[i])
    else
       FormatControl := FormatControl + Format( ', V[%d]',[i]);

    IsFirst := False;
    I := I + 1;
  end;

  InsertSql := Format('INSERT INTO %s (%s) VALUES (%s);',[FTableName,Fileds,Values]);


  SourceText.Add(_WD(2) + Format('Sql := format(''%s'',[%s]);',[InsertSql,FormatControl]));
  SourceText.Add('end;');
  Result :=  SourceText.Text;
  SourceText.Free;
end;

function TDBTable.GetClassName: String;
begin
  Result := 'TTableOf' + FTableName;
end;

procedure TDBTable.SaveToDir(const Path: String);
var
  Source : TStringList;
  Define:TStringList;
  implement:TStringList;
  FieldData : TFieldData;
  IsFirst:Boolean;

  InsertSql ,UpdateSql : String;

  function GetInsertProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'procedure Insert();'
    else
      Result := 'procedure ' + GetClassName() + '.Insert();'
  end;


  function GetUpdateProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'procedure Update();'
    else
      Result := 'procedure ' + GetClassName() + '.Update();'
  end;


  function GetFileName():string;
  begin
    Result := 'DB_' + FTableName;
  end;


begin
  Source := TStringList.Create;
  Source.Assign(Templelate);
  Source[0] := 'unit ' + GetFileName() + ';';

  //类定义头开始
  Define := TStringlist.Create;
  Define.Add('Type');
  Define.Add(GetClassName() + ' = class');


  for FieldData in FFields do
  begin
    Define.Add(FieldData.GetDelphiFiledString());
  end;

  Define.Add(GetInsertProcName(true));
  Define.Add(GetUpdateProcName(true));

  //类定义结束
  Define.Add('end;');
  Source.Text := StringReplace(Source.Text,'//#Type',Define.Text,[rfReplaceAll]);
  Define.Free;

  //功能实现开始
  implement := TStringList.Create;

  //插入函数 Insert
  implement.Add(GetInsertProcName(false));
  implement.Append(GenInsertProcCodeText());


  //更新函数 Update
  implement.Add(GetUpdateProcName(false));
  implement.Append(GenInsertProcCodeText());


  Source.Text := StringReplace(Source.Text,'//#implementation',implement.Text,[rfReplaceAll]);
  implement.Free;

  Source.SaveToFile(Path + '\' + GetFileName()+ '.pas');
  Source.Free;
end;

{ TFieldData }

function TFieldData.CodeOfValueToString: String;
var
  OP:TFieldCodeOp;
begin
  OP := GetDelphiTypeOP(Self.FDelphiType);
  case OP of
    fcOpNone: Result := Self.FFieldName ;
    fcOpIntToStr: Result := 'IntToStr(' + Self.FFieldName + ')';
  end;

  if AsJson in  Self.FMysqlOps  then
  begin
    Result := Format('RecordToJson<%s>(%s)',[FDelphiType,FFieldName]);
  end;
end;

constructor TFieldData.Create(const FiledName, DelphiType: string;
  MysqlOps: TMysqlOPSets);
begin
  FFieldName := FiledName;
  FDelphiType := DelphiType;
  FMysqlOps :=  MysqlOps;
end;

function TFieldData.GetDelphiFiledString: String;
begin
  Result := FFieldName + ':' + FDelphiType + ';'
end;

function TFieldData.IsNeedQuote: Boolean;
begin
  Result := false;
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
