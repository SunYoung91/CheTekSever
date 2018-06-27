unit FieldData;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,Utils;
type
    TFieldData = class
    private
      FFieldName:String;
      FDelphiType:String;
      FMysqlOps : TMysqlOPSets;
      FLength:Integer; //�ֶγ��� ���� Text �������Ļ�����ָ������ �����Ļ��ͻ���varchar
      FComment:String;//ע��
      FQueryAndGroup:TList<Integer>; //���ɲ�ѯ����ID And ����
      FQueryOrGroup:TList<Integer>; //���ɲ�ѯ����ID Query ����
    public
      constructor Create();
      destructor Destroy();override;
      procedure AddQueryAndGroup(GroupID:Integer);
      procedure AddQueryOrGroup(GroupID:Integer);
      function GetDelphiFiledString():String;
      function IsNeedQuote():Boolean; //�費��Ҫ�õ����� ������
      function CodeOfValueToString():String;
      function IsUIDField():Boolean;
      function GetUPDATEFieldCodeText():string;
      function GetMysqlCloumnDesc():String;
      function GetToJsonCodeText():String;
      function GetFromJsonCodeText():String;
      property FieldName:String read FFieldName write FFieldName;
      property MysqlOPs:TMysqlOPSets read FMysqlOps write FMysqlOps;
      property Length:Integer read FLength write FLength;
      property DelphiType :String read FDelphiType write FDelphiType;
      property Comment :String read FComment write FComment;
      property QueryAndGroup:TList<Integer> read FQueryAndGroup;
      property QueryOrGroup:TList<Integer> read FQueryOrGroup;
    end;
implementation

{ TFieldData }

procedure TFieldData.AddQueryAndGroup(GroupID: Integer);
var
  ID:Integer;
begin
  for ID  in FQueryAndGroup do
  begin
    if ID = GroupID  then
    begin
      Exit
    end;
  end;

  FQueryAndGroup.Add(GroupID);

end;

procedure TFieldData.AddQueryOrGroup(GroupID: Integer);
var
  ID:Integer;
begin
  for ID  in FQueryOrGroup do
  begin
    if ID = GroupID  then
    begin
      Exit
    end;
  end;
  FQueryOrGroup.Add(GroupID);
end;

function TFieldData.CodeOfValueToString: String;
var
  OP:TFieldCodeOp;
begin
  OP := GetDelphiTypeOP(Self.FDelphiType);
  case OP of
    fcOpNone: Result :=  'Self._Q(' + Self.FFieldName + ')' ;
    fcOpIntToStr: Result := 'IntToStr(' + Self.FFieldName + ')';
    fcOpBoolean : Result := 'MysqlBoolean(' + Self.FFieldName + ')';
    fcOpDouble  : Result := 'FloatToStr(' + Self.FFieldName + ')';
    fcOpToMysqlTime :Result := 'FormatDateTime(' + _Q(1) + 'YYYY-MM-DD HH:NN:SS' + _Q(1) + ', Self.' + Self.FFieldName + ');'
  end;

  if mopAsJson in  Self.FMysqlOps  then
  begin
    Result := Format('Self._Q(TSerialObject.JsonFromRtti(@%s,TypeInfo(%s)))',[FFieldName,FDelphiType]);
  end else if mopJsonClass in Self.FMysqlOps then
  begin
    Result := 'Self._Q(' +  FFieldName + '.ToJsonString());';
  end;
end;

constructor TFieldData.Create;
begin
  FQueryAndGroup := TList<Integer>.Create;
  FQueryOrGroup := TList<Integer>.Create;
end;

destructor TFieldData.Destroy;
begin
  FQueryAndGroup.Free;
  FQueryAndGroup.Free;
  inherited;
end;

function TFieldData.GetDelphiFiledString: String;
begin
  Result := FFieldName + ':' + FDelphiType + ';'
end;

function TFieldData.GetMysqlCloumnDesc: String;
var
  MysqlType:String;
begin
    if (mopAsJson in FMysqlOps) or (mopJsonClass in FMysqlOps) then
    begin
      MysqlType := 'json';
    end else
    begin

      MysqlType := DelphiTypeToMysql(FDelphiType);
      if MysqlType = '' then
      begin
        raise Exception.Create('Can not get mysql type , DelphiType is :  ' + FDelphiType);
      end;

      if (mopPrimaryKey in FMysqlOps) and (LowerCase(FDelphiType) = 'string') then
      begin
        if (FLength > (65535 div 4)) or (FLength < 1) then
        begin
          raise Exception.Create( format('string ��Ϊ���� ����ָ������ ����ȡֵ��ΧӦ����:[1-%d] FieldName:%s',[65535 div 4,FFieldName]) );
        end;
        MysqlType := Format('varchar(%d)',[FLength]);
      end;
    end;

    if FComment <> '' then
    begin
      Result := Format('`%s` %s comment %s,',[FFieldName,MysqlType,FComment])
    end else
    begin
      Result := Format('`%s` %s ,',[FFieldName,MysqlType])
    end;

end;

function TFieldData.GetToJsonCodeText: String;
begin

end;

function TFieldData.GetFromJsonCodeText: String;
begin

end;

function TFieldData.GetUPDATEFieldCodeText: string;
begin
  Result := _Q(1) + FFieldName + ' = ' + _Q(1) +' + ' +  CodeOfValueToString();
end;

function TFieldData.IsNeedQuote: Boolean;
begin
  Result := false;
end;

function TFieldData.IsUIDField: Boolean;
begin
  Result := mopUID in FMysqlOps;
end;


end.
