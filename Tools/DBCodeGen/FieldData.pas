unit FieldData;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,Utils;
type
    TGroupData = record
      GroupID:Integer;
      Limit1:Boolean;
    end;

    TFieldData = class
    private
      FFieldName:String;
      FDelphiType:String;
      FMysqlOps : TMysqlOPSets;
      FLength:Integer; //�ֶγ��� ���� Text �������Ļ�����ָ������ �����Ļ��ͻ���varchar
      FComment:String;//ע��
      FQueryAndGroup:TList<TGroupData>; //���ɲ�ѯ����ID And ����
      FQueryOrGroup:TList<TGroupData>; //���ɲ�ѯ����ID Query ����
    public
      constructor Create();
      destructor Destroy();override;
      procedure AddQueryAndGroup(GroupID:Integer ; limit1:Boolean);
      procedure AddQueryOrGroup(GroupID:Integer ; limit1 :Boolean);
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
      property QueryAndGroup:TList<TGroupData> read FQueryAndGroup;
      property QueryOrGroup:TList<TGroupData> read FQueryOrGroup;
    end;
implementation

{ TFieldData }

procedure TFieldData.AddQueryAndGroup(GroupID:Integer ; limit1:Boolean);
var
  Group : TGroupData;
begin
  for Group in FQueryAndGroup do
  begin
    if Group.GroupID = GroupID  then
    begin
      Exit
    end;
  end;
  Group.Limit1 := False;
  Group.GroupID := GroupID;
  Group.Limit1 := limit1;
  FQueryAndGroup.Add(Group);
end;

procedure TFieldData.AddQueryOrGroup(GroupID:Integer ; limit1:Boolean);
var
  Group : TGroupData;
begin
  for Group  in FQueryOrGroup do
  begin
    if Group.GroupID = GroupID  then
    begin
      Exit
    end;
  end;

  Group.Limit1 := False;
  Group.GroupID := GroupID;
  Group.Limit1 := limit1;
  FQueryOrGroup.Add(Group);
end;

function TFieldData.CodeOfValueToString: String;
var
  OP:TFieldCodeOp;
begin
  OP := GetDelphiTypeOP(Self.FDelphiType);
  case OP of
    fcOpNone: Result :=  'MysqlQuote(' + Self.FFieldName + ')' ;
    fcOpIntToStr: Result := 'IntToStr(' + Self.FFieldName + ')';
    fcOpBoolean : Result := 'MysqlBoolean(' + Self.FFieldName + ')';
    fcOpDouble  : Result := 'FloatToStr(' + Self.FFieldName + ')';
    fcOpToMysqlTime :Result := 'MysqlQuote(FormatDateTime(' + _Q(1) + 'YYYY-MM-DD HH:NN:SS' + _Q(1) + ', Self.' + Self.FFieldName + '));'
  end;

  if (mopAsJson in  Self.FMysqlOps) or (mopJsonClass in Self.FMysqlOps) then
  begin
    Result := Format('MysqlQuote(_DBJsonFromRtti(@%s,TypeInfo(%s)))',[FFieldName,FDelphiType]);
  end;
end;

constructor TFieldData.Create;
begin
  FQueryAndGroup := TList<TGroupData>.Create;
  FQueryOrGroup := TList<TGroupData>.Create;
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
        MysqlType := 'json'; //û�к��ʵ����ͽ������Ϊjson
      end;

      if ((mopPrimaryKey in FMysqlOps) or (mopUnikey in FMysqlOps)) and (LowerCase(FDelphiType) = 'string') then
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
var
 Op : TFieldCodeOp;
begin
  Op := GetDelphiTypeOP(FDelphiType);
  case Op of
    fcOpNone: Result := True ;
    fcOpIntToStr: Result := False ;
    fcOpBoolean: Result := False ;
    fcOpDouble: Result := False ;
    fcOpToMysqlTime : Result := True ;
  end;
  Result := false;
end;

function TFieldData.IsUIDField: Boolean;
begin
  Result := mopUID in FMysqlOps;
end;


end.
