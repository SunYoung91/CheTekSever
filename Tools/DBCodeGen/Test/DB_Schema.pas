unit DB_Schema;

interface
uses AttrType;

Type
  Actor = record
    ActorID :UInt64;  //#[uid,pkey] comment : 角色ID
    Account :string; //#[pkey(20),queryand(1)] comment:所属账号
    ActorName : string;  //#[uni,queryand(1)] comment:角色名
    ServerID:Word;  //#comment : 服务器ID
    OrginalServerID:Word; //#comment : 原始服务器ID
    Attr : TAttrs; // #[json]
  end;

implementation

end.
