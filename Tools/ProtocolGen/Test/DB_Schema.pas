unit DB_Schema;

interface
uses AttrType;

Type
  //角色表
  Actor = record
    //uses Classes;  //引用的单元文件
    ActorID :UInt64;  //#[uid,pkey,queryor(2)] comment : 角色ID
    Account :string; //#[pkey(20),queryand(1),queryor(2)] comment:所属账号
    ActorName : string;  //#[uni,queryand(1)] comment:角色名
    ServerID:Word;  //#comment : 服务器ID
    OrginalServerID:Word; //#comment : 原始服务器ID
  end;

  //账号表
  Account = record
    //uses Classes;  //引用的单元文件
    Account :string; //#[pkey(20),uid] comment:所属账号
    Password:string; //#comment:密码
    ChannelID:Integer; //#comment:渠道ID
    ChannelName:String; //#comment:渠道名称
    RegisterTime:TDateTime; //#comment:注册时间
    MobilePhoneNumber:String; //#comment:手机号码
    EMail:string;//#comment:电子邮箱地址
    IdentificationNumber:String; //#comment:身份证号码
    Question1:String; //#comment:问题1
    Question2:String;//#comment:问题2
    Answer1:String;//#comment:回答1
    Answer2:String; //#comment:回答2
    Recharge:Int64; //#comment:累计充值
  end;


  //物品表
  Item = record
    SerialNumber:UInt64;//#[pkey,uid]序列号
    ActorID:UInt64; //#[queryand(1)]comment : 所属角色ID
    ItemName:String; //#comment :物品名称
    ConfigID:Integer; //#comment :配置的物品ID
    Attrs:TAttrs; // #[jsonclass]   comment : 物品属性
    ExpireTime:TDateTime; // #comment : 物品属性 过期时间
  end;

implementation

end.
