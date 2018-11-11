unit CheTek.ByteArray;
//����AS3��ByteArray ʵ�� ����  2017-12-23
interface

uses
  Classes, SysUtils, Windows;

type
  UInt = NativeUInt;
  Int = NativeInt;
  TCheTekByteArray = class
  protected
    FSize, FCapacity, FIncrement: UInt;
    FMemory: Pointer;
    FPosition:NativeUInt;
    procedure EnsureCapacity(NewSize: UInt);
    function GetLeftSize:UInt;
    function GetOffSetPointer : Pointer;
    function GetBytesAvailable():NativeUInt;
  public
    constructor Create(InitialCapacity: UInt = 8192; Increment: UInt = 8192);
    destructor Destroy; override;

    procedure Assign(source:TCheTekByteArray);
    procedure Write(Buf: PByte; Size: NativeUInt); inline;
    procedure WriteByteArray(ByteArray:TCheTekByteArray) ;
    procedure AppendBuffer(ASource: TCheTekByteArray); inline;
    function Read(Buf: Pointer; Size: UInt): UInt;
    function Delete(Size: UInt): UInt; //ɾ��ͷ������ ���ƶ�����
    procedure Clear; inline;
    procedure WriteString(value:String); //д��һ���ַ��� ��Ϊuf8����ģʽ ��Ԥ��д��2���ֽ���Ϊstring ���� �������65535 ��д��մ�
    procedure WriteByteData(value:TBytes); //д��TBytes ���͵�����
    procedure WriteByte(value:Byte);
    procedure WriteWord(value:word);
    procedure WriteInteger(value:Integer);
    procedure WriteInt64(value:Int64);
    procedure WriteUInt64(value:Int64);
    procedure WriteCardinal(value:Cardinal);
    procedure WriteBoolean(value:Boolean);

    function WriteStream(Source:TStream;ToCount:NativeUInt = 0):NativeUInt;

    //==============================��ȡ���ֵķ���========================
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadString():String;
    function ReadBoolean():Boolean;
    function WriteToStream(Source:TStream;ToCount:NativeUInt = 0):NativeUInt;

    procedure SaveToFile(const FileName:String);

    //�ض����ݳ���Ϊ0
    procedure Trunc();
    procedure SizeAdd(Value:UInt); //�ⲿ�ֶ���������size ͨ������ʹ��OffsetMemory ��ȡԭʼ��ַ�� move���ڴ���������ӵ�
    property Size: UInt read FSize;
    property Memory: Pointer read FMemory;
    property Position : NativeUInt read FPosition write FPosition;
    property LeftSize:UInt read GetLeftSize; //�ڲ��������ݵĵ�ǰ��ʣ����ٴ�С
    property OffsetMemory:Pointer read GetOffSetPointer;
    property BytesAvailable:NativeUInt read GetBytesAvailable; //��ȡ��ǰλ�õ���β��ʣ�����ֽڿ��Խ��ж�ȡ
  end;

implementation

function Min(const A, B: UInt): UInt; inline;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

constructor TCheTekByteArray.Create(InitialCapacity, Increment: UInt);
begin
  inherited Create;
  GetMem(FMemory, InitialCapacity);
  if FMemory = Nil then
    raise EOutOfMemory.Create('TCheTekByteArray.Create');
  FCapacity := InitialCapacity;
  FIncrement := Increment;
  FSize := 0;
  FPosition := 0;
end;

destructor TCheTekByteArray.Destroy;
begin
  FreeMem(FMemory);
  inherited Destroy;
end;

procedure TCheTekByteArray.EnsureCapacity(NewSize: UInt);
var
  NewCapacity: UInt;
begin
  if NewSize > FCapacity then
  begin
    if NewSize mod FIncrement = 0 then
      NewCapacity := NewSize
    else
      NewCapacity := FIncrement * ((NewSize div FIncrement) + 1);

    Assert(NewCapacity >= NewSize);
    ReallocMem(FMemory, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TCheTekByteArray.Write(Buf: PByte; Size: NativeUInt);
begin
  if FCapacity - FPosition < Size then
    EnsureCapacity(FPosition + Size);

  Move(Buf^, PByte(NativeUInt(FMemory) + FPosition)^, Size);
  Inc(FPosition,Size);
  if FPosition > FSize then
    FSize := FPosition;
end;

procedure TCheTekByteArray.AppendBuffer(ASource: TCheTekByteArray);
begin
  if ASource.FSize > 0 then
    Write(ASource.FMemory, ASource.FSize);
end;

function TCheTekByteArray.Read(Buf: Pointer; Size: UInt): UInt;
var
  ReadSize:UInt;
begin
  Result := 0;
  ReadSize := GetBytesAvailable();
  if ReadSize > 0 then
  begin
    Result := Min(Size, ReadSize);
    Move(PByte(NativeUInt(FMemory) + FPosition)^,Buf^, Result);
    Inc(FPosition,Result);
  end;
end;

function TCheTekByteArray.GetBytesAvailable: NativeUInt;
begin
  Result := FSize - FPosition;
end;

function TCheTekByteArray.GetLeftSize: UInt;
begin
  Result := FCapacity - FSize ;
end;

function TCheTekByteArray.GetOffSetPointer: Pointer;
begin
 Result := Pointer(UInt(FMemory) + FSize);
end;

function TCheTekByteArray.Delete(Size: UInt): UInt;
var
  DeleteSize:UInt;
begin
  DeleteSize := Min(Size, FSize);

  //����ȥ�Ĵ�С���� �ܴ�С�Ǿ���Ҫ�ƶ��ڴ档���￼����һ����ʼƫ��  ����С���ڴ��Ƴ���һֱ�ƶ��ڴ�
  if FSize <> DeleteSize then
    Move(PByte(UInt(FMemory) + DeleteSize)^, FMemory^, FSize - DeleteSize);

  Dec(FSize, DeleteSize);
  Dec(FPosition,DeleteSize);
  Result := DeleteSize;
end;

procedure TCheTekByteArray.Assign(source: TCheTekByteArray);
begin
  Self.EnsureCapacity(Source.FSize);
  Self.FSize := Source.FSize;
  Self.FCapacity := source.FCapacity;
  Self.FIncrement := Source.FIncrement;
  Self.FPosition := Source.FPosition;
end;

procedure TCheTekByteArray.Clear;
begin
  Read(nil, Size);
end;

function TCheTekByteArray.ReadInteger: Integer;
begin
  Read(@Result, Sizeof(Integer));
end;

function TCheTekByteArray.ReadString: String;
var
  BytesLen:Word;
  Bytes:TBytes;
begin
  BytesLen := ReadWord();
  if BytesLen > 0 then
  begin
    SetLength(Bytes,BytesLen);
    Read(Bytes,BytesLen);
    Result := TEncoding.UTF8.GetString(Bytes);
  end else
  begin
    Result := '';
  end;
end;

function TCheTekByteArray.ReadUInt64: UInt64;
begin
  Read(@Result, Sizeof(UInt64));
end;

function TCheTekByteArray.ReadInt64: Int64;
begin
  Read(@Result, Sizeof(Int64));
end;

function TCheTekByteArray.ReadBoolean: Boolean;
begin
  Read(@Result, Sizeof(Boolean));
end;

function TCheTekByteArray.ReadByte: Byte;
begin
  Read(@Result, Sizeof(Result));
end;

function TCheTekByteArray.ReadCardinal: Cardinal;
begin
  Read(@Result, Sizeof(Result));
end;

function TCheTekByteArray.ReadWord: Word;
begin
  Read(@Result, Sizeof(Result));
end;

procedure TCheTekByteArray.SaveToFile(const FileName: String);
var
  FileStream : TFileStream;
begin
  FileStream := TFileStream.Create(FileName,fmCreate);
  WriteToStream(FileStream);
  FileStream.Free;
end;

procedure TCheTekByteArray.SizeAdd(Value: UInt);
begin
  if FSize + Value > FCapacity then
  begin
    raise Exception.Create('TCheTekByteArray.SizeAdd Ԥ�����ӵĴ�С�����˻������ܴ�С');
  end else
  begin
    FSize := FSize + Value;
  end;
end;

procedure TCheTekByteArray.Trunc;
begin
  FSize := 0;
  FPosition := 0;
end;

procedure TCheTekByteArray.WriteBoolean(value: Boolean);
begin
  Write(@value,1);
end;

procedure TCheTekByteArray.WriteByte(value: Byte);
begin
  Write(@value,1);
end;

procedure TCheTekByteArray.WriteByteArray(ByteArray: TCheTekByteArray);
var
  NewSize:NativeUInt;
  Ptr:PByte;
begin
  NewSize := (ByteArray.Size - ByteArray.Position) + (Self.Size - Self.Position);
  EnsureCapacity(NewSize);
  Ptr := PByte(NativeUInt(ByteArray.FMemory) + ByteArray.Position) ;
  Self.Write(Ptr,ByteArray.Size - ByteArray.Position);
end;

procedure TCheTekByteArray.WriteByteData(value: TBytes);
begin
  Write(@value[0],Length(value));
end;

procedure TCheTekByteArray.WriteCardinal(value: Cardinal);
begin
  Write(@value,4);
end;

procedure TCheTekByteArray.WriteInteger(value: Integer);
begin
  Write(@value,4);
end;

procedure TCheTekByteArray.WriteInt64(value: Int64);
begin
  Write(@value,8);
end;

procedure TCheTekByteArray.WriteWord(value: word);
begin
  Write(@value,2);
end;

function TCheTekByteArray.WriteStream(Source:TStream;ToCount:NativeUInt = 0):NativeUInt;
const
  MaxBufSize = $F000;
var
  BufSize, N: NativeUInt;
  Buffer: TBytes;
begin
  if ToCount <= 0 then
  begin
    Source.Position := 0;
    ToCount := Source.Size;
  end;
  Result := ToCount;
  if ToCount > MaxBufSize then BufSize := MaxBufSize else BufSize := ToCount;
  SetLength(Buffer, BufSize);
  try
    while ToCount <> 0 do
    begin
      if ToCount > BufSize then N := BufSize else N := ToCount;
      Source.ReadBuffer(Buffer, N);
      Write(@Buffer[0],N);
      Dec(ToCount, N);
    end;
  finally
    SetLength(Buffer, 0);
  end;
end;


function TCheTekByteArray.WriteToStream(Source:TStream;ToCount:NativeUInt = 0):NativeUInt;
var
  Ptr : PByte;
begin

  if ToCount <= 0 then
  begin
    Self.Position := 0;
    ToCount := Self.Size;
  end;

  Result := ToCount;
  Ptr := PByte(NativeUint(Self.FMemory) +  Self.Position);
  Source.Write(Ptr^,ToCount);
end;

procedure TCheTekByteArray.WriteUInt64(value: Int64);
begin
  Write(@value,8);
end;

procedure TCheTekByteArray.WriteString(value: String);
var
  Bytes : TBytes;
  BytesLength: NativeUInt;
begin
  if value = '' then
  begin
    WriteWord(0);
  end else
  begin
    Bytes := TEncoding.UTF8.GetBytes(value);
    BytesLength := Length(Bytes);
    if BytesLength < $FFFF then
    begin
      WriteWord(BytesLength);
      WriteByteData(Bytes);
    end else
    begin
      WriteWord(0); //���ȳ���ֱ��д0
    end;
  end;
end;
end.
