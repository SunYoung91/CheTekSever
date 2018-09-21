unit CheTek.ThreadByteArray;
//类似AS3的ByteArray 实现 随云  2017-12-23
interface

uses
  Classes, SysUtils, Windows,CheTek.ByteArray,uSyncObj;

type
  UInt = NativeUInt;
  Int = NativeInt;
  TCheTekThreadByteArray = class(TCheTekByteArray)
  protected
	FLock: TFixedCriticalSection;
  public
    constructor Create(InitialCapacity: UInt = 8192; Increment: UInt = 8192);
    destructor Destroy; override;
	procedure Lock; inline;
    procedure Unlock; inline;
    function TryLock():Boolean;
  end;

implementation

constructor TCheTekThreadByteArray.Create(InitialCapacity, Increment: UInt);
begin
  inherited Create;
  FLock := TFixedCriticalSection.Create;
end;

destructor TCheTekThreadByteArray.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TCheTekThreadByteArray.TryLock: Boolean;
begin
  Result := FLock.TryEnter();
end;

procedure TCheTekThreadByteArray.Lock;
begin
  FLock.Enter;
end;

procedure TCheTekThreadByteArray.Unlock;
begin
  FLock.Leave;
end;

end.
