unit ProtocolHandleTemplelate;

interface
uses System.Classes,System.SysUtils,SocketManager,CheTek.ByteArray{uses};

procedure HandleSocket(Socket:TSocketData;ByteArray:TCheTekByteArray;BodySize:Integer);
implementation
type
THandleProc = procedure (Socket:TSocketData;ByteArray:TCheTekByteArray);
var
  ProcArray : array[Word] of THandleProc;

procedure HandleSocket(Socket:TSocketData;ByteArray:TCheTekByteArray;BodySize:Integer);
var
  ID:Word;
  OldPosition : NativeUInt;
  NowPosition : NativeUInt;
  ReadSize : NativeInt;
begin
  OldPosition := ByteArray.Position;
  ID := ByteArray.ReadWord();
  if Assigned(ProcArray[ID]) then
  begin
    ProcArray[ID](Socket,ByteArray);
  end else
  begin
    //
  end;

  NowPosition := ByteArray.Position;

  ReadSize := NowPosition - OldPosition;

  //如果位置不正确 那么要看看是不是出现了问题。
  if ReadSize <> BodySize  then
  begin
    //如果读取的大小
    if ReadSize > BodySize then
    begin

    end else
    begin

    end;
  end;

end;

initialization

{$PROTOCOLREGISTER}

end.
