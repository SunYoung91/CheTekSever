unit HandleProtocol;
interface
uses Login_S2C,System.Classes ; 
//INTERFACE
procedure Handle_SC2_ActorList_1(ByteArray:TCheTekByteArray);
//INTERFACE
implementation
//IMPL
procedure Handle_SC2_ActorList_1(ByteArray:TCheTekByteArray);
var
data : SC2_ActorList_1
begin
  data.DeserializeFrom(ByteArray);

end
//IMPL
end.
