unit Utils;

interface


function _WD(Width:Integer):string;
function _Q(Count : Integer):String;

implementation
const LowPreFix = 1;
const HighPreFix = 4;
const LowQuote = 1;
const HighQute = 4;

const  WidthPreFix : Array[LowPreFix..HighPreFix] of string = (' ', '  ', '   ', '    ');
const  Quote: array[LowQuote..HighQute] of String = (#39,#39#39,#39#39#39,#39#39#39#39) ;


function _WD(Width:Integer):string;
begin
   Result := '';
   if (Width >= LowPreFix) and (Width <= HighPreFix) then
   begin
     Result := WidthPreFix[Width];
   end;
end;

function _Q(Count : Integer):String;
begin
   Result := '';
   if (Count >= LowQuote) and (Count <= HighQute) then
   begin
     Result := Quote[Count];
   end;
end;
end.
