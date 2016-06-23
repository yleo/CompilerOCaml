program HelloWorld(output);
var t : array[0..10000] of integer;
var i : integer;   
begin
   for i:=0 to 10000 do
      t[i]:=i;
   for i:=0 to 10000 do
      writeln(t[i]);
end.