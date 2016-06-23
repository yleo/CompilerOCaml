program HelloWorld(output);
var y : integer;
function access(var a:integer):integer;
begin
   a:=12;
   access:=0
end;
begin
   y:=0;
   writeln(access(y));
   writeln(y)
end.
