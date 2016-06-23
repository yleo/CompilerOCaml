program HelloWorld(output);
function f(x : integer):integer;
begin
   if x>0 then
   begin
      writeln("fest appele avec x qui vaut :");
      writeln(x);
      f:=f(x-1);
   end;
   f:=0;
end;
begin
   writeln(f(15));
end.