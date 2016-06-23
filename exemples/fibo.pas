program HelloWorld(output);
function fibo(i:integer):integer;
begin
   if i>1 then
   begin
      fibo := fibo(i-2)+ fibo(i-1);
   end;
   if i=0 then
      fibo:=0;
   if i=1 then
      fibo :=1;
end;
begin
   writeln(fibo(8));
end.
