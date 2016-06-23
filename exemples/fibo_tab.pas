program HelloWorld(output);
var t : array[0..20] of integer;
function fibo(tab: array of integer;i:integer):integer;
begin 
   if tab[i] <> (0-1) then
   begin
      fibo:=tab[i]
   end
   else
   begin
      if i>1 then
      begin
	 tab[i]:= fibo(tab,i-2)+fibo(tab,i-1);
      end;
      fibo:=tab[i];
   end;
   if i=0 then
   begin
      tab[0]:= 0;
      fibo:=0;
   end;
   if i=1 then
      begin
	 tab[1]:= 1;
	 fibo :=1;
      end;
end;
var i : integer;
begin
   for i:=0 to 20 do
   begin
      t[i] := 0-1;
   end;
   writeln(fibo(t,20));
   for i:=0 to 20 do
   begin
      writeln(t[i]);
   end;
end.
