program HelloWorld(output);
function factoriel(i:integer):integer;
begin
   if i=0 then
      factoriel:=1
else
   factoriel:=i*factoriel(i-1)
end;
begin
  writeln(factoriel(8))
end.
