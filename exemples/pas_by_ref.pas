program HelloWorld(output);
var x : integer = 99999;
function sideeffect(var i: integer):integer;
begin
   i:=11111;
    sideeffect :=1;
end;
begin
   writeln("Au debut x vaut :");
   writeln(x);
   writeln("On appelle la fonction sideeffect qui vaut :");
   writeln(sideeffect(x));
   writeln("Maintenant, x vaut :");
   writeln(x);
end.