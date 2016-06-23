program HelloWorld(output);
var t : array[0..10] of array[0..9] of integer;
var t2 : array[0..10] of array[0..9] of array[0..22] of integer;   
function access(tab : array of array of  integer;i:integer; j:integer):integer;
begin
   access:=tab[i][j]
end;
var i : integer;
var j : integer;
var k : integer;
begin
   for i:=0 to 10 do
   for j:=0 to 9 do
   begin
      t[i][j]:=i+j;
   end;
   for i:=0 to 10 do
   for j:=0 to 9 do
   for k:=0 to 22 do   
   begin
      t2[i][j][k]:=i*j*k;
   end;
   writeln("dix plus neuf");
   writeln(t[10][9]);
   writeln("dix fois neuf fois vingt");
   writeln(t2[10][9][20]);
   writeln("dix plus cinq");
   writeln(access(t,10,5));
   writeln("deux fois cinq fois cinq");
   writeln(access(t2[2],5,5));
   writeln("indefini");
   writeln(t[10][1000]);
end.
