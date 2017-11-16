/* This tests for a dangling else */

fun foo (i:int, j:int, foo:int[1024])
   temp:int;
  begin
   if i > j then 
    begin 
    temp := foo[i];
    foo[i] := foo[j];
    foo[j] := temp
/*    end */
  else 
    print("Hmm, we have a problem\n")
 end



        
