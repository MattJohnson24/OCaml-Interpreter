type stackValue = BOOL of bool | ERROR | INT of int | STRING of string | NAME of string | UNIT | CLOSURE of ((string) * (command list) * (string * stackValue) list * (string)) | CLOSURE2 of (command list)

and command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END | FUN of (string * stackValue) | RETURN | FUNEND of (stackValue)| CALL | INOUT of (string * stackValue)

let interpreter ((input : string), (output : string)) : unit = 

  (*Code from starting interpreter demo video and HW1*)
  let ic = open_in input in

  let oc = open_out output in

  let rec loop_read acc =
    try
      let l = String.trim(input_line ic) in loop_read (l::acc)
    with
      | End_of_file ->  List.rev acc in
  let ls_str = loop_read [] in

  let file_write interpreterVal = Printf.fprintf oc "%s\n" interpreterVal in

  let str2sv (s : string) = 
    if ((s.[5] <= '9' && s.[5] >= '0') || s.[5] == '-') then
      try
        INT (int_of_string(String.sub s 5 ((String.length s)-5)))
      with Failure _ -> ERROR
    else if (s.[5] == '"' && s.[String.length(s)-1] == '"') then
      STRING (String.sub s 6 ((String.length s)-7))
    else if ((s.[5] == '_' ) || ((s.[5] >= 'A' && s.[5] <= 'Z') || (s.[5] >= 'a' && s.[5] <= 'z'))) then
      NAME (String.sub s 5 ((String.length s)-5))
    else if (String.equal s "push :true:") then
      BOOL true
    else if (String.equal s "push :false:") then
      BOOL false
    else if (String.equal s "push :error:") then
      ERROR
    else if (String.equal s "push :unit:") then
      UNIT
    else ERROR

  in

  let str2com (s : string) =
    let split = String.split_on_char ' ' s in
    if (String.equal (List.nth split 0) "fun") then
      FUN ((List.nth split 1),(CLOSURE((List.nth split 2),  [],  [], "isFunctionDeclarer")))
    else if(String.equal (List.nth split 0) "inOutFun") then
      INOUT ((List.nth split 1),(CLOSURE((List.nth split 2),  [],  [], "isInOutDeclarer")))
    else
      match s with
      | "add" -> ADD
      | "sub" -> SUB
      | "mul" -> MUL
      | "div" -> DIV
      | "pop" -> POP
      | "rem" -> REM
      | "neg" -> NEG
      | "swap" -> SWAP
      | "cat" -> CAT
      | "and" -> AND
      | "or" -> OR
      | "not" -> NOT
      | "equal" -> EQUAL
      | "bind" -> BIND
      | "lessThan" -> LESSTHAN
      | "toString" -> TOSTRING
      | "println" -> PRINTLN
      | "quit" -> QUIT
      | "if" -> IF
      | "let" -> LET
      | "end" -> END
      | "return" -> RETURN
      | "call" -> CALL
      | _ -> PUSH (str2sv s)

  in
  
  let rec map (iList : string list) (testcl : command list list)=
    match (iList, testcl) with 
    | (hd::tl, tcl::tcl2) -> let split = String.split_on_char ' ' hd in let split1 = List.nth split 0 in (match split1 with 
      | "fun" -> (map tl ([]::((str2com hd)::tcl)::tcl2))
      | "inOutFun" -> (map tl ([]::((str2com hd)::tcl)::tcl2))
      | "funEnd" -> (match tcl2 with
        | hd2::tl2 -> map tl ((FUNEND(CLOSURE2(List.rev tcl))::hd2)::tl2)
        | _ -> [])
      | _ -> (map tl ((str2com hd::tcl)::tcl2)))
    | _ -> testcl
    
  in
  let commandList = map ls_str [[]] in

  let rec map2 (testcl : command list list)=
  match (testcl) with 
  | (tcl::tcl2) -> List.rev tcl::tcl2
  | _ -> testcl
  in

  let fixedCommandList = map2 commandList in

  let rec updateName (environment : ((string * stackValue) list) list) (name : stackValue list)=
  match (environment, name) with 
  | ((tcl::tcl2), n) -> (match (List.nth n 1) with
    | NAME(z) -> (match List.assoc z tcl with
      | INT(y) -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x, INT(y))::(z, INT(y))::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | STRING(y) -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x, STRING(y))::(z, STRING(y))::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | UNIT -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x, UNIT)::(z, UNIT)::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | BOOL(y) -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x, BOOL(y))::(z, BOOL(y))::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | NAME(y) -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x, NAME(y))::(z, NAME(y))::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | CLOSURE(y) -> (match tcl2 with  
        | hd1::tl1 -> (match (List.nth n 2) with
          | NAME(x) -> (((x,  CLOSURE(y))::(z,  CLOSURE(y))::hd1)::tl1)
          | _ -> environment)
        | _ -> environment)
      | _ -> environment)
    | _ -> environment)
  | _ -> environment
  in

  let rec com2str (cl : command list list) (stack : (stackValue list) list) (bindings : ((string * stackValue) list) list)=
    match (cl, stack, bindings) with
    | ((PUSH v::restOfCommands)::cc, s, m) -> (match s with
      | hd::tl -> com2str (restOfCommands::cc) ((v::hd)::tl) m
      | _ -> com2str (restOfCommands::cc) s m)
    | ((POP::restOfCommands)::cc, (v::s)::ss, m) -> com2str (restOfCommands::cc) (s::ss) m
    | ((POP::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
    | ((ADD::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> com2str (restOfCommands::cc) ((INT(a+b)::s)::ss) m
    | ((ADD::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
    | INT(c) -> com2str (restOfCommands::cc) ((INT(c+b)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
      with
      | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
    | ((ADD::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
    | INT(d) -> com2str (restOfCommands::cc) ((INT(a+d)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
      with
      | Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
    | ((ADD::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
    | INT(c) -> (match List.assoc b m with
      | INT(d) -> com2str (restOfCommands::cc) ((INT(c+d)::s)::ss) (m::mm)
      | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
    with
    | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) mm)
    | ((ADD::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
    | ((SUB::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
    | INT(c) -> com2str (restOfCommands::cc) ((INT(b-c)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
      with
      | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
    | ((SUB::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
        | INT(d) -> com2str (restOfCommands::cc) ((INT(d-a)::s)::ss) (m::mm)
        | _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
      with
      | Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
    | ((SUB::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
    | INT(c) -> (match List.assoc b m with
      | INT(d) -> com2str (restOfCommands::cc) ((INT(d-c)::s)::ss) (m::mm)
      | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
    with
    | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
    | ((SUB::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> com2str (restOfCommands::cc) ((INT(b-a)::s)::ss) m
    | ((SUB::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
    | ((MUL::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
    | INT(c) -> com2str (restOfCommands::cc) ((INT(c*b)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
  with
  | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
| ((MUL::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
    | INT(d) -> com2str (restOfCommands::cc) ((INT(a*d)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
  with
  | Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
| ((MUL::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(c) -> (match List.assoc b m with
  | INT(d) -> com2str (restOfCommands::cc) ((INT(c*d)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((MUL::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> com2str (restOfCommands::cc) ((INT(a*b)::s)::ss) m
| ((MUL::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((DIV::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(c) -> if c != 0 then com2str (restOfCommands::cc) ((INT(b/c)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
| ((DIV::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
| INT(d) -> if a != 0 then com2str (restOfCommands::cc) ((INT(d/a)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
| ((DIV::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(c) -> (match List.assoc b m with
| INT(d) -> if c != 0 then com2str (restOfCommands::cc) ((INT(d/c)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((DIV::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> if a != 0 then com2str (restOfCommands::cc) ((INT(b/a)::s)::ss) m else com2str (restOfCommands::cc) ((ERROR::INT(a)::INT(b)::s)::ss) m
| ((DIV::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m    
| ((REM::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(c) -> if c != 0 then com2str (restOfCommands::cc) ((INT(b mod c)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
| ((REM::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
| INT(d) -> if a != 0 then com2str (restOfCommands::cc) ((INT(d mod a)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
| ((REM::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(c) -> (match List.assoc b m with
| INT(d) -> if c != 0 then com2str (restOfCommands::cc) ((INT(d mod c)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((REM::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> if a != 0 then com2str (restOfCommands::cc) ((INT(b mod a)::s)::ss) m else com2str (restOfCommands::cc) ((ERROR::INT(a)::INT(b)::s)::ss) m
| ((REM::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((NEG::restOfCommands)::cc, (NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
| INT(a) -> com2str (restOfCommands::cc) ((INT(a*(-1))::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::s)::ss) (m::mm))
| ((NEG::restOfCommands)::cc, (INT(a)::s)::ss, m) -> com2str (restOfCommands::cc) ((INT(a*(-1))::s)::ss) m
| ((NEG::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((SWAP::restOfCommands)::cc, (a::b::s)::ss, m) -> com2str (restOfCommands::cc) ((b::a::s)::ss) m
| ((SWAP::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (INT(a)::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(string_of_int(a))::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (BOOL(a)::s)::ss, m) -> if a then com2str (restOfCommands::cc) ((STRING(":true:")::s)::ss) m else com2str (restOfCommands::cc) ((STRING(":false:")::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (ERROR::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(":error:")::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (UNIT::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(":unit:")::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (STRING(a)::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(a)::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, (NAME(a)::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(a)::s)::ss) m
| ((TOSTRING::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((PRINTLN::restOfCommands)::cc, (STRING(a)::s)::ss, m) -> file_write a; com2str (restOfCommands::cc) (s::ss) m
| ((PRINTLN::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) (s::ss) m
| ((CAT::restOfCommands)::cc, (NAME(a)::STRING(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| STRING(a) -> com2str (restOfCommands::cc) ((STRING(b ^ a)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::STRING(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::STRING(b)::s)::ss) (m::mm))
| ((CAT::restOfCommands)::cc, (STRING(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc b m with
| STRING(b) -> com2str (restOfCommands::cc) ((STRING(b ^ a)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::STRING(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::STRING(a)::NAME(b)::s)::ss) (m::mm))
| ((CAT::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try (match List.assoc a m with
| STRING(a) -> (match List.assoc b m with
| STRING(b) -> com2str (restOfCommands::cc) ((STRING(b ^ a)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((CAT::restOfCommands)::cc, (STRING(a)::STRING(b)::s)::ss, m) -> com2str (restOfCommands::cc) ((STRING(b ^ a)::s)::ss) m
| ((CAT::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((AND::restOfCommands)::cc, (BOOL(a)::BOOL(b)::s)::ss, m) -> if (a && b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) m else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) m
| ((AND::restOfCommands)::cc, (NAME(a)::BOOL(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
  | BOOL(c) -> if (c && b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::BOOL(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::BOOL(b)::s)::ss) (m::mm))
| ((AND::restOfCommands)::cc, (BOOL(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc b m with
  | BOOL(d) -> if (a && d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::BOOL(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::BOOL(a)::NAME(b)::s)::ss) (m::mm))
| ((AND::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
  | BOOL(c) -> (match List.assoc b m with
    | BOOL(d) -> if (c && d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((AND::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((OR::restOfCommands)::cc, (BOOL(a)::BOOL(b)::s)::ss, m) -> if (a || b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) m else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) m
| ((OR::restOfCommands)::cc, (NAME(a)::BOOL(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| BOOL(c) -> if (c || b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::BOOL(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::BOOL(b)::s)::ss) (m::mm))
| ((OR::restOfCommands)::cc, (BOOL(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc b m with
| BOOL(d) -> if (a || d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::BOOL(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::BOOL(a)::NAME(b)::s)::ss) (m::mm))
| ((OR::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| BOOL(c) -> (match List.assoc b m with
  | BOOL(d) -> if (c || d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((OR::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((NOT::restOfCommands)::cc, (BOOL(a)::s)::ss, m) -> if a then com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) m else com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) m
| ((NOT::restOfCommands)::cc, (NAME(a)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| BOOL(c) -> if c then com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::s)::ss) (m::mm))
| ((NOT::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((EQUAL::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> if (a == b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) m else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) m
| ((EQUAL::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| INT(c) -> if (c == b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
| ((EQUAL::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc b m with
| INT(d) -> if (a == d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
| ((EQUAL::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| INT(c) -> (match List.assoc b m with
  | INT(d) -> if (c == d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((EQUAL::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((LESSTHAN::restOfCommands)::cc, (INT(a)::INT(b)::s)::ss, m) -> if (a > b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) m else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) m
| ((LESSTHAN::restOfCommands)::cc, (NAME(a)::INT(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| INT(c) -> if (c > b) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::INT(b)::s)::ss) (m::mm))
| ((LESSTHAN::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc b m with
| INT(d) -> if (a > d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(a)::NAME(b)::s)::ss) (m::mm))
| ((LESSTHAN::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) ->  (try (match List.assoc a m with
| INT(c) -> (match List.assoc b m with
  | INT(d) -> if (c > d) then com2str (restOfCommands::cc) ((BOOL(true)::s)::ss) (m::mm) else com2str (restOfCommands::cc) ((BOOL(false)::s)::ss) (m::mm)
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((LESSTHAN::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((BIND::restOfCommands)::cc, (INT(a)::NAME(b)::s)::ss, m) -> (match m with
      | hd::tl -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, INT(a))::hd)::tl)
      | _ -> com2str (restOfCommands::cc) ((UNIT::s)::ss) m)
| ((BIND::restOfCommands)::cc, (STRING(a)::NAME(b)::s)::ss, m) -> (match m with
  | hd::tl -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, STRING(a))::hd)::tl)
  | _ -> com2str (restOfCommands::cc) ((UNIT::s)::ss) m)
| ((BIND::restOfCommands)::cc, (BOOL(a)::NAME(b)::s)::ss, m) -> (match m with
  | hd::tl -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, BOOL(a))::hd)::tl)
  | _ -> com2str (restOfCommands::cc) ((UNIT::s)::ss) m)
| ((BIND::restOfCommands)::cc, (NAME(a)::NAME(b)::s)::ss, m::mm) -> (try 
  (match List.assoc a m with
    | INT(a) -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, INT(a))::m)::mm)
    | STRING(a) -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, STRING(a))::m)::mm)
    | BOOL(a) -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, BOOL(a))::m)::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
  with
  | Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(a)::NAME(b)::s)::ss) (m::mm))
| ((BIND::restOfCommands)::cc, (UNIT::NAME(b)::s)::ss, m) -> (match m with
  | hd::tl -> com2str (restOfCommands::cc) ((UNIT::s)::ss) (((b, UNIT)::hd)::tl)
  | _ -> com2str (restOfCommands::cc) ((UNIT::s)::ss) m)
| ((BIND::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((QUIT::restOfCommands)::cc, ss, m) -> stack
| ((IF::restOfCommands)::cc, ((a)::(b)::BOOL(c)::s)::ss, m) -> if (c) then com2str (restOfCommands::cc) (((a)::s)::ss) m else com2str (restOfCommands::cc) (((b)::s)::ss) m
| ((IF::restOfCommands)::cc, ((a)::(b)::NAME(c)::s)::ss, m::mm) ->  (try (match List.assoc c m with
| BOOL(e) -> if (e) then com2str (restOfCommands::cc) (((a)::s)::ss) (m::mm) else com2str (restOfCommands::cc) (((b)::s)::ss) (m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::(a)::(b)::NAME(c)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::(a)::(b)::NAME(c)::s)::ss) (m::mm))
| ((IF::restOfCommands)::cc, s::ss, m) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) m
| ((LET::restOfCommands)::cc, ss, mm) -> com2str (restOfCommands::cc) ([]::ss) (match mm with
  | hd::tl -> hd::mm
  | _ -> [])
| ((END::restOfCommands)::cc, s::ss, m::mm) -> com2str (restOfCommands::cc) (match s with
  | hd::tl -> (match ss with
    | hd2::tl2 -> (hd::hd2)::tl2
    | _ -> [])
  | _ -> []) (mm)
  | ((FUN v::FUNEND x::restOfCommands)::cc, s::ss, m) -> (match v with
    | (str, sv) -> (match sv with
      | CLOSURE(str2, arr1, arr2, bo) -> (match x with
        | CLOSURE2(commands) -> (match m with
          | hd::tl -> com2str ((restOfCommands)::cc) ((UNIT::s)::ss) (((str, CLOSURE(str2, commands, hd, bo))::hd)::tl)
          | _ -> com2str (restOfCommands::cc) (s::ss) (m))
        | _ -> com2str (restOfCommands::cc) (s::ss) (m))
      | _ -> com2str (restOfCommands::cc) (s::ss) (m)))
  | ((FUNEND(v)::restOfCommands)::cc, s::ss, m::mm) -> com2str (restOfCommands::cc) (ss) (mm)
  | ((INOUT v::FUNEND x::restOfCommands)::cc, s::ss, m) -> (match v with
    | (str, sv) -> (match sv with
      | CLOSURE(str2, arr1, arr2, bo) -> (match x with
        | CLOSURE2(commands) -> (match m with
          | hd::tl -> com2str ((restOfCommands)::cc) ((UNIT::s)::ss) (((str, CLOSURE(str2, commands, hd, bo))::hd)::tl)
          | _ -> com2str (restOfCommands::cc) (s::ss) (m))
        | _ -> com2str (restOfCommands::cc) (s::ss) (m))
      | _ -> com2str (restOfCommands::cc) (s::ss) (m)))
  | ((RETURN::restOfCommands)::cc, s::ss, m::mm) -> com2str (cc) (match s with
  | hd::tl -> (match ss with
    | hd2::tl2 -> (match hd with
      | NAME(z) -> (try (match List.assoc z m with
        | INT(y) -> (INT(y)::hd2)::tl2
        | BOOL(y) -> (BOOL(y)::hd2)::tl2
        | STRING(y) -> (STRING(y)::hd2)::tl2
        | UNIT -> (UNIT::hd2)::tl2
        | _ -> (hd::hd2)::tl2)
      with
      | Not_found-> (hd::hd2)::tl2)
      | _ -> (hd::hd2)::tl2)
    | _ -> ss)
  | _ -> ss) (mm)
  | ((CALL::restOfCommands)::cc, (NAME(b)::NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
  | CLOSURE(c,d,e,w) -> (match List.assoc b m with
    | INT(f) -> com2str (d::restOfCommands::cc) ((NAME(b)::NAME(c)::STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, INT(f))::e)::m::mm)
    | STRING(f) -> com2str (d::restOfCommands::cc) ((NAME(b)::STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, STRING(f))::e)::m::mm)
    | BOOL(f) -> com2str (d::restOfCommands::cc) ((NAME(b)::STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, BOOL(f))::e)::m::mm)
    | UNIT -> com2str (d::restOfCommands::cc) ((NAME(b)::STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, UNIT)::e)::m::mm)
    | CLOSURE(f,g,h,w) -> com2str (d::restOfCommands::cc) ((NAME(b)::STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, CLOSURE(f,g,h,w))::e)::m::mm)
    | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(b)::NAME(a)::s)::ss) (m::mm))
  | _ -> com2str (restOfCommands::cc) ((ERROR::NAME(b)::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::NAME(b)::NAME(a)::s)::ss) (m::mm))
| ((CALL::restOfCommands)::cc, (INT(b)::NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
| CLOSURE(c,d,e,w) -> com2str (d::restOfCommands::cc) ((STRING(w)::[])::s::ss) (((a, CLOSURE(c,d,e,w))::(c, INT(b))::e)::m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::INT(b)::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::INT(b)::NAME(a)::s)::ss) (m::mm))
| ((CALL::restOfCommands)::cc, (UNIT::NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
| CLOSURE(c,d,e,w) -> com2str (d::restOfCommands::cc) ([]::s::ss) (((a, CLOSURE(c,d,e,w))::(c, UNIT)::e)::m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::UNIT::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::UNIT::NAME(a)::s)::ss) (m::mm))
| ((CALL::restOfCommands)::cc, (BOOL(b)::NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
| CLOSURE(c,d,e,w) -> com2str (d::restOfCommands::cc) ([]::s::ss) (((a, CLOSURE(c,d,e,w))::(c, BOOL(b))::e)::m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::BOOL(b)::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::BOOL(b)::NAME(a)::s)::ss) (m::mm))
| ((CALL::restOfCommands)::cc, (STRING(b)::NAME(a)::s)::ss, m::mm) -> (try (match List.assoc a m with
| CLOSURE(c,d,e,w) -> com2str (d::restOfCommands::cc) ([]::s::ss) (((a, CLOSURE(c,d,e,w))::(c, STRING(b))::e)::m::mm)
| _ -> com2str (restOfCommands::cc) ((ERROR::STRING(b)::NAME(a)::s)::ss) (m::mm))
with
| Not_found-> com2str (restOfCommands::cc) ((ERROR::STRING(b)::NAME(a)::s)::ss) (m::mm))
| ((CALL::restOfCommands)::cc, s::ss, m::mm) -> com2str (restOfCommands::cc) ((ERROR::s)::ss) (m::mm)
| (cc, ss, mm) -> (match cc with 
  | hd::tl -> (match ss with
    | hd2::tl2 -> (match mm with
      | hd3::tl3 -> (match List.nth (List.rev hd2) 0 with
        | STRING("isInOutDeclarer") -> com2str tl (tl2) (updateName (hd3::tl3) (List.rev (hd2)))
        | _ -> com2str tl (tl2) (hd3::tl3))
      | _ -> stack)
    | _ -> stack)
  | _ -> stack)
      
  in
  let finaloutput = com2str fixedCommandList [[]] [[]] in

  let rec printoutput (sv : (stackValue list) list) =
    match sv with
    | (INT(a)::rest)::ss -> file_write(string_of_int(a)); printoutput (rest::ss);
    | (STRING(a)::rest)::ss ->  file_write(a); printoutput (rest::ss);
    | (NAME(a)::rest)::ss ->  file_write(a); printoutput (rest::ss);
    | (BOOL(a)::rest)::ss -> if a then file_write ":true:" else file_write ":false:"; printoutput (rest::ss);
    | (ERROR::rest)::ss -> file_write ":error:"; printoutput (rest::ss);
    | (UNIT::rest)::ss -> file_write ":unit:"; printoutput (rest::ss);
    | _ -> ()
  in
  printoutput finaloutput
;;

(*interpreter ("input/input4.txt", "output1.txt")*)
