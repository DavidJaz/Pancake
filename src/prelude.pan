pop "quote2 a b = {a b}";
let 'quote2 { . swap quote dip {quote} };

pop "dup2 a b = a b a b";
let 'dup2 { dip {dup}; dup; dip {swap} }

pop "swap a b = b a";
let 'swap { quote; dip };

pop "dip2 {f} a b c = a b f c";
let 'dip2 { swap; dip; };
let 'dip3 { swap; dip {swap}; dip };

pop "@2 {f} a b = f b a b"
let '@2 { dip2 {dup}; dip {rot3}; do }

pop "both {f} a b = f a f b"
let 'both { dup; dip {swap}; dip {do}; do }

pop "rot3 a b c = c a b"
let 'rot3 { quote2; swap; dip {do} };

pop "General recursion";
pop "rec f = f rec f";
let 'rec { dup; dip {rec}; do };

pop "Truth Values as fst and snd"; 
let 'fst { dip {pop} };
let 'snd { pop };
let 'true { {dip {pop}} };
let 'false { {pop} };


pop "if {A} {B} true = A
     if {A} {B} false = B";
let 'if { rot3; do; do };

let 'not {if {false} {true}};
let 'and {
    if
      {if {true} {false}}
      {false}
};
let 'or {
    if
      {true}
      {if {true} {false}}
};

pop "map {f} {a as} = {f a map {f} {as}}"
let 'map {
    dip head; pop "{f} {a} {as}";
    dup; . dip {swap}; pop "{f a} {f} {as}"
    dip {map};
    .;
};