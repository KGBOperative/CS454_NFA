lminimise_dfa.m /¦ò¯¯¯°°°°°¯¯¯°°¯°°¯°°°­partition ¯°°®®°°¯¯°°®°°¯°¯¯°«°«¯¯®¯and * °1 ¯¯and * 1 °¯°°¯­partition ®°®®¯®¯¯and ãoncat ¯°¯=   °°¯°°¯¯¯­¯¯=   ¯°­ð¯ =   ¯° ¯=   ¯ð° ¯=   ¯* ¯¯4 4 ¯or * °1 2 : ( (   eqclasses fst eqclasses : ( (  c eqivclasses ¨¦ð­­ðð¨®®°®°¯hd ««¬addtoclass ê R addtoclass  R addtoclass °foldr addtoclass ð F part - °¯map - part * ( ( (  ; partition ¯¯°°°¯min * ¯hd ° 4 * ¯¯®°°®°°¯¯°¯¯°°°°°°; 1 °¯- ¯=   °°°­®¯°= ð* 1 eqclasses : :   minimise regexp.m /¦~¨"   ð@¨#  ­ð¨$  ð(¯¯ ð|° ð)¨%  ¯¯ð( ° ð)¨&  ð(° ð*)ê M printRE  ¨"   ð¨#  ­ð¨$  ¯  ¨%  ¯  ¨&   ê ? literals  $ %   %    %  % &    %  % &    $ %   $ " &   $  %    # b # a ¯°¯foldr1 $ map # ¯map decode °code code  ®% &  $ "     reg     ! "   " #  # $  $ %  % &  &   string   ' sets.m /¦¸ $ set , ( ¯°®°2 ) ) id * À+ ¯concat map ð
, sort ¯¨¦ðð¨¦ð­ð¯¨°°¯°¯¯°»¬hd ê X remdups - foldr . 0 / filter 0 - map 1 2 ¨¦ðï¨¦ðî¨¯¯°°»î®¯¯°3 °°¯¯°»3 «íê A subset 3 ¨¦ðî¨®¯¯®»4 °°°ïîê < memset 4 ð5 ¨¦ðð¨¦ð¨®¯¯®»°¯¯¯¬6 «®¯¯°6 6 «ê 4 diff 6 ¨¦ðð¨¦ðð¨®®®°°¯¯¯¯»7 «®®¯¯°¬7 7 «ê . inter 7 ¨¦ð¨¦ð¨®¯¯®»°¯¯¯¬8 «®®¯¯°¬8 ¯¬8 «ê ( union 8 ­ð9 nfa_types.m /¦f ' nfa   : ;   ;  % move   < =   = >  > sets.m /¦¸(  )  *  +  ,  -  . 	 / 
 0  1  2  3  4  5  6  7  8  9   ¨×&    ¨×%      ¨×$      ¨×#  óhowchar ×"     ¨×>  ×=   óhowchar  Ø®;        ïð; ð> < nfa_types.m % Emove = < nfa_types.m % Move < 
nfa_types.m % move ; ( ( < ( : nfa_types.m ' NFA : 
nfa_types.m ' nfa 9 (  & sing 8 ( ( (  ( union 7 ( ( (  . inter 6 ( ( (  4 diff 5 (  : empty 4 (  < memset 3 ( (  A subset 2 ( (  L eqset 1 ( (  N mapset 0 ( (  P filterset / ( (  R separate . (  T foldset - (  V makeset , (  _ showset + (  a card * (  c flatten ) ( ( ( (  l setlimit ( 
 $ set ' 
regexp.m  string & ! ! regexp.m  Star % ! ! ! regexp.m  Then $ ! ! ! regexp.m  Or # ! regexp.m  Literal " ! regexp.m  Epsilon ! 
regexp.m  reg   ! ! regexp.m  opt  ! ! regexp.m  plus  ! regexp.m ' range  ! regexp.m / a  ! regexp.m 0 b  ! regexp.m 2 rex1  ! regexp.m 3 rex2  ! regexp.m 5 regexp0  ! regexp.m 6 regexp1  ! regexp.m 7 regexp2  ! regexp.m ? literals  ! regexp.m M printRE 