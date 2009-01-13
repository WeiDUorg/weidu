BEGIN wes

IF ~~ THEN BEGIN start
  SAY ~Acid Arrows~
  IF ~~ THEN EXTERN BODHI bodhi_chain
END

CHAIN2 BODHI bodhi_chain
  ~Black Dragon~ = ~Chromatic Orb~ ==
  ~Dragon Fear~ = ~Efreeti~ ==
  ~Fireball~ = ~Ghoul~ ==
  ~Hakeashar~ = ~Imp~ 
END wes wes_yada

IF ~~ THEN BEGIN wes_yada
  SAY ~Jaheira~
  IF ~~ THEN EXIT
END
