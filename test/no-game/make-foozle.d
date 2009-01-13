// weidu --nogame make-foozle.d --tlkout dialog.tlk
// weidu --nogame FOOZLE.DLG dialog.tlk 
// cat FOOZLE.D 

BEGIN foozle

IF ~~ THEN BEGIN 0
  SAY ~Hello, friend. My name is Foozle.~
  IF ~~ THEN REPLY ~Nice to meet you, Foozle.~ EXIT
  IF ~~ THEN REPLY ~Say something else.~ GOTO something
END

IF ~~ THEN BEGIN something
  SAY ~How about if I say "Abracadabra"?~
  IF ~~ THEN REPLY ~That was nice. Goodbye.~ EXIT
END
