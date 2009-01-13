BEGIN foo

IF ~~ THEN BEGIN /* forgot state label */
  SAY ~Hello~
  IF ~~ THEN REPLY ~Goodbye~ EXIT
END
