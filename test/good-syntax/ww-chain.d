// Simple "CHAIN" test
CHAIN BODHI entry_label
  ~Hi, Jon~ = ~How's it going?~ 
  == FATESP ~Oh, no soul.~ = ~Thirst for revenge.~
  == BODHI ~I know how it goes.~
END BODHI exit_label

APPEND BODHI 
  IF ~exit-label~ THEN BEGIN exit_label
    SAY ~Ta ta for now!~
    IF ~~ THEN EXIT
  END
END
