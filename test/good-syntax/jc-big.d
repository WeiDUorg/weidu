// WARNING:
// This is an example of proper SYNTAX only! If you this file, do not
// expect the results to be meaningful.




// SAHPR4 STATE 7 DOES "IF ~~ THEN EXTERN SAHPR2 10" at the end.

ADD_TRANS_TRIGGER SAHPR4 7
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND SAHPR4 7
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseySAHPR4
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseySAHPR4
    SAY ~Urk. Who was the lucky donor?~
    IF ~~ THEN EXTERN SAHPR2 10
  END
END


// SUDEMIN



ADD_TRANS_TRIGGER SUDEMIN 24
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP SUDEMIN 24
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseySUDEMIN1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseySUDEMIN1
    SAY ~I see. Make him human. Worst fate there is. I doubt you even realize how insulting that is.~
    IF ~~ THEN EXTERN SUDEMIN 25
  END
END

ADD_TRANS_TRIGGER SUDEMIN 15
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP SUDEMIN 15
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseySUDEMIN2
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseySUDEMIN2
    SAY ~So, Irenicus is the elven kingdom's own home-town, born-and-bred kind of maniac.~
    IF ~~ THEN EXTERN SUDEMIN 16
  END
END

EXTEND_BOTTOM SUDEMIN 2
	IF ~PartyHasItem("J#KelBdy")~ THEN REPLY ~I lost a loved one in a battle with his sister, Bodhi.  I seek a means of curing him.~ EXTERN SUDEMIN 3
END


// SAHKNG01

ADD_TRANS_TRIGGER SAHKNG01 37
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP SAHKNG01 37
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseySAHKNG01
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseySAHKNG01
    SAY ~There's a joke there, somewhere...he would give his entire kingdom for a heart. Somebody should write that down...~
    IF ~~ THEN EXTERN SAHKNG01 38
  END
END

// UDDUER03

ADD_TRANS_TRIGGER UDDUER03 5
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP UDDUER03 5
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyUDDUER03
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyUDDUER03
    SAY ~At this point, I doubt any of us could disagree.~
    IF ~~ THEN EXIT
  END
END


//TRCAR01

ADD_TRANS_TRIGGER TRCAR01 2
~!IsValidForPartyDialog("J#Kelsey")~


EXTEND_TOP TRCAR01 2
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyTRCAR01
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyTRCAR01
    SAY ~<CHARNAME>...don't let the sad stories fool you, exactly, as every man and woman in this town would love to have their own powerful trade monopoly...but it sounds like these genies are playing extremely dirty.~
    IF ~~ THEN EXTERN TRCAR01 3
  END
END


// CELOGAN

ADD_TRANS_TRIGGER CELOGAN 59
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP CELOGAN 59
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyCELOGAN
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyCELOGAN
    SAY ~Lord Logan is not overstating things, <CHARNAME>. For better or for worse, Trademeet is built for the easy flow of goods in and out of the city. A monopoly, especially one enforced by powerful creatures, would eventually kill the town and ruin countless lives.~
    IF ~~ THEN EXTERN CELOGAN 60
  END
END

// MESSEN

ADD_TRANS_TRIGGER MESSEN 1
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP MESSEN 1
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyMESSEN1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyMESSEN1
    SAY ~<CHARNAME>, if Trademeet is in trouble, the region's entire economy could be at stake. Helping them would be a powerful statement...and probably lucrative as well.~
    IF ~~ THEN DO ~SetGlobal("TMAcceptance","GLOBAL",0)~ EXIT
  END
END


ADD_TRANS_TRIGGER MESSEN 10
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP MESSEN 10
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN 
DO ~SetGlobal("TMAcceptance","GLOBAL",1) 
RevealAreaOnMap("AR2000") 
EraseJournalEntry(54924) 
EscapeArea()~ 
JOURNAL #54895 
FLAGS 86 
EXTERN J#KLSYJ KelseyMESSEN2
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyMESSEN2
    SAY ~Whatever the cause, if Trademeet is in bad enough trouble that they are looking for relief all the way in Athkatla...it means the region's entire economy could be at stake.~
    IF ~~ THEN EXIT
  END
END



// PIRCOR02


ADD_TRANS_TRIGGER PIRCOR02 4
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PIRCOR02 4
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyPIRCOR02
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyPIRCOR02
    SAY ~<CHARNAME>! This girl should not be forced to sell herself. I think we can do better than simply telling her that life is tough.~
    IF ~~ THEN EXIT
  END
END


ADD_TRANS_TRIGGER PIRPIR11 1 ~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PIRPIR11 1
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyPIRPIR11
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyPIRPIR11
    SAY ~Wow. I guess Brynnlaw's total lack of barding colleges really shows.~
    IF ~~ THEN EXIT
  END
END



ADD_TRANS_TRIGGER PPIMOEN 4
~!IsValidForPartyDialog("J#Kelsey")~


EXTEND_TOP PPIMOEN 4
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyPPIMOEN
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyPPIMOEN
    SAY ~Oh, <CHARNAME>, I'm sorry...I hope we are not too late to undo the damage they have done to her.~
    IF ~~ THEN 
    DO ~SetGlobal("SpokeImoen","GLOBAL",1)~ 
    JOURNAL #15752
    FLAGS 92 EXIT

  END
END


ADD_TRANS_TRIGGER UHKID02 6
~!IsValidForPartyDialog("J#Kelsey")~


EXTEND_TOP UHKID02 6
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyUHKID02
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyUHKID02
    SAY ~If it's really their money, let them blow it on booze, <CHARNAME>...hopefully they'll learn quickly enough that it's about the most profitless way to spend hard-earned coin.~
    IF ~~ THEN EXTERN UHKID03 7
  END
END


ADD_TRANS_TRIGGER GOVWAU01 5
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP GOVWAU01 5
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyGOVWAU01
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyGOVWAU01
    SAY ~I know it seems strange, <CHARNAME>, but if you think about it, almost any way of life worth living has its patron and its religion.~
    IF ~~ THEN EXTERN GOVWAU01 6
  END
END


ADD_TRANS_TRIGGER ACOLYTE1 5
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP ACOLYTE1 5
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyACOLYTE1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyACOLYTE1
    SAY ~It's hard to argue with a salesman who convinces you that you'll die unless you buy his product. He wins either way. ~
    IF ~~ THEN EXIT
  END
END


ADD_TRANS_TRIGGER PTAGGET 5
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PTAGGET 5
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyPTAGGET
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyPTAGGET
    SAY ~If we destroy the jailer, we may be able to save others from being imprisoned here so easily...give it some thought, <CHARNAME>.~
    IF ~~ THEN EXTERN PTAGGET 6
  END
END


ADD_TRANS_TRIGGER MURCRAG 11
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP MURCRAG 11
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyMURCRAG
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyMURCRAG
    SAY ~<CHARNAME>, if she has truly done nothing wrong, leave her be. You remember what happened to your Imoen.~
    IF ~~ THEN 
    DO ~SetGlobal("TalkedToCragmoon","GLOBAL",1)~ 
    JOURNAL #34267 FLAGS 92 EXIT
  END
END


ADD_TRANS_TRIGGER INSPECT 11
~!IsValidForPartyDialog("J#Kelsey")~


EXTEND_TOP INSPECT 11
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyINSPECT
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyINSPECT
    SAY ~So...killing just to kill, and brutally. I doubt that even if they had coin it would save them from that sort of monster.~
    IF ~~ THEN EXTERN INSPECT 13
  END
END


//// UDDROW34 One-shot ////

APPEND UDDROW34
	IF ~IsValidForPartyDialog("J#Kelsey")
	    Global("J#KelseyUDDROW34","LOCALS",0)~ THEN BEGIN KelseyUDDROW34_1
		SAY ~You, there! I do not like the look of your magic aura at all, male! What manner of deviant are you?~
		IF ~~ THEN DO ~SetGlobal("J#KelseyUDDROW34","LOCALS",1)~ EXTERN J#KlsyJ KelseyUDDROW34_2
	END
END


APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyUDDROW34_2
		SAY ~Well...~
      IF ~IsValidForPartyDialogue("Viconia")~ THEN EXTERN VICONIJ UDDROW_V1
      IF ~!IsValidForPartyDialogue("Viconia")
IsValidForPartyDialogue("Jaheira")~ THEN EXTERN JAHEIRAJ UDDROW_J1
	IF ~!IsValidForPartyDialogue("Viconia")
!IsValidForPartyDialogue("Jaheira")
IsValidForPartyDialogue("Mazzy")~ THEN EXTERN MAZZYJ UDDROW_M1
IF ~!IsValidForPartyDialogue("Viconia")
!IsValidForPartyDialogue("Jaheira")
!IsValidForPartyDialogue("Mazzy")~ THEN EXTERN J#KLSYJ UDDROW34_3
	END
END

// Viconia rebuts UDDROW34

APPEND VICONIJ
	IF ~~ THEN BEGIN UDDROW_V1
		SAY ~This one's magic is exactly as I wish it to be, male. If you are troubled by what you see, I shall have your eyes plucked out.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW_V2
	END
END

APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW_V2
		SAY ~Ah, no! I am not troubled at all, honored female! I truly meant no offense to you or your House!~
	IF ~~ THEN EXTERN VICONIJ UDDROW_V3
	END
END


APPEND VICONIJ
	IF ~~ THEN BEGIN UDDROW_V3
		SAY ~Then begone, lest we take your insults personally.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW_V4
	END
END


APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW_V4
		SAY ~Yes, yes!~
	IF ~~ THEN DO ~RunAwayFrom("Viconia",15)~ EXTERN J#KLSYJ UDDROW_V5
	END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW_V5
		SAY ~Thank you for your help, Viconia...I wasn't sure how to handle that.~
	IF ~~ THEN EXTERN VICONIJ UDDROW_V6
	END
END


APPEND VICONIJ
	IF ~~ THEN BEGIN UDDROW_V6
		SAY ~Yes, yes, I know. Rather than watch you try and pitifully fail to verbally fence with a Drow, I thought I would save us all some embarrassment.~ 
IF ~~ THEN EXIT
	END
END


// Jaheira rebuts UDDROW34

APPEND JAHEIRAJ
	IF ~~ THEN BEGIN UDDROW_J1
		SAY ~You are clearly paranoid, male. I see nothing wrong with my wizard. Go about your business.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW_J2
	END
END


APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW_J2
		SAY ~Ah...as you command, honored female.~
	IF ~~ THEN EXTERN J#KLSYJ UDDROW_J3
	END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW_J3
		SAY ~Whew! Thanks, Jaheira.~
	IF ~~ THEN EXTERN JAHEIRAJ UDDROW_J4
	END
END


APPEND JAHEIRAJ
	IF ~~ THEN BEGIN UDDROW_J4
		SAY ~Pay attention, Kelsey! Try to be a little more assertive, will you? We must all do our parts in this masquerade if we are to survive this madness.~

	IF ~~ THEN EXTERN J#KLSYJ UDDROW_J5
	END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW_J5
		SAY ~Yes, honored female.~
	IF ~~ THEN EXIT
	END
END


// Mazzy rebuts UDDROW34

APPEND MAZZYJ
	IF ~~ THEN BEGIN UDDROW_M1
		SAY ~He is under my protection. Do not concern yourself.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW_M2
	END
END


APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW_M2
		SAY ~Ah...as you command, honored female.~
	IF ~~ THEN EXTERN J#KLSYJ UDDROW_M3
	END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW_M3
		SAY ~That could have been close...thank you, Mazzy.~
	IF ~~ THEN EXTERN MAZZYJ UDDROW_M4
	END
END


APPEND MAZZYJ
	IF ~~ THEN BEGIN UDDROW_M4
		SAY ~It is strange and unnatural, the abject fear these men have of drow women...but we must take what advantage we have in this unholy place, I suppose.~
	IF ~~ THEN EXIT
	END
END


// Kelsey rebuts UDDROW34

APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW34_3
		SAY ~My differences are not open for discussion.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW34_4
	END
END


APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW34_4
		SAY ~Oh? What do you hide, worm?~
	IF ~~ THEN EXTERN J#KLSYJ UDDROW34_5
	END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN UDDROW34_5
		SAY ~I think you are imagining things. I will be on my way now.~
	IF ~~ THEN EXTERN UDDROW34 UDDROW34_6
	END
END


APPEND UDDROW34
	IF ~~ THEN BEGIN UDDROW34_6
		SAY ~I have my eye on you! The matrons will never let you take my place!~
	IF ~~ THEN EXIT
	END
END


// FFCUST02 One-shot

APPEND FFCUST02
IF ~Global("FWKelseyFFCUST02","LOCALS",0)

InParty("J#Kelsey")

!StateCheck("J#Kelsey",STATE_SLEEPING)

RandomNum(2,1)
~ THEN BEGIN FFCUST02_1 // from:
  SAY ~My, what colors. Is that what the nouveau riches wear these days? How charming.~
  IF ~~ THEN DO ~SetGlobal("InteractMinsc","LOCALS",1)

~ EXTERN J#KLSYJ FFCUST02_2
END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN FFCUST02_2
		SAY ~Not that you care, but I dress for comfort, not to parade myself in front of the likes of you.~
	IF ~~ THEN EXIT
	END
END


// BAMNG02 One-shot

APPEND BAMNG02
IF ~InParty("J#Kelsey")

See("J#Kelsey")

!StateCheck("J#Kelsey",STATE_SLEEPING)

RandomNum(2,1)

Global("FWKelseyBAMNG02","GLOBAL",0)
~ THEN BEGIN BAMNG02_1 // from:
  SAY ~Hey, you! You look like magic! (hic) Make something appear for me!~
  IF ~~ THEN DO ~SetGlobal("FWKelseyBAMNG02","GLOBAL",1)
~ EXTERN J#KLSYJ BAMNG02_2
END
END


APPEND J#KLSYJ
	IF ~~ THEN BEGIN BAMNG02_2
		SAY ~Sorry, I don't do mixed drinks.~
	IF ~~ THEN EXIT
	END
END


// LOUTM02 One-shot
APPEND LOUTM02
IF ~InParty("J#Kelsey")

See("J#Kelsey")

!StateCheck("J#Kelsey",STATE_SLEEPING)

RandomNum(2,1)

Global("FWKelseyloutm02","GLOBAL",0)~
THEN BEGIN LOUTM02_1 // from:
  SAY ~Why, if it isn't that sniveling sorcerer...Kelsey, wasn't it? Still trying to get the hang of Chromatic Orb, I imagine.~ 
  IF ~~ THEN DO ~SetGlobal("FWKelseyloutm02","GLOBAL",1)

~ EXTERN J#KLSYJ LOUTM02_2
END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN LOUTM02_2
		SAY ~Don't talk to me. Just stand there and look tough or whatever it is you enjoy doing.~
	IF ~~ THEN EXIT
	END
END


// SCROLL01 One-shot

APPEND SCROLL01
	IF ~IsValidForPartyDialog("J#Kelsey")
	    Global("FWKelseySCROLL01","LOCALS",0)~ THEN BEGIN SCROLL01_1
		SAY ~Ah, the sorcerer returns.~
		IF ~~ THEN DO ~SetGlobal("FWKelseySCROLL01","LOCALS",1)~ 
		EXTERN J#KlsyJ SCROLL01_2
	END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN SCROLL01_2
		SAY ~I...~
	IF ~~ THEN EXTERN SCROLL01 SCROLL01_3
	END
END

APPEND SCROLL01
	IF ~~ THEN BEGIN SCROLL01_3
		SAY ~Relax, I remember your face, and you are safe here. You purchased a scroll from me recently, did you not?~
	IF ~~ THEN EXTERN J#KLSYJ SCROLL01_4
	END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN SCROLL01_4
		SAY ~Yes, I did.~
	IF ~~ THEN EXTERN SCROLL01 SCROLL01_5
	END
END

APPEND SCROLL01
	IF ~~ THEN BEGIN SCROLL01_5
		SAY ~And?~
	IF ~~ THEN EXTERN J#KLSYJ SCROLL01_6
	END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN SCROLL01_6
		SAY ~...and, as usual, I stared at it for a while until I was sure I would never be able to scribe it or learn it in any meaningful way.~
	IF ~~ THEN EXTERN J#KLSYJ SCROLL01_7
	END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN SCROLL01_7
		SAY ~So then I cast it. When nobody was around, of course. Stupid wizard eye wanted to follow me around for hours, it seemed.~
	IF ~~ THEN EXTERN SCROLL01 SCROLL01_8
	END
END

APPEND SCROLL01
	IF ~~ THEN BEGIN SCROLL01_8
		SAY ~Sorcery is a gift even some talented wizards would love to have at their disposal.~
	IF ~~ THEN EXTERN SCROLL01 SCROLL01_9
	END
END

APPEND SCROLL01
	IF ~~ THEN BEGIN SCROLL01_9
		SAY ~And, of course, my library stands ready to stock you with any spells you are not...intimately familiar with.~
	IF ~~ THEN EXTERN J#KLSYJ SCROLL01_10
	END
END

APPEND J#KLSYJ
	IF ~~ THEN BEGIN SCROLL01_10
		SAY ~Of course. We will return if we need anything. Good business to you, Lady Yuth.~
	IF ~~ THEN EXTERN SCROLL01 SCROLL01_11
	END
END

APPEND SCROLL01
	IF ~~ THEN BEGIN SCROLL01_11
		SAY ~And to you.~
	IF ~~ THEN EXIT
	END
END


// UHINN01 One-shot

APPEND UHINN01
  IF ~NumTimesTalkedToGT(0)
IsValidForPartyDialog("J#Kelsey")
Dead("SHADEL")
Global("J#KelseyUHINN01","LOCALS",0)~
 THEN BEGIN KelseyUHINN01_1
	SAY ~What can Vincenzo be doin' for you, then?~
	IF ~~ THEN DO ~SetGlobal("J#KelseyUHINN01","LOCALS",1)~ EXTERN J#KLSYJ UHINN01_2
END
END


APPEND J#KLSYJ
  IF ~~ THEN BEGIN UHINN01_2
    SAY ~Ah, sir...about that book of yours...~
    IF ~~ THEN EXTERN UHINN01 UHINN01_3
  END
END

APPEND UHINN01
  IF ~~ THEN BEGIN UHINN01_3
    SAY ~Yeah? Gripping stuff, innit? And true, every word, Vincenzo is sure o' that!~
    IF ~~ THEN EXTERN J#KLSYJ UHINN01_4
  END
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN UHINN01_4
    SAY ~Well...I did notice that there were a fair few copies in the back...you know, it might sell better if you didn't keep insisting that Umar is real...~
    IF ~~ THEN EXTERN UHINN01 UHINN01_5
  END
END

APPEND UHINN01
  IF ~~ THEN BEGIN UHINN01_5
    SAY ~And compromise me art? Sir, you insult me in me own bar!~
    IF ~~ THEN EXTERN UHINN02 UHINN01_6
  END
END

APPEND UHINN02
  IF ~~ THEN BEGIN UHINN01_6
    SAY ~You ARE an insult in yer own bar!~
    IF ~~ THEN EXTERN UHINN01 UHINN01_7
  END
END

CHAIN UHINN01 UHINN01_7
~Shut it, you!~ = ~As for your little criticism, what exactly is an honest bloke supposed to do?~
END J#KLSYJ UHINN01_8

APPEND J#KlsyJ
  IF ~~ THEN BEGIN UHINN01_8
    SAY ~Dress it up a little. Embellish the story, perhaps change some of the names and characters, and just try to sell it as straight fiction. Some people like scary stories, after all.~
    IF ~~ THEN EXTERN UHINN01 UHINN01_9
  END
END

APPEND UHINN01
  IF ~~ THEN BEGIN UHINN01_9
    SAY ~Hmmm. I don't rightly know. Seems to me Umar might take it as an insult, like, if I were ta just write 'er out of history like that.~
    IF ~~ THEN EXTERN J#KLSYJ UHINN01_10
  END
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN UHINN01_10
    SAY ~(sigh) Never mind, then.~
    IF ~~ THEN EXIT
  END
END


// C6ELHAN2

EXTEND C6ELHAN2 70
IF ~PartyHasItem("J#KelBdy")~ THEN REPLY ~Elhan!  I've the half vampire body of Kelsey here because of you!  You'll tell me now!~ EXTERN C6ELHAN2 76
END


// WARSAGE

EXTEND WARSAGE 0
IF ~!Dead("c6bodhi)
Global("J#KelseyVampire","GLOBAL",2)~
 THEN REPLY ~A loved one was taken by a vampire. What can I expect when I find them?~ EXTERN WARSAGE 6
END

EXTEND WARSAGE 0
IF ~PartyHasItem("J#KelBdy")~
 THEN REPLY ~Someone I care about has fallen to a vampire. Is there any way to save them?~ EXTERN WARSAGE 5
END


// GNOBLEM1

APPEND GNOBLEM1
IF ~InParty("J#Kelsey")

See("J#Kelsey")

!StateCheck("J#Kelsey",STATE_SLEEPING)

RandomNum(2,1)

Global("KelseyGNOBLEM1","LOCALS",0)
~ THEN BEGIN GNOBLEM1_1
  SAY ~Oh, you again. I told you, if you insist on pressing that ridiculous matter you will have to do it through my secretary.~
  IF ~~ THEN DO ~SetGlobal(KelseyGNOBLEM1","LOCALS",1)~
EXTERN J#KLSYJ GNOBLEM1_2
END
END

APPEND J#KLSYJ
  IF ~~ THEN BEGIN GNOBLEM1_2
    SAY ~No, I have better things to do now than chase down your bad debts to my family. But if you put as much money in the hands of the people you owed as you do in your silks, we wouldn't have this problem.~
    IF ~~ THEN EXTERN GNOBLEM1 GNOBLEM1_3
  END
END

APPEND GNOBLEM1
  IF ~~ THEN BEGIN GNOBLEM1_3
    SAY ~Oh, do run along now.~
    IF ~~ THEN EXIT
  END
END


// TRGYP02

EXTEND TRGYP02 2
IF ~!InPartySlot(LastTalkedToBy,0)

Name("J#Kelsey",LastTalkedToBy)~
 THEN EXTERN TRGYP02 TRGYP02_1
END

APPEND TRGYP02
  IF ~~ THEN BEGIN TRGYP02_1
    SAY ~You are troubled, both by gifts and by scorn you feel are undeserved. Embrace the former within, and learn to disregard the latter without, or be consumed by torment of your own making.~
    IF ~~ THEN EXTERN J#KLSYJ TRGYP02_2
  END
END

APPEND J#KLSYJ
  IF ~~ THEN BEGIN TRGYP02_2
	SAY ~I imagine everybody should try to be more self-sufficient...I could have told you that for free.~
  IF ~~ THEN EXIT
END
END


// CORNEIL

REPLACE CORNEIL 
IF ~~ THEN BEGIN 8 // from: 6.1 7.0
  SAY #59583
  IF ~ReputationGT(LastTalkedToBy,3)
!IsValidForPartyDialog("J#Kelsey")~ THEN REPLY #59584 GOTO 9
  IF ~ReputationGT(LastTalkedToBy,3)
IsValidForPartyDialog("J#Kelsey")~ THEN REPLY ~And just how much is this 'monetary sacrifice', exactly?~ /* #59584 */ EXTERN J#KLSYJ CORNEIL_1
  IF ~ReputationLT(LastTalkedToBy,4)~ THEN REPLY #59589 GOTO 10
  IF ~~ THEN REPLY #59585 GOTO 5
END
END

APPEND J#KLSYJ
  IF ~~ THEN BEGIN CORNEIL_1
	SAY ~This isn't going to be pretty, <CHARNAME>...the Cowled Wizards are basically a monopoly on magic, and monopolies keep prices high and quantities low...~
  IF ~~ THEN EXTERN CORNEIL 9
END
END


REPLACE CORNEIL
IF ~~ THEN BEGIN 11 // from: 9.0
  SAY #59594
  IF ~!IsValidForPartyDialog("J#Kelsey")~ THEN DO ~SetGlobal("BribedCowled","GLOBAL",1)~ EXIT
  IF ~IsValidForPartyDialog("J#Kelsey")~ THEN DO ~SetGlobal("BribedCowled","GLOBAL",1)~ EXTERN J#KLSYJ CORNEIL_2
END
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN CORNEIL_2
    SAY ~Yow! That was worse than I thought. But if it means I can cast freely while we're in the city, perhaps it was worthwhile. I doubt we're totally free of their scrutiny, however.~
    IF ~~ THEN EXIT
  END
END


// PPUMB01

ADD_TRANS_TRIGGER PPUMB01 0
~OR(2)
!IsValidForPartyDialogue("J#Kelsey")
!Global("KelseyPPUMB01","LOCALS",0)~

EXTEND PPUMB01 0
IF ~IsValidForPartyDialogue("J#Kelsey")
Global("FWKelseyPPUMB01","LOCALS",0)~
THEN EXTERN J#KLSYJ PPUMB01_1
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN PPUMB01_1
    SAY ~Heh. I really enjoy when clerics with bad attitudes try to make me feel all warm and welcome. They're so bad at it!~
    IF ~~ THEN DO ~SetGlobal("FWKelseyPPUMB01","LOCALS",1)~ EXTERN PPUMB01 PPUMB01_2
  END
END

APPEND PPUMB01
IF ~~ THEN BEGIN PPUMB01_2
    SAY ~Silence! Do you have need of Umberlee's services or not?~
  IF ~~ THEN REPLY ~I do. What services have you available?~ 
    DO ~StartStore("ppumb01",LastTalkedToBy())~ EXIT
  IF ~~ THEN REPLY ~I do not have need of your services.~ EXTERN PPUMB01 1
END
  END




REPLACE PPUMB01

IF ~True()~ THEN BEGIN 0 // from:
  SAY #47559
  IF ~(IsValidForPartyDialogue("J#Kelsey")
Global("KelseyPPUMB01","LOCALS",0)~ THEN EXTERN J#KLSYJ PPUMB01_1
  IF ~OR(2)
!IsValidForPartyDialogue("J#Kelsey")
!Global("KelseyPPUMB01","LOCALS",0)~ THEN REPLY #47560 DO ~StartStore("ppumb01",LastTalkedToBy())~ EXIT
  IF ~OR(2)
!IsValidForPartyDialogue("J#Kelsey")
!Global("KelseyPPUMB01","LOCALS",0)~ THEN REPLY #47561 GOTO 1
END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN PPUMB01_1
    SAY ~Heh. I really enjoy when clerics with bad attitudes try to make me feel all warm and welcome. They're so bad at it!~
    IF ~~ THEN EXTERN PPUMB01 PPUMB01_2
  END
END

APPEND PPUMB01
IF ~~ THEN BEGIN PPUMB01_2
    SAY ~Silence! Do you have need of Umberlee's services or not?~
  IF ~~ THEN REPLY ~I do. What services have you available?~ 
    DO ~StartStore("ppumb01",LastTalkedToBy())~ EXIT
  IF ~~ THEN REPLY ~I do not have need of your services.~ EXTERN PPUMB01 1
END
  END


/* SHOP06 */ 

REPLACE SHOP06

IF ~NumTimesTalkedToGT(0)
OR(2)
!IsValidForPartyDialog("J#Kelsey")
!Global("J#KelseySHOP06","LOCALS",0)~ THEN BEGIN 9
  SAY #16072
  IF ~~ THEN REPLY #16073 GOTO 5
  IF ~~ THEN REPLY #16076 GOTO 10
  IF ~~ THEN REPLY #16077 GOTO 1
END
END

APPEND SHOP06
IF ~NumTimesTalkedToGT(0)
IsValidForPartyDialog("J#Kelsey")
Global("J#KelseySHOP06","LOCALS",0)~
THEN BEGIN KELSEYSHOP06_1
  SAY ~What can I do for you this day?~
  IF ~~ THEN DO ~SetGlobal("J#KelseySHOP06","LOCALS",1)~
  EXTERN J#KLSYJ KELSEYSHOP06_2
END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_2
    SAY ~Why, it's Hes, isn't it?~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_3
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_3
    SAY ~Yes! It is Hes! And it is Kelsey! How are times finding you?~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP06_4
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_4
    SAY ~Things go well, Hes...business obligations are light, but I am travelling with <CHARNAME> now.~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_5
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_5
    SAY ~Indeed, and the travel seems to suit you. Since you have so much to do, you will be needing to equip well, yes?~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP06_6
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_6
    SAY ~Ah, well, it's <CHARNAME>'s loot to spend, Hes, but rest assured, I will be sure to mention your name when <PRO_HESHE> needs supplies.~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_7
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_7
    SAY ~Then this has been a good day for both of us!~
    IF ~~ THEN EXIT
  END
END

/* SHOP07 */

REPLACE SHOP07

IF ~OR(2)
!IsValidForPartyDialog("J#Kelsey")
!Global("J#KelseySHOP07","LOCALS",0)~ 
THEN BEGIN 0 // from:
  SAY #24761
  IF ~~ THEN REPLY #24763 DO ~StartStore("shop07",LastTalkedToBy())~ EXIT
  IF ~~ THEN REPLY #14641 GOTO 1
END
END



APPEND SHOP07
IF ~IsValidForPartyDialog("J#Kelsey")
Global("J#KelseySHOP07","LOCALS",0)~
THEN BEGIN KELSEYSHOP07_1
  SAY ~`Allo! Step closer, so that you might see me wares better!~
  IF ~~ THEN DO ~SetGlobal("J#KelseySHOP07","LOCALS",1)~
  EXTERN J#KLSYJ KELSEYSHOP07_2
END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP07_2
    SAY ~Is that you, Alic? Still in business, I see.~
    IF ~~ THEN EXTERN SHOP07 KELSEYSHOP07_3
  END
END

APPEND SHOP07

  IF ~~ THEN BEGIN KELSEYSHOP07_3
    SAY ~Yes, business still finds me in business, Kelsey...and I find business wherever I can.~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP07_4
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP07_4
    SAY ~I seem to remember that on my last trip, you were within the Promenade proper...why the move?~
    IF ~~ THEN EXTERN SHOP07 KELSEYSHOP07_5
  END
END


CHAIN SHOP07 KELSEYSHOP07_5
~Business is of course good in the Promenade, but it is so crowded, so many competing merchants ready to undercut you. And you have but one chance.~ 
    = ~Here, by the tunnel, they have no choice but to see me! And I have a chance to get them coming and going, so to speak!~
END J#KLSYJ KELSEYSHOP07_6


APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP07_6
    SAY ~Good point, Alic. Good business to you.~
    IF ~~ THEN EXTERN SHOP07 KELSEYSHOP07_7
  END
END

APPEND SHOP07

  IF ~~ THEN BEGIN KELSEYSHOP07_7
    SAY ~And to you and your friends, who are of course welcome to peruse everything I have to offer.~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP07_8
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP07_8
    SAY ~Oh, of course.~
    IF ~~ THEN EXIT
  END
END

// UDDROW25

REPLACE UDDROW25

IF ~True()~ THEN BEGIN 0 // from:
  SAY #52932
  IF ~!IsValidForPartyDialog("J#Kelsey")~ THEN REPLY #52934 DO ~StartStore("uddrow25",LastTalkedToBy())~ EXIT
  IF ~IsValidForPartyDialog("J#Kelsey")~ THEN REPLY ~Yes, I shall see what you have.~ EXTERN J#KLSYJ UDDROW25_1
  IF ~~ THEN REPLY #52935 GOTO 1
END
END

APPEND J#KLSYJ
IF ~~ THEN BEGIN UDDROW25_1
	SAY ~Psst...you do realize that we're not talking about supporting a few fences in Athkatla, that you're proposing to do trade with the Drow, here, right?~ 
   IF ~~ THEN DO ~StartStore("uddrow25",LastTalkedToBy())~ EXIT
END
END


// DADROW21

REPLACE DADROW21

IF ~~ THEN BEGIN 2 // from: 1.0
  SAY #57449
  IF ~Gender(LastTalkedToBy,MALE)
!InPartySlot(LastTalkedToBy,0)
!Name("Anomen",LastTalkedToBy)
!Name("Keldorn",LastTalkedToBy)
!Name("Edwin",LastTalkedToBy)
!Name("Cernd",LastTalkedToBy)
!Name("Korgan",LastTalkedToBy)
!Name("Jan",LastTalkedToBy)
!Name("Minsc",LastTalkedToBy)
!Name("J#Kelsey",LastTalkedToBy)~ THEN REPLY #57450 GOTO 4

  IF ~Gender(LastTalkedToBy,FEMALE)
!InPartySlot(LastTalkedToBy,0)
!Name("Aerie",LastTalkedToBy)
!Name("Jaheira",LastTalkedToBy)
!Name("Mazzy",LastTalkedToBy)~ THEN REPLY #57451 GOTO 4

  IF ~Gender(LastTalkedToBy,MALE)
InPartySlot(LastTalkedToBy,0)
!Global("JaheiraRomanceActive","GLOBAL",1)
!Global("JaheiraRomanceActive","GLOBAL",2)
!Global("AerieRomanceActive","GLOBAL",1)
!Global("AerieRomanceActive","GLOBAL",2)~ THEN REPLY #57452 GOTO 4

  IF ~Gender(LastTalkedToBy,FEMALE)
InPartySlot(LastTalkedToBy,0)
!Global("AnomenRomanceActive","GLOBAL",1)
!Global("AnomenRomanceActive","GLOBAL",2)
!Global("J#KelseyRomanceActive","GLOBAL",1)
!Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN REPLY #57453 GOTO 4
  IF ~~ THEN REPLY #57454 GOTO 3
END
END


// DADROW23

REPLACE DADROW23

IF ~~ THEN BEGIN 2 // from: 1.0
  SAY #57470 
  IF ~!InPartySlot(LastTalkedToBy,0)
!Name("Anomen",LastTalkedToBy)
!Name("J#Kelsey",LastTalkedToBy)
!Name("HaerDalis",LastTalkedToBy)
!Name("Keldorn",LastTalkedToBy)
!Name("Cernd",LastTalkedToBy)
!Name("Jan",LastTalkedToBy)
!Name("Minsc",LastTalkedToBy)~ THEN REPLY #57471 GOTO 3

  IF ~!InPartySlot(LastTalkedToBy,0)
!Name("Keldorn",LastTalkedToBy)~ THEN REPLY #57472 GOTO 4

  IF ~!InPartySlot(LastTalkedToBy,0)
!Name("Anomen",LastTalkedToBy)
!Name("HaerDalis",LastTalkedToBy)
!Name("Cernd",LastTalkedToBy)
!Name("J#Kelsey",LastTalkedToBy)
!Name("Jan",LastTalkedToBy)
!Name("Minsc",LastTalkedToBy)~ THEN REPLY #57473 GOTO 5

  IF ~InPartySlot(LastTalkedToBy,0)
!Global("JaheiraRomanceActive","GLOBAL",1)
!Global("JaheiraRomanceActive","GLOBAL",2)
!Global("AerieRomanceActive","GLOBAL",1)
!Global("AerieRomanceActive","GLOBAL",2)~ THEN REPLY #57480 GOTO 3
  IF ~InPartySlot(LastTalkedToBy,0)~ THEN REPLY #57481 GOTO 4
  IF ~InPartySlot(LastTalkedToBy,0)~ THEN REPLY #57482 GOTO 5
END
END


// UDSIMYAZ


ADD_TRANS_TRIGGER UDSIMYAZ 16
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP UDSIMYAZ 16 
IF ~IsValidForPartyDialogue("J#Kelsey")~ THEN EXTERN J#KLSYJ UDSIMYAZ_1
END

APPEND J#KLSYJ
IF ~~ THEN BEGIN UDSIMYAZ_1
	SAY ~Ah, well, if you want to make a deal at knifepoint, we seem to be the gang to talk to these days...~ 
  IF ~~ THEN EXTERN UDSIMYAZ 17
END
END



// Kelsey interrupts Anomen LT 2

ADD_TRANS_TRIGGER BANOMEN 79
~OR(2)
!Global("J#KelseyRomanceActive","GLOBAL",1)
!IsValidForPartyDialog("J#Kelsey")~


EXTEND BANOMEN 79
IF ~Global("J#KelseyRomanceActive","GLOBAL",1)
IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN BJ#KLSY AnomenLT2_1
END

APPEND BJ#KLSY
IF ~~ THEN BEGIN AnomenLT2_1
SAY ~Why, do you have the story available in book form? In my experience, anyone using the word "prithee" is usually selling something.~ [FWKLSYF6]
IF ~~ THEN EXTERN BANOMEN AnomenLT2_2
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT2_2
SAY ~Do you often answer to "my lady", Kelsey? Perhaps you should have your dainty ears checked, so you will know when you are not being addressed.~
IF ~~ THEN EXTERN BANOMEN AnomenLT2_3
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT2_3
SAY ~<CHARNAME>, please forgive the interruption. I stand ready to share...the gaps in your knowledge of my accomplishments are indeed extensive.~
  IF ~~ THEN REPLY ~I am not interested in hearing of your travels, Anomen.~ EXTERN BANOMEN 80
  IF ~~ THEN REPLY ~And that's exactly the way I like it.~  EXTERN BANOMEN 81
  IF ~~ THEN REPLY ~Alright, then...tell me of your journeys, if you wish.~ EXTERN BANOMEN 82
  IF ~~ THEN REPLY ~Sorry, Anomen, this is no time to speak.~ EXTERN BANOMEN 86

END
END




// Kelsey interrupts Anomen LT 4

ADD_TRANS_TRIGGER BANOMEN 106
~OR(2)
!Global("J#KelseyRomanceActive","GLOBAL",1)
!IsValidForPartyDialog("J#Kelsey")~


EXTEND BANOMEN 106
IF ~Global("J#KelseyRomanceActive","GLOBAL",1)
IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN BJ#KLSY AnomenLT4_1
END

APPEND BJ#KLSY
IF ~~ THEN BEGIN AnomenLT4_1
SAY ~Let me guess. You founded it, and sculpted the columns that hold up the barracks with a butter knife.~ [FWKLSYF7]
IF ~~ THEN EXTERN BANOMEN AnomenLT4_2
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT4_2
SAY ~Your petty sarcasm cannot touch the honor of the Order, but it certainly speaks volumes about you. The only "order" in your future is an order for textiles and knick-nacks.~
IF ~~ THEN EXTERN BJ#KLSY AnomenLT4_3
END
END

APPEND BJ#KLSY
IF ~~ THEN BEGIN AnomenLT4_3
SAY ~I'm not taking that bait. I like a good paladin as much as the next guy, but to hear you talk, the whole thing would fall apart without you.~
IF ~~ THEN EXTERN BANOMEN AnomenLT4_4
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT4_4
SAY ~Your interruptions wear at my patience. I spoke not for you, boy, but for <CHARNAME>, who can appreciate both the Order and my significant contributions.~
IF ~~ THEN EXTERN BANOMEN AnomenLT4_5
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT4_5
SAY ~My lady, before we were so rudely interrupted, I had asked a question of you...I must dare ask it again: are you at all versed with the Order of the Most Radiant Heart?~
  IF ~~ THEN REPLY ~Not very much, I'm afraid.~ EXTERN BANOMEN 107
  IF ~~ THEN REPLY ~A little.~ EXTERN BANOMEN 112
  IF ~~ THEN REPLY ~All I know is that it is full of arrogant and pompous knights like yourself.~ EXTERN BANOMEN 113
  IF ~~ THEN REPLY ~I don't care to discuss this right now, Anomen.~ EXTERN BANOMEN 114

END
END



// Kelsey interrupts Anomen LT 12

ADD_TRANS_TRIGGER BANOMEN 173
~OR(2)
!Global("J#KelseyRomanceActive","GLOBAL",1)
!IsValidForPartyDialog("J#Kelsey")~


EXTEND BANOMEN 173
IF ~Global("J#KelseyRomanceActive","GLOBAL",1)
IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN BJ#KLSY AnomenLT12_1
END

APPEND BJ#KLSY
IF ~~ THEN BEGIN AnomenLT12_1
SAY ~Why don't you just hire a minstrel? You could save your energy for fighting, rather than constantly singing your own praises.~ [FWKLSYF8]
IF ~~ THEN EXTERN BANOMEN AnomenLT12_2
END
END


APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT12_2
SAY ~Tread carefully, sorcerer, before you press this line of inquiry. I have had more than enough of your impudent interruptions.~
IF ~~ THEN EXTERN BJ#KLSY AnomenLT12_3
END
END

APPEND BJ#KLSY
IF ~~ THEN BEGIN AnomenLT12_3
SAY ~Well, obviously it's not making much of an impression on you. Still as transparent as ever.~
IF ~~ THEN EXTERN BANOMEN AnomenLT12_4
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT12_4
SAY ~You'll get an impression soon enough, if you do not take exceedingly good care.~ 
IF ~~ THEN EXTERN BANOMEN AnomenLT12_5
END
END

APPEND BANOMEN
IF ~~ THEN BEGIN AnomenLT12_5
SAY ~Apologies, my lady. Nothing would help me clear my mind of this unpleasantness more than being able to tell you how I came into Helm's service.~
  IF ~~ THEN REPLY ~Please. I'd like to hear the tale.~ /* #35254 */ EXTERN BANOMEN 174
  IF ~~ THEN REPLY ~I've no interest in hearing about it.~ /* #35255 */ EXTERN BANOMEN 179
  IF ~~ THEN REPLY ~I thought you were trying to become a knight.~ /* #35256 */ EXTERN BANOMEN 180
END
END



// UHMAY01

ADD_TRANS_TRIGGER UHMAY01 18
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP UHMAY01 18
IF ~IsValidForPartyDialogue("J#Kelsey")~ THEN JOURNAL #2099 FLAGS 82 EXTERN J#KLSYJ KELSEYUHMAY01_1
END

APPEND J#KLSYJ
IF ~~ THEN BEGIN KELSEYUHMAY01_1
	SAY ~We should be careful, <CHARNAME>...if the town's ranger and an adventuring party are already missing, we don't want to become part of the statistics.~ 
IF ~~ THEN EXTERN UHMAY01 19
END
END

// PPAPHRIL

ADD_TRANS_TRIGGER PPAPHRIL 4
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PPAPHRIL 4
IF ~IsValidForPartyDialogue("J#Kelsey")~ THEN EXTERN J#KLSYJ KELSEYPPAPHRIL_1
END

ADD_TRANS_TRIGGER PPAPHRIL 5
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PPAPHRIL 5
IF ~IsValidForPartyDialogue("J#Kelsey")~ THEN EXTERN J#KLSYJ KELSEYPPAPHRIL_1
END

APPEND J#KLSYJ
IF ~~ THEN BEGIN KELSEYPPAPHRIL_1
	SAY ~Amazing...but I imagine she would give almost anything to be able to turn it off.~ 
IF ~~ THEN EXIT
END
END


// PPIRENI1


ADD_TRANS_TRIGGER VALYGARJ 79
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND VALYGARJ 79
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyVALYGARJ_1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyVALYGARJ_1
    SAY ~Well, you might start by laying off the dirty looks in my direction, then...~
    IF ~~ THEN EXTERN PPIRENI1 25
  END
END


ADD_TRANS_TRIGGER PPIRENI1 14
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP PPIRENI1 14
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyPPIRENI1_1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyPPIRENI1_1
    SAY ~What goes around, comes around. Perhaps your turn is coming soon.~
    IF ~~ THEN EXTERN PPIRENI1 15
  END
END


// UDPHAE01

ADD_TRANS_TRIGGER UDPHAE01 88
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP UDPHAE01 88
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN 
DO ~SetGlobal("SolaufeinJob","GLOBAL",8)
SetGlobalTimer("udPhaTimer","GLOBAL",THREE_DAYS)
SetGlobal("udPhaTimerOn","GLOBAL",1)~ JOURNAL #23485 FLAGS 86 EXTERN J#KLSYJ KELSEYUDPHAE01
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KELSEYUDPHAE01
    SAY ~We've been pushed around by just about everyone since we got here. I think it's time we stop acting automatically and figure out if there isn't a better deal to be had...~
    IF ~~ THEN EXIT
  END
END


// SAHBEH01

ADD_TRANS_TRIGGER SAHBEH01 38
~!IsValidForPartyDialog("J#Kelsey")~

EXTEND_TOP SAHBEH01 38
	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KLSYJ KELSEYSAHBEH01
END

APPEND J#KLSYJ
  IF ~~ THEN BEGIN KELSEYSAHBEH01
    SAY ~Are you sure? I'll bet your contract is vacated once there's nothing left to guard.~
    IF ~~ THEN EXTERN SAHBEH01 40
  END
END

ADD_TRANS_TRIGGER UDDEMON 7 ~!IsValidForPartyDialog("J#Kelsey")~

EXTEND UDDEMON 7
	IF
		~IsValidForPartyDialog("J#Kelsey")
		Dead("Phaere")
		PartyHasItem("MISC9t")
		Alignment(Player1,MASK_EVIL)~
	THEN
		REPLY #55900 // "Hold demons! I have the eggs..."
		EXTERN J#KlsyJ J#KelseyDemon_1
END

APPEND J#KlsyJ
 	IF ~~ THEN BEGIN J#KelseyDemon_1
 		SAY ~This is a despicable deal, <CHARNAME!> You...you are on your own to deal with the consequences.~

 	IF
		~IsValidForPartyDialog("Nalia")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN NaliaJ 280

 	IF
		~!IsValidForPartyDialog("Nalia")
		IsValidForPartyDialog("Cernd")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN CerndJ 111

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		IsValidForPartyDialog("Aerie")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN AerieJ 149

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		IsValidForPartyDialog("Keldorn")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN KeldorJ 201

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		IsValidForPartyDialog("Valygar")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN ValygarJ 85

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		!IsValidForPartyDialog("Valygar")
		IsValidForPartyDialog("Mazzy")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN MaazyJ 175

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		!IsValidForPartyDialog("Valygar")
		!IsValidForPartyDialog("Mazzy")
		IsValidForPartyDialog("Anomen")
		Alignment("Anomen",LAWFUL_GOOD)~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN AnomenJ 271

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		!IsValidForPartyDialog("Valygar")
		IsValidForPartyDialog("Mazzy")
		OR(2)
			!IsValidForPartyDialog("Anomen")
			!Alignment("Anomen",LAWFUL_GOOD)
		IsValidForPartyDialog("Jaheira")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN JaheiraJ 470

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		!IsValidForPartyDialog("Valygar")
		IsValidForPartyDialog("Mazzy")
		OR(2)
			!IsValidForPartyDialog("Anomen")
			!Alignment("Anomen",LAWFUL_GOOD)
		!IsValidForPartyDialog("Jaheira")
		IsValidForPartyDialog("Minsc")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN MinscJ 170

 	IF
		~!IsValidForPartyDialog("Nalia")
		!IsValidForPartyDialog("Cernd")
		!IsValidForPartyDialog("Aerie")
		!IsValidForPartyDialog("Keldorn")
		!IsValidForPartyDialog("Valygar")
		IsValidForPartyDialog("Mazzy")
		OR(2)
			!IsValidForPartyDialog("Anomen")
			!Alignment("Anomen",LAWFUL_GOOD)
		!IsValidForPartyDialog("Jaheira")
		!IsValidForPartyDialog("Minsc")~
	THEN
		DO~LeaveParty()
		EscapeArea()~
		EXTERN UDDEMON 8

 	END
 END

EXTEND UDDEMON 7
	IF
		~Dead("Phaere")
		IsValidForPartyDialog("J#Kelsey")~
	THEN
		REPLY ~Begone, demon!  There is nothing for you here.  She who has summoned you is dead.~
		GOTO 13
END

EXTEND UDDEMON 7
	IF
		~Dead("Phaere")
		IsValidForPartyDialog("J#Kelsey")~
	THEN
		REPLY ~(remain silent)~
		GOTO 13
END

EXTEND UDDEMON 7
	IF
		~!Dead("Phaere")
		IsValidForPartyDialog("J#Kelsey")~
	THEN
		REPLY ~(Wait)~
		EXTERN UDPHAE01 159
END

ADD_TRANS_TRIGGER KYLIE1 5 ~!IsValidForPartyDialog("J#Kelsey")~

EXTEND KYLIE1 5
  IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlSyJ KelseyKylie1
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN KelseyKylie1
    SAY ~Actually, we learned the hard way that turnips are a losing proposition. Some crazy gnome once talked my father into investing into ten thousand tons of turnip futures. It did not end well...but then, you're all giving me this look that says you don't care. Never mind.~
    IF ~~ THEN EXTERN KYLIE1 6
  END
END

////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER TIRDIR 2 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND TIRDIR 2
   IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyTirdir1
 END
 
 APPEND J#KlsyJ
   IF ~~ THEN BEGIN KelseyTirdir1
     SAY ~This almost sounds too implausible to be true, <CHARNAME>...but I can't imagine anyone burying themselves just to set some sort of trap for us.~
     IF ~~ THEN EXTERN TIRDIR 3
   END
 END
 
//////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER VALYGAR 28 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND VALYGAR 28
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyValygar1
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyValygar1
 		SAY ~You have either been cowed or seduced by the evil of the Cowled Wizards, <CHARNAME>, and I will have no part of this, or of you.~
 		IF ~~ THEN DO ~LeaveParty() EscapeArea()~ EXIT
 	END
 END
 
 ///////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER LEHTIN 22 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND LEHTIN 22
   IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyLehtin1
 END
 
 APPEND J#KlsyJ
   IF ~~ THEN BEGIN KelseyLehtin1
     SAY ~You are actually going to support this slaver? Don't you believe in people having the freedom to live their own lives?~
     IF ~~ THEN DO ~SetGlobal("SlavingJerk","GLOBAL",1)
 GiveItemCreate("MISC4Z",LastTalkedToBy,0,0,0)~ EXIT
   END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER AnomenJ 162 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND AnomenJ 162
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyHendak1
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyHendak1
 		SAY ~Anomen, you and <CHARNAME> both have a hell of a lot of nerve. When was the last time you were chained up and forced to fight for someone else's entertainment?~
 		IF ~~ THEN EXTERN AnomenJ KelseyHendak2
 	END
 END
 
 APPEND ANOMENJ
 	IF ~~ THEN BEGIN KelseyHendak2
 		SAY ~These rabble have clearly earned their station, and if they were truly worthy to be free men, they would break their own shackles. Most of these men are likely deserters and escaped prisoners, else how would they run afoul of slavers to begin with?~
 		IF ~~ THEN EXIT
 	END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER THIEF7 10 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND THIEF7 10
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyThief7
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyThief7
 		SAY ~Is it just me, <CHARNAME>, or is this lady really creepy?~
 		IF ~~ THEN EXIT
 	END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER ARNMAN04 12 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND ARNMAN04 12
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyArnman041
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyArnman041
 		SAY ~Wow. I really hope you're proud of yourself.~
 		IF ~~ THEN EXIT
 	END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
 APPEND BSAILOR3
 	IF ~Global("J#InteractKelsey","LOCALS",0)
	    IsValidForPartyDialog("J#Kelsey")
	    RandomNum(2,1)~
	THEN BEGIN KelseyBSailor30
 		SAY ~Didn't your ma ever tell you the ladies can't see what you have to give them under all those robes? Give us a look.~
 		IF ~~ THEN DO ~SetGlobal("J#InteractKelsey","LOCALS",1)~ EXTERN J#KlsyJ KelseyBSailor31
 	END
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyBSailor31
 		SAY ~You must be joking.~
 		IF ~~ THEN EXIT
 	END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
ADD_TRANS_TRIGGER INSPECT 11 ~!IsValidForPartyDialog("J#Kelsey")~

 EXTEND INSPECT 11
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyInspect1
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyInspect1
 		SAY ~So...killing just to kill, and brutally. I doubt that even if they had coin it would save them from that sort of monster.~
 		IF ~~ THEN EXTERN INSPECT 12
 	END
 END
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////

ADD_TRANS_TRIGGER MURCRAG 11 ~!IsValidForPartyDialog("J#Kelsey")~
 
 EXTEND MURCRAG 11
 	IF ~IsValidForPartyDialog("J#Kelsey")~ THEN EXTERN J#KlsyJ KelseyMurcrag1
 END
 
 APPEND J#KlsyJ
 	IF ~~ THEN BEGIN KelseyMurcrag1
 		SAY ~<CHARNAME>, if she has truly done nothing wrong, leave her be. You remember what happened to your Imoen.~
 		IF ~~ THEN EXIT
 	END
 END

//////////////////////////////////////////////////////////////////////////////////////////////////////////

REPLACE TRNOBM02

IF ~Dead("cefald01")
Global("geniesgone","GLOBAL",1)
CheckStatGT(LastTalkedToBy,13,CHR)
Global("feudplot","GLOBAL",0)
!IsValidForPartyDialog("J#Kelsey")~

THEN BEGIN 3 
  SAY ~I suppose you are a hero and should be congratulated, my good <MANWOMAN>.  You have my thanks, even though you are not dressed as appropriate company.~
  IF ~~ THEN EXIT
END

IF ~Dead("cefald01")
!IsValidForPartyDialog("J#Kelsey")
CheckStatLT(LastTalkedToBy,14,CHR)~ THEN BEGIN 7
  SAY ~Bah!  I do not associate with common criminal rabble!~
  IF ~~ THEN DO ~RunAwayFrom(LastTalkedToBy,15)~ EXIT
END
END

APPEND TRNOBM02
	IF
		~!IsValidForPartyDialog("J#Kelsey")
		Global("GeniesGone","GLOBAL",1)
		Dead("CEFALD01")~
	THEN BEGIN J#KelseyTRNOBM02-1
		SAY ~Although... I can't say I approve of having to thank a common trader for saving Trademeet.~
		IF ~~ THEN EXTERN J#KlsyJ J#KelseyTRNOBM02-2
	END
END

APPEND J#KlsyJ
 	IF ~~ THEN BEGIN  J#KelseyTRNOBM02-2
 		SAY ~Oh, my. I don't know how I'll get to sleep tonight.~
 		IF ~~ THEN EXIT
 	END
 END


ADD_TRANS_TRIGGER TRHMER01 0 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 9 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 14 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 15 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 30 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 32 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

ADD_TRANS_TRIGGER TRHMER01 33 ~OR(2)
				GlobalGT("J#KelseyBusya","LOCALS",0)
				!IsValidForPartyDialog("J#Kelsey")~

APPEND TRHMER01
	IF ~IsValidForPartyDialog("J#Kelsey")
	    Global("J#KelseyBusya","LOCALS",0)~ THEN BEGIN KelseyBusya1
		SAY ~Kelsey? Is that you?~
		IF ~~ THEN DO ~SetGlobal("J#KelseyBusya","LOCALS",1)~ EXTERN J#KlsyJ KelseyBusya2
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya2
		SAY ~Aye, Busya...Guildmistress now, I see?~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya3
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya3
		SAY ~Yes, I have assumed the post here. And you, Kelsey? How has business found you lately?~
		IF ~Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN EXTERN J#KlsyJ KelseyBusya4
		IF ~!Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN EXTERN J#KlsyJ KelseyBusya5
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya4
		SAY ~Business has been slow, but I have never been happier. I have been travelling with <CHARNAME>...she is very dear to me.~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya6
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya5
		SAY ~Business has been slow, but the world finds me well, I would have to say. I am travelling with <CHARNAME>, at least for a time.~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya6
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya6
		SAY ~I am glad to hear it. <CHARNAME>, I hope you realize what an asset you have in our man Kelsey.~
		IF ~~ THEN REPLY ~I certainly do appreciate his talents, yes.~ GOTO KelseyBusya7
		IF ~~ THEN REPLY ~He is competent, but let's not exaggerate.~ GOTO KelseyBusya8
		IF ~~ THEN REPLY ~Actually, he is just filling space until I find someone better.~ GOTO KelseyBusya9
		IF ~Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN REPLY ~I treasure him above all else.~ GOTO KelseyBusya10
	END
END

CHAIN TRHMER01 KelseyBusya7
	~I do not know how much Kelsey has told you about his travels in the past, but this is not the first time we have met.~
	= ~Some years...four summers ago, I believe, Kelsey rode with a caravan I was leading into Trademeet. It was high value, high risk-spices, crafted goods, even a small assortment of magical goods and scrolls...carried discreetly, you understand.~
	= ~Still many miles out to the east, we were hit by raiders who knew our cargo was valuable. Their archers took out the two guard captains almost immediately, and we were being overrun.~
	= ~We were all in danger of losing a fortune, if not our lives.~
	= ~Kelsey saved us all. He seemed to be the only one who remembered the scrolls we were carrying.~
	= ~After doing what he could with his own spells, he raced over to the wagon where the scrolls were stocked, took out...what was it, again, Kelsey?~
END J#KlsyJ KelseyBusya11

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya8
		SAY ~Exaggerate? No, I don't exaggerate about people.~
		IF ~~ THEN GOTO KelseyBusya7
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya9
		SAY ~You must run your company as you see fit, but I believe you may be underestimating him.~
		IF ~~ THEN GOTO KelseyBusya7
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya10
		SAY ~Then I am pleased for the both of you.~
		IF ~~ THEN GOTO KelseyBusya7
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya11
		SAY ~Chain Lightning.~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya12
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya12
		SAY ~Yes, of course...grabbed a Chain Lightning scroll, and read it. The bandit vanguard was killed instantly, and the stragglers were cut down by our rather inspired remaining guards.~
		IF ~~ THEN EXTERN J#KlsyJ KelseyBusya13
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya13
		SAY ~It...was hardly that amazing, Busya. I simply reached for the first scroll case I could find. I could just as easily have found an Identify scroll and looked like an idiot when I tried to cast it at them.~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya14
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya14
		SAY ~It doesn't matter how you did it, Kelsey. We will always be grateful to you for your quick thinking.~
		IF ~~ THEN EXTERN J#KlsyJ KelseyBusya15
	END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya15
		SAY ~This has been...a nice ego boost, Busya.~
	IF ~~ THEN EXTERN J#KLSYJ KelseyBusya16
END
END

APPEND J#KlsyJ
	IF ~~ THEN BEGIN KelseyBusya16
		SAY ~But, perhaps you should speak with <CHARNAME> if there is any pressing business.~
		IF ~~ THEN EXTERN TRHMER01 KelseyBusya17
	END
END

APPEND TRHMER01
	IF ~~ THEN BEGIN KelseyBusya17
		SAY ~Yes, <CHARNAME>. Please let me know when you have a moment.~
		IF ~~ THEN EXIT
	END
END

//////////////////////////////////////////////////////////////////////////////////////////////////////////
