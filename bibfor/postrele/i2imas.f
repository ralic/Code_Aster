      SUBROUTINE I2IMAS (EPSI,CONEC,COORD,TYP,NBM,NUMAIL,XA,YA,
     +                   XB,YB,NBSEG,SGTOR,SGTEX,MAIL1,MAIL2,
     +                   FACOR,FACEX,PAROR,PAREX)
      IMPLICIT   NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C
C
C--------------ENTREES----------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER       NBM, NUMAIL(*)
      REAL*8        XA,YA,XB,YB,EPSI
      CHARACTER*24  CONEC,COORD,TYP
C
C--------------SORTIES---------------------------------------------
C
      INTEGER NBSEG,MAIL1(*),MAIL2(*),FACOR(*),FACEX(*)
      REAL*8  PAROR(*),PAREX(*),SGTOR(*),SGTEX(*)
C
C--------------VARIABLES LOCALES----------------------------------
C
C---------------------DESCRIPTION D' UNE MAILLE----------------------
C
      INTEGER     M, NBCOTE, NBNEUD, IMA, IATYMA
      CHARACTER*8 TYPM, K8B
C
C---------------------DESCRIPTION D' UNE FACE------------------------
C
      INTEGER NUMS,ADRS,C
      REAL*8  XD,YD,XF,YF,XI,YI
      LOGICAL DROIT
C
C---------------------DESCRIPTION DE L' INTERSECTION AVEC UNE MAILLE--
C
      REAL*8  ABS1,ABS2,ABR1,ABR2
      INTEGER NBPM,NBPF
C
C---------------------GESTION DE L' INTERSECTION---------------------
C
      INTEGER PT,I,ADRGT,K,F1OR,F2OR,F1EX,F2EX,FCOM,M1,M2
      LOGICAL ADANSM,BDANSM,ELIMIN,ATROUV,BTROUV,FINI,FINCAL
      REAL*8  OR,EX,ROR,REX,XM,YM,XT,YT,INDIC,COEF, CRIT
      REAL*8  S,R1,XNEWM,YNEWM,XNEWT,YNEWT,INF,SUP,SM
      INTEGER F1
C
C---------------------ADRESSE DES OBJETS JEVEUX----------------------
C
      INTEGER  ATYPM,ACOORD,ADRVLC,AXSOM,AXINT,AYSOM,AYINT
      INTEGER ASLOC,AR1LOC,AR2LOC,AF1LOC,AF2LOC,ACOTDR,ACONEC
C
C------------FONCTIONS D' ACCES JEVEUX-------------------------------
C
C
C
C
C
C
C---------INITIALISATION---------------------------------------------
C
      CALL JEMARQ()
      NBPF   = 0
      NBPM   = 0
      M      = 0
      M1     = 0
      M2     = 0
      C      = 0
      K      = 1
      I      = 1
      PT     = 1
      ADRGT  = 1
      ADRS   = 0
      NUMS   = 0
      FCOM   = 0
      F1OR   = 0
      F2OR   = 0
      F1EX   = 0
      F2EX   = 0
      F1     = 0
C
      DROIT  = .FALSE.
      ADANSM = .FALSE.
      BDANSM = .FALSE.
      ATROUV = .FALSE.
      BTROUV = .FALSE.
      ELIMIN = .FALSE.
      FINI   = .FALSE.
      FINCAL = .FALSE.
C
      ABS1 = 0.0D0
      ABS2 = 0.0D0
      ABR1 = 0.0D0
      ABR2 = 0.0D0
      XD   = 0.0D0
      YD   = 0.0D0
      XI   = 0.0D0
      YI   = 0.0D0
      XF   = 0.0D0
      YF   = 0.0D0
      XM   = 0.0D0
      YM   = 0.0D0
      XT   = 0.0D0
      YT   = 0.0D0
      OR   = 0.0D0
      EX   = 0.0D0
      ROR  = 0.0D0
      REX  = 0.0D0
      R1   = 0.0D0
C
      INF = 0.0D0
      SUP = 1.0D0
C
      INDIC = 1.0D0
      COEF  = 0.0D0
C
C---------RECUPERATION DES ADRESSE DES OBJETS JEVEUX-----------------
C
      CALL JEVEUO(CONEC,'L',ACONEC)
      CALL JEVEUO(COORD,'L',ACOORD)
C
C---------CREATION DES TABLEAUX LOCAUX JEVEUX-----------------------
C
      CALL JECREO('&INTERSLOC','V V R')
      CALL JEECRA('&INTERSLOC','LONMAX',8,' ')
      CALL JECREO('&INTERR1LOC','V V R')
      CALL JEECRA('&INTERR1LOC','LONMAX',8,' ')
      CALL JECREO('&INTERR2LOC','V V R')
      CALL JEECRA('&INTERR2LOC','LONMAX',8,' ')
      CALL JECREO('&INTERF1LOC','V V I')
      CALL JEECRA('&INTERF1LOC','LONMAX',8,' ')
      CALL JECREO('&INTERF2LOC','V V I')
      CALL JEECRA('&INTERF2LOC','LONMAX',8,' ')
      CALL JECREO('&INTERCOTDR','V V L')
      CALL JEECRA('&INTERCOTDR','LONMAX',4,' ')
      CALL JECREO('&INTERXSOM' ,'V V R')
      CALL JEECRA('&INTERXSOM' ,'LONMAX',5,' ')
      CALL JECREO('&INTERYSOM' ,'V V R')
      CALL JEECRA('&INTERYSOM' ,'LONMAX',5,' ')
      CALL JECREO('&INTERXINT' ,'V V R')
      CALL JEECRA('&INTERXINT' ,'LONMAX',4,' ')
      CALL JECREO('&INTERYINT' ,'V V R')
      CALL JEECRA('&INTERYINT' ,'LONMAX',4,' ')
C
C---------RECUPERATION DES ADRESSES DES TABLEAUX LOCAUX-----------
C
      CALL JEVEUO('&INTERSLOC', 'E',ASLOC)
      CALL JEVEUO('&INTERR1LOC','E',AR1LOC)
      CALL JEVEUO('&INTERR2LOC','E',AR2LOC)
      CALL JEVEUO('&INTERF1LOC','E',AF1LOC)
      CALL JEVEUO('&INTERF2LOC','E',AF2LOC)
      CALL JEVEUO('&INTERCOTDR','E',ACOTDR)
      CALL JEVEUO('&INTERXSOM', 'E',AXSOM)
      CALL JEVEUO('&INTERYSOM', 'E',AYSOM)
      CALL JEVEUO('&INTERXINT', 'E',AXINT)
      CALL JEVEUO('&INTERYINT', 'E',AYINT)
C
C---------BOUCLE DE BALAYAGE DES MAILLES-----------------------------
C
10    CONTINUE
      IF ( .NOT. FINI ) THEN
C
C---------------INITIALISATION DE L' INTERSECTION---------------------
C---------------AVEC LA MAILLE COURANTE          ---------------------
C
         M  = M + 1
         IMA = NUMAIL(M)
C
         CALL JEVEUO ( JEXNUM(CONEC,IMA), 'L', ADRVLC )
         CALL JELIRA ( JEXNUM(CONEC,IMA), 'LONMAX', NBNEUD, K8B )
C
C---------------RECUPERATION DU NOM DU TYPE DE LA MAILLE COURANTE----
C
         CALL JEVEUO ( TYP, 'L', IATYMA )
         ATYPM = IATYMA - 1 + IMA
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ATYPM)),TYPM)
C
C---------------SI LA MAILLE COURANTE N' EST PAS UNE MAILLE---------
C---------------SURFACIQUE ALORS ON L' IGNORE              ---------
C
         IF ( ( TYPM .NE. 'POI1' )  .AND.
     +        ( TYPM .NE. 'SEG2' )  .AND.
     +        ( TYPM .NE. 'SEG3' )  ) THEN
C
            PT = 1
C
            DO 100, I = 1, 8, 1
C
               ZR(ASLOC  + I-1) = -1.0D0
               ZR(AR1LOC + I-1) = -1.0D0
               ZR(AR2LOC + I-1) = -1.0D0
               ZI(AF1LOC + I-1) =  0
               ZI(AF2LOC + I-1) =  0
C
100         CONTINUE
C
            DO 110, I = 1, 4, 1
C
               ZL(ACOTDR + I-1) = .FALSE.
C
110         CONTINUE
C
C---------------SAISIE DE LA MAILLE COURANTE--------------------------
C
            CALL I2NBRF ( NBNEUD , NBCOTE )
C
CC
            DO 120, I = 1, NBCOTE, 1
C
               NUMS = ZI(ADRVLC+I-1)
               ADRS = 3*(NUMS-1)
C
               ZR(AXSOM + I-1) = ZR(ACOORD + ADRS)
               ZR(AYSOM + I-1) = ZR(ACOORD + ADRS+1)
C
               IF ( NBNEUD .GT. NBCOTE ) THEN
C
                  NUMS = ZI(ADRVLC+I-1+ NBCOTE)
                  ADRS = 3*(NUMS-1)
C
                  ZR(AXINT + I-1) = ZR(ACOORD + ADRS)
                  ZR(AYINT + I-1) = ZR(ACOORD + ADRS+1)
C
               ELSE
C
                  ZR(AXINT + I-1) = 0.0D0
                  ZR(AYINT + I-1) = 0.0D0
C
               ENDIF
C
120         CONTINUE
C
            ZR(AXSOM + NBCOTE) = ZR(AXSOM)
            ZR(AYSOM + NBCOTE) = ZR(AYSOM)
C
C-------------BOUCLE DE BALAYAGE DES COTES DE LA MAILLE M-----------
C
            XF = ZR(AXSOM)
            YF = ZR(AYSOM)
C
            DO 20, C = 1, NBCOTE, 1
C
C-------------SAISIE DU COTE COURANT-------------------------------
C
               XD = XF
               YD = YF
               XF = ZR(AXSOM + C)
               YF = ZR(AYSOM + C)
               XI = ZR(AXINT + C-1)
               YI = ZR(AYINT + C-1)
C
               CRIT = SQRT( (XF-XD)**2 + (YF-YD)**2 )
               CRIT = EPSI * CRIT
C
               CALL I2TYPF (CRIT,XD,YD,XI,YI,XF,YF,TYPM,DROIT)
C
               ZL(ACOTDR + C-1) = DROIT
C
C
C-------------CALCUL DE L' INTERSECTION SEGMENT-COTE-----------------
C-------------ET RANGEMENT DES RESULTAS ELEMENTAIRES-----------------
C
               IF ( DROIT ) THEN
C
C----------------------CAS D' UN COTE DROIT--------------------------
C
                  CALL I2ISGT (CRIT,XA,YA,XB,YB,XD,YD,XF,YF,
     +                         NBPF,ABS1,ABS2,ABR1,ABR2)
C
                  IF ( NBPF .GE. 1 ) THEN
C
                     CALL I2RGEL (CRIT,ABS1,ABR1,C,
     +                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     +                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
                  IF ( NBPF .EQ. 2 ) THEN
C
                     CALL I2RGEL (CRIT,ABS2,ABR2,C,
     +                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     +                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
               ELSE
C
C-------------------CAS D' UN COTE COURBE-----------------------------
C
                  CALL I2ISGC (CRIT,XA,YA,XB,YB,XD,YD,XI,YI,XF,YF,
     +                         NBPF,ABS1,ABS2,ABR1,ABR2,ELIMIN)
C
                  IF ( .NOT. ELIMIN ) THEN
C
                     IF ( NBPF .GE. 1 ) THEN
C
                       CALL I2RGEL (CRIT,ABS1,ABR1,C,
     +                               ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     +                               ZI(AF1LOC),ZI(AF2LOC),PT)
C
                     ENDIF
C
                     IF ( NBPF .EQ. 2 ) THEN
C
                        CALL I2RGEL (CRIT,ABS2,ABR2,C,
     +                               ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     +                               ZI(AF1LOC),ZI(AF2LOC),PT)
C
                     ENDIF
C
                  ENDIF
C
               ENDIF
C
20          CONTINUE
C
            NBPM = PT - 1
C
C--------------RANGEMENT DE L' INTERSECTION ELEMENTAIRE--------------
C--------------DANS LA STRUCTURE DE DONNEES ASSOCIEE A --------------
C--------------L' INTERSECTION GLOBALE                 --------------
C
            IF ( NBPM .EQ. 0 ) THEN
C
               IF ( .NOT. ATROUV ) THEN
C
                  CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                     ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     OR     =  0.0D0
                     EX     =  1.0D0
                     ROR    = -1.0D0
                     REX    = -1.0D0
                     F1OR   =  0
                     F1EX   =  0
                     M1     =  IMA
                     M2     =  0
C
                     CALL I2RGMA(CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                           SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                           MAIL1,MAIL2,ADRGT)
C
                     FINI = .TRUE.
C
                  ENDIF
C
               ENDIF
C
            ENDIF
C
            IF ( NBPM .EQ. 1 ) THEN
C
               S  = ZR(ASLOC)
               R1 = ZR(AR1LOC)
               F1 = ZI(AF1LOC)
C
               IF ( (.NOT. ATROUV) .AND. ( ABS(S) .LT. CRIT ) ) THEN
C
                  CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                        ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                  IF ( BDANSM ) THEN
C
                     OR   =  0.0D0
                     EX   =  1.0D0
                     F1OR =  F1
                     F1EX =  0
                     ROR  =  R1
                     REX  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                      SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                      MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     FINI   = .TRUE.
C
                  ENDIF
C
               ENDIF
C
               IF ( (.NOT. BTROUV) .AND. (ABS(1.0D0-S) .LT. CRIT) ) THEN
C
                  CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                        ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     OR   =  0.0D0
                     EX   =  1.0D0
                     F1EX =  F1
                     F1OR =  0
                     REX  =  R1
                     ROR  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                      SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                      MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     FINI   = .TRUE.
C
                  ENDIF
C
               ENDIF
C
               IF ((ABS(S).GT.CRIT) .AND. (ABS(1.0D0-S).GT.CRIT)) THEN
C
                  IF ( .NOT. ATROUV ) THEN
C
                     CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                          ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                     IF ( ADANSM ) THEN
C
                        OR   =  0.0D0
                        EX   =  S
                        F1EX =  F1
                        F1OR =  0
                        REX  =  R1
                        ROR  = -1.0D0
                        M1   =  IMA
                        M2   =  0
C
                        CALL I2RGMA(CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                         SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                         MAIL1,MAIL2,ADRGT)
C
                        ATROUV = .TRUE.
C
                        CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                               ADRGT,FINCAL)
C
                     ENDIF
C
                  ENDIF
C
                  IF ( .NOT. BTROUV ) THEN
C
                     CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                           ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                     IF ( BDANSM ) THEN
C
                        EX   =  1.0D0
                        OR   =  S
                        F1OR =  F1
                        F1EX =  0
                        ROR  =  R1
                        REX  = -1.0D0
                        M1   =  IMA
                        M2   =  0
C
                        CALL I2RGMA(CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                            MAIL1,MAIL2,ADRGT)
C
                        BTROUV = .TRUE.
C
                        CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                               ADRGT,FINCAL)
C
                     ENDIF
C
                  ENDIF
C
               ENDIF
C
            ENDIF
C
            IF ( NBPM .GE. 2 ) THEN
C
C--------------TRAITEMENT DES NBPM-1 SOUS SEGMENTS-------------------
C--------------DONNES PAR LA MAILLE COURANTE      -------------------
C
               EX   = ZR(ASLOC)
               F1EX = ZI(AF1LOC)
               F2EX = ZI(AF2LOC)
C
               DO 200, K = 1, NBPM-1, 1
C
                  OR   = EX
                  EX   = ZR(ASLOC + K)
                  F1OR = F1EX
                  F2OR = F2EX
                  F1EX = ZI(AF1LOC + K)
                  F2EX = ZI(AF2LOC + K)
                  FCOM = 0
C
                  IF ( F1OR .EQ. F1EX ) THEN
C
                     FCOM = F1OR
                     ROR  = ZR(AR1LOC + K-1)
                     REX  = ZR(AR1LOC + K)
C
                  ELSE IF ( F1OR .EQ. F2EX ) THEN
C
                     FCOM = F1OR
                     ROR  = ZR(AR1LOC + K-1)
                     REX  = ZR(AR2LOC + K)
C
                  ELSE IF ( F2OR .EQ.F1EX ) THEN
C
                     FCOM = F2OR
                     ROR  = ZR(AR2LOC + K-1)
                     REX  = ZR(AR1LOC + K)
C
                  ELSE IF ((F2OR .NE. 0) .AND. (F2OR .EQ. F2EX)) THEN
C
                     FCOM = F2OR
                     ROR  = ZR(AR2LOC + K-1)
                     REX  = ZR(AR2LOC + K)
C
                  ELSE
C
                  ENDIF
C
                  IF (FCOM .EQ. 0) THEN
C
                     ROR  = ZR(AR1LOC + K-1)
                     REX  = ZR(AR1LOC + K)
                     M1  = IMA
                     M2  = 0
C
                     CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                            MAIL1,MAIL2,ADRGT)
C
                     CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                            ADRGT,FINCAL)
C
                     IF ( .NOT. ATROUV     .AND.
     +                    ( ABS(OR).LT.CRIT .OR. ABS(EX).LT.CRIT )
     +                  ) THEN
C
                        ATROUV = .TRUE.
C
                     ENDIF
C
                     IF ( .NOT. BTROUV      .AND.
     +             ( ABS(1.0D0-OR).LT.CRIT .OR. ABS(1.0D0-EX).LT.CRIT )
     +                  ) THEN
C
                        BTROUV = .TRUE.
C
                     ENDIF
C
                  ENDIF
C
                  IF ( FCOM .NE. 0 ) THEN
C
                     IF ( ZL(ACOTDR + FCOM-1) ) THEN
C
                        M1 =  IMA
                        M2 = -1
C
                        CALL I2RGMA(CRIT,OR,EX,ROR,REX,M1,M2,FCOM,FCOM,
     +                             SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                           MAIL1,MAIL2,ADRGT)
C
                        CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                               ADRGT,FINCAL)
C
                        IF ( .NOT. ATROUV     .AND.
     +                       ( ABS(OR).LT.CRIT .OR. ABS(EX).LT.CRIT )
     +                     ) THEN
C
                           ATROUV = .TRUE.
C
                        ENDIF
C
                        IF ( .NOT. BTROUV      .AND.
     +             ( ABS(1.0D0-OR).LT.CRIT .OR. ABS(1.0D0-EX).LT.CRIT )
     +                     ) THEN
C
                              BTROUV = .TRUE.
C
                        ENDIF
C
                     ELSE
C
                        SM = 0.5D0*(OR+EX)
                        XM = SM*(XB-XA) + XA
                        YM = SM*(YB-YA) + YA
C
                        IF ( FCOM .NE. NBCOTE ) THEN
C
                           XT = ZR(AXSOM + FCOM+1)
                           YT = ZR(AYSOM + FCOM+1)
C
                        ELSE
C
                           XT = ZR(AXSOM + 1)
                           YT = ZR(AYSOM + 1)
C
                        ENDIF
C
                        XD = ZR(AXSOM + FCOM-1)
                        YD = ZR(AYSOM + FCOM-1)
                        XF = ZR(AXSOM + FCOM)
                        YF = ZR(AYSOM + FCOM)
                        XI = ZR(AXINT + FCOM-1)
                        YI = ZR(AYINT + FCOM-1)
C
                        CALL I2CHAX(XD,YD,XI,YI,XF,YF,XM,YM,XT,YT,
     +                              COEF,XNEWM,YNEWM,XNEWT,YNEWT)
C
                        INDIC = YNEWM-COEF*XNEWM*XNEWM
                        INDIC = INDIC * (YNEWT-COEF*XNEWT*XNEWT)
C
                        IF ( INDIC .GE. 0.0D0 ) THEN
C
                           M1 = IMA
                           M2 = 0
C
                           CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,FCOM,
     +                                  FCOM,SGTOR,SGTEX,PAROR,PAREX,
     +                                  FACOR,FACEX,MAIL1,MAIL2,ADRGT)
C
                           CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                                  ADRGT,FINCAL)
C
                           IF ( .NOT. ATROUV      .AND.
     +                         ( ABS(OR).LT.CRIT .OR. ABS(EX).LT.CRIT )
     +                        ) THEN
C
                              ATROUV = .TRUE.
C
                           ENDIF
C
                           IF ( .NOT. BTROUV      .AND.
     +             ( ABS(1.0D0-OR).LT.CRIT .OR. ABS(1.0D0-EX).LT.CRIT )
     +                        ) THEN
C
                                 BTROUV = .TRUE.
C
                           ENDIF
C
                        ENDIF
C
                     ENDIF
C
                  ENDIF
C
200            CONTINUE
C
C--------------CAS PATHOLOGIQUE DU A LA POSSIBILITE-----------------
C--------------DE PERTE DE CONVEXITE DE LA MAILLE  -----------------
C--------------COURANTE   PERTE ASSOCIEE A LA      -----------------
C--------------PRESENCE D' UNE FACE COURBE         -----------------
C
               IF ( .NOT. BTROUV    .AND.
     +              ABS(ZR(ASLOC + NBPM-1)-1.0D0) .GT. CRIT   ) THEN
C
                  CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                        ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                  IF ( BDANSM ) THEN
C
                     OR   =  ZR(ASLOC + NBPM-1)
                     EX   =  1.0D0
                     F1OR =  ZI(AF1LOC + NBPM-1)
                     F1EX =  0
                     ROR  =  ZR(AR1LOC + NBPM-1)
                     REX  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                            MAIL1,MAIL2,ADRGT)
C
                     BTROUV = .TRUE.
C
                     CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                            ADRGT,FINCAL)
C
                  ENDIF
C
               ENDIF
C
               IF ( .NOT. ATROUV               .AND.
     +              ABS(ZR(ASLOC)) .GT. CRIT   .AND.
     +              ABS(1.0D0-ZR(AR1LOC)) .GT. CRIT   ) THEN
C
                  CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     +                        ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     EX   =  ZR(ASLOC)
                     OR   =  0.0D0
                     F1EX =  ZI(AF1LOC)
                     F1OR =  0
                     REX  =  ZR(AR1LOC)
                     ROR  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (CRIT,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     +                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     +                            MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
C
                     CALL I2FINI (CRIT,INF,SUP,SGTOR,SGTEX,MAIL2,
     +                            ADRGT,FINCAL)
C
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
         FINI = ( FINI .OR. ( M .EQ. NBM ) .OR. FINCAL)
C
         GOTO 10
C
      ENDIF
C
      NBSEG = ADRGT - 1
C
      CALL JEDETR('&INTERSLOC')
      CALL JEDETR('&INTERR1LOC')
      CALL JEDETR('&INTERR2LOC')
      CALL JEDETR('&INTERF1LOC')
      CALL JEDETR('&INTERF2LOC')
      CALL JEDETR('&INTERCOTDR')
      CALL JEDETR('&INTERXSOM')
      CALL JEDETR('&INTERYSOM')
      CALL JEDETR('&INTERXINT')
      CALL JEDETR('&INTERYINT')
C
      CALL JEDEMA()
      END
