      SUBROUTINE I2IMAC (EPSI,CONEC,COORD,TYP,NBM,NUMAIL,
     &                   XC,YC,R,ALFINF,ALFSUP,
     &                   NBSEG,SGTOR,SGTEX,MAIL1,MAIL2,
     &                   FACOR,FACEX,PAROR,PAREX)
      IMPLICIT  NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER       NBM, NUMAIL(*)
      REAL*8        XC, YC, R, EPSI, ALFINF, ALFSUP
      CHARACTER*24  CONEC, COORD, TYP
C
C--------------SORTIES---------------------------------------------
C
      INTEGER  NBSEG, MAIL1(*), MAIL2(*), FACOR(*), FACEX(*)
      REAL*8   PAROR(*), PAREX(*), SGTOR(*), SGTEX(*)
C
C--------------VARIABLES LOCALES----------------------------------
C
C---------------------DESCRIPTION D' UNE MAILLE----------------------
C
      INTEGER     M, IMA, NBCOTE, NBNEUD, IATYMA
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
      REAL*8  ABS3,ABS4,ABR3,ABR4
      INTEGER NBPM,NBPF
C
C---------------------GESTION DE L' INTERSECTION---------------------
C
      INTEGER I,ADRGT,K,F1OR,F2OR,F1EX,F2EX,FCOM,M1,M2,PT, F1
      LOGICAL ADANSM,BDANSM,ELIMIN,ATROUV,BTROUV,FINI,FINCAL
      REAL*8  OR,EX,ROR,REX,XM,YM
      REAL*8  S,R1,XA,YA,XB,YB,SM
C
C---------------------DIVERS-----------------------------------------
C
      REAL*8 PI,R8PI
C
C---------------------ADRESSE DES OBJETS JEVEUX----------------------
C
      INTEGER  ATYPM,ACOORD,ADRVLC,AXSOM,AXINT,AYSOM,AYINT
      INTEGER  ASLOC,AR1LOC,AR2LOC,AF1LOC,AF2LOC,ACOTDR,ACONEC
C
C------------FONCTIONS D' ACCES JEVEUX-------------------------------
C
      CHARACTER*32 JEXNOM,JEXNUM
C
C------------COMMUNS NORMALISES JEVEUX-------------------------------
C
      INTEGER              ZI
      COMMON      /IVARJE/ ZI(1)
      REAL*8               ZR
      COMMON      /RVARJE/ ZR(1)
      COMPLEX*16           ZC
      COMMON      /CVARJE/ ZC(1)
      LOGICAL              ZL
      COMMON      /LVARJE/ ZL(1)
      CHARACTER*8          ZK8
      CHARACTER*16         ZK16
      CHARACTER*24         ZK24
      CHARACTER*32         ZK32
      CHARACTER*80         ZK80
      COMMON      /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C------------FIN DES COMMUNS NORMALISES JEVEUX-----------------------
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
      ABS3 = 0.0D0
      ABS4 = 0.0D0
      ABR3 = 0.0D0
      ABR4 = 0.0D0
      XD   = 0.0D0
      YD   = 0.0D0
      XI   = 0.0D0
      YI   = 0.0D0
      XF   = 0.0D0
      YF   = 0.0D0
      XM   = 0.0D0
      YM   = 0.0D0
      OR   = 0.0D0
      EX   = 0.0D0
      ROR  = 0.0D0
      REX  = 0.0D0
      S    = 0.0D0
      R1   = 0.0D0
      PI=R8PI()
C
      IF (ALFINF.GE.ALFSUP) CALL U2MESS('F','POSTRELE_15')
      IF (ALFINF.LT.-PI.OR.ALFSUP.GT.PI) CALL U2MESS('F','POSTRELE_16')
C
      XA = XC + R*COS(ALFINF)
      YA = YC + R*SIN(ALFINF)
      XB = XC + R*COS(ALFSUP)
      YB = YC + R*SIN(ALFSUP)
C
C---------RECUPERATION DES ADRESSES DES OBJETS JEVEUX-----------------
C
      CALL JEVEUO(CONEC,'L',ACONEC)
      CALL JEVEUO(COORD,'L',ACOORD)
C
C---------CREATION DES TABLEAUX LOCAUX JEVEUX-----------------------
C
      CALL JECREO('&INTERSLOC','V V R')
      CALL JEECRA('&INTERSLOC','LONMAX',16,' ')
      CALL JECREO('&INTERR1LOC','V V R')
      CALL JEECRA('&INTERR1LOC','LONMAX',16,' ')
      CALL JECREO('&INTERR2LOC','V V R')
      CALL JEECRA('&INTERR2LOC','LONMAX',16,' ')
      CALL JECREO('&INTERF1LOC','V V I')
      CALL JEECRA('&INTERF1LOC','LONMAX',16,' ')
      CALL JECREO('&INTERF2LOC','V V I')
      CALL JEECRA('&INTERF2LOC','LONMAX',16,' ')
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
C---------------AVEC LA MAILLE &INTERCOURANTE    ---------------------
C
         M  = M + 1
         IMA = NUMAIL(M)
C
         CALL JEVEUO ( JEXNUM(CONEC,IMA), 'L', ADRVLC )
         CALL JELIRA ( JEXNUM(CONEC,IMA), 'LONMAX', NBNEUD, K8B )
C
C-------------RECUPERATION DU NOM DU TYPE DE LA MAILLE COURANTE----
C
         CALL JEVEUO ( TYP, 'L', IATYMA )
         ATYPM = IATYMA - 1 + IMA
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ATYPM)),TYPM)
C
C---------------SI LA MAILLE COURANTE N' EST PAS UNE MAILLE---------
C---------------SURFACIQUE ALORS ON L' IGNORE              ---------
C
         IF ( (TYPM .NE. 'POI1') .AND.
     &        (TYPM .NE. 'SEG2') .AND.
     &        (TYPM .NE. 'SEG3')
     &      ) THEN
C
            PT = 1
C
            DO 100, I = 1, 16, 1
C
               ZR(ASLOC + I-1)  = -1.0D0
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
            CALL I2NBRF(NBNEUD,NBCOTE)
C
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
               CALL I2TYPF (EPSI,XD,YD,XI,YI,XF,YF,TYPM,DROIT)
C
               DROIT = .TRUE.
C
               ZL(ACOTDR + C-1) = DROIT
C
C-------------CALCUL DE L' INTERSECTION SEGMENT-COTE-----------------
C-------------ET RANGEMENT DES RESULTAS ELEMENTAIRES-----------------
C
               IF ( DROIT ) THEN
C
C----------------------CAS D' UN COTE DROIT--------------------------
C
                  CALL I2IACS (EPSI,XC,YC,R,ALFINF,ALFSUP,XD,YD,XF,YF,
     &                         NBPF,ABS1,ABS2,ABR1,ABR2,ELIMIN)
C
                  IF ( .NOT. ELIMIN ) THEN
C
                     IF ( NBPF .GE. 1 ) THEN
C
                        CALL I2RGEL (EPSI,ABS1,ABR1,C,
     &                               ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                               ZI(AF1LOC),ZI(AF2LOC),PT)
C
                     ENDIF
C
                     IF ( NBPF .EQ. 2 ) THEN
C
                        CALL I2RGEL (EPSI,ABS2,ABR2,C,
     &                               ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                               ZI(AF1LOC),ZI(AF2LOC),PT)
C
                     ENDIF
C
                  ENDIF
C
               ELSE
C
C-------------------CAS D' UN COTE COURBE-----------------------------
C
                  CALL I2IACC (EPSI,XC,YC,R,ALFINF,ALFSUP,
     &                         XD,YD,XI,YI,XF,YF,
     &                         NBPF,ABS1,ABS2,ABS3,ABS4,
     &                         ABR1,ABR2,ABR3,ABR4)
C
                  IF ( NBPF .GE. 1 ) THEN
C
                     CALL I2RGEL (EPSI,ABS1,ABR1,C,
     &                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
                  IF ( NBPF .GE. 2 ) THEN
C
                     CALL I2RGEL (EPSI,ABS2,ABR2,C,
     &                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
                  IF ( NBPF .GE. 3 ) THEN
C
                     CALL I2RGEL (EPSI,ABS3,ABR3,C,
     &                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
                  IF ( NBPF .EQ. 4 ) THEN
C
                     CALL I2RGEL (EPSI,ABS4,ABR4,C,
     &                            ZR(ASLOC),ZR(AR1LOC),ZR(AR2LOC),
     &                            ZI(AF1LOC),ZI(AF2LOC),PT)
C
                  ENDIF
C
               ENDIF
C
20          CONTINUE
C
            NBPM = PT - 1
            IF ( NBPM .GE. 1) THEN
            ENDIF
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
     &                     ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     OR     =  ALFINF
                     EX     =  ALFSUP
                     ROR    = -1.0D0
                     REX    = -1.0D0
                     F1OR   =  0
                     F1EX   =  0
                     M1     =  IMA
                     M2     =  0
C
                     CALL I2RGMA(EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                           SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                           MAIL1,MAIL2,ADRGT)
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
               IF ( (.NOT. ATROUV) .AND.
     &              ( ABS(S-ALFINF) .LT. EPSI ) ) THEN
C
                  CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                        ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                  IF ( BDANSM ) THEN
C
                     OR   =  ALFINF
                     EX   =  ALFSUP
                     F1OR =  F1
                     F1EX =  0
                     ROR  =  R1
                     REX  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                      SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                      MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     FINI   = .TRUE.
C
                  ENDIF
C
               ENDIF
C
               IF ((.NOT. BTROUV).AND.(ABS(ALFSUP-S).LT.EPSI)) THEN
C
                  CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                        ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     OR   =  ALFINF
                     EX   =  ALFSUP
                     F1EX =  F1
                     F1OR =  0
                     REX  =  R1
                     ROR  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                      SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                      MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
                     BTROUV = .TRUE.
                     FINI   = .TRUE.
C
                  ENDIF
C
               ENDIF
C
               IF ( ( ABS(ALFINF-S) .GT. EPSI ) .AND.
     &              ( ABS(ALFSUP-S) .GT. EPSI )
     &            ) THEN
C
                  IF ( .NOT. ATROUV ) THEN
C
                     CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                          ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                     IF ( ADANSM ) THEN
C
                        OR   =  ALFINF
                        EX   =  S
                        F1EX =  F1
                        F1OR =  0
                        REX  =  R1
                        ROR  = -1.0D0
                        M1   =  IMA
                        M2   =  0
C
                        CALL I2RGMA(EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                         SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                         MAIL1,MAIL2,ADRGT)
C
                        ATROUV = .TRUE.
C
                        CALL I2FINI (EPSI,ALFINF,ALFSUP,
     &                               SGTOR,SGTEX,MAIL2,
     &                               ADRGT,FINCAL)
C
                     ENDIF
C
                  ENDIF
C
                  IF ( .NOT. BTROUV ) THEN
C
                     CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                           ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                     IF ( BDANSM ) THEN
C
                        EX   =  ALFSUP
                        OR   =  S
                        F1OR =  F1
                        F1EX =  0
                        ROR  =  R1
                        REX  = -1.0D0
                        M1   =  IMA
                        M2   =  0
C
                        CALL I2RGMA(EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                            MAIL1,MAIL2,ADRGT)
C
                        BTROUV = .TRUE.
C
                        CALL I2FINI (EPSI,ALFINF,ALFSUP,
     &                               SGTOR,SGTEX,MAIL2,ADRGT,FINCAL)
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
                  SM = 0.5D0*(OR+EX)
                  XM = XC + R*COS(SM)
                  YM = YC + R*SIN(SM)
C
                  CALL I2APPM (XM,YM,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                        ZR(AYINT),ZL(ACOTDR),NBCOTE,ELIMIN)
C
                  IF ( ELIMIN ) THEN
C
                     ROR  = ZR(AR1LOC + K-1)
                     REX  = ZR(AR1LOC + K)
                     M1  = IMA
                     M2  = 0
C
                     IF ( FCOM .NE. 0 ) THEN
C
                        F1OR = FCOM
                        F2OR = FCOM
C
                     ENDIF
C
                     CALL I2RGMA (EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                         SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                         MAIL1,MAIL2,ADRGT)
C
                     CALL I2FINI (EPSI,ALFINF,ALFSUP,
     &                         SGTOR,SGTEX,MAIL2,ADRGT,FINCAL)
C
                     IF ( (.NOT. ATROUV)
     &                     .AND.
     &                    (
     &                      (ABS(OR-ALFINF).LT.EPSI)
     &                      .OR.
     &                      (ABS(EX-ALFINF).LT.EPSI)
     &                    )
     &                  ) THEN
C
                        ATROUV = .TRUE.
C
                     ENDIF
C
                     IF ( (.NOT. BTROUV)
     &                     .AND.
     &                    (
     &                      (ABS(OR-ALFSUP).LT.EPSI)
     &                      .OR.
     &                      (ABS(EX-ALFSUP).LT.EPSI)
     &                    )
     &                   ) THEN
C
                        BTROUV = .TRUE.
C
                     ENDIF
C
                  ENDIF
C
200            CONTINUE
C
C--------------CAS PATHOLOGIQUE DU A LA POSSIBILITE-----------------
C--------------DE PRESENCE DE A ET/OU B DANS LA    -----------------
C--------------MAILLE COURANTE, BIEN QUE CETTE     -----------------
C--------------MAILLE DONNE PLUS DE 2 POINTS       -----------------
C
               IF ( (.NOT. BTROUV)
     &               .AND.
     &              (ABS(ZR(ASLOC + NBPM-1)-ALFSUP) .GT. EPSI)
     &            ) THEN
C
                  CALL I2APPM (XB,YB,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                        ZR(AYINT),ZL(ACOTDR),NBCOTE,BDANSM)
C
                  IF ( BDANSM ) THEN
C
                     OR   =  ZR(ASLOC + NBPM-1)
                     EX   =  ALFSUP
                     F1OR =  ZI(AF1LOC + NBPM-1)
                     F1EX =  0
                     ROR  =  ZR(AR1LOC + NBPM-1)
                     REX  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                            MAIL1,MAIL2,ADRGT)
C
                     BTROUV = .TRUE.
C
                     CALL I2FINI (EPSI,ALFINF,ALFSUP,
     &                            SGTOR,SGTEX,MAIL2,ADRGT,FINCAL)
C
                  ENDIF
C
               ENDIF
C
               IF ( (.NOT. ATROUV)
     &               .AND.
     &              (ABS(ZR(ASLOC)-ALFINF) .GT. EPSI)
     &            ) THEN
C
                  CALL I2APPM (XA,YA,ZR(AXSOM),ZR(AYSOM),ZR(AXINT),
     &                        ZR(AYINT),ZL(ACOTDR),NBCOTE,ADANSM)
C
                  IF ( ADANSM ) THEN
C
                     EX   =  ZR(ASLOC)
                     OR   =  ALFINF
                     F1EX =  ZI(AF1LOC)
                     F1OR =  0
                     REX  =  ZR(AR1LOC)
                     ROR  = -1.0D0
                     M1   =  IMA
                     M2   =  0
C
                     CALL I2RGMA (EPSI,OR,EX,ROR,REX,M1,M2,F1OR,F1EX,
     &                            SGTOR,SGTEX,PAROR,PAREX,FACOR,FACEX,
     &                            MAIL1,MAIL2,ADRGT)
C
                     ATROUV = .TRUE.
C
                     CALL I2FINI (EPSI,ALFINF,ALFSUP,
     &                            SGTOR,SGTEX,MAIL2,ADRGT,FINCAL)
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
C
         GOTO 10
C
      ENDIF
C
      NBSEG = ADRGT - 1
C
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
