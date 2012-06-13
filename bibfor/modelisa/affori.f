        SUBROUTINE AFFORI(TYP,NOMT,CARA,VAL,JAD,JDNO,JDCO,IVR,
     &                    NUTYMA,NTSEG,CARORI,NCO,IER)
        IMPLICIT        NONE
      INCLUDE 'jeveux.h'
        INTEGER         NCO,IVR(*),NUTYMA,NTSEG,JAD,JDNO,JDCO,IER
        CHARACTER*(*)   TYP,NOMT,CARA,CARORI(NCO)
        REAL*8          VAL(6)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C       ----------------------------------------------------------------
C       AFFECTATION DES ORIENTATIONS AUX POI1 ET SEG2 POSSIBLES
C       DANS LE VECTEUR TAMPON TMPORI
C       ----------------------------------------------------------------
C
        CHARACTER*16    CMD,CAR,NOM
        CHARACTER*24    VALK(2)
        INTEGER         IBID,NO1,NO2,LG,I
        REAL*8          X1(3),X2(3),X3(3),ANGL(3),VALR(2)
        REAL*8          ALPHA,BETA,GAMMA
        REAL*8          R8DGRD,DGRD
        REAL*8          R8MIEM,TST,R8PREM
        COMMON          /OPCARA/ CMD
C       ----------------------------------------------------------------
C
        DGRD = R8DGRD()
        TST  = R8MIEM()
        CAR  = CARA
        NOM  = NOMT
C --- ------------------------------------------------------------------
C ---   VERIFS PRELIMINAIRES
C       0 = PAS VERIF, 1 = VERIF, (1) = MAILLE, (2) = NOEUD, (3) = IMPR
C
C --- ------------------------------------------------------------------
C --- CALCUL DE LA LONGUEUR DU SEGMENT
      IF(TYP(1:6).EQ.'MAILLE')THEN
         IF(NUTYMA.EQ.NTSEG)THEN
            NO1 = ZI(JDNO)
            NO2 = ZI(JDNO+1)
            DO 10 I = 1 , 3
               X1(I) = ZR(JDCO+(NO1-1)*3+I-1)
               X2(I) = ZR(JDCO+(NO2-1)*3+I-1)
 10         CONTINUE
            CALL VDIFF(3,X2,X1,X3)
            IF(   ABS(X3(1)).GT.TST .OR.
     &            ABS(X3(2)).GT.TST .OR.
     &            ABS(X3(3)).GT.TST )THEN
               LG = 1
            ELSE
               LG = 0
            ENDIF
         ELSE
            LG = 0
         ENDIF
      ENDIF

C --- ------------------------------------------------------------------
C ---   CARA = "ANGL_VRIL"
      IF(CAR.EQ.CARORI(4))THEN
         GAMMA = DGRD * VAL(1)
C --- ------------------------------------------------------------------
C ---    MAILLE
         IF(TYP(1:6).EQ.'MAILLE')THEN
C ---       SI LA MAILLE N EST PAS UN SEG2 > RETURN
            IF(NUTYMA.NE.NTSEG)THEN
               IF( IVR(1) .EQ. 1 )THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_87', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ENDIF
C ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NULLE > RETURN
            IF(LG.EQ.0)THEN
               IF( IVR(1) .EQ. 1 )THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_88', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ENDIF
C --- ------------------------------------------------------------------
C ---    REGLES DE SURCHARGE
            IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+2)
               VALR(2) = GAMMA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
C ---       SI MAILLE=SEG2, LONGUEUR<>0 : AFFECTATION DE GAMMA SEUL
            ZR(JAD+2) = GAMMA
            GOTO 9999
         ELSE
C --- ------------------------------------------------------------------
C ---    NOEUD
C --        PAS D AFFECTATION SUR UN NOEUD POI1 > RETURN
            IF( IVR(2) .EQ. 1 )THEN
               VALK(1) = CAR
               VALK(2) = NOM
               CALL U2MESK('A','MODELISA_89', 2 ,VALK)
               IER = IER + 1
            ENDIF
            GOTO 9999
         ENDIF
      ENDIF

C --- ------------------------------------------------------------------
C --- CARA = "ANGL_NAUT"
      IF( CAR.EQ.CARORI(3) )THEN
         ALPHA = DGRD * VAL(1)
         BETA  = DGRD * VAL(2)
         GAMMA = DGRD * VAL(3)
C --- ------------------------------------------------------------------
C ----   MAILLE
         IF(TYP(1:6).EQ.'MAILLE')THEN
C ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NON NULLE > RETURN
            IF(LG.EQ.1)THEN
               IF( IVR(1) .EQ. 1 ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_90', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ELSE
C ---          ---------------------------------------------------------
C ---          REGLES DE SURCHARGE
               IF ( ABS(ZR(JAD)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD)
                  VALR(2) = ALPHA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
               IF ( ABS(ZR(JAD+1)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD+1)
                  VALR(2) = BETA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
               IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD+2)
                  VALR(2) = GAMMA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
C ---          ---------------------------------------------------------
C ---          SI MAILLE SEG2 (LONGUEUR=0) OU POI1 : 3 ANGLES
               ZR(JAD)   = ALPHA
               ZR(JAD+1) = BETA
               ZR(JAD+2) = GAMMA
               GOTO 9999
            ENDIF
         ELSE
C --- ------------------------------------------------------------------
C ---    NOEUD
C ---       ------------------------------------------------------------
C ---       REGLES DE SURCHARGE
            IF ( ABS(ZR(JAD)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD)
               VALR(2) = ALPHA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
            IF ( ABS(ZR(JAD+1)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+1)
               VALR(2) = BETA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
            IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+2)
               VALR(2) = GAMMA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
C ---       ------------------------------------------------------------
C --        SI NOEUD (POI1) > AFFECTATION DES 3 ANGLES
            ZR(JAD)   = ALPHA
            ZR(JAD+1) = BETA
            ZR(JAD+2) = GAMMA
            GOTO 9999
         ENDIF
      ENDIF

C --- ------------------------------------------------------------------
C --- CARA = "VECT_X_Y"
      IF(CAR.EQ.CARORI(2))THEN
C --- ------------------------------------------------------------------
C ---    MAILLE
         IF(TYP(1:6).EQ.'MAILLE')THEN
C ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NON NULLE > RETURN
            IF(LG.EQ.1)THEN
               IF( IVR(1) .EQ. 1 )THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_90', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ELSE
C ---       SI MAILLE SEG2 (LONGUEUR=0) OU POI1 : 3 ANGLES
               CALL ANGVXY ( VAL(1), VAL(4), ANGL )
               ALPHA = ANGL(1)
               BETA  = ANGL(2)
               GAMMA = ANGL(3)
C ---          ---------------------------------------------------------
C ---          REGLES DE SURCHARGE
               IF ( ABS(ZR(JAD)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD)
                  VALR(2) = ALPHA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
               IF ( ABS(ZR(JAD+1)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD+1)
                  VALR(2) = BETA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
               IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  VALR(1) = ZR(JAD+2)
                  VALR(2) = GAMMA
                  CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
               ENDIF
C ---          ---------------------------------------------------------
               ZR(JAD)   = ALPHA
               ZR(JAD+1) = BETA
               ZR(JAD+2) = GAMMA
               GOTO 9999
            ENDIF
         ELSE
C --- ------------------------------------------------------------------
C ---    NOEUD
C ---       SI NOEUD (POI1) > AFFECTATION DES 3 ANGLES
            CALL ANGVXY ( VAL(1), VAL(4), ANGL )
            ALPHA = ANGL(1)
            BETA  = ANGL(2)
            GAMMA = ANGL(3)
C ---       ------------------------------------------------------------
C ---       REGLES DE SURCHARGE
            IF ( ABS(ZR(JAD)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD)
               VALR(2) = ALPHA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
            IF ( ABS(ZR(JAD+1)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+1)
               VALR(2) = BETA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
            IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+2)
               VALR(2) = GAMMA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
C ---       ------------------------------------------------------------
            ZR(JAD)   = ALPHA
            ZR(JAD+1) = BETA
            ZR(JAD+2) = GAMMA
            GOTO 9999
         ENDIF
      ENDIF

C --- ------------------------------------------------------------------
C --- CARA = "VECT_Y"
      IF(CAR.EQ.CARORI(1))THEN
C --- ------------------------------------------------------------------
C ---    MAILLE
         IF(TYP(1:6).EQ.'MAILLE')THEN
C ---       SI LA MAILLE N EST PAS UN SEG2 > RETURN
            IF(NUTYMA.NE.NTSEG)THEN
               IF( IVR(1) .EQ. 1 )THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_91', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ENDIF
C ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NULLE > GOTO 9999
            IF(LG.EQ.0)THEN
               IF( IVR(1) .EQ. 1 )THEN
                  VALK(1) = CAR
                  VALK(2) = NOM
                  CALL U2MESK('A','MODELISA_88', 2 ,VALK)
                  IER = IER + 1
               ENDIF
               GOTO 9999
            ENDIF
C ---       SI MAILLE = SEG2 LONGUEUR<>0 : AFFECTATION DE GAMMA SEUL
            CALL ANGVXY ( X3, VAL(1), ANGL )
            ALPHA = ANGL(1)
            BETA  = ANGL(2)
            GAMMA = ANGL(3)
C ---       ---------------------------------------------------------
C ---       REGLES DE SURCHARGE
            IF ( ABS(ZR(JAD+2)) .GT. R8PREM() ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               VALR(1) = ZR(JAD+2)
               VALR(2) = GAMMA
               CALL U2MESG('A','MODELISA2_7',2,VALK,0,IBID,2,VALR)
            ENDIF
C ---       ---------------------------------------------------------
            ZR(JAD+2) = GAMMA
            GOTO 9999
         ELSE
C --- ------------------------------------------------------------------
C ---    NOEUD
C ---       PAS D AFFECTATION SUR UN NOEUD POI1 > RETURN
            IF( IVR(2) .EQ. 1 ) THEN
               VALK(1) = CAR
               VALK(2) = NOM
               CALL U2MESK('A','MODELISA_89', 2 ,VALK)
               IER = IER + 1
            ENDIF
            GOTO 9999
         ENDIF
      ENDIF
C
 9999   CONTINUE
        END
