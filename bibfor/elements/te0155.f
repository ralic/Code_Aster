      SUBROUTINE TE0155(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C TOLE CRP_20
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
C     1- CALCUL FORCES ELEMENTAIRES LINEIQUES
C     2- CALCULE LE CHARGEMENT INDUIT PAR UNE ELEVATION UNIFORME DE
C        TEMPERATURE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA_PESA_R' : CHARGES DE PESANTEUR
C        'CHAR_MECA_FR1D1D' : FORCES LINEIQUES (REEL)
C        'CHAR_MECA_FF1D1D' : FORCES LINEIQUES (FONCTION)
C        'CHAR_MECA_SR1D1D' : FORCES LINEIQUES SUIVEUSES (FONCTION)
C        'CHAR_MECA_TEMP_R' : ELEVATION DE TEMPERATURE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_BARRE'       : BARRE
C        'MECA_2D_BARRE'       : BARRE
C
C
      INTEGER CODRES
      CHARACTER*4  FAMI
      CHARACTER*8  NOMPAR(4),POUM
      REAL*8       A, E, RHO, XL, TEMP, XDEP, XRIG, W(6),W2(3)
      REAL*8       PGL(3,3), FL(6), QG(6), QL(6), VALPA1(4), VALPA2(4)
      REAL*8       R8MIN,S,S2,S3,S4,S5,XXX,R8BID,VECT(6)
      INTEGER      NNO,NC,LX,LORIEN,IDEPLA,IDEPLP,I,LVECT,LSECT
      INTEGER      LMATER,LPESA,LFORC,ITEMPS,NBPAR,IRET
      INTEGER      IFCX,IADZI,IAZK24,KPG,SPT
      CHARACTER*8  NOMPAV(1),NOMAIL
      REAL*8       VALPAV(1),FCX,VITE2,VP(3),ANG1(3),U(3),V(3),INSTAN
      LOGICAL      NORMAL,GLOBAL,OKVENT

      REAL*8       KENDOG,KDESSI,SECH,HYDR
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IRET1 
      REAL*8 DDOT ,EPSTH ,R8MIEM ,SREF 
C-----------------------------------------------------------------------
      DATA         NOMPAR / 'X' , 'Y' , 'Z' , 'INST' /
      DATA         NOMPAV /'VITE'/
C     ------------------------------------------------------------------
      R8MIN = R8MIEM()
C
      NNO = 2
      NC  = 3
      FAMI = 'RIGI'
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1

C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C     --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL ---

      IF ( OPTION .EQ. 'CHAR_MECA_SR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C          ------------------------------
        CALL JEVECH('PDEPLMR','L',IDEPLA)
        CALL JEVECH('PDEPLPR','L',IDEPLP)
        IF (NOMTE.EQ.'MECA_BARRE') THEN
          DO 10 I = 1 , 3
            W(I)   = ZR(LX+I)   + ZR(IDEPLA-1+I) + ZR(IDEPLP-1+I)
            W(I+3) = ZR(LX+I+3) + ZR(IDEPLA+2+I) + ZR(IDEPLP+2+I)
            W2(I)  = W(I+3) - W(I)
10        CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          DO 11 I = 1 , 2
            W(I)   = ZR(LX+I)   + ZR(IDEPLA-1+I) + ZR(IDEPLP-1+I)
            W(I+2) = ZR(LX+I+2) + ZR(IDEPLA+1+I) + ZR(IDEPLP+1+I)
            W2(I)  = W(I+2) - W(I)
11        CONTINUE
        ENDIF
        CALL ANGVX(W2,ANG1(1),ANG1(2))
        ANG1(3) = ZR(LORIEN+2)
        CALL MATROT ( ANG1 , PGL )
      ELSE
        IF (NOMTE.EQ.'MECA_BARRE') THEN
          DO 12 I = 1 , 3
            W(I)   = ZR(LX+I)
            W(I+3) = ZR(LX+I+3)
            W2(I)  = W(I+3) - W(I)
12        CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          DO 13 I = 1 , 2
            W(I)   = ZR(LX+I)
            W(I+2) = ZR(LX+I+2)
            W2(I)  = W(I+2) - W(I)
13        CONTINUE
        ENDIF
        CALL MATROT ( ZR(LORIEN) , PGL )
      ENDIF
      IF (NOMTE.EQ.'MECA_BARRE') THEN
        S=DDOT(3,W2,1,W2,1)
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        S=DDOT(2,W2,1,W2,1)
      ENDIF
      XL = SQRT(S)
      IF( XL .EQ. 0.D0 ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     --- INITIALISATION DE FL ---
      DO 15 I =1,6
         FL(I) = 0.D0
15    CONTINUE
C
      CALL JEVECH ('PVECTUR','E',LVECT)
C
      IF ( OPTION.EQ.'CHAR_MECA_PESA_R') THEN
C
C     --- CAS DE CHARGE DE PESANTEUR ---
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH ('PCAGNBA', 'L',LSECT)
         A = ZR(LSECT)
C        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
         CALL JEVECH ('PMATERC', 'L', LMATER)
         KPG=1
         SPT=1
         POUM='+'
         CALL RCVALB('FPG1',KPG,SPT,POUM,ZI(LMATER),' ','ELAS',0,' ',
     &               R8BID,1,'RHO',RHO,CODRES, 1)
C
         CALL JEVECH('PPESANR','L',LPESA)
         DO 20 I = 1,3
             QG(I)   = RHO * ZR(LPESA) * ZR(LPESA+I)
             QG(I+3) = QG(I)
 20      CONTINUE
C
C        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE ---
         CALL UTPVGL ( NNO, NC, PGL, QG(1), QL(1))
C
C        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
C         FL(1) = QL(1) * A * XL / 2.D0
C         FL(4) = QL(4) * A * XL / 2.D0
         DO 22 I = 1 , 6
           FL(I)= QL(I) * A * XL / 2.D0
 22      CONTINUE
         CALL UTPVLG ( NNO, NC, PGL, FL(1), VECT )
      ENDIF
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
      IF (NOMTE.EQ.'MECA_BARRE') THEN
        DO 23 I=1,6
          ZR(LVECT+I-1) = VECT(I)
 23     CONTINUE
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        ZR(LVECT)     = VECT(1)
        ZR(LVECT+1)   = VECT(2)
        ZR(LVECT+2)   = VECT(4)
        ZR(LVECT+3)   = VECT(5)
      ENDIF
C
      OKVENT = .FALSE.
      IF ( OPTION .EQ. 'CHAR_MECA_FR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SR1D1D' ) THEN
C          ------------------------------
C        POUR LE CAS DU VENT
         CALL TECACH('NNN','PVITER',1,LFORC,IRET)
         IF ( LFORC .NE. 0 ) THEN
           IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
C OPTION NON PROGRAMMEE
             CALL ASSERT(.FALSE.)
            ENDIF
           NORMAL = .TRUE.
           OKVENT = .TRUE.
           GLOBAL = .TRUE.
         ELSE
           CALL JEVECH ('PFR1D1D','L',LFORC)
           XXX = ABS(ZR(LFORC+3))
           GLOBAL = XXX .LT. 1.D-3
           NORMAL = XXX .GT. 1.001D0
         ENDIF
      ELSEIF ( OPTION .EQ. 'CHAR_MECA_FF1D1D' .OR.
     &         OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C              ------------------------------
         CALL JEVECH('PFF1D1D','L',LFORC)
         NORMAL = ZK8(LFORC+3) .EQ. 'VENT'
         GLOBAL = ZK8(LFORC+3) .EQ. 'GLOBAL'
         CALL TECACH ( 'NNN', 'PTEMPSR', 1, ITEMPS,IRET )
         IF ( ITEMPS .NE. 0 ) THEN
            VALPA1(4) = ZR(ITEMPS)
            VALPA2(4) = ZR(ITEMPS)
            NBPAR = 4
         ELSE
            NBPAR = 3
         ENDIF
      END IF
C
      IF ( OPTION.EQ.'CHAR_MECA_FR1D1D' ) THEN
C     --- FORCES REPARTIES PAR VALEURS REELLES---
         DO 30 I = 1, 3
            QG(I)   =  ZR(LFORC+I-1)
            QG(I+3) =  QG(I)
30       CONTINUE
         IF ( NORMAL ) THEN
           S=DDOT(3,W2,1,W2,1)
           S2=1.D0/S
           S=DDOT(3,QG(1),1,QG(1),1)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(1),U)
             S=DDOT(3,U,1,U,1)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(1))
           ENDIF
           S=DDOT(3,QG(4),1,QG(4),1)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(4),U)
             S=DDOT(3,U,1,U,1)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(4))
           ENDIF
         ENDIF
         IF ( GLOBAL .OR. NORMAL ) THEN
            CALL UTPVGL ( NNO, NC, PGL, QG(1), QL(1))
         ELSE
            DO 32 I = 1, 6
               QL(I) = QG(I)
32          CONTINUE
         ENDIF
C
C        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
C         FL(1) = QL(1) * XL / 2.D0
C         FL(4) = QL(4) * XL / 2.D0
         DO 34 I = 1 , 6
           FL(I)= QL(I) * XL / 2.D0
34       CONTINUE
         CALL UTPVLG ( NNO, NC, PGL, FL(1),VECT)
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 35 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 35        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
      ENDIF
C
      ELSEIF ( OPTION .EQ.'CHAR_MECA_FF1D1D' .OR.
     &         OPTION .EQ.'CHAR_MECA_SF1D1D' ) THEN
C     --- FORCES REPARTIES PAR FONCTIONS ---
         DO 40 I = 1, 3
            VALPA1(I) = W(I)
            VALPA2(I) = W(I+3)
40       CONTINUE
         DO 42 I = 1, 3
           CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPA1,QG(I),
     &                 IRET)
           CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPA2,QG(I+3),
     &                 IRET)
42       CONTINUE
C
         IF ( NORMAL ) THEN
           S=DDOT(3,W2,1,W2,1)
           S2=1.D0/S
           S=DDOT(3,QG(1),1,QG(1),1)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(1),U)
             S=DDOT(3,U,1,U,1)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(1))
           ENDIF

           S=DDOT(3,QG(4),1,QG(4),1)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(4),U)
             S=DDOT(3,U,1,U,1)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(4))
           ENDIF
         ENDIF
         IF ( GLOBAL .OR. NORMAL ) THEN
            CALL UTPVGL ( NNO, NC, PGL, QG(1), QL(1) )
         ELSE
            DO 44 I = 1, 6
              QL(I) = QG(I)
44          CONTINUE
         ENDIF
C
C        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
C         FL(1) = QL(1) * XL / 2.D0
C         FL(4) = QL(4) * XL / 2.D0
         DO 46 I = 1 , 6
           FL(I)= QL(I) * XL / 2.D0
46       CONTINUE
         CALL UTPVLG ( NNO, NC, PGL, FL(1),VECT )
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 45 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 45        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
         ENDIF
C
C
      ELSEIF ( OPTION.EQ.'CHAR_MECA_SR1D1D' ) THEN
C     --- FORCES SUIVEUSES REPARTIES PAR VALEURS REELLES---

C        SEUL LE CAS DU VENT DONNE PAR 'PVITER' EST ACCEPTE
         IF ( .NOT. OKVENT ) GOTO 998
C        RECUPERATION DE LA VITESSE DE VENT RELATIVE AUX NOEUDS
         DO 50 I = 1 , 6
           QG(I)=ZR(LFORC-1+I)
50       CONTINUE
         DO 51 I = 1 , 3
           VP(I)=0.D0
51       CONTINUE

C        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
         S=DDOT(3,W2,1,W2,1)
         S2=1.D0/S

         S=DDOT(3,QG(1),1,QG(1),1)
         S4 = SQRT(S)
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,QG(1),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           VITE2=DDOT(3,VP,1,VP,1)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH('ONN','PVENTCX',1,IFCX,IRET)
             IF ( IRET .NE. 0 ) GOTO  999
             IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
             CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
             FCX = FCX / VALPAV(1)
           ENDIF
         ENDIF
         CALL PSCVEC(3,FCX,VP,QG(1))

         S=DDOT(3,QG(4),1,QG(4),1)
         S4 = SQRT(S)
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,QG(4),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           VITE2=DDOT(3,VP,1,VP,1)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH('ONN','PVENTCX',1,IFCX,IRET)
             IF ( IRET .NE. 0 ) GOTO  999
             IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
             CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
             FCX = FCX / VALPAV(1)
           ENDIF
         ENDIF
         CALL PSCVEC(3,FCX,VP,QG(4))

         CALL UTPVGL ( NNO, NC, PGL, QG(1), QL(1) )
C        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
         DO 52 I = 1 , 6
           FL(I)= QL(I) * XL / 2.D0
 52      CONTINUE
         CALL UTPVLG ( NNO, NC, PGL, FL(1),VECT )
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 53 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 53        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
         ENDIF
C

      ELSEIF ( OPTION.EQ.'CHAR_MECA_TEMP_R' ) THEN
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH ('PCAGNBA', 'L',LSECT)
         A = ZR(LSECT)
C        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
         CALL JEVECH ('PMATERC', 'L', LMATER)
         CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),' ','ELAS',
     &               0,' ',R8BID,1,'E',E,
     &               CODRES,1)
C
C        TEMPERATURE DE REFERENCE
         CALL VERIFT(FAMI,1,1,'+',ZI(LMATER),'ELAS',1,EPSTH,IRET)
C
C        TERME DE LA MATRICE ELEMENTAIRE
         XRIG = E * A / XL
C
C        DEPLACEMENT INDUIT PAR LA TEMPERATURE
         XDEP = EPSTH * XL
C
C        --- CALCUL DES FORCES INDUITES ---
         FL(1) = -XRIG * XDEP
         FL(4) =  XRIG * XDEP
         CALL UTPVLG ( NNO, NC, PGL, FL(1), VECT )
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 54 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 54        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
         ENDIF
C

      ELSEIF ( OPTION.EQ.'CHAR_MECA_SECH_R' ) THEN
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH ('PCAGNBA', 'L',LSECT)
         A = ZR(LSECT)
C        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
         CALL JEVECH ('PMATERC', 'L', LMATER)
         CALL RCVALB(FAMI,1,1,'+',ZI(LMATER),' ','ELAS',
     &               0,' ',R8BID,1,'E',E,
     &               CODRES,1)

        CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
        IF (ITEMPS.NE.0) THEN
          INSTAN = ZR(ITEMPS)
        ELSE
          INSTAN = 0.D0
         ENDIF
C
C        TEMPERATURE EFFETIVE
         CALL RCVARC(' ','TEMP','+',FAMI,1,1,TEMP,IRET)
C
         CALL RCVARC(' ','SECH','+','RIGI',1,1,SECH,IRET)
         IF (IRET.NE.0) SECH=0.D0
         CALL RCVARC(' ','SECH','REF','RIGI',1,1,SREF,IRET)
         IF (IRET.NE.0) SREF=0.D0

         NOMPAR(1) = 'TEMP'
         VALPA2(1) = TEMP
         NOMPAR(2) = 'INST'
         VALPA2(2) = INSTAN
         NOMPAR(3) = 'SECH'
         VALPA2(3) = SECH
C
C        TERME DE LA MATRICE ELEMENTAIRE
         XRIG = E * A / XL
C
C ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
C           DU SECHAGE
C           ----------------------------------------------------------
            CALL RCVALB('RIGI',1,1,'+',ZI(LMATER),' ','ELAS',
     &          3,NOMPAR,VALPA2,1,'K_DESSIC',KDESSI, CODRES, 0)
C
            IF (CODRES.NE.0) KDESSI=0.D0
C
CC        DEPLACEMENT INDUIT PAR LE SECHAGE
         XDEP = -KDESSI*(SREF-SECH) * XL
C
C        --- CALCUL DES FORCES INDUITES ---
         FL(1) = -XRIG * XDEP
         FL(4) =  XRIG * XDEP
         CALL UTPVLG ( NNO, NC, PGL, FL(1), VECT )
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 55 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 55        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
         ENDIF
C

      ELSEIF ( OPTION.EQ.'CHAR_MECA_HYDR_R' ) THEN
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
         CALL JEVECH ('PCAGNBA', 'L',LSECT)
         A = ZR(LSECT)
C        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
         CALL JEVECH ('PMATERC', 'L', LMATER)
         CALL RCVALB('RIGI',1,1,'+',ZI(LMATER),' ','ELAS',
     &               0,' ',R8BID,1,'E',E,
     &               CODRES,1)

C ---- RECUPERATION DE L'INSTANT
C      -------------------------
         CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
         IF (ITEMPS.NE.0) THEN
         INSTAN = ZR(ITEMPS)
         ELSE
         INSTAN = 0.D0
         ENDIF
C
C        TEMPERATURE EFFETIVE
         CALL RCVARC(' ','TEMP','+',FAMI,1,1,TEMP,IRET1)

C        HYDRATATION EFFECTIVE
         CALL RCVARC(' ','HYDR','+','RIGI',1,1,HYDR,IRET)
         IF (IRET.NE.0) HYDR=0.D0

      NOMPAR(1) = 'TEMP'
      VALPA2(1) = TEMP
      NOMPAR(2) = 'INST'
      VALPA2(2) = INSTAN
      NOMPAR(3) = 'HYDR'
      VALPA2(3) = HYDR
C
C        TERME DE LA MATRICE ELEMENTAIRE
         XRIG = E * A / XL
C
C ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
C           OU DE L HYDRATATION
C           ----------------------------------------------------------
            CALL RCVALB('RIGI',1,1,'+',ZI(LMATER),' ','ELAS',
     &          3,NOMPAR,VALPA2,1,
     &          'B_ENDOGE',KENDOG, CODRES, 0)
C
            IF (CODRES.NE.0) KENDOG=0.D0
C
CC        DEPLACEMENT INDUIT PAR LE SECHAGE
         XDEP = -KENDOG*HYDR * XL
C
C        --- CALCUL DES FORCES INDUITES ---
         FL(1) = -XRIG * XDEP
         FL(4) =  XRIG * XDEP
         CALL UTPVLG ( NNO, NC, PGL, FL(1), VECT )
C
C ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
C
         IF (NOMTE.EQ.'MECA_BARRE') THEN
           DO 56 I=1,6
            ZR(LVECT+I-1) = VECT(I)
 56        CONTINUE
         ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          ZR(LVECT)     = VECT(1)
          ZR(LVECT+1)   = VECT(2)
          ZR(LVECT+2)   = VECT(4)
          ZR(LVECT+3)   = VECT(5)
         ENDIF
C
      ENDIF
C
      GOTO 1000
998   CONTINUE
      CALL U2MESS('F','ELEMENTS3_34')

999   CONTINUE
      CALL U2MESS('F','ELEMENTS3_35')

 1000 CONTINUE
      END
