      SUBROUTINE TE0155(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/10/2001   AUTEUR MJBHHPE J.L.FLEJOU 
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*2  CODRES
      CHARACTER*8  NOMPAR(4)
      CHARACTER*16 CH16
      REAL*8       A, E, RHO, ALPHAT, XL, TEMP, XDEP, XRIG, W(6),W2(3)
      REAL*8       PGL(3,3), FL(6), QG(6), QL(6), VALPA1(4), VALPA2(4)
      REAL*8       R8MIN,S,S2,S3,S4,S5,XXX,R8BID,VECT(6)
      INTEGER      NNO,NC,LX,LORIEN,IDEPLA,IDEPLP,I,LVECT,LSECT
      INTEGER      LMATER,LPESA,LFORC,ITEMPS,NBPAR,IRET,LTREF,LTEMP
      INTEGER      IFCX
      CHARACTER*8  NOMPAV(1)
      REAL*8       VALPAV(1),FCX,VITE2,VP(3),ANG1(3),U(3),V(3)
      LOGICAL      NORMAL,GLOBAL,OKVENT
C     ------------------------------------------------------------------
      DATA         NOMPAR / 'X' , 'Y' , 'Z' , 'INST' /
      DATA         NOMPAV /'VITE'/
C     ------------------------------------------------------------------
      R8MIN = R8MIEM()
C
      NNO = 2
      NC  = 3
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
        CALL PSCAL(3,W2,W2,S)
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        CALL PSCAL(2,W2,W2,S)
      ENDIF
      XL = SQRT(S)
      IF( XL .EQ. 0.D0 ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE BARRE (TE0155)',
     +                  'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
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
         CALL RCVALA ( ZI(LMATER),'ELAS',0,' ',R8BID,1,'RHO',RHO,
     +                 CODRES, 'FM' )
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
         CALL TECACH(.FALSE.,.FALSE.,'PVITER',1,LFORC)
         IF ( LFORC .NE. 0 ) THEN
           IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
             CALL UTMESS('F','TE0155','OPTION NON DISPONIBLE')
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
         CALL TECACH ( .FALSE., .FALSE., 'PTEMPSR', 1, ITEMPS )
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
           CALL PSCAL(3,W2,W2,S)
           S2=1.D0/S
           CALL PSCAL(3,QG(1),QG(1),S)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(1),U)
             CALL PSCAL(3,U,U,S)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(1))
           ENDIF
           CALL PSCAL(3,QG(4),QG(4),S)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(4),U)
             CALL PSCAL(3,U,U,S)
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
     +                 IRET)
           CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPA2,QG(I+3),
     +                 IRET)
42       CONTINUE
C
         IF ( NORMAL ) THEN
           CALL PSCAL(3,W2,W2,S)
           S2=1.D0/S
           CALL PSCAL(3,QG(1),QG(1),S)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(1),U)
             CALL PSCAL(3,U,U,S)
             S3 = SQRT(S)
             S5 = S3*SQRT(S2)/S4
             CALL PROVEC(U,W2,V)
             CALL PSCVEC(3,S2,V,U)
             CALL PSCVEC(3,S5,U,QG(1))
           ENDIF

           CALL PSCAL(3,QG(4),QG(4),S)
           S4 = SQRT(S)
           IF ( S4 .GT. R8MIN) THEN
             CALL PROVEC(W2,QG(4),U)
             CALL PSCAL(3,U,U,S)
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
         CALL PSCAL(3,W2,W2,S)
         S2=1.D0/S

         CALL PSCAL(3,QG(1),QG(1),S)
         S4 = SQRT(S)
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,QG(1),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           CALL PSCAL(3,VP,VP,VITE2)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH(.TRUE.,.FALSE.,'PVENTCX',1,IFCX)
             IF ( IFCX .LE. 0 ) GOTO  999
             IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
             CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
             FCX = FCX / VALPAV(1)
           ENDIF
         ENDIF
         CALL PSCVEC(3,FCX,VP,QG(1))

         CALL PSCAL(3,QG(4),QG(4),S)
         S4 = SQRT(S)
         FCX = 0.0D0
         IF ( S4 .GT. R8MIN) THEN
           CALL PROVEC(W2,QG(4),U)
           CALL PROVEC(U,W2,V)
           CALL PSCVEC(3,S2,V,VP)
C          NORME DE LA VITESSE PERPENDICULAIRE
           CALL PSCAL(3,VP,VP,VITE2)
           VALPAV(1) = SQRT( VITE2 )
           IF ( VALPAV(1) .GT. R8MIN ) THEN
C            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
             CALL TECACH(.TRUE.,.FALSE.,'PVENTCX',1,IFCX)
             IF ( IFCX .LE. 0 ) GOTO  999
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
         CALL RCVALA(ZI(LMATER),'ELAS',0,' ',R8BID,1,'E',E,CODRES,'FM')
         CALL RCVALA(ZI(LMATER),'ELAS',0,' ',R8BID,1,'ALPHA',
     +                                              ALPHAT,CODRES,'FM')
C
C        TEMPERATURE DE REFERENCE
         CALL JEVECH('PTEREF','L',LTREF)
C
C        TEMPERATURE EFFETIVE
         CALL JEVECH('PTEMPER','L',LTEMP)
C
         TEMP = ZR(LTEMP) - ZR(LTREF)
C
C        TERME DE LA MATRICE ELEMENTAIRE
         XRIG = E * A / XL
C
C        DEPLACEMENT INDUIT PAR LA TEMPERATURE
         XDEP = ALPHAT * TEMP * XL
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
      ENDIF
C
      GOTO 1000
998   CONTINUE
      CALL UTMESS('F','TE0155',
     &        'SEULES LES FORCES SUIVEUSES DE TYPE VENT DEFINIES '//
     &        'PAR UN EVOL_CHAR SONT AUTORISEES')

999   CONTINUE
      CALL UTMESS('F','TE0155',
     &        'UN CHAMP DE VITESSE DE VENT EST IMPOSE SANS DONNER '//
     &        'UN CX DEPENDANT DE LA VITESSE SUR UNE DES BARRES.')

 1000 CONTINUE
      END
