      SUBROUTINE PROJAX( VECPG, NBVEC, NBORDR, PROAXE, IFLAG,
     &                   RMIMA, RAXE1, RAXE2 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/11/2003   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, IFLAG(NBVEC)
      REAL*8        VECPG(2*NBVEC*NBORDR), RMIMA(4*NBVEC)
      REAL*8        RAXE1(NBVEC*NBORDR), RAXE2(NBVEC*NBORDR)
      CHARACTER*16  PROAXE
C ----------------------------------------------------------------------
C BUT: PROJETER SUR UN OU DEUX AXES LES POINTS REPRESANTANT LE
C      CISAILLEMENT TAU DANS LE PLAN u, v.
C ----------------------------------------------------------------------
C ARGUMENTS:
C VECPG     IN   R  : VECTEUR DE TRAVAIL CONTENANT LES
C                     COMPOSANTES u ET v DU VECTEUR TAU (CISAILLEMENT),
C                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
C                     NUMEROS D'ORDRE.
C                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
C NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                     STRUCTURE DE DONNEES RESULTAT.
C PROAXE    IN   K16: TYPE DE PROJECTION (UN OU DEUX AXES).
C IFLAG     IN   I  : VECTEUR DE DRAPEAUX INDIQUANT :
C                      - IFLAG(i) = 0 --> CAS GENERAL
C                      - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES VERTICALEMENT.
C                      - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES HORIZONTALEMENT.
C                      - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         CONTENUS DANS UN CADRE DE
C                                         COTES INFERIEURS A EPSILO.
C RMIMA     IN   R  : VECTEUR CONTENANT LES COORDONNEES DES POINTS
C                     EXTREMES DU CADRE (UMIN, UMAX, VMIN, VMAX).
C                     POUR TOUS LES VECTEURS NORMAUX.
C RAXE1     OUT  R  : VECTEUR CONTENANT L'AMPLITUDE DES POINTS
C                     PROJETES SUR l'AXE 1.
C RAXE2     OUT  R  : VECTEUR CONTENANT L'LAMPLITUDE DES POINTS
C                     PROJETES SUR l'AXE 2.
C
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      N1, IVECT, IORDR
C
      REAL*8       UMIN, UMAX, VMIN, VMAX, UO, VO
      REAL*8       CSTA, CSTB, CSTK, UAXE2, VAXE2
      REAL*8       UI, VI, UIP, VIP, A1, B1, A2, B2
      REAL*8       UP, VP, VAL
C
C-----------------------------------------------------------------------
C234567                                                              012
C
      CALL JEMARQ()
C
      N1 = 0
C
      DO 10 IVECT=1, NBVEC
         UMIN = RMIMA(4*(IVECT-1) + 1)
         UMAX = RMIMA(4*(IVECT-1) + 2)
         VMIN = RMIMA(4*(IVECT-1) + 3)
         VMAX = RMIMA(4*(IVECT-1) + 4)
         UO = UMIN + (UMAX - UMIN)/2.0D0
         VO = VMIN + (VMAX - VMIN)/2.0D0
C
C 1. SI PROAXE == DEUX_AXES ET SI ON N'EST PAS DANS UN CAS PARTICULIER,
C    ON DETERMINE LE SECOND AXE
C
         IF ( (PROAXE .EQ. 'DEUX_AXES') .AND. 
     &        (IFLAG(IVECT) .EQ. 0) ) THEN
            CSTA = UMAX-UMIN
            CSTB = VMAX-VMIN
            CSTK = (CSTA*UO) + (CSTB*VO)
            UAXE2 = UO + 1.0D0
            VAXE2 = (CSTK - (CSTA*UAXE2))/CSTB
         ENDIF
C
C 2. PROJECTION SUR UN AXE OU DEUX AXES
C
         DO 20 IORDR=1, NBORDR
            N1 = N1 + 1
C
C 2.1 PROJECTION SUR L'AXE 1
C
            IF ( IFLAG(IVECT) .EQ. 0 ) THEN
C
               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
C
               CSTA = UMAX-UMIN
               CSTB = VMAX-VMIN
               CSTK = (CSTA*UI) + (CSTB*VI)
               UIP = UI + 1.0D0
               VIP = (CSTK - (CSTA*UIP))/CSTB
C
               A1 = (VMAX-VMIN)/(UMAX-UMIN)
               B1 = (UMAX*VMIN - UMIN*VMAX)/(UMAX-UMIN)
               A2 = (VIP-VI)/(UIP-UI)
               B2 = (UIP*VI - UI*VIP)/(UIP-UI)
C
               UP = (B2-B1)/(A1-A2)
               VP = (A1*B2 - A2*B1)/(A1-A2)
               VAL = SQRT((UP-UO)**2 + (VP-VO)**2)
               IF (UP .LT. UO) THEN
                  VAL = -VAL
               ENDIF
               RAXE1(N1) = VAL
C
            ELSEIF ( IFLAG(IVECT) .EQ. 1 ) THEN
C
               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
               UP = UI
               VP = VI
               VAL = SQRT((UP-UO)**2 + (VP-VO)**2)
               IF (VP .LT. VO) THEN
                  VAL = -VAL
               ENDIF
               RAXE1(N1) = VAL
C
            ELSEIF ( IFLAG(IVECT) .EQ. 2 ) THEN
C
               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
               UP = UI
               VP = VI
               VAL = SQRT((UP-UO)**2 + (VP-VO)**2)
               IF (UP .LT. UO) THEN
                  VAL = -VAL
               ENDIF
               RAXE1(N1) = VAL
C
            ENDIF
C
C 2.2 PROJECTION SUR L'AXE 2 SI DEMANDE ET POSSIBLE
C
            IF ( (PROAXE .EQ. 'DEUX_AXES') .AND.
     &           (IFLAG(IVECT) .EQ. 0) ) THEN
C
               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
C
               CSTA = UAXE2-UO
               CSTB = VAXE2-VO
               CSTK = (CSTA*UI) + (CSTB*VI)
               UIP = UI + 1.0D0
               VIP = (CSTK - (CSTA*UIP))/CSTB
C
               A1 = (VMAX-VMIN)/(UMAX-UMIN)
               B1 = (UMAX*VMIN - UMIN*VMAX)/(UMAX-UMIN)
               A2 = (VIP-VI)/(UIP-UI)
               B2 = (UIP*VI - UI*VIP)/(UIP-UI)
               UP = (B2-B1)/(A1-A2)
               VP = (A1*B2 - A2*B1)/(A1-A2)
C
               VAL = SQRT((UP-UO)**2 + (VP-VO)**2)
               IF (UP .GT. UO) THEN
                  VAL = -VAL
               ENDIF
               RAXE2(N1) = VAL
            ENDIF
C
 20      CONTINUE
 10   CONTINUE
C
      CALL JEDEMA()
      END
