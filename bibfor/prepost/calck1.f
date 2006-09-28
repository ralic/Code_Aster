      SUBROUTINE CALCK1( NOREV, NOMDB, SIGMRV, SIGMDB, TBSCRV, TBSCMB,
     &                   PRODEF, LONDEF, DEKLAG, LREV, K1A, K1B )
C
      IMPLICIT     NONE
      INTEGER      NOREV, NOMDB
      REAL*8       PRODEF, LONDEF, DEKLAG, LREV, K1A, K1B
      CHARACTER*19 SIGMRV, SIGMDB, TBSCRV, TBSCMB
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES
C ======================================================================
C IN  : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
C --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
C --- : SIGMRV : CONTRAINTES COTE REVETEMENT ---------------------------
C --- : SIGMDB : CONTRAINTES COTE METAL DE BASE ------------------------
C --- : TBSCRV : ABSCISSES CURVILIGNES COTE REVETEMENT -----------------
C --- : TBSCMB : ABSCISSES CURVILIGNES COTE METAL DE BASE --------------
C --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
C --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
C --- : LREV   : LONGUEUR DU REVETEMENT --------------------------------
C --- : DEKLAG : DECALAGE DU DEFAUT COTE REVETEMENT (TOUJOURS NEGATIF) -
C OUT : K1A    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE A -----------
C --- : K1B    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE B -----------
C ======================================================================
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
      INTEGER JSIGMR, JSIGMB, JABSRV, JABSMB, IFIC
      REAL*8  ZERO, UN, DEUX, RAPPO, GAMMA1, GAMMA2, FA, FB, FAB
      REAL*8  A, B, PI, R8PI, VALMIN, ALPHA, BETA, Z, Z2, Z3, Z4, Z5
      REAL*8  GAMX, GAMY, R8PREM, LDEFO, RTOLE
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( ZERO   =  0.0D0 )
      PARAMETER       ( UN     =  1.0D0 )
      PARAMETER       ( DEUX   =  2.0D0 )
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- INITIALISATIONS DES VECTEURS -------------------------------------
C ======================================================================
      CALL JEVEUO ( TBSCRV, 'L', JABSRV )
      CALL JEVEUO ( TBSCMB, 'L', JABSMB )
      CALL JEVEUO ( SIGMRV, 'L', JSIGMR )
      CALL JEVEUO ( SIGMDB, 'L', JSIGMB )
C ======================================================================
C --- INITIALISATIONS DES VARIABLES REPRESENTANT LES FACTEURS ----------
C --- D'INTENSITE ------------------------------------------------------
C ======================================================================
      K1A   = ZERO
      K1B   = ZERO
      RTOLE = 1.0D-10
C ======================================================================
C --- INITIALISATIONS DES VARIABLES NECESSAIRE AU CALCUL ---------------
C ======================================================================
      LDEFO = ZERO
      A     = PRODEF/DEUX
      B     = LONDEF/DEUX
      PI    = R8PI()
C ======================================================================
C --- VERIFICATION DE LA COHERENCE DE LA PROFONDEUR DU DEFAUT ET -------
C --- DES ABSCISSES CURVILIGNES COTE REVETEMENT ET COTE METAL DE BASE --
C ======================================================================
      LDEFO = ZR(JABSRV+NOREV-1) + ZR(JABSMB+NOMDB-1)
      IF ( ABS(LDEFO - PRODEF) .GT. RTOLE ) THEN
         CALL U2MESS('F','PREPOST_5')
      ENDIF
C ======================================================================
C --- CALCULS DES FACTEURS D'INTENSITE DE CONTRAINTES COTE REVETEMENT --
C ======================================================================
      DO 10 IFIC = 1, NOREV-1
         ALPHA  = ( ZR(JSIGMR+IFIC) - ZR(JSIGMR+IFIC-1) ) /
     &            ( ZR(JABSRV+IFIC) - ZR(JABSRV+IFIC-1) )
         BETA   = ZR(JSIGMR+IFIC-1) - ALPHA * ( ZR(JABSRV+IFIC-1) - A )
         GAMX   = ( ZR(JABSRV+IFIC-1) - A )
         GAMY   = SQRT( ABS(A*A - GAMX*GAMX) )
         IF ( GAMY.LE.R8PREM() ) THEN
            IF ( GAMX.LT.ZERO ) THEN
               GAMMA1 = - PI / DEUX
            ELSE
               GAMMA1 =   PI / DEUX
            ENDIF
         ELSE
            GAMMA1 = ATAN2( GAMX/GAMY , UN )
         ENDIF
         GAMX   = ( ZR(JABSRV+IFIC  ) - A )
         GAMY   = SQRT( ABS(A*A - GAMX*GAMX) )
         IF ( GAMY.LE.R8PREM() ) THEN
            IF ( GAMX.LT.ZERO ) THEN
               GAMMA2 = - PI / DEUX
            ELSE
               GAMMA2 =   PI / DEUX
            ENDIF
         ELSE
            GAMMA2 = ATAN2( GAMX/GAMY , UN )
         ENDIF
         K1A    = K1A + (BETA-ALPHA*A/2) * (GAMMA2-GAMMA1) +
     &           (BETA-ALPHA*A) * (COS(GAMMA2)-COS(GAMMA1)) +
     &            ALPHA*A*(SIN(2*GAMMA2)-SIN(2*GAMMA1))/4
         K1B    = K1B + (BETA+ALPHA*A/2) * (GAMMA2-GAMMA1) -
     &           (BETA+ALPHA*A) * (COS(GAMMA2)-COS(GAMMA1)) -
     &            ALPHA*A*(SIN(2*GAMMA2)-SIN(2*GAMMA1))/4
 10   CONTINUE
C ======================================================================
C --- CALCULS DES FACTEURS D'INTENSITE DE CONTRAINTES COTE METAL DE BASE
C ======================================================================
      DO 20 IFIC = 1, NOMDB-1
         ALPHA  = ( ZR(JSIGMB+IFIC) - ZR(JSIGMB+IFIC-1) ) /
     &            ( ZR(JABSMB+IFIC) - ZR(JABSMB+IFIC-1) )
         BETA   = ZR(JSIGMB+IFIC-1) -
     &                        ALPHA * ( ZR(JABSMB+IFIC-1) - DEKLAG - A )
         GAMX   = ( ZR(JABSMB+IFIC-1) - DEKLAG - A )
         GAMY   = SQRT( ABS(A*A - GAMX*GAMX) )
         IF ( GAMY.LE.R8PREM() ) THEN
            IF ( GAMX.LT.ZERO ) THEN
               GAMMA1 = - PI / DEUX
            ELSE
               GAMMA1 =   PI / DEUX
            ENDIF
         ELSE
            GAMMA1 = ATAN2( GAMX/GAMY , UN )
         ENDIF
         GAMX   = ( ZR(JABSMB+IFIC  ) - DEKLAG - A )
         GAMY   = SQRT( ABS(A*A - GAMX*GAMX) )
         IF ( GAMY.LE.R8PREM() ) THEN
            IF ( GAMX.LT.ZERO ) THEN
               GAMMA2 = - PI / DEUX
            ELSE
               GAMMA2 =   PI / DEUX
            ENDIF
         ELSE
            GAMMA2 = ATAN2( GAMX/GAMY , UN )
         ENDIF
         K1A    = K1A + (BETA-ALPHA*A/2) * (GAMMA2-GAMMA1) +
     &           (BETA-ALPHA*A) * (COS(GAMMA2)-COS(GAMMA1)) +
     &            ALPHA*A*(SIN(2*GAMMA2)-SIN(2*GAMMA1))/4
         K1B    = K1B + (BETA+ALPHA*A/2) * (GAMMA2-GAMMA1) -
     &           (BETA+ALPHA*A) * (COS(GAMMA2)-COS(GAMMA1)) -
     &            ALPHA*A*(SIN(2*GAMMA2)-SIN(2*GAMMA1))/4
 20   CONTINUE
      K1A = K1A * SQRT(A/PI)
      K1B = K1B * SQRT(A/PI)
C ======================================================================
C --- CORRECTION PAR LES FACTEURS DE BORDS -----------------------------
C ======================================================================
      Z  = A  / (A + LREV + DEKLAG)
      Z2 = Z  * Z
      Z3 = Z2 * Z
      Z4 = Z3 * Z
      Z5 = Z4 * Z
      FA = 0.998742D0 + 0.142801D0*Z - 1.133379D0*Z2 + 5.491256D0*Z3 -
     &     8.981896D0*Z4 + 5.765252D0*Z5
      IF (Z.LE.(0.92D0)) THEN
         FB = 1.0D0 - 0.012328D0*Z+ 0.395205D0*Z2 - 0.527964D0*Z3 +
     &        0.432714D0*Z4
      ELSE
         FB = - 414.20286D0 + 1336.75998D0*Z - 1436.1197D0*Z2 +
     &          515.14949D0*Z3
      ENDIF
C ======================================================================
C --- CORRECTION PAR LES FACTEURS D'ELLIPTICITE ------------------------
C ======================================================================
      RAPPO = A/B
      IF ( A.LE.B ) THEN
         FAB = 1.0D0 / SQRT(1.0D0+1.464D0*(RAPPO**1.65D0))
      ELSE
         FAB = 1.0D0 /
     &         ( RAPPO * SQRT(1.0D0+1.464D0*((1.0D0/RAPPO)**1.65D0)))
      ENDIF
      K1A = K1A * FA * FAB
      K1B = K1B * FB * FAB
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
