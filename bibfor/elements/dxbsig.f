      SUBROUTINE DXBSIG (NOMTE, XYZL , PGL , SIGMA, BSIGMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 NOMTE
      REAL*8       XYZL(3,1) , PGL(3,1)
      REAL*8       SIGMA(1)  , BSIGMA(1)
C     ------------------------------------------------------------------
C --- CALCUL DES FORCES INTERNES B*SIGMA AUX NOEUDS DE L'ELEMENT
C --- DUES AU CHAMP DE CONTRAINTES SIGMA DEFINI AUX POINTS
C --- D'INTEGRATION POUR LES ELEMENTS : DST, DKT, DSQ, DKQ, Q4G
C     ------------------------------------------------------------------
C     IN  NOMTE         : NOM DU TYPE D'ELEMENT
C     IN  XYZL(3,NNO)   : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
C                         DANS LE REPERE LOCAL DE L'ELEMENT
C     IN  PGL(3,3)      : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
C                         LOCAL
C     IN  SIGMA(1)      : CONTRAINTES GENERALISEES DEFINIES AUX POINTS
C                         D'INTEGRATION DE L'ELEMENT
C     OUT BSIGMA(1)     : FORCES INTERNES AUX NOEUDS DE L'ELEMENT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      PARAMETER     (NBSIG   = 8)
      PARAMETER     (LGLIGB  = 24)
C
      CHARACTER*24  DESR
      REAL*8        BMAT(NBSIG,LGLIGB)
      REAL*8        BSILOC(LGLIGB), JACGAU, DISTN, CARA(25)
C     ------------------------------------------------------------------
C
      ZERO   = 0.0D0
C
      IF (NOMTE(1:8) .EQ.'MEDKTR3 '.OR.NOMTE(1:8) .EQ.'MEDSTR3 '.OR.
     &    NOMTE(1:8) .EQ.'MEGRDKT '.OR.NOMTE(1:8) .EQ.'MEDKTG3 ') THEN
          NPG   = 3
          NNO   = 3
C
C ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE
C      -------------------------------------------------
         CALL GTRIA3 (XYZL , CARA )
C
      ELSEIF (NOMTE(1:8) .EQ.'MEDKQU4 '.OR.NOMTE(1:8) .EQ.'MEDSQU4 '
     &.OR.NOMTE(1:8) .EQ.'MEQ4QU4 '.OR.NOMTE(1:8) .EQ.'MEDKQG4 ') THEN
          NPG   = 4
          NNO   = 4
C
C ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE
C      ---------------------------------------------------
         CALL GQUAD4 (XYZL , CARA )
C
      ELSE
         CALL U2MESK('F','ELEMENTS_14',1,NOMTE(1:8))
      ENDIF
C
C --- INITIALISATIONS :
C     -----------------
      DO 10 I = 1, LGLIGB
         BSILOC(I) = ZERO
         BSIGMA(I) = ZERO
         DO 20 J = 1, NBSIG
            BMAT(J,I) = ZERO
 20      CONTINUE
 10   CONTINUE
C
C --- CALCUL DE SOMME_ELEMENT(BT_SIGMA) :
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
      DO 30 IGAU = 1, NPG
C
C  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
C  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION
C  --      COURANT : (EPS_1) = (B)*(UN)
C          ----------------------------
C
             CALL DXBMAT(NOMTE, CARA, XYZL, PGL, IGAU, JACGAU, BMAT)
C
C  --        CALCUL DU PRODUIT (BT)*(SIGMA)*JACOBIEN*POIDS
C          ---------------------------------------------
C
           CALL BTSIG(LGLIGB,NBSIG,JACGAU,BMAT,SIGMA(1+8*(IGAU-1)),
     &               BSILOC)
  30  CONTINUE
C
C --- PERMUTATION DES COMPOSANTES EN BETA_X ET  BETA_Y
C ---                             EN TETA_Y ET -TETA_X
C     -----------------------------------------------
      DO 40 I = 1, NNO
           BSIVAR             =  BSILOC(4+6*(I-1))
           BSILOC(4+6*(I-1))  = -BSILOC(5+6*(I-1))
           BSILOC(5+6*(I-1))  =  BSIVAR
 40   CONTINUE
C
C --- PASSAGE DU VECTEUR(BT_SIGMA) DU REPERE LOCAL AU REPERE GLOBAL :
C --- (LE RESULTAT EST ICI LE VECTEUR BSIGMA)
C      --------------------------------------
      CALL UTPVLG ( NNO , 6 , PGL , BSILOC , BSIGMA )
C
      END
