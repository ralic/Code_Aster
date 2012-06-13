        SUBROUTINE BETJPL ( MOD,NMAT,MATER,SIG,VIN,ELGEOM,DSDE)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C       BETON_DOUBLE_DP: LOI ELASTO PLASTIQUE AVEC DOUBLE CRITERE DE
C       PLASTICITE AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTO_PLASTIQUE
C       EN VITESSE A T OU T+DT
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           TEMP   :  TEMPERATURE
C           MATER  :  COEFFICIENTS MATERIAU
C           SIG    :  CONTRAINTES
C           VIN    :  VARIABLES INTERNES
C           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                     AUX LOIS DE COMPORTEMENT
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
      INCLUDE 'jeveux.h'
        INTEGER         NMAT, NSEUIL
        REAL*8          UN , ZERO , RAC2 , DEUX , TROIS
        PARAMETER       ( DEUX = 2.D0   )
        PARAMETER       ( TROIS = 3.D0   )
        PARAMETER       ( UN   =  1.D0   )
        PARAMETER       ( ZERO =  0.D0   )
C
        REAL*8          VIN(*),SIG(6)
        REAL*8          HOOK(6,6), DSDE(6,6), VTMP(6)
C
        REAL*8          MATER(NMAT,2) , ELGEOM(*)
C
        CHARACTER*8     MOD
        REAL*8          TRAV1(6), TRAV2(6), PI0(6), DEV(6)
        REAL*8          SIGEQ , P, MATR1(6,6)
        REAL*8          FC , FT  , BETA, KUC, KUT, KE
        REAL*8          A, B, C, D
        REAL*8          PC, PT, DFCDLC, DFTDLT, DFCDS(6), DFTDS(6)
        REAL*8          COEF1, COEF2, HDFCDS(6), HDFTDS(6)
        REAL*8          CC, CCC, TT, TTT, CT, TC, DISCR
        INTEGER         IADZI, IAZK24
        CHARACTER*8     NOMAIL
C       ----------------------------------------------------------------
        DATA  PI0       /UN     , UN    , UN    , ZERO , ZERO , ZERO/
C       ----------------------------------------------------------------
C
C
C --- INITIALISATION
C
      KUC = 0
      KUT = 0
      RAC2   = SQRT (DEUX)
      BETA   = MATER(3,2)
C
      A = RAC2 * (BETA - UN) / (DEUX * BETA - UN)
      B = RAC2 / TROIS * BETA / (DEUX * BETA - UN)
      C = RAC2
      D = DEUX * RAC2 / TROIS
C
      CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , HOOK )
C
      PC  = VIN(1)
      PT  = VIN(2)
      NSEUIL = INT (VIN(4) + 0.5D0)
C
C --- CONTRAINTE EQUIVALENTE
C
      CALL LCDEVI ( SIG , DEV )
      CALL LCPRSC ( DEV , DEV , P)
      SIGEQ = SQRT (1.5D0 * P)
      IF(SIGEQ.EQ.ZERO) THEN
         CALL TECAEL ( IADZI, IAZK24 )
         NOMAIL = ZK24(IAZK24-1+3)(1:8)
         CALL U2MESK('A','ALGORITH_48',1,NOMAIL)
         SIGEQ = 1.D0
      ENDIF
C
C --  CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
C
      CALL BETFPP ( MATER, NMAT, ELGEOM, PC, PT, NSEUIL, FC, FT,
     &              DFCDLC, DFTDLT, KUC, KUT, KE)
C
C --- DERIVEES DU CRITERE EN COMPRESSION
C
      IF(NSEUIL.EQ.1 .OR. NSEUIL.EQ.3) THEN
C
         COEF1 = UN / (RAC2 * B * SIGEQ)
         COEF2 = A / (TROIS * B)
         CALL LCPRSV (COEF1 , DEV , TRAV1)
         CALL LCPRSV (COEF2 , PI0 , TRAV2)
         CALL LCSOVE (TRAV1 , TRAV2 , DFCDS)
       ENDIF
C
C --- DERIVEES DU CRITERE EN TRACTION
C
      IF(NSEUIL.EQ.2 .OR. NSEUIL.EQ.3) THEN
C
         COEF1 = UN / (RAC2 * D * SIGEQ)
         COEF2 = C / (TROIS * D)
         CALL LCPRSV (COEF1 , DEV , TRAV1)
         CALL LCPRSV (COEF2 , PI0 , TRAV2)
         CALL LCSOVE (TRAV1 , TRAV2 , DFTDS)
      ENDIF
C
C --- DERIVEES DU CRITERE EN TRACTION AVEC PROJECTION AU SOMMET DES
C     CONES DE TRACTION ET DE COMPRESSION
C
      IF(NSEUIL.EQ.11 .OR. NSEUIL.EQ.33) THEN
         COEF2 = A / (TROIS * B)
         CALL LCPRSV (COEF2 , PI0 , DFCDS)
      ENDIF
      IF(NSEUIL.EQ.22 .OR. NSEUIL.EQ.33) THEN
         COEF2 = C / (TROIS * D)
         CALL LCPRSV (COEF2 , PI0 , DFTDS)
      ENDIF
C
C --- MATRICE DE COMPORTEMENT TANGENT
C
      IF (NSEUIL.EQ.3 .OR. NSEUIL.EQ.33) THEN
        CALL LCPRMV ( HOOK  , DFCDS  , HDFCDS  )
        CALL LCPRMV ( HOOK  , DFTDS  , HDFTDS  )
        CALL LCPRSC ( DFCDS   , HDFCDS  , CC  )
        CALL LCPRSC ( DFTDS   , HDFCDS  , TC  )
        CALL LCPRSC ( DFCDS   , HDFTDS  , CT  )
        CALL LCPRSC ( DFTDS   , HDFTDS  , TT  )
        CCC = CC + DFCDLC
        TTT = TT + DFTDLT
        DISCR = -UN / (CCC*TTT - CT*TC)
        CALL LCPRSV ( (DISCR*TTT)     , HDFCDS  , VTMP )
        CALL LCPRTE ( HDFCDS          , VTMP    , DSDE )
        CALL LCPRSV ( (DISCR*CCC)     , HDFTDS  , VTMP )
        CALL LCPRTE ( HDFTDS          , VTMP    , MATR1 )
        CALL LCSOMA ( MATR1           , DSDE    , DSDE )
        CALL LCPRSV ( DISCR*CT        , HDFTDS  , VTMP )
        CALL LCPRTE ( HDFCDS          , VTMP    , MATR1 )
        CALL LCDIMA ( DSDE            , MATR1   , DSDE )
        CALL LCPRSV ( DISCR*TC        , HDFCDS  , VTMP )
        CALL LCPRTE ( HDFTDS          , VTMP    , MATR1 )
        CALL LCDIMA ( DSDE            , MATR1   , DSDE )
        CALL LCSOMA ( HOOK            , DSDE    , DSDE )
      ENDIF
C
      IF (NSEUIL.EQ.2 .OR. NSEUIL.EQ.22) THEN
        CALL LCPRMV ( HOOK     , DFTDS   , HDFTDS )
        CALL LCPRSC ( DFTDS    , HDFTDS  , TT  )
        TTT = TT + DFTDLT
        DISCR = -UN / TTT
        CALL LCPRSV ( DISCR    , HDFTDS  , VTMP )
        CALL LCPRTE ( HDFTDS   , VTMP    , DSDE )
        CALL LCSOMA ( HOOK     , DSDE    , DSDE )
      ENDIF
C
      IF (NSEUIL.EQ.1 .OR. NSEUIL.EQ.11) THEN
        CALL LCPRMV ( HOOK     , DFCDS   , HDFCDS )
        CALL LCPRSC ( DFCDS    , HDFCDS  , CC  )
        CCC = CC + DFCDLC
        DISCR = -UN / CCC
        CALL LCPRSV ( DISCR    , HDFCDS  , VTMP )
        CALL LCPRTE ( HDFCDS   , VTMP    , DSDE )
        CALL LCSOMA ( HOOK     , DSDE    , DSDE )
      ENDIF
C
      END
