      SUBROUTINE DXTFOR (GLOBAL, XYZL, PGL, FOR, VECL )
      IMPLICIT  NONE
      REAL*8          XYZL(3,*),PGL(3,*), FOR(6,*), VECL(*)
      LOGICAL         GLOBAL
C MODIF ELEMENTS  DATE 23/05/2011   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     CHARGEMENT FORCE_FACE DES ELEMENTS DE PLAQUE DKT ET DST
C     ------------------------------------------------------------------
C     IN  GLOBAL : VARIABLE LOGIQUE DE REPERE GLOBAL OU LOCAL
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  FOR    : FORCE APPLIQUEE SUR LA FACE
C     OUT VECL   : CHARGEMENT NODAL RESULTANT
C     ------------------------------------------------------------------
      INTEGER  I, NNO
      REAL*8   AIRE
      REAL*8   FX, FY, CARAT3(21), T2EV(4), T2VE(4)
C     ------------------------------------------------------------------
      NNO = 3
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )
      CALL DXREPE ( PGL, T2EV, T2VE)
C
      IF (.NOT.GLOBAL) THEN
        DO 10 I = 1,NNO
          FX = FOR(1,I)
          FY = FOR(2,I)
          FOR(1,I) = T2EV(1)*FX + T2EV(3)*FY
          FOR(2,I) = T2EV(2)*FX + T2EV(4)*FY
          FX = FOR(4,I)
          FY = FOR(5,I)
          FOR(4,I) = T2EV(1)*FX + T2EV(3)*FY
          FOR(5,I) = T2EV(2)*FX + T2EV(4)*FY
   10   CONTINUE
      END IF
C
      AIRE = CARAT3(8)
C
      DO 20 I = 1,6*NNO
        VECL(I) = 0.D0
   20 CONTINUE
C
      DO 30 I = 1,6
        VECL(I   ) = FOR(I,1)*AIRE/3.D0
        VECL(I+6 ) = FOR(I,2)*AIRE/3.D0
        VECL(I+12) = FOR(I,3)*AIRE/3.D0
   30 CONTINUE
C
      END
