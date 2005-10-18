      SUBROUTINE DXTFOR ( TYPELE, GLOBAL, XYZL, PGL, FOR, VECL )
      IMPLICIT  NONE
      REAL*8          XYZL(3,*),PGL(3,*), FOR(6,3), VECL(*)
      LOGICAL         GLOBAL
      CHARACTER*8     TYPELE
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     ------------------------------------------------------------------
C     CHARGEMENT FORCE_FACE DES ELEMENTS DE PLAQUE DKT ET DST
C     ------------------------------------------------------------------
C     IN  TYPELE : TYPE DE L'ELEMENT
C     IN  GLOBAL : VARIABLE LOGIQUE DE REPERE GLOBAL OU LOCAL
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  FOR    : FORCE APPLIQUEE SUR LA FACE
C     OUT VECL   : CHARGEMENT NODAL RESULTANT
C     ------------------------------------------------------------------
      INTEGER  I, NNO
      REAL*8   AIRE, C1, C2, L4, L5, L6
      REAL*8   A1, A2, A3, PI, R8PI, TRIGOM
      REAL*8   FX, FY, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
C     ------------------------------------------------------------------
      NNO = 3
      PI  = R8PI()
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )
      CALL DXREPE ( PGL, T2EV, T2VE, T1VE )
C
      IF (.NOT.GLOBAL) THEN
        DO 10 I = 1,NNO
          FX = FOR(1,I)
          FY = FOR(2,I)
          FOR(1,I) = FX*T2VE(1) + FY*T2VE(3)
          FOR(2,I) = FX*T2VE(2) + FY*T2VE(4)
          FX = FOR(4,I)
          FY = FOR(5,I)
          FOR(4,I) = FX*T2VE(1) + FY*T2VE(3)
          FOR(5,I) = FX*T2VE(2) + FY*T2VE(4)
   10   CONTINUE
      END IF
      L4 = CARAT3(13)
      L5 = CARAT3(14)
      L6 = CARAT3(15)
      AIRE = CARAT3(8)
C     ---- CALCUL DES ANGLES DU TRIANGLE ---------
      A1 = TRIGOM('ACOS', (L4*L4+L6*L6-L5*L5)/ (2.D0*L4*L6))
      A2 = TRIGOM('ACOS', (L4*L4+L5*L5-L6*L6)/ (2.D0*L4*L5))
      A3 = PI - A1 - A2
      A1 = A1/PI
      A2 = A2/PI
      A3 = A3/PI
C
      DO 20 I = 1,6*NNO
        VECL(I) = 0.D0
   20 CONTINUE
      C1 = 1.D0/2.D0
      C2 = 1.D0/4.D0
      DO 30 I = 1,6
        VECL(I   ) = (C1*FOR(I,1)+C2*FOR(I,2)+C2*FOR(I,3))*AIRE*A1
        VECL(I+6 ) = (C2*FOR(I,1)+C1*FOR(I,2)+C2*FOR(I,3))*AIRE*A2
        VECL(I+12) = (C2*FOR(I,1)+C2*FOR(I,2)+C1*FOR(I,3))*AIRE*A3
   30 CONTINUE
C
      END
