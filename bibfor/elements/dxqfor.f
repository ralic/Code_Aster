      SUBROUTINE DXQFOR (GLOBAL , XYZL , PGL , FOR , VECL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      LOGICAL      GLOBAL
      REAL*8       XYZL(3,*) , PGL(3,*)
      REAL*8       FOR(6,*)
      REAL*8       VECL(*)
C     ------------------------------------------------------------------
C     CHARGEMENT FORCE_FACE DES ELEMENTS DE PLAQUE DKQ ET DSQ
C     ------------------------------------------------------------------
C     IN  GLOBAL : VARIABLE LOGIQUE DE REPERE GLOBAL OU LOCAL
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  FOR    : FORCE APPLIQUE SUR LA FACE
C     OUT VECL   : CHARGEMENT NODAL RESULTANT
C     ------------------------------------------------------------------
      REAL*8       AIRETR(4) , C1 , C2 , FNO(6,4,4)
      REAL*8       FX , FY , ALPHA, BETA , R8DGRD
      REAL*8       T2EV(4), T2VE(4), CARAQ4(25),C,S
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,INO ,IT ,J ,K ,NNO, JCARA
C-----------------------------------------------------------------------
      NNO = 4
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL , CARAQ4 )

      CALL JEVECH ('PCACOQU', 'L', JCARA)
      ALPHA = ZR(JCARA+1) * R8DGRD()
      BETA  = ZR(JCARA+2) * R8DGRD()
      CALL COQREP(PGL,ALPHA,BETA,T2EV,T2VE,C,S)
C
      IF (.NOT. GLOBAL) THEN
         DO 50 I = 1, NNO
            FX = FOR(1,I)
            FY = FOR(2,I)
            FOR(1,I) = T2EV(1)*FX + T2EV(3)*FY
            FOR(2,I) = T2EV(2)*FX + T2EV(4)*FY
            FX = FOR(4,I)
            FY = FOR(5,I)
            FOR(4,I) = T2EV(1)*FX + T2EV(3)*FY
            FOR(5,I) = T2EV(2)*FX + T2EV(4)*FY
   50    CONTINUE
      ENDIF
C
      DO 100 INO = 1, NNO
         AIRETR(INO) = CARAQ4(21+INO)
  100 CONTINUE
C
      DO 110 I = 1, 6
         DO 111 J = 1, NNO
            DO 112 K = 1, NNO
               FNO(I,J,K) = 0.D0
  112       CONTINUE
  111    CONTINUE
  110 CONTINUE
C
      DO 120 I = 1, 6*NNO
         VECL(I) = 0.D0
  120 CONTINUE
C
      C1 = 1.D0 /  6.D0
      C2 = 1.D0 / 12.D0
C
      DO 200 I = 1, 6
         FNO(I,1,1) = (C1*FOR(I,1)+C2*FOR(I,2)+C2*FOR(I,4)) * AIRETR(1)
         FNO(I,1,2) = (C2*FOR(I,1)+C1*FOR(I,2)+C2*FOR(I,4)) * AIRETR(1)
         FNO(I,1,4) = (C2*FOR(I,1)+C2*FOR(I,2)+C1*FOR(I,4)) * AIRETR(1)
         FNO(I,2,2) = (C1*FOR(I,2)+C2*FOR(I,3)+C2*FOR(I,1)) * AIRETR(2)
         FNO(I,2,3) = (C2*FOR(I,2)+C1*FOR(I,3)+C2*FOR(I,1)) * AIRETR(2)
         FNO(I,2,1) = (C2*FOR(I,2)+C2*FOR(I,3)+C1*FOR(I,1)) * AIRETR(2)
         FNO(I,3,3) = (C1*FOR(I,3)+C2*FOR(I,4)+C2*FOR(I,2)) * AIRETR(3)
         FNO(I,3,4) = (C2*FOR(I,3)+C1*FOR(I,4)+C2*FOR(I,2)) * AIRETR(3)
         FNO(I,3,2) = (C2*FOR(I,3)+C2*FOR(I,4)+C1*FOR(I,2)) * AIRETR(3)
         FNO(I,4,4) = (C1*FOR(I,4)+C2*FOR(I,1)+C2*FOR(I,3)) * AIRETR(4)
         FNO(I,4,1) = (C2*FOR(I,4)+C1*FOR(I,1)+C2*FOR(I,3)) * AIRETR(4)
         FNO(I,4,3) = (C2*FOR(I,4)+C2*FOR(I,1)+C1*FOR(I,3)) * AIRETR(4)
         DO 160 INO = 1, NNO
            DO 150 IT = 1, NNO
               VECL(I+6*(INO-1)) = VECL(I+6*(INO-1)) + FNO(I,IT,INO)
  150       CONTINUE
            VECL(I+6*(INO-1)) = VECL(I+6*(INO-1)) / 2.D0
  160    CONTINUE
  200 CONTINUE
C
      END
