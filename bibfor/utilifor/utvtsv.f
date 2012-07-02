      SUBROUTINE UTVTSV (RAZ,N,S,V,VTSV)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
      CHARACTER*(*) RAZ
      INTEGER  N
      REAL*8   S(*) , V(*) , VTSV
C     ------------------------------------------------------------------
C     PRODUIT VT . S . V - S CARREE SYMETRIQUE - V VECTEUR
C     ------------------------------------------------------------------
CIN   K4  RAZ  'ZERO' : ON FAIT VTSV = 0.   + VT*S*V
C              'CUMU' : ON FAIT VTSV = VTSV + VT*S*V
CIN   I   N    ORDRE DE S ET V
CIN   R   S    MATRICE S           (N*(N+1)/2)
CIN   R   V    VECTEUR V           (N)
COUT  R   VTSV PRODUIT VT . S . V
C     ------------------------------------------------------------------
      CHARACTER*4   RAZ2
      INTEGER I ,IK ,J ,K ,L,K1,K2
C-----------------------------------------------------------------------
      K1(I,J)=J*(J-1)/2+I
      K2(I,J)=I*(I-1)/2+J
C-----------------------------------------------------------------------
      RAZ2=RAZ
      IF (RAZ2.EQ.'ZERO') VTSV = 0.D0
C
      DO 25 K = 1 , N
         DO 20 L = 1 , N
            IK = K1(K,L)
            IF ( K .GT. L ) IK = K2(K,L)
            VTSV = VTSV + S(IK) * V(K) * V(L)
   20    CONTINUE
   25 CONTINUE
      END
