      SUBROUTINE VPZRBK (Z,H,D,MM,IZH,K,L)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  MM,IZH,K,L
      REAL*8             Z(IZH,1),H(IZH,1),D(1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
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
C     TRANSFORMATION ARRIERE POUR OBTENIR LES VECTEURS (ROUTINE ORTBAK)
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE 350
C     ------------------------------------------------------------------
      INTEGER            M,MA,I,J
      REAL*8             G,ZERO
C
      ZERO = 0.D0
      DO 30 M =  L-2, K , -1
         MA=M+1
         IF (H(MA,M).NE.ZERO) THEN
            DO 5 I=M+2, L
               D(I)=H(I,M)
    5       CONTINUE
            IF (MA.LE.L) THEN
               DO 25 J=1,MM
                  G=ZERO
                  DO 15 I=MA,L
                     G=G+D(I)*Z(I,J)
   15             CONTINUE
C
                  G = (G/D(MA))/H(MA,M)
                  DO 20 I=MA,L
                     Z(I,J)=Z(I,J)+G*D(I)
   20             CONTINUE
   25          CONTINUE
            ENDIF
         ENDIF
   30 CONTINUE
      END
