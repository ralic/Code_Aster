      SUBROUTINE FORME3(M,TYPEMA,W,NNO,NCMP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C A_UTIL
C ----------------------------------------------------------------------
C        CALCUL DES DERIVEES TROISIEMES DES FONCTIONS DE FORME
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8      M(*)        : POINT SUR MAILLE DE REFERENCE (X,[Y],[Z])
C CHARACTER*8 TYPEMA      : TYPE DE LA MAILLE
C
C VARIABLES DE SORTIE
C REAL*8      W(NCMP,NNO) : DERIVEES 3IEMES DES FONCTIONS DE FORME EN M
C                          ( D3W1/DX2DY, D3W1/DXDY2, [D3W1/DX2DZ],
C                           [D3W1/DY2DZ], [D3W1/DXDZ2], [D3W1/DYDZ2],
C                            D3W2/DX2DY, ...)
C INTEGER     NNO         : NOMBRE DE NOEUDS
C INTEGER     NCMP        : NOMBRE DE COMPOSANTES
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      REAL*8      M(*),W(*),X,Y
      INTEGER     NNO,NCMP

      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        NCMP = 1
        IF (TYPEMA(5:5).EQ.'7') THEN
          W(1) = -6.D0
          W(2) = -6.D0
          W(3) = -6.D0
          W(4) = 24.D0
          W(5) = 24.D0
          W(6) = 24.D0
          W(7) = -54.D0
          NNO = 7
        ELSE
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        IF (TYPEMA(5:5).EQ.'6') THEN
          NCMP = 1
          W(1) = -0.5D0
          W(2) = -0.5D0
          W(3) =  0.5D0
          W(4) =  0.5D0
          W(5) =   1.D0
          W(6) =  -1.D0
          NNO = 6
        ELSEIF (TYPEMA(5:5).EQ.'8') THEN
          NCMP = 2
          W(1)  = -0.5D0
          W(2)  = -0.5D0
          W(3)  = -0.5D0
          W(4)  =  0.5D0
          W(5)  =  0.5D0
          W(6)  =  0.5D0
          W(7)  =  0.5D0
          W(8)  = -0.5D0
          W(9)  =   1.D0
          W(10) =   0.D0
          W(11) =   0.D0
          W(12) =  -1.D0
          W(13) =  -1.D0
          W(14) =   0.D0
          W(15) =   0.D0
          W(16) =   1.D0
          NNO = 8
        ELSEIF (TYPEMA(5:5).EQ.'9') THEN
          NCMP = 2
          X = M(1)
          Y = M(2)
          W(1) = Y - 0.5D0
          W(2) = X - 0.5D0
          W(3) = Y - 0.5D0
          W(4) = X + 0.5D0
          W(5) = Y + 0.5D0
          W(6) = X + 0.5D0
          W(7) = Y + 0.5D0
          W(8) = X - 0.5D0
          X = 2.D0 * X
          Y = 2.D0 * Y
          W(9) = 1.D0 - Y
          W(10) = -X
          W(11) = -Y
          W(12) = -1.D0 - X
          W(13) = -1.D0 - Y
          W(14) = -X
          W(15) = -Y
          W(16) = 1.D0 - X
          W(17) = 2.D0*Y
          W(18) = 2.D0*X
          NNO = 9
        ELSE
          GOTO 10
        ENDIF

      ELSE
        GOTO 10
      ENDIF

      GOTO 20

 10   CONTINUE

      CALL U2MESK('F','CALCULEL2_31',1,TYPEMA)

 20   CONTINUE

      END
